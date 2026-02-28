#!/usr/bin/env python3
"""Operational audit for XAU autobot (3-day default)."""

from __future__ import annotations

import argparse
import glob
import json
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Tuple


AUDIT_PROFILES: Dict[str, Dict[str, float]] = {
    "default": {
        "window_days": 3.0,
        "gap_reject_rate_min": 0.15,
        "gap_reject_rate_max": 0.45,
        "tp_sl_ratio_min": 0.70,
        "closed_per_day_min": 0.70,
        "expectancy_min_exclusive": 0.0,
    },
    "m45": {
        "window_days": 7.0,
        "gap_reject_rate_min": 0.10,
        "gap_reject_rate_max": 0.55,
        "tp_sl_ratio_min": 1.00,
        "closed_per_day_min": 0.20,
        "expectancy_min_exclusive": 0.0,
    },
    "h2": {
        "window_days": 7.0,
        "gap_reject_rate_min": 0.10,
        "gap_reject_rate_max": 0.60,
        "tp_sl_ratio_min": 0.85,
        "closed_per_day_min": 0.15,
        "expectancy_min_exclusive": 0.0,
    },
    "h3": {
        "window_days": 14.0,
        "gap_reject_rate_min": 0.05,
        "gap_reject_rate_max": 0.70,
        "tp_sl_ratio_min": 1.20,
        "closed_per_day_min": 0.07,
        "expectancy_min_exclusive": 0.0,
    },
    "h5": {
        "window_days": 14.0,
        "gap_reject_rate_min": 0.05,
        "gap_reject_rate_max": 0.70,
        "tp_sl_ratio_min": 1.00,
        "closed_per_day_min": 0.05,
        "expectancy_min_exclusive": 0.0,
    },
}


def _as_float(value: object, default: Optional[float] = None) -> Optional[float]:
    try:
        if value is None:
            return default
        return float(value)
    except (TypeError, ValueError):
        return default


def _as_int(value: object, default: int = 0) -> int:
    try:
        if value is None:
            return default
        return int(value)
    except (TypeError, ValueError):
        return default


def _normalize_profile(profile: str) -> str:
    key = str(profile or "default").strip().lower()
    if key not in AUDIT_PROFILES:
        raise ValueError(f"unsupported audit profile: {profile}")
    return key


def _resolve_profile(profile: str) -> Dict[str, float]:
    key = _normalize_profile(profile)
    return dict(AUDIT_PROFILES[key])


def _parse_utc(value: object) -> Optional[datetime]:
    text = str(value or "").strip()
    if not text:
        return None
    try:
        dt = datetime.fromisoformat(text.replace("Z", "+00:00"))
    except ValueError:
        return None
    if dt.tzinfo is None:
        return dt.replace(tzinfo=timezone.utc)
    return dt.astimezone(timezone.utc)


def _expand_globs(patterns: Iterable[str]) -> List[Path]:
    out: List[Path] = []
    for pattern in patterns:
        for candidate in sorted(glob.glob(pattern)):
            path = Path(candidate)
            if path.exists() and path.is_file():
                out.append(path)
    dedup: Dict[str, Path] = {}
    for path in out:
        dedup[str(path)] = path
    return list(dedup.values())


def _matches_run_filter(*, run_id_filter: str, path: Path, payload: Dict[str, Any]) -> bool:
    needle = str(run_id_filter or "").strip()
    if not needle:
        return True
    if needle in path.name:
        return True
    for key in ("run_id", "comment_prefix", "comment", "trial_run_id"):
        value = str(payload.get(key, ""))
        if needle in value:
            return True
    return False


def _iter_jsonl_objects(path: Path) -> Iterable[Tuple[Dict[str, Any], datetime]]:
    modified = datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc)
    with path.open("r", encoding="utf-8", errors="ignore") as f:
        for raw in f:
            line = raw.strip()
            if not line.startswith("{"):
                continue
            try:
                payload = json.loads(line)
            except json.JSONDecodeError:
                continue
            if not isinstance(payload, dict):
                continue
            ts = (
                _parse_utc(payload.get("timestamp_utc"))
                or _parse_utc(payload.get("timestamp"))
                or _parse_utc(payload.get("generated_at"))
                or modified
            )
            yield payload, ts


def _extract_runtime_snapshot(payload: Dict[str, Any]) -> Optional[Dict[str, int]]:
    metrics = payload.get("runtime_metrics")
    src = metrics if isinstance(metrics, dict) else payload
    signal_eval_count = _as_int(src.get("signal_eval_count"), default=-1)
    gap_reject_count = _as_int(src.get("gap_reject_count"), default=-1)
    if signal_eval_count < 0:
        signal_eval_count = _as_int(src.get("gate_check_count"), default=-1)
    if gap_reject_count < 0:
        gap_reject_count = _as_int(src.get("gate_reject_gap_count"), default=-1)
    if signal_eval_count < 0 or gap_reject_count < 0:
        return None
    signal_counts = src.get("signal_counts", {})
    if not isinstance(signal_counts, dict):
        signal_counts = {}
    return {
        "signal_eval_count": signal_eval_count,
        "gap_reject_count": gap_reject_count,
        "spread_reject_count": max(0, _as_int(src.get("spread_reject_count"), 0)),
        "session_reject_count": max(0, _as_int(src.get("session_reject_count"), 0)),
        "maxpos_reject_count": max(0, _as_int(src.get("maxpos_reject_count"), 0)),
        "gate_check_count": signal_eval_count,
        "gate_reject_gap_count": gap_reject_count,
        "buy": _as_int(signal_counts.get("BUY"), 0),
        "sell": _as_int(signal_counts.get("SELL"), 0),
        "hold": _as_int(signal_counts.get("HOLD"), 0),
    }


def _delta_with_reset(current: int, previous: int) -> int:
    if current >= previous:
        return current - previous
    return max(0, current)


def _summarize_runtime_snapshots(
    snapshots: List[Tuple[datetime, Dict[str, int]]],
    *,
    source_file_count: int,
    source: str = "",
) -> Dict[str, Any]:
    snapshots.sort(key=lambda item: item[0])
    signal_eval_count = 0
    gap_reject_count = 0
    spread_reject_count = 0
    session_reject_count = 0
    maxpos_reject_count = 0
    signal_counts = {"BUY": 0, "SELL": 0, "HOLD": 0}
    if len(snapshots) == 1:
        only = snapshots[0][1]
        signal_eval_count = int(max(0, only["signal_eval_count"]))
        gap_reject_count = int(max(0, only["gap_reject_count"]))
        spread_reject_count = int(max(0, only["spread_reject_count"]))
        session_reject_count = int(max(0, only["session_reject_count"]))
        maxpos_reject_count = int(max(0, only["maxpos_reject_count"]))
        signal_counts["BUY"] = int(max(0, only["buy"]))
        signal_counts["SELL"] = int(max(0, only["sell"]))
        signal_counts["HOLD"] = int(max(0, only["hold"]))
    elif len(snapshots) >= 2:
        prev = snapshots[0][1]
        for _ts, cur in snapshots[1:]:
            signal_eval_count += _delta_with_reset(cur["signal_eval_count"], prev["signal_eval_count"])
            gap_reject_count += _delta_with_reset(cur["gap_reject_count"], prev["gap_reject_count"])
            spread_reject_count += _delta_with_reset(cur["spread_reject_count"], prev["spread_reject_count"])
            session_reject_count += _delta_with_reset(cur["session_reject_count"], prev["session_reject_count"])
            maxpos_reject_count += _delta_with_reset(cur["maxpos_reject_count"], prev["maxpos_reject_count"])
            signal_counts["BUY"] += _delta_with_reset(cur["buy"], prev["buy"])
            signal_counts["SELL"] += _delta_with_reset(cur["sell"], prev["sell"])
            signal_counts["HOLD"] += _delta_with_reset(cur["hold"], prev["hold"])
            prev = cur

    gap_reject_rate: Optional[float] = None
    if signal_eval_count > 0:
        gap_reject_rate = float(gap_reject_count) / float(signal_eval_count)

    out = {
        "snapshot_count": len(snapshots),
        "signal_eval_count": signal_eval_count,
        "gap_reject_count": gap_reject_count,
        "spread_reject_count": spread_reject_count,
        "session_reject_count": session_reject_count,
        "maxpos_reject_count": maxpos_reject_count,
        "gate_check_count": signal_eval_count,
        "gate_reject_gap_count": gap_reject_count,
        "gap_reject_rate": gap_reject_rate,
        "signal_counts": signal_counts,
        "source_file_count": int(source_file_count),
    }
    if source:
        out["source"] = source
    return out


def load_runtime_metrics(
    *,
    runtime_globs: List[str],
    start_utc: datetime,
    end_utc: datetime,
    run_id_filter: str,
) -> Dict[str, Any]:
    snapshots: List[Tuple[datetime, Dict[str, int]]] = []
    paths = _expand_globs(runtime_globs)
    for path in paths:
        for payload, ts in _iter_jsonl_objects(path):
            if ts < start_utc or ts > end_utc:
                continue
            if not _matches_run_filter(run_id_filter=run_id_filter, path=path, payload=payload):
                continue
            snapshot = _extract_runtime_snapshot(payload)
            if snapshot is None:
                continue
            snapshots.append((ts, snapshot))
    return _summarize_runtime_snapshots(
        snapshots,
        source_file_count=len(paths),
        source="journal",
    )


def load_runtime_metrics_from_live_reports(
    *,
    live_globs: List[str],
    start_utc: datetime,
    end_utc: datetime,
    run_id_filter: str,
) -> Dict[str, Any]:
    snapshots: List[Tuple[datetime, Dict[str, int]]] = []
    paths = _expand_globs(live_globs)
    for path in paths:
        payload = _load_json_object(path)
        if payload is None:
            continue
        if not _matches_run_filter(run_id_filter=run_id_filter, path=path, payload=payload):
            continue
        ts = (
            _parse_utc(payload.get("timestamp"))
            or _parse_utc(payload.get("generated_at"))
            or datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc)
        )
        if ts < start_utc or ts > end_utc:
            continue
        snapshot = _extract_runtime_snapshot(payload)
        if snapshot is None:
            continue
        snapshots.append((ts, snapshot))
    return _summarize_runtime_snapshots(
        snapshots,
        source_file_count=len(paths),
        source="live_report.runtime_metrics",
    )


def resolve_runtime_metrics_with_fallback(
    *,
    runtime_globs: List[str],
    live_globs: List[str],
    start_utc: datetime,
    end_utc: datetime,
    run_id_filter: str,
) -> Tuple[Dict[str, Any], str, str]:
    journal_metrics = load_runtime_metrics(
        runtime_globs=runtime_globs,
        start_utc=start_utc,
        end_utc=end_utc,
        run_id_filter=run_id_filter,
    )
    journal_source = str(journal_metrics.get("source", "journal") or "journal")
    if int(journal_metrics.get("snapshot_count", 0)) > 0:
        return journal_metrics, journal_source, ""

    fallback = load_runtime_metrics_from_live_reports(
        live_globs=live_globs,
        start_utc=start_utc,
        end_utc=end_utc,
        run_id_filter=run_id_filter,
    )
    if int(fallback.get("snapshot_count", 0)) > 0:
        return fallback, str(fallback.get("source", "live_report.runtime_metrics")), "journal_snapshot_count_zero"
    return journal_metrics, journal_source, "journal_snapshot_count_zero"


def _load_json_object(path: Path) -> Optional[Dict[str, Any]]:
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return None
    if not isinstance(payload, dict):
        return None
    return payload


def _derive_closed_per_day(summary: Dict[str, Any], payload: Dict[str, Any]) -> Optional[float]:
    value = _as_float(summary.get("closed_per_day"), None)
    if value is not None:
        return value
    closed = _as_float(summary.get("closed_positions"), None)
    start_utc = _parse_utc(payload.get("start_utc"))
    end_utc = _parse_utc(payload.get("end_utc"))
    if closed is None or start_utc is None or end_utc is None or end_utc <= start_utc:
        return None
    window_days = float((end_utc - start_utc).total_seconds() / 86400.0)
    if window_days <= 0.0:
        return None
    return float(closed) / float(window_days)


def _derive_expectancy(summary: Dict[str, Any]) -> Optional[float]:
    value = _as_float(summary.get("expectancy"), None)
    if value is not None:
        return value
    win_rate = _as_float(summary.get("win_rate"), None)
    avg_win = _as_float(summary.get("avg_win"), None)
    avg_loss = _as_float(summary.get("avg_loss"), None)
    if win_rate is None or avg_win is None or avg_loss is None:
        return None
    return float(win_rate) * float(avg_win) + (1.0 - float(win_rate)) * float(avg_loss)


def _mean_optional(values: List[Optional[float]]) -> Optional[float]:
    nums = [float(v) for v in values if v is not None]
    if not nums:
        return None
    return float(sum(nums) / float(len(nums)))


def load_live_metrics(
    *,
    live_globs: List[str],
    start_utc: datetime,
    end_utc: datetime,
    run_id_filter: str,
) -> Dict[str, Any]:
    daily_latest: Dict[str, Tuple[datetime, Dict[str, Any]]] = {}
    paths = _expand_globs(live_globs)
    for path in paths:
        payload = _load_json_object(path)
        if payload is None:
            continue
        if not _matches_run_filter(run_id_filter=run_id_filter, path=path, payload=payload):
            continue
        ts = (
            _parse_utc(payload.get("timestamp"))
            or _parse_utc(payload.get("generated_at"))
            or datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc)
        )
        if ts < start_utc or ts > end_utc:
            continue
        day_key = ts.date().isoformat()
        current = daily_latest.get(day_key)
        if current is None or ts > current[0]:
            daily_latest[day_key] = (ts, payload)

    selected = [item[1] for item in sorted(daily_latest.values(), key=lambda row: row[0])]
    tp_sl_values: List[Optional[float]] = []
    closed_per_day_values: List[Optional[float]] = []
    expectancy_values: List[Optional[float]] = []
    for payload in selected:
        summary = payload.get("summary", {})
        if not isinstance(summary, dict):
            summary = {}
        tp_sl_values.append(_as_float(summary.get("tp_sl_ratio"), None))
        closed_per_day_values.append(_derive_closed_per_day(summary, payload))
        expectancy_values.append(_derive_expectancy(summary))

    return {
        "report_count": len(selected),
        "tp_sl_ratio": _mean_optional(tp_sl_values),
        "closed_per_day": _mean_optional(closed_per_day_values),
        "expectancy": _mean_optional(expectancy_values),
        "source_file_count": len(paths),
    }


def evaluate_audit_status(
    *,
    runtime_metrics: Dict[str, Any],
    live_metrics: Dict[str, Any],
    profile: str = "default",
) -> Dict[str, Any]:
    profile_name = _normalize_profile(profile)
    profile_cfg = _resolve_profile(profile_name)
    gap_reject_rate_min = float(profile_cfg["gap_reject_rate_min"])
    gap_reject_rate_max = float(profile_cfg["gap_reject_rate_max"])
    tp_sl_ratio_min = float(profile_cfg["tp_sl_ratio_min"])
    closed_per_day_min = float(profile_cfg["closed_per_day_min"])
    expectancy_min_exclusive = float(profile_cfg["expectancy_min_exclusive"])

    gap_reject_rate = _as_float(runtime_metrics.get("gap_reject_rate"), None)
    tp_sl_ratio = _as_float(live_metrics.get("tp_sl_ratio"), None)
    closed_per_day = _as_float(live_metrics.get("closed_per_day"), None)
    expectancy = _as_float(live_metrics.get("expectancy"), None)

    checks = {
        "gap_reject_rate": {
            "value": gap_reject_rate,
            "min": gap_reject_rate_min,
            "max": gap_reject_rate_max,
            "ok": (gap_reject_rate is not None and gap_reject_rate_min <= gap_reject_rate <= gap_reject_rate_max),
        },
        "tp_sl_ratio": {
            "value": tp_sl_ratio,
            "min": tp_sl_ratio_min,
            "ok": (tp_sl_ratio is not None and tp_sl_ratio >= tp_sl_ratio_min),
        },
        "closed_per_day": {
            "value": closed_per_day,
            "min": closed_per_day_min,
            "ok": (closed_per_day is not None and closed_per_day >= closed_per_day_min),
        },
        "expectancy": {
            "value": expectancy,
            "min_exclusive": expectancy_min_exclusive,
            "ok": (expectancy is not None and expectancy > expectancy_min_exclusive),
        },
    }

    recommendations: List[Dict[str, str]] = []
    if gap_reject_rate is not None and gap_reject_rate > 0.5 and closed_per_day is not None and closed_per_day < 0.5:
        recommendations.append(
            {
                "pattern": "A",
                "summary": "gap_reject_rate過剰 + 回転不足",
                "action": "max_gapを2.8-3.0へ拡張",
            }
        )
    if gap_reject_rate is not None and gap_reject_rate < 0.05 and tp_sl_ratio is not None and tp_sl_ratio < 0.6:
        recommendations.append(
            {
                "pattern": "B",
                "summary": "gapゲート弱すぎ + TP/SL改善なし",
                "action": "min_gapを0.9から1.1へ引き上げ",
            }
        )
    if tp_sl_ratio is not None and tp_sl_ratio >= 0.7 and expectancy is not None and expectancy <= 0.0:
        recommendations.append(
            {
                "pattern": "C",
                "summary": "TP/SLは改善するが期待値が負",
                "action": "RRを2.0/2.5方向へ再調整（TP縮小）",
            }
        )

    required_values = [gap_reject_rate, tp_sl_ratio, closed_per_day, expectancy]
    if any(value is None for value in required_values):
        status = "INSUFFICIENT_DATA"
    elif all(bool(item.get("ok", False)) for item in checks.values()):
        status = "PASS"
    else:
        status = "FAIL" if recommendations or sum(1 for item in checks.values() if not bool(item.get("ok", False))) >= 2 else "WARN"

    return {
        "profile": profile_name,
        "status": status,
        "checks": checks,
        "recommendations": recommendations,
    }


def _split_glob_args(values: List[str]) -> List[str]:
    out: List[str] = []
    for value in values:
        parts = [part.strip() for part in str(value).split(",")]
        for part in parts:
            if part:
                out.append(part)
    return out


def main() -> None:
    parser = argparse.ArgumentParser(description="Operational audit for XAU autobot")
    parser.add_argument("--profile", default="default", choices=sorted(AUDIT_PROFILES.keys()))
    parser.add_argument("--days", type=float, default=None)
    parser.add_argument(
        "--runtime-log-glob",
        action="append",
        default=[],
        help="Glob for runtime JSON/JSONL logs (repeatable, comma-separated allowed)",
    )
    parser.add_argument(
        "--live-report-glob",
        action="append",
        default=[],
        help="Glob for xau_autobot live reports (repeatable, comma-separated allowed)",
    )
    parser.add_argument("--run-id-filter", default="")
    parser.add_argument("--now-utc", default="")
    parser.add_argument("--write-report", default="data/reports/xau_autobot_operational_audit.json")
    args = parser.parse_args()

    profile_name = _normalize_profile(str(args.profile))
    profile_cfg = _resolve_profile(profile_name)
    now_utc = _parse_utc(args.now_utc) or datetime.now(timezone.utc)
    if args.days is None:
        days = float(profile_cfg["window_days"])
    else:
        days = float(args.days) if float(args.days) > 0 else float(profile_cfg["window_days"])
    start_utc = now_utc - timedelta(days=days)

    runtime_globs = _split_glob_args(args.runtime_log_glob) or [
        "data/reports/xau_autobot_runtime_journal*.jsonl",
        "logs/xau_live_*.log",
        "data/runtime/xau_autobot_trial_v2_*.log",
        "data/runtime/xau_autobot_trial_start_*.log",
    ]
    live_globs = _split_glob_args(args.live_report_glob) or [
        "data/reports/xau_autobot_live_report_trial_v2_*.json",
    ]

    live_metrics = load_live_metrics(
        live_globs=live_globs,
        start_utc=start_utc,
        end_utc=now_utc,
        run_id_filter=str(args.run_id_filter),
    )
    runtime_metrics, runtime_metrics_source, runtime_metrics_fallback_reason = resolve_runtime_metrics_with_fallback(
        runtime_globs=runtime_globs,
        live_globs=live_globs,
        start_utc=start_utc,
        end_utc=now_utc,
        run_id_filter=str(args.run_id_filter),
    )
    verdict = evaluate_audit_status(
        runtime_metrics=runtime_metrics,
        live_metrics=live_metrics,
        profile=profile_name,
    )

    output = {
        "generated_at": now_utc.isoformat(),
        "audit_profile": profile_name,
        "window": {
            "start_utc": start_utc.isoformat(),
            "end_utc": now_utc.isoformat(),
            "days": days,
        },
        "run_id_filter": str(args.run_id_filter or ""),
        "runtime_metrics": runtime_metrics,
        "runtime_metrics_source": runtime_metrics_source,
        "runtime_metrics_fallback_reason": runtime_metrics_fallback_reason,
        "live_metrics": live_metrics,
        **verdict,
    }
    print(json.dumps(output, ensure_ascii=True))

    write_path = Path(args.write_report)
    write_path.parent.mkdir(parents=True, exist_ok=True)
    with write_path.open("w", encoding="utf-8") as f:
        json.dump(output, f, ensure_ascii=True, indent=2)
        f.write("\n")


if __name__ == "__main__":
    main()
