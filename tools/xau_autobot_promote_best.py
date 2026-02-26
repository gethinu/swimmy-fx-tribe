#!/usr/bin/env python3
"""Promote best XAU autobot period config from comparison summary."""

from __future__ import annotations

import argparse
import json
import shutil
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple


def _verdict_score(verdict: str) -> float:
    key = str(verdict or "").upper()
    if key == "GO":
        return 1.0
    if key == "CAUTION":
        return 0.4
    return -1.0


def _as_float(value: object, default: float = 0.0) -> float:
    try:
        if value is None:
            return default
        return float(value)
    except (TypeError, ValueError):
        return default


def _as_int(value: object, default: int = -1) -> int:
    try:
        if value is None:
            return default
        return int(value)
    except (TypeError, ValueError):
        return default


def _build_live_summary_from_report(data: Dict[str, object]) -> Dict[str, float]:
    summary = data.get("summary") if isinstance(data.get("summary"), dict) else {}
    return {
        "closed_positions": _as_float(summary.get("closed_positions", 0.0), 0.0),
        "win_rate": _as_float(summary.get("win_rate", 0.0), 0.0),
        "net_profit": _as_float(summary.get("net_profit", 0.0), 0.0),
        "profit_factor": _as_float(summary.get("profit_factor", 0.0), 0.0),
    }


def _identity_matches(
    payload: Dict[str, object],
    *,
    expected_magic: Optional[int],
    expected_comment_prefix: str,
) -> bool:
    if expected_magic is not None:
        if _as_int(payload.get("magic"), default=-1) != int(expected_magic):
            return False
    expected_prefix = expected_comment_prefix.strip()
    if expected_prefix:
        if str(payload.get("comment_prefix", "")).strip() != expected_prefix:
            return False
    return True


def _load_live_report(
    path: Path,
    *,
    expected_magic: Optional[int] = None,
    expected_comment_prefix: str = "",
) -> Optional[Dict[str, float]]:
    try:
        with path.open("r", encoding="utf-8") as f:
            data = json.load(f)
    except Exception:
        return None
    if not isinstance(data, dict):
        return None
    if not _identity_matches(
        data,
        expected_magic=expected_magic,
        expected_comment_prefix=expected_comment_prefix,
    ):
        return None
    return _build_live_summary_from_report(data)


def _path_age_hours(path: Path, *, now_utc: datetime) -> float:
    modified = datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc)
    return float((now_utc - modified).total_seconds() / 3600.0)


def resolve_live_summary(
    *,
    live_report: str,
    live_reports_dir: Path,
    max_age_hours: float = 48.0,
    now_utc: Optional[datetime] = None,
    expected_magic: Optional[int] = None,
    expected_comment_prefix: str = "",
) -> Tuple[Optional[Path], Dict[str, float]]:
    if now_utc is None:
        now_utc = datetime.now(timezone.utc)

    explicit = str(live_report or "").strip()
    if explicit:
        explicit_path = Path(explicit)
        summary = _load_live_report(
            explicit_path,
            expected_magic=expected_magic,
            expected_comment_prefix=expected_comment_prefix,
        )
        return (explicit_path, summary or {})

    candidates = sorted(
        live_reports_dir.glob("xau_autobot_live_report_*.json"),
        key=lambda p: p.stat().st_mtime,
        reverse=True,
    )

    valid: List[Tuple[Path, Dict[str, float]]] = []
    nonzero: List[Tuple[Path, Dict[str, float]]] = []
    for candidate in candidates:
        summary = _load_live_report(
            candidate,
            expected_magic=expected_magic,
            expected_comment_prefix=expected_comment_prefix,
        )
        if summary is None:
            continue
        if max_age_hours > 0.0:
            age_hours = _path_age_hours(candidate, now_utc=now_utc)
            if age_hours > max_age_hours:
                continue
        valid.append((candidate, summary))
        if summary.get("closed_positions", 0.0) > 0.0:
            nonzero.append((candidate, summary))

    if nonzero:
        return nonzero[0]
    if valid:
        return valid[0]
    return (None, {})


def _live_stress_multiplier(live_summary: Optional[Dict[str, float]]) -> float:
    if not live_summary:
        return 1.0
    closed_positions = _as_float(live_summary.get("closed_positions", 0.0), 0.0)
    if closed_positions < 10.0:
        return 1.0

    win_rate = _as_float(live_summary.get("win_rate", 0.0), 0.0)
    profit_factor = _as_float(live_summary.get("profit_factor", 0.0), 0.0)
    net_profit = _as_float(live_summary.get("net_profit", 0.0), 0.0)

    stress = 0.0
    if profit_factor < 1.0:
        stress += min(1.0, 1.0 - profit_factor)
    if win_rate < 0.45:
        stress += min(0.8, 0.45 - win_rate)
    if net_profit < 0.0:
        stress += 0.6
    return 1.0 + stress


def score_period_summary(row: Dict[str, object], *, live_summary: Optional[Dict[str, float]] = None) -> float:
    backtest = row.get("backtest", {}) if isinstance(row.get("backtest"), dict) else {}
    readiness = row.get("readiness", {}) if isinstance(row.get("readiness"), dict) else {}
    cost_guard = row.get("cost_guard", {}) if isinstance(row.get("cost_guard"), dict) else {}
    robustness = readiness.get("robustness", {}) if isinstance(readiness.get("robustness"), dict) else {}

    pf = _as_float(backtest.get("pf", 0.0), 0.0)
    total_return = _as_float(backtest.get("total_return", 0.0), 0.0)
    max_dd = _as_float(backtest.get("max_dd", 1.0), 1.0)

    readiness_v = _verdict_score(str(readiness.get("verdict", "")))
    cost_v = _verdict_score(str(cost_guard.get("verdict", "")))

    if readiness_v < 0.0 or cost_v < 0.0:
        return -999.0

    stress_multiplier = _live_stress_multiplier(live_summary)
    score = (5.0 * total_return) + (1.5 * pf) - ((3.0 * stress_multiplier) * max_dd) + readiness_v + cost_v

    oos_count = _as_float(robustness.get("oos_count", 0.0), 0.0)
    if oos_count > 0.0:
        oos_worst_total_return = _as_float(robustness.get("oos_worst_total_return", 0.0), 0.0)
        oos_worst_pf = _as_float(robustness.get("oos_worst_pf", 1.0), 1.0)
        oos_negative_ratio = _as_float(robustness.get("oos_negative_return_ratio", 0.0), 0.0)
        score += (
            (4.0 * oos_worst_total_return)
            + (0.8 * (oos_worst_pf - 1.0))
            - ((1.2 * stress_multiplier) * oos_negative_ratio)
        )
        if oos_worst_pf < 1.0:
            score -= (0.6 * stress_multiplier) * (1.0 - oos_worst_pf)
        if oos_worst_total_return < 0.0:
            score -= 0.5 * stress_multiplier

    return score


def choose_best_period(
    rows: Sequence[Dict[str, object]],
    *,
    live_summary: Optional[Dict[str, float]] = None,
) -> Dict[str, object]:
    if not rows:
        raise ValueError("no period rows")
    scoreboard = build_period_scoreboard(rows, live_summary=live_summary)
    best_period = str(scoreboard[0].get("period", "")).strip()
    for row in rows:
        if str(row.get("period", "")).strip() == best_period:
            return row
    raise RuntimeError("best period not found in rows")


def build_period_scoreboard(
    rows: Sequence[Dict[str, object]],
    *,
    live_summary: Optional[Dict[str, float]] = None,
) -> List[Dict[str, object]]:
    board: List[Dict[str, object]] = []
    for row in rows:
        period = str(row.get("period", "")).strip()
        backtest = row.get("backtest", {}) if isinstance(row.get("backtest"), dict) else {}
        readiness = row.get("readiness", {}) if isinstance(row.get("readiness"), dict) else {}
        cost_guard = row.get("cost_guard", {}) if isinstance(row.get("cost_guard"), dict) else {}
        board.append(
            {
                "period": period,
                "score": score_period_summary(row, live_summary=live_summary),
                "backtest_pf": _as_float(backtest.get("pf", 0.0), 0.0),
                "backtest_total_return": _as_float(backtest.get("total_return", 0.0), 0.0),
                "backtest_max_dd": _as_float(backtest.get("max_dd", 1.0), 1.0),
                "readiness_verdict": str(readiness.get("verdict", "")),
                "cost_guard_verdict": str(cost_guard.get("verdict", "")),
            }
        )
    board.sort(key=lambda x: _as_float(x.get("score", 0.0), 0.0), reverse=True)
    return board


def build_live_gap(
    best_row: Dict[str, object],
    *,
    live_summary: Optional[Dict[str, float]] = None,
    min_closed_positions: float = 10.0,
    min_live_profit_factor: float = 1.0,
    max_profit_factor_drop: float = 0.15,
    max_win_rate_drop: float = 0.08,
    require_nonnegative_net_profit: bool = True,
) -> Dict[str, object]:
    if not live_summary:
        return {}

    backtest = best_row.get("backtest", {}) if isinstance(best_row.get("backtest"), dict) else {}
    bt_win_rate = _as_float(backtest.get("win_rate", 0.0), 0.0)
    bt_profit_factor = _as_float(backtest.get("pf", 0.0), 0.0)
    bt_total_return = _as_float(backtest.get("total_return", 0.0), 0.0)
    bt_trades = _as_float(backtest.get("trades", 0.0), 0.0)

    live_closed_positions = _as_float(live_summary.get("closed_positions", 0.0), 0.0)
    live_win_rate = _as_float(live_summary.get("win_rate", 0.0), 0.0)
    live_profit_factor = _as_float(live_summary.get("profit_factor", 0.0), 0.0)
    live_net_profit = _as_float(live_summary.get("net_profit", 0.0), 0.0)

    sample_quality = "ok" if live_closed_positions >= min_closed_positions else "low"
    underperforming_reasons: List[str] = []
    if sample_quality == "ok":
        if live_profit_factor < min_live_profit_factor:
            underperforming_reasons.append("live_pf_below_1")
        if require_nonnegative_net_profit and live_net_profit < 0.0:
            underperforming_reasons.append("live_net_profit_negative")
        if (bt_profit_factor - live_profit_factor) > max_profit_factor_drop:
            underperforming_reasons.append("pf_gap_large")
        if (bt_win_rate - live_win_rate) > max_win_rate_drop:
            underperforming_reasons.append("win_rate_gap_large")
    underperforming = bool(underperforming_reasons)

    return {
        "sample_quality": sample_quality,
        "live_closed_positions": live_closed_positions,
        "backtest_trades": bt_trades,
        "backtest_profit_factor": bt_profit_factor,
        "live_profit_factor": live_profit_factor,
        "delta_profit_factor": live_profit_factor - bt_profit_factor,
        "backtest_win_rate": bt_win_rate,
        "live_win_rate": live_win_rate,
        "delta_win_rate": live_win_rate - bt_win_rate,
        "backtest_total_return": bt_total_return,
        "live_net_profit": live_net_profit,
        "underperforming": underperforming,
        "underperforming_reasons": underperforming_reasons,
    }


def should_block_promotion(
    live_gap: Dict[str, object],
    *,
    fail_on_live_underperforming: bool,
) -> bool:
    if not fail_on_live_underperforming:
        return False
    if not live_gap:
        return False
    if str(live_gap.get("sample_quality", "")).lower() != "ok":
        return False
    return bool(live_gap.get("underperforming", False))


def build_promotion_notification_lines(report: Dict[str, object]) -> List[str]:
    lines = [
        f"selected_period={report.get('selected_period')}",
        f"selected_score={report.get('selected_score')}",
        f"promotion_blocked={report.get('promotion_blocked')}",
    ]
    live_gap = report.get("live_gap", {}) if isinstance(report.get("live_gap"), dict) else {}
    if live_gap:
        lines.append(
            f"sample_quality={live_gap.get('sample_quality')} underperforming={live_gap.get('underperforming')}"
        )
        reasons = live_gap.get("underperforming_reasons", [])
        if isinstance(reasons, list) and reasons:
            joined = ",".join(str(x) for x in reasons)
            lines.append(f"underperforming_reasons={joined}")
    return lines


def build_discord_headers() -> Dict[str, str]:
    return {
        "Content-Type": "application/json",
        "User-Agent": "Mozilla/5.0 (compatible; xau-autobot/1.0)",
    }


def _post_discord(webhook_url: str, report: Dict[str, object]) -> None:
    payload = {
        "embeds": [
            {
                "title": "XAU AutoBot Promotion",
                "description": "\n".join(build_promotion_notification_lines(report)),
                "color": (15158332 if bool(report.get("promotion_blocked")) else 3447003),
            }
        ]
    }
    body = json.dumps(payload).encode("utf-8")
    request = urllib.request.Request(
        webhook_url,
        data=body,
        method="POST",
        headers=build_discord_headers(),
    )
    try:
        with urllib.request.urlopen(request, timeout=10):
            return
    except urllib.error.URLError as exc:
        raise RuntimeError(f"failed to post discord webhook: {exc}") from exc


def dispatch_discord_notification(
    webhook_urls: Sequence[str],
    report: Dict[str, object],
    *,
    strict: bool,
) -> Dict[str, object]:
    cleaned = [url.strip() for url in webhook_urls if isinstance(url, str) and url.strip()]
    if not cleaned:
        return {"notified": False, "error": "no webhook configured"}

    last_error = ""
    for idx, url in enumerate(cleaned, start=1):
        try:
            _post_discord(url, report)
            return {"notified": True, "used_webhook": url, "attempted": idx}
        except Exception as exc:
            last_error = str(exc)
            continue

    if strict:
        raise RuntimeError(last_error or "notification failed")
    return {"notified": False, "error": last_error or "notification failed", "attempted": len(cleaned)}


def apply_notify_result(report: Dict[str, object], notify_result: Dict[str, object]) -> Dict[str, object]:
    merged = dict(report)
    merged["notify"] = dict(notify_result)
    return merged


def append_history_jsonl(path: Path, report: Dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as f:
        f.write(json.dumps(report, ensure_ascii=True))
        f.write("\n")


def _load_comparison(path: Path) -> Dict[str, object]:
    with path.open("r", encoding="utf-8") as f:
        data = json.load(f)
    if not isinstance(data, dict):
        raise RuntimeError("comparison JSON must be object")
    return data


def _extract_period_rows(data: Dict[str, object]) -> List[Dict[str, object]]:
    periods = data.get("periods", [])
    if not isinstance(periods, list):
        return []
    return [row for row in periods if isinstance(row, dict)]


def _source_config_from_period(period: str, config_dir: Path) -> Path:
    return config_dir / f"xau_autobot.tuned_auto_gc_m5_{period}.json"


def _load_source_identity(path: Path) -> Tuple[Optional[int], str]:
    try:
        with path.open("r", encoding="utf-8") as f:
            payload = json.load(f)
    except Exception:
        return (None, "")
    if not isinstance(payload, dict):
        return (None, "")
    magic_raw = payload.get("magic")
    comment_prefix = str(payload.get("comment", "")).strip()
    if magic_raw is None or comment_prefix == "":
        return (None, "")
    return (_as_int(magic_raw, default=-1), comment_prefix)


def _infer_expected_live_identity(rows: Sequence[Dict[str, object]], config_dir: Path) -> Tuple[int, str]:
    identities: set[Tuple[int, str]] = set()
    for row in rows:
        period = str(row.get("period", "")).strip()
        if not period:
            continue
        cfg = _source_config_from_period(period, config_dir)
        magic, comment_prefix = _load_source_identity(cfg)
        if magic is None or magic < 0 or comment_prefix == "":
            continue
        identities.add((int(magic), comment_prefix))

    if not identities:
        raise RuntimeError("could not infer live report identity from period configs")
    if len(identities) > 1:
        raise RuntimeError("multiple live report identities found in period configs")
    return next(iter(identities))


def main() -> None:
    parser = argparse.ArgumentParser(description="Promote best XAU period config to active config")
    parser.add_argument("--comparison", default="data/reports/xau_autobot_cycle_comparison.json")
    parser.add_argument("--config-dir", default="tools/configs")
    parser.add_argument("--write-active", default="tools/configs/xau_autobot.tuned_auto_active.json")
    parser.add_argument("--write-report", default="data/reports/xau_autobot_promotion.json")
    parser.add_argument(
        "--write-history-jsonl",
        default="data/reports/xau_autobot_promotion_history.jsonl",
    )
    parser.add_argument("--live-report", default="")
    parser.add_argument("--live-reports-dir", default="data/reports")
    parser.add_argument("--live-max-age-hours", type=float, default=48.0)
    parser.add_argument("--live-min-closed-positions", type=float, default=10.0)
    parser.add_argument("--live-min-profit-factor", type=float, default=1.0)
    parser.add_argument("--live-max-profit-factor-drop", type=float, default=0.15)
    parser.add_argument("--live-max-win-rate-drop", type=float, default=0.08)
    parser.add_argument("--live-ignore-net-profit-check", action="store_true")
    parser.add_argument("--fail-on-live-underperforming", action="store_true")
    parser.add_argument("--discord-webhook", default="")
    parser.add_argument("--discord-webhook-fallbacks", default="")
    parser.add_argument("--notify-strict", action="store_true")
    args = parser.parse_args()

    comparison_path = Path(args.comparison)
    config_dir = Path(args.config_dir)
    active_path = Path(args.write_active)
    report_path = Path(args.write_report)
    history_path = Path(args.write_history_jsonl)
    live_reports_dir = Path(args.live_reports_dir)

    data = _load_comparison(comparison_path)
    rows = _extract_period_rows(data)
    if not rows:
        raise RuntimeError("no period rows")
    expected_magic, expected_comment_prefix = _infer_expected_live_identity(rows, config_dir)
    live_report_path, live_summary = resolve_live_summary(
        live_report=args.live_report,
        live_reports_dir=live_reports_dir,
        max_age_hours=float(args.live_max_age_hours),
        expected_magic=expected_magic,
        expected_comment_prefix=expected_comment_prefix,
    )
    if not live_summary:
        raise RuntimeError(
            "no live report matched expected identity "
            f"(magic={expected_magic}, comment_prefix={expected_comment_prefix})"
        )
    scoreboard = build_period_scoreboard(rows, live_summary=live_summary)
    if not scoreboard:
        raise RuntimeError("no period rows")
    period = str(scoreboard[0].get("period", "")).strip()
    best = choose_best_period(rows, live_summary=live_summary)
    if not period:
        raise RuntimeError("best period missing")

    src_cfg = _source_config_from_period(period, config_dir)
    if not src_cfg.exists():
        raise RuntimeError(f"source config missing: {src_cfg}")

    live_gap = (
        build_live_gap(
            best,
            live_summary=live_summary,
            min_closed_positions=float(args.live_min_closed_positions),
            min_live_profit_factor=float(args.live_min_profit_factor),
            max_profit_factor_drop=float(args.live_max_profit_factor_drop),
            max_win_rate_drop=float(args.live_max_win_rate_drop),
            require_nonnegative_net_profit=(not bool(args.live_ignore_net_profit_check)),
        )
        if live_summary
        else {}
    )
    promotion_blocked = should_block_promotion(
        live_gap,
        fail_on_live_underperforming=bool(args.fail_on_live_underperforming),
    )

    active_path.parent.mkdir(parents=True, exist_ok=True)
    if not promotion_blocked:
        shutil.copyfile(src_cfg, active_path)

    report = {
        "comparison": str(comparison_path),
        "selected_period": period,
        "selected_score": score_period_summary(best, live_summary=live_summary),
        "source_config": str(src_cfg),
        "active_config": str(active_path),
        "best_row": best,
        "scoreboard": scoreboard,
        "live_max_age_hours": float(args.live_max_age_hours),
        "live_identity": {
            "expected_magic": int(expected_magic),
            "expected_comment_prefix": expected_comment_prefix,
        },
        "live_underperformance_thresholds": {
            "min_closed_positions": float(args.live_min_closed_positions),
            "min_live_profit_factor": float(args.live_min_profit_factor),
            "max_profit_factor_drop": float(args.live_max_profit_factor_drop),
            "max_win_rate_drop": float(args.live_max_win_rate_drop),
            "require_nonnegative_net_profit": (not bool(args.live_ignore_net_profit_check)),
        },
        "fail_on_live_underperforming": bool(args.fail_on_live_underperforming),
        "promotion_blocked": promotion_blocked,
    }
    if live_summary:
        report["live_summary"] = live_summary
        report["live_gap"] = live_gap
    if live_report_path is not None:
        report["live_report"] = str(live_report_path)

    report_path.parent.mkdir(parents=True, exist_ok=True)
    with report_path.open("w", encoding="utf-8") as f:
        json.dump(report, f, ensure_ascii=True, indent=2)
        f.write("\n")

    webhook_candidates: List[str] = []
    if args.discord_webhook:
        webhook_candidates.append(args.discord_webhook)
    if args.discord_webhook_fallbacks:
        webhook_candidates.extend(args.discord_webhook_fallbacks.split(","))
    if webhook_candidates:
        notify_result = dispatch_discord_notification(webhook_candidates, report, strict=args.notify_strict)
        report = apply_notify_result(report, notify_result)
        with report_path.open("w", encoding="utf-8") as f:
            json.dump(report, f, ensure_ascii=True, indent=2)
            f.write("\n")
        print(json.dumps(notify_result, ensure_ascii=True))

    append_history_jsonl(history_path, report)

    if promotion_blocked:
        reasons = live_gap.get("underperforming_reasons", [])
        if isinstance(reasons, list) and reasons:
            reason_text = ",".join(str(x) for x in reasons)
        else:
            reason_text = "live_underperforming"
        raise SystemExit(f"promotion blocked: {reason_text}")

    print(json.dumps(report, ensure_ascii=True))


if __name__ == "__main__":
    main()
