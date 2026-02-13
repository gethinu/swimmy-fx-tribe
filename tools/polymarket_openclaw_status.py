#!/usr/bin/env python3
"""Show current Polymarket OpenClaw cycle status from latest_status.json."""

from __future__ import annotations

import argparse
import json
import os
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Mapping, Sequence


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


def parse_iso_datetime(text: str) -> datetime | None:
    value = str(text or "").strip()
    if not value:
        return None
    normalized = value[:-1] + "+00:00" if value.endswith("Z") else value
    try:
        dt = datetime.fromisoformat(normalized)
    except ValueError:
        return None
    if dt.tzinfo is None:
        return dt.replace(tzinfo=timezone.utc)
    return dt


def load_status_payload(status_file: Path) -> Dict[str, Any]:
    if not status_file.exists() or not status_file.is_file():
        raise FileNotFoundError(f"status file not found: {status_file}")
    payload = json.loads(status_file.read_text(encoding="utf-8"))
    if not isinstance(payload, Mapping):
        raise ValueError(f"status payload must be object: {status_file}")
    return dict(payload)


def load_status_history(*, history_file: Path, last_runs: int) -> List[Dict[str, Any]]:
    limit = max(0, int(last_runs))
    if limit == 0:
        return []
    if not history_file.exists() or not history_file.is_file():
        return []
    rows: List[Dict[str, Any]] = []
    for line in history_file.read_text(encoding="utf-8").splitlines():
        text = line.strip()
        if not text:
            continue
        try:
            payload = json.loads(text)
        except json.JSONDecodeError:
            continue
        if isinstance(payload, Mapping):
            rows.append(dict(payload))
    if len(rows) > limit:
        rows = rows[-limit:]
    return rows


def _to_int(value: Any, default: int = 0) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return int(default)


def _to_float(value: Any, default: float = 0.0) -> float:
    try:
        return float(value)
    except (TypeError, ValueError):
        return float(default)


def _env_int(key: str, default: int) -> int:
    raw = str(os.getenv(key, "")).strip()
    if not raw:
        return int(default)
    try:
        return int(raw)
    except ValueError:
        return int(default)


def summarize_recent_runs(
    *,
    recent_runs: Sequence[Mapping[str, Any]],
    window_minutes: int,
    now_iso: str = "",
) -> Dict[str, Any]:
    now = parse_iso_datetime(now_iso) if now_iso.strip() else datetime.now(timezone.utc)
    window = max(0, int(window_minutes))
    rows: List[Mapping[str, Any]] = []
    if window <= 0:
        rows = list(recent_runs)
    else:
        threshold_seconds = float(window * 60)
        for row in recent_runs:
            updated_at = parse_iso_datetime(str(row.get("updated_at", "")).strip())
            if updated_at is None:
                continue
            age_seconds = max(0.0, (now - updated_at).total_seconds())
            if age_seconds <= threshold_seconds:
                rows.append(row)

    runs = len(rows)
    entries = 0
    execution_sent = 0
    execution_failed = 0
    total_stake_usd = 0.0
    for row in rows:
        entries += max(0, _to_int(row.get("entries"), 0))
        execution_sent += max(0, _to_int(row.get("execution_sent"), 0))
        execution_failed += max(0, _to_int(row.get("execution_failed"), 0))
        total_stake_usd += max(0.0, _to_float(row.get("total_stake_usd"), 0.0))

    return {
        "window_minutes": window,
        "runs": runs,
        "entries": entries,
        "execution_sent": execution_sent,
        "execution_failed": execution_failed,
        "total_stake_usd": round(total_stake_usd, 8),
    }


def build_health(
    *,
    payload: Mapping[str, Any],
    max_age_seconds: int,
    window_summary: Mapping[str, Any],
    min_runs_in_window: int,
    min_sent_in_window: int,
    min_entries_in_window: int,
    now_iso: str = "",
) -> Dict[str, Any]:
    now = parse_iso_datetime(now_iso) if now_iso.strip() else datetime.now(timezone.utc)
    updated_at_raw = str(payload.get("updated_at") or "").strip()
    updated_at = parse_iso_datetime(updated_at_raw)
    age_seconds = 0.0
    status = "ok"
    reason = ""
    if updated_at is None:
        status = "warn"
        reason = "missing updated_at"
    else:
        age_seconds = max(0.0, (now - updated_at).total_seconds())
        if max_age_seconds > 0 and age_seconds > float(max_age_seconds):
            status = "stale"
            reason = f"status age exceeded threshold: {age_seconds:.1f}s > {int(max_age_seconds)}s"

    runs = max(0, _to_int(window_summary.get("runs"), 0))
    entries = max(0, _to_int(window_summary.get("entries"), 0))
    sent = max(0, _to_int(window_summary.get("execution_sent"), 0))
    min_runs = max(0, int(min_runs_in_window))
    min_sent = max(0, int(min_sent_in_window))
    min_entries = max(0, int(min_entries_in_window))
    window_minutes = max(0, _to_int(window_summary.get("window_minutes"), 0))
    if status == "ok" and min_runs > 0 and runs < min_runs:
        status = "warn"
        reason = f"runs below threshold: {runs} < {min_runs} (window={window_minutes}m)"
    if status == "ok" and min_entries > 0 and entries < min_entries:
        status = "warn"
        reason = f"entries below threshold: {entries} < {min_entries} (window={window_minutes}m)"
    if status == "ok" and min_sent > 0 and sent < min_sent:
        status = "warn"
        reason = f"sent below threshold: {sent} < {min_sent} (window={window_minutes}m)"

    live_enabled = bool(payload.get("live_execution_enabled", False))
    failed_orders = int(payload.get("execution_failed", 0) or 0)
    if live_enabled and failed_orders > 0:
        status = "error"
        reason = f"failed orders detected: {failed_orders}"

    return {
        "status": status,
        "reason": reason,
        "age_seconds": round(float(age_seconds), 3),
    }


def render_text_summary(
    *,
    payload: Mapping[str, Any],
    health: Mapping[str, Any],
    recent_runs: Sequence[Mapping[str, Any]] | None = None,
    window_summary: Mapping[str, Any] | None = None,
) -> str:
    lines = [
        f"Health: {health.get('status', 'unknown')}",
        f"Reason: {health.get('reason', '') or '-'}",
        f"AgeSeconds: {health.get('age_seconds', 0)}",
        f"UpdatedAt: {payload.get('updated_at', '')}",
        f"Run: {payload.get('run_id', '')} ({payload.get('run_date', '')})",
        (
            f"Entries: {int(payload.get('entries', 0) or 0)} "
            f"StakeUSD: {float(payload.get('total_stake_usd', 0.0) or 0.0):.4f}"
        ),
        (
            f"Signals: total={int(payload.get('signal_count', 0) or 0)} "
            f"agent={int(payload.get('agent_signal_count', 0) or 0)} "
            f"ratio={float(payload.get('agent_signal_ratio', 0.0) or 0.0):.6f}"
        ),
        (
            f"Markets: open={int(payload.get('open_markets', 0) or 0)} "
            f"blocked={int(payload.get('blocked_open_markets', 0) or 0)} "
            f"quality_filtered={int(payload.get('quality_filtered_markets', 0) or 0)}"
        ),
        (
            f"Execution: enabled={bool(payload.get('live_execution_enabled', False))} "
            f"ok={bool(payload.get('execution_ok', False))} "
            f"skipped={bool(payload.get('execution_skipped', False))} "
            f"attempted={int(payload.get('execution_attempted', 0) or 0)} "
            f"sent={int(payload.get('execution_sent', 0) or 0)} "
            f"failed={int(payload.get('execution_failed', 0) or 0)}"
        ),
        f"PlanFile: {payload.get('plan_file', '')}",
        f"ReportFile: {payload.get('report_file', '')}",
        f"JournalFile: {payload.get('journal_file', '')}",
    ]
    runs = list(recent_runs or [])
    if runs:
        lines.append("RecentRuns:")
        for row in runs:
            lines.append(
                (
                    f"- {row.get('run_id', '')} entries={int(row.get('entries', 0) or 0)} "
                    f"sent={int(row.get('execution_sent', 0) or 0)} "
                    f"failed={int(row.get('execution_failed', 0) or 0)} "
                    f"updated_at={row.get('updated_at', '')}"
                )
            )
    if window_summary is not None:
        lines.append("WindowSummary:")
        lines.append(
            (
                f"- window={int(window_summary.get('window_minutes', 0) or 0)}m "
                f"runs={int(window_summary.get('runs', 0) or 0)} "
                f"entries={int(window_summary.get('entries', 0) or 0)} "
                f"sent={int(window_summary.get('execution_sent', 0) or 0)} "
                f"failed={int(window_summary.get('execution_failed', 0) or 0)} "
                f"stake={float(window_summary.get('total_stake_usd', 0.0) or 0.0):.4f}"
            )
        )
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser(description="Show latest Polymarket OpenClaw status")
    parser.add_argument("--output-dir", default="")
    parser.add_argument("--status-file", default="")
    parser.add_argument("--history-file", default="")
    parser.add_argument("--last-runs", type=int, default=_env_int("POLYCLAW_STATUS_LAST_RUNS", 5))
    parser.add_argument("--window-minutes", type=int, default=_env_int("POLYCLAW_STATUS_WINDOW_MINUTES", 120))
    parser.add_argument(
        "--min-runs-in-window",
        type=int,
        default=_env_int("POLYCLAW_STATUS_MIN_RUNS_IN_WINDOW", 0),
    )
    parser.add_argument(
        "--min-sent-in-window",
        type=int,
        default=_env_int("POLYCLAW_STATUS_MIN_SENT_IN_WINDOW", 0),
    )
    parser.add_argument(
        "--min-entries-in-window",
        type=int,
        default=_env_int("POLYCLAW_STATUS_MIN_ENTRIES_IN_WINDOW", 0),
    )
    parser.add_argument("--max-age-seconds", type=int, default=_env_int("POLYCLAW_STATUS_MAX_AGE_SECONDS", 3600))
    parser.add_argument("--json", action="store_true")
    parser.add_argument("--fail-on-problem", action="store_true")
    args = parser.parse_args()

    base_dir = resolve_base_dir()
    output_dir = Path(args.output_dir).expanduser() if args.output_dir.strip() else Path(
        os.getenv("POLYCLAW_OUTPUT_DIR", str(base_dir / "data" / "reports" / "polymarket_openclaw"))
    ).expanduser()
    status_file = Path(args.status_file).expanduser() if args.status_file.strip() else output_dir / "latest_status.json"
    history_file = (
        Path(args.history_file).expanduser() if args.history_file.strip() else output_dir / "status_history.jsonl"
    )
    payload = load_status_payload(status_file)
    recent_runs = load_status_history(history_file=history_file, last_runs=max(0, int(args.last_runs)))
    window_summary = summarize_recent_runs(
        recent_runs=recent_runs,
        window_minutes=max(0, int(args.window_minutes)),
    )
    health = build_health(
        payload=payload,
        max_age_seconds=max(0, int(args.max_age_seconds)),
        window_summary=window_summary,
        min_runs_in_window=max(0, int(args.min_runs_in_window)),
        min_sent_in_window=max(0, int(args.min_sent_in_window)),
        min_entries_in_window=max(0, int(args.min_entries_in_window)),
    )
    out = dict(payload)
    out["health"] = health
    out["status_file"] = str(status_file)
    out["history_file"] = str(history_file)
    out["recent_runs"] = recent_runs
    out["window_summary"] = window_summary

    if args.json:
        print(json.dumps(out, ensure_ascii=False, indent=2))
    else:
        print(
            render_text_summary(
                payload=payload,
                health=health,
                recent_runs=recent_runs,
                window_summary=window_summary,
            )
        )

    if args.fail_on_problem and str(health.get("status", "ok")).lower() != "ok":
        raise SystemExit(1)


if __name__ == "__main__":
    main()
