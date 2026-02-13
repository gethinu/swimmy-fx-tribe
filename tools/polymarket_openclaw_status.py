#!/usr/bin/env python3
"""Show current Polymarket OpenClaw cycle status from latest_status.json."""

from __future__ import annotations

import argparse
import json
import os
import sys
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


def _env_str(key: str, default: str = "") -> str:
    raw = str(os.getenv(key, "")).strip()
    return raw if raw else str(default)


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

    if payload.get("cycle_ok", True) is False:
        status = "error"
        error_text = str(payload.get("cycle_error", "") or "").strip()
        reason = f"cycle failed: {error_text}" if error_text else "cycle failed"

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


def should_discord_notify(*, when: str, health_status: str) -> bool:
    mode = str(when or "").strip().lower()
    status = str(health_status or "").strip().lower()
    if mode in {"", "never", "off", "0", "false", "no"}:
        return False
    if mode in {"always", "on", "1", "true", "yes"}:
        return True
    if mode in {"problem", "issues", "not_ok"}:
        return status != "ok"
    return False


def resolve_discord_webhook(*, webhook: str, webhook_env: str) -> str:
    explicit = str(webhook or "").strip()
    if explicit:
        return explicit
    key = str(webhook_env or "").strip()
    if not key:
        return ""
    return str(os.getenv(key, "")).strip()


def discord_color_for_status(status: str) -> int:
    value = str(status or "").strip().lower()
    if value == "ok":
        return 3066993  # green
    if value in {"warn", "stale"}:
        return 16705372  # yellow
    if value == "error":
        return 15158332  # red
    return 10070709  # gray


def build_discord_status_payload(
    *,
    status_payload: Mapping[str, Any],
    health: Mapping[str, Any],
    window_summary: Mapping[str, Any],
    now: datetime | None = None,
) -> Dict[str, Any]:
    ts = now or datetime.now(timezone.utc)
    health_status = str(health.get("status", "unknown"))
    title = f"Polymarket OpenClaw Status: {health_status.upper()}"
    reason = str(health.get("reason") or "-")

    def fmt_bool(key: str) -> str:
        return "true" if bool(status_payload.get(key, False)) else "false"

    def fmt_int(key: str) -> str:
        try:
            return str(int(status_payload.get(key, 0) or 0))
        except Exception:
            return "0"

    def fmt_float(key: str) -> str:
        try:
            return f"{float(status_payload.get(key, 0.0) or 0.0):.4f}"
        except Exception:
            return "0.0000"

    updated_at = str(status_payload.get("updated_at", "") or "").strip()
    run_id = str(status_payload.get("run_id", "") or "").strip()
    run_date = str(status_payload.get("run_date", "") or "").strip()
    age_seconds = health.get("age_seconds", 0)
    try:
        age_text = f"{float(age_seconds):.1f}s"
    except Exception:
        age_text = str(age_seconds)

    entries = fmt_int("entries")
    stake = fmt_float("total_stake_usd")
    execution_sent = fmt_int("execution_sent")
    execution_failed = fmt_int("execution_failed")
    open_markets = fmt_int("open_markets")
    blocked_markets = fmt_int("blocked_open_markets")
    quality_filtered = fmt_int("quality_filtered_markets")
    signal_total = fmt_int("signal_count")
    signal_agent = fmt_int("agent_signal_count")
    try:
        signal_ratio = f"{float(status_payload.get('agent_signal_ratio', 0.0) or 0.0):.6f}"
    except Exception:
        signal_ratio = str(status_payload.get("agent_signal_ratio", 0.0) or 0.0)

    window_minutes = int(window_summary.get("window_minutes", 0) or 0)
    window_runs = int(window_summary.get("runs", 0) or 0)
    window_sent = int(window_summary.get("execution_sent", 0) or 0)
    window_failed = int(window_summary.get("execution_failed", 0) or 0)
    try:
        window_stake = float(window_summary.get("total_stake_usd", 0.0) or 0.0)
    except Exception:
        window_stake = 0.0

    fields = [
        {"name": "Health", "value": f"{health_status} ({reason})", "inline": False},
        {"name": "Updated", "value": f"{updated_at or '-'} (age {age_text})", "inline": False},
        {"name": "Run", "value": f"{run_id or '-'} ({run_date or '-'})", "inline": False},
        {"name": "Entries", "value": f"{entries} | stake ${stake}", "inline": True},
        {
            "name": "Execution",
            "value": f"sent={execution_sent} failed={execution_failed} enabled={fmt_bool('live_execution_enabled')}",
            "inline": True,
        },
        {"name": "Signals", "value": f"total={signal_total} agent={signal_agent} ratio={signal_ratio}", "inline": True},
        {"name": "Markets", "value": f"open={open_markets} blocked={blocked_markets} qf={quality_filtered}", "inline": True},
        {
            "name": "Window",
            "value": (
                f"{window_minutes}m runs={window_runs} sent={window_sent} "
                f"failed={window_failed} stake=${window_stake:.4f}"
            ),
            "inline": False,
        },
    ]

    return {
        "embeds": [
            {
                "title": title,
                "description": "",
                "color": discord_color_for_status(health_status),
                "timestamp": ts.isoformat(),
                "fields": fields,
            }
        ]
    }


def queue_discord_notification_via_notifier(
    *,
    webhook_url: str,
    payload: Mapping[str, Any],
    zmq_host: str,
    zmq_port: int,
) -> None:
    # Import locally so status checks still work in environments without notifier deps.
    import zmq  # type: ignore

    base_dir = resolve_base_dir()
    python_src = base_dir / "src" / "python"
    if str(python_src) not in sys.path:
        sys.path.insert(0, str(python_src))

    from aux_sexp import sexp_request  # type: ignore

    message = sexp_request(
        {
            "type": "NOTIFIER",
            "action": "SEND",
            "webhook": webhook_url,
            "payload_json": json.dumps(payload, ensure_ascii=False),
        }
    )

    ctx = zmq.Context()
    try:
        sock = ctx.socket(zmq.PUSH)
        sock.setsockopt(zmq.LINGER, 0)
        sock.connect(f"tcp://{zmq_host}:{int(zmq_port)}")
        sock.send_string(message)
        sock.close()
    finally:
        ctx.term()


def maybe_queue_discord_status_notification(
    *,
    when: str,
    webhook: str,
    webhook_env: str,
    status_payload: Mapping[str, Any],
    health: Mapping[str, Any],
    window_summary: Mapping[str, Any],
    zmq_host: str,
    zmq_port: int,
) -> bool:
    if not should_discord_notify(when=when, health_status=str(health.get("status", ""))):
        return False

    webhook_url = resolve_discord_webhook(webhook=webhook, webhook_env=webhook_env)
    if not webhook_url:
        return False

    discord_payload = build_discord_status_payload(
        status_payload=status_payload,
        health=health,
        window_summary=window_summary,
    )
    try:
        queue_discord_notification_via_notifier(
            webhook_url=webhook_url,
            payload=discord_payload,
            zmq_host=zmq_host,
            zmq_port=zmq_port,
        )
    except Exception as exc:
        # Never log webhook URLs (tokens are embedded).
        msg = str(exc)
        msg = msg if len(msg) <= 200 else msg[:197] + "..."
        print(
            f"[WARN] Discord notify failed: {type(exc).__name__}: {msg}",
            file=sys.stderr,
            flush=True,
        )
    return True


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
    parser.add_argument(
        "--discord-when",
        choices=["never", "problem", "always"],
        default=_env_str("POLYCLAW_STATUS_DISCORD_WHEN", "never"),
    )
    parser.add_argument("--discord-webhook", default=_env_str("POLYCLAW_STATUS_DISCORD_WEBHOOK", ""))
    parser.add_argument("--discord-webhook-env", default=_env_str("POLYCLAW_STATUS_DISCORD_WEBHOOK_ENV", ""))
    parser.add_argument("--discord-zmq-host", default=_env_str("POLYCLAW_STATUS_DISCORD_ZMQ_HOST", "localhost"))
    parser.add_argument("--discord-zmq-port", type=int, default=_env_int("SWIMMY_PORT_NOTIFIER", 5562))
    args = parser.parse_args()

    base_dir = resolve_base_dir()
    output_dir = Path(args.output_dir).expanduser() if args.output_dir.strip() else Path(
        os.getenv("POLYCLAW_OUTPUT_DIR", str(base_dir / "data" / "reports" / "polymarket_openclaw"))
    ).expanduser()
    status_file = Path(args.status_file).expanduser() if args.status_file.strip() else output_dir / "latest_status.json"
    history_file = (
        Path(args.history_file).expanduser() if args.history_file.strip() else output_dir / "status_history.jsonl"
    )
    status_load_ok = True
    try:
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
    except Exception as exc:
        status_load_ok = False
        err_text = f"{type(exc).__name__}: {exc}".strip()
        err_text = err_text if len(err_text) <= 240 else err_text[:237] + "..."
        payload = {
            "updated_at": "",
            "run_id": "",
            "run_date": "",
            "entries": 0,
            "total_stake_usd": 0.0,
            "signal_count": 0,
            "agent_signal_count": 0,
            "agent_signal_ratio": 0.0,
            "open_markets": 0,
            "blocked_open_markets": 0,
            "quality_filtered_markets": 0,
            "live_execution_enabled": False,
            "execution_ok": False,
            "execution_skipped": True,
            "execution_attempted": 0,
            "execution_sent": 0,
            "execution_failed": 0,
            "plan_file": "",
            "report_file": "",
            "journal_file": "",
            "cycle_ok": False,
            "cycle_error": err_text,
        }
        recent_runs = []
        window_summary = {
            "window_minutes": max(0, int(args.window_minutes)),
            "runs": 0,
            "entries": 0,
            "execution_sent": 0,
            "execution_failed": 0,
            "total_stake_usd": 0.0,
        }
        health = {
            "status": "error",
            "reason": f"status read failed: {err_text}",
            "age_seconds": 0.0,
        }
    out = dict(payload)
    out["health"] = health
    out["status_file"] = str(status_file)
    out["history_file"] = str(history_file)
    out["recent_runs"] = recent_runs
    out["window_summary"] = window_summary

    maybe_queue_discord_status_notification(
        when=str(args.discord_when),
        webhook=str(args.discord_webhook),
        webhook_env=str(args.discord_webhook_env),
        status_payload=payload,
        health=health,
        window_summary=window_summary,
        zmq_host=str(args.discord_zmq_host),
        zmq_port=int(args.discord_zmq_port),
    )

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
    if not status_load_ok:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
