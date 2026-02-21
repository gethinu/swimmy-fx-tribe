#!/usr/bin/env python3
"""Generate daily edge scorecard (KPI-0..3)."""

from __future__ import annotations

import argparse
import json
import os
import sqlite3
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, Dict, Iterable, Mapping

try:
    from tools import check_rank_conformance as rc
except ImportError:  # pragma: no cover
    import check_rank_conformance as rc  # type: ignore


LIVE_PF_FLOOR = 1.05
LIVE_WR_FLOOR = 0.35
LIVE_NET_PNL_FLOOR = 0.0
LIVE_MAX_LOSS_STREAK = 3


def _save_json(path: Path, payload: Mapping[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    text = json.dumps(payload, ensure_ascii=False, indent=2)
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(text, encoding="utf-8")
    tmp.replace(path)


def _to_int(value: Any, default: int = 0) -> int:
    try:
        if value is None:
            return default
        return int(value)
    except (TypeError, ValueError):
        return default


def _to_float(value: Any, default: float = 0.0) -> float:
    try:
        if value is None:
            return default
        return float(value)
    except (TypeError, ValueError):
        return default


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


def _compute_live_edge_guard_kpi(conn: sqlite3.Connection) -> Dict[str, Any]:
    counts: Dict[str, int] = {}
    for decision, count in conn.execute(
        """
        SELECT COALESCE(decision, 'UNKNOWN') AS decision, COUNT(*)
        FROM deployment_gate_status
        GROUP BY COALESCE(decision, 'UNKNOWN')
        """
    ).fetchall():
        key = str(decision).strip().upper() or "UNKNOWN"
        counts[key] = _to_int(count, 0)

    total_count = sum(counts.values())
    live_ready_count = counts.get("LIVE_READY", 0)
    status = "ok" if live_ready_count > 0 else "degraded"
    reasons = []
    if total_count == 0:
        reasons.append("deployment_gate_status_empty")
    if live_ready_count == 0:
        reasons.append("no_live_ready_strategy")

    return {
        "status": status,
        "total_count": total_count,
        "live_ready_count": live_ready_count,
        "decision_counts": counts,
        "reasons": reasons,
    }


def _compute_window_trade_metrics(rows: Iterable[sqlite3.Row]) -> Dict[str, Any]:
    trade_count = 0
    win_count = 0
    net_pnl = 0.0
    gross_profit = 0.0
    gross_loss_abs = 0.0
    current_loss_streak = 0
    max_loss_streak = 0

    for row in rows:
        trade_count += 1
        pnl = _to_float(row["pnl"], 0.0)
        net_pnl += pnl
        if pnl > 0:
            win_count += 1
            gross_profit += pnl
            current_loss_streak = 0
        elif pnl < 0:
            gross_loss_abs += abs(pnl)
            current_loss_streak += 1
            if current_loss_streak > max_loss_streak:
                max_loss_streak = current_loss_streak

    win_rate = (win_count / trade_count) if trade_count > 0 else 0.0
    if gross_loss_abs > 0:
        profit_factor = gross_profit / gross_loss_abs
    elif gross_profit > 0:
        profit_factor = 999.0
    else:
        profit_factor = 0.0

    return {
        "trade_count": trade_count,
        "win_count": win_count,
        "win_rate": win_rate,
        "profit_factor": profit_factor,
        "net_pnl": net_pnl,
        "max_loss_streak": max_loss_streak,
    }


def _compute_live_pnl_health_kpi(
    conn: sqlite3.Connection,
    *,
    now_ts: int,
    short_days: int,
    long_days: int,
) -> Dict[str, Any]:
    short_start = now_ts - max(1, int(short_days)) * 86400
    long_start = now_ts - max(1, int(long_days)) * 86400

    short_rows = conn.execute(
        "SELECT timestamp, pnl FROM trade_logs WHERE timestamp >= ? ORDER BY timestamp ASC",
        (short_start,),
    ).fetchall()
    long_rows = conn.execute(
        "SELECT timestamp, pnl FROM trade_logs WHERE timestamp >= ? ORDER BY timestamp ASC",
        (long_start,),
    ).fetchall()

    short_metrics = _compute_window_trade_metrics(short_rows)
    long_metrics = _compute_window_trade_metrics(long_rows)

    reasons = []
    if long_metrics["trade_count"] <= 0:
        reasons.append("no_trades_long_window")
    else:
        if float(long_metrics["profit_factor"]) < LIVE_PF_FLOOR:
            reasons.append("long_window_pf_below_floor")
        if float(long_metrics["win_rate"]) < LIVE_WR_FLOOR:
            reasons.append("long_window_wr_below_floor")
        if float(long_metrics["net_pnl"]) < LIVE_NET_PNL_FLOOR:
            reasons.append("long_window_net_pnl_negative")
        if int(long_metrics["max_loss_streak"]) > LIVE_MAX_LOSS_STREAK:
            reasons.append("long_window_loss_streak_exceeded")

    status = "ok" if not reasons else "degraded"
    return {
        "status": status,
        "window_7d": short_metrics,
        "window_30d": long_metrics,
        "reasons": reasons,
    }


def _compute_rank_conformance_kpi(rank_report_path: Path) -> Dict[str, Any]:
    if not rank_report_path.exists():
        return {
            "status": "degraded",
            "violations_total": 0,
            "floor": {},
            "conformance": {},
            "transitions": {},
            "reasons": ["rank_conformance_report_missing"],
        }
    try:
        payload = json.loads(rank_report_path.read_text(encoding="utf-8"))
    except Exception:
        return {
            "status": "degraded",
            "violations_total": 0,
            "floor": {},
            "conformance": {},
            "transitions": {},
            "reasons": ["rank_conformance_report_invalid_json"],
        }

    violations = payload.get("violations", {}) if isinstance(payload, dict) else {}
    transitions = payload.get("transitions", {}) if isinstance(payload, dict) else {}
    floor = violations.get("floor", {}) if isinstance(violations, Mapping) else {}
    conformance = violations.get("conformance", {}) if isinstance(violations, Mapping) else {}
    total = _to_int(violations.get("total"), 0) if isinstance(violations, Mapping) else 0
    status = "ok" if total == 0 else "degraded"
    reasons = [] if total == 0 else ["rank_conformance_violations_detected"]

    return {
        "status": status,
        "violations_total": total,
        "floor": floor if isinstance(floor, Mapping) else {},
        "conformance": conformance if isinstance(conformance, Mapping) else {},
        "transitions": transitions if isinstance(transitions, Mapping) else {},
        "reasons": reasons,
    }


def _compute_breeder_parent_quality_kpi(conn: sqlite3.Connection) -> Dict[str, Any]:
    rows = conn.execute(
        """
        SELECT rank, trades, sharpe, profit_factor, win_rate, max_dd,
               oos_sharpe, cpcv_pass_rate, cpcv_median_maxdd
        FROM strategies
        """
    ).fetchall()

    stats: Dict[str, Dict[str, Any]] = {
        "B": {"total_count": 0, "pass_count": 0},
        "A": {"total_count": 0, "pass_count": 0},
        "S": {"total_count": 0, "pass_count": 0},
    }

    for row in rows:
        row_dict = dict(row)
        rank = rc.normalize_rank(row_dict.get("rank"))
        if rank not in stats:
            continue
        stats[rank]["total_count"] += 1
        if rc.meets_rank_criteria(row_dict, rank):
            stats[rank]["pass_count"] += 1

    reasons = []
    status = "ok"
    for rank in ("B", "A", "S"):
        total = _to_int(stats[rank]["total_count"], 0)
        passed = _to_int(stats[rank]["pass_count"], 0)
        pass_rate = (passed / total) if total > 0 else None
        stats[rank]["pass_rate"] = pass_rate
        if total > 0 and passed < total:
            status = "degraded"
            reasons.append(f"{rank}_parent_conformance_drift")

    return {
        "status": status,
        "reasons": sorted(set(reasons)),
        "B": stats["B"],
        "A": stats["A"],
        "S": stats["S"],
    }


def build_edge_scorecard(
    *,
    db_path: Path,
    rank_report_path: Path,
    now_ts: int | None = None,
    short_days: int = 7,
    long_days: int = 30,
) -> Dict[str, Any]:
    now_unix = _to_int(now_ts, 0) if now_ts is not None else int(datetime.now(timezone.utc).timestamp())
    generated_at = datetime.fromtimestamp(now_unix, tz=timezone.utc).replace(microsecond=0).isoformat()

    with sqlite3.connect(str(db_path)) as conn:
        conn.row_factory = sqlite3.Row
        kpi_live_edge_guard = _compute_live_edge_guard_kpi(conn)
        kpi_live_pnl_health = _compute_live_pnl_health_kpi(
            conn,
            now_ts=now_unix,
            short_days=short_days,
            long_days=long_days,
        )
        kpi_breeder_parent_quality = _compute_breeder_parent_quality_kpi(conn)

    kpi_rank_conformance = _compute_rank_conformance_kpi(rank_report_path)
    statuses = [
        kpi_live_edge_guard["status"],
        kpi_live_pnl_health["status"],
        kpi_rank_conformance["status"],
        kpi_breeder_parent_quality["status"],
    ]
    overall_status = "ok" if all(s == "ok" for s in statuses) else "degraded"

    return {
        "generated_at": generated_at,
        "db_path": str(db_path),
        "rank_report_path": str(rank_report_path),
        "kpi_live_edge_guard": kpi_live_edge_guard,
        "kpi_live_pnl_health": kpi_live_pnl_health,
        "kpi_rank_conformance": kpi_rank_conformance,
        "kpi_breeder_parent_quality": kpi_breeder_parent_quality,
        "overall_status": overall_status,
    }


def run_edge_scorecard(
    *,
    db_path: Path,
    out_path: Path,
    history_dir: Path,
    rank_report_path: Path,
    now_ts: int | None = None,
    short_days: int = 7,
    long_days: int = 30,
) -> Dict[str, Any]:
    report = build_edge_scorecard(
        db_path=db_path,
        rank_report_path=rank_report_path,
        now_ts=now_ts,
        short_days=short_days,
        long_days=long_days,
    )
    _save_json(out_path, report)
    history_dir.mkdir(parents=True, exist_ok=True)

    dt = datetime.fromtimestamp(
        _to_int(now_ts, int(datetime.now(timezone.utc).timestamp())),
        tz=timezone.utc,
    )
    stamp = dt.strftime("%Y%m%d_%H%M%S")
    _save_json(history_dir / f"edge_scorecard_{stamp}.json", report)
    return report


def render_summary_line(report: Mapping[str, Any]) -> str:
    return (
        "Edge scorecard summary: "
        f"status={report.get('overall_status')} "
        f"k0={report.get('kpi_live_edge_guard', {}).get('status')} "
        f"k1={report.get('kpi_live_pnl_health', {}).get('status')} "
        f"k2={report.get('kpi_rank_conformance', {}).get('status')} "
        f"k3={report.get('kpi_breeder_parent_quality', {}).get('status')}"
    )


def should_discord_notify(*, when: str, overall_status: str) -> bool:
    mode = str(when or "").strip().lower()
    status = str(overall_status or "").strip().lower()
    if mode in {"", "never", "off", "0", "false", "no"}:
        return False
    if mode in {"always", "on", "1", "true", "yes"}:
        return True
    if mode in {"problem", "issues", "not_ok"}:
        return status in {"degraded", "critical"}
    return False


def resolve_discord_webhook(*, webhook: str, webhook_env: str) -> str:
    explicit = str(webhook or "").strip()
    if explicit:
        return explicit
    key = str(webhook_env or "").strip()
    if not key:
        return ""
    return str(os.getenv(key, "")).strip()


def _discord_color_for_status(status: str) -> int:
    value = str(status or "").strip().lower()
    if value == "ok":
        return 3066993  # green
    if value == "critical":
        return 15158332  # red
    if value == "degraded":
        return 16705372  # yellow
    return 10070709  # gray


def build_discord_scorecard_payload(*, report: Mapping[str, Any], now: datetime | None = None) -> Dict[str, Any]:
    ts = now or datetime.now(timezone.utc)
    status = str(report.get("overall_status", "unknown"))
    generated_at = str(report.get("generated_at", "")).strip() or "-"
    fields = [
        {"name": "Status", "value": status, "inline": True},
        {"name": "Generated", "value": generated_at, "inline": True},
        {"name": "KPI-0", "value": str(report.get("kpi_live_edge_guard", {}).get("status", "unknown")), "inline": True},
        {"name": "KPI-1", "value": str(report.get("kpi_live_pnl_health", {}).get("status", "unknown")), "inline": True},
        {"name": "KPI-2", "value": str(report.get("kpi_rank_conformance", {}).get("status", "unknown")), "inline": True},
        {
            "name": "KPI-3",
            "value": str(report.get("kpi_breeder_parent_quality", {}).get("status", "unknown")),
            "inline": True,
        },
    ]
    return {
        "embeds": [
            {
                "title": f"Edge Scorecard: {status.upper()}",
                "description": render_summary_line(report),
                "color": _discord_color_for_status(status),
                "timestamp": ts.isoformat(),
                "fields": fields,
            }
        ]
    }


def _post_json_webhook(url: str, payload: Mapping[str, Any], timeout_sec: int = 15) -> tuple[bool, str]:
    body = json.dumps(dict(payload), ensure_ascii=False).encode("utf-8")
    req = urllib.request.Request(
        url,
        data=body,
        headers={
            "Content-Type": "application/json",
            "User-Agent": "swimmy-edge-scorecard/1.0",
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=max(1, int(timeout_sec))) as resp:
            code = int(getattr(resp, "status", 0) or resp.getcode() or 0)
    except urllib.error.HTTPError as exc:
        return False, f"http_{int(exc.code)}"
    except Exception as exc:
        return False, f"{type(exc).__name__}:{exc}"
    if 200 <= code < 300:
        return True, str(code)
    return False, str(code)


def send_discord_notification(
    report: Mapping[str, Any],
    *,
    when: str,
    webhook_url: str,
    webhook_env: str,
    post_func: Callable[[str, Dict[str, Any], int], tuple[bool, str]] | None = None,
) -> Dict[str, Any]:
    status = str(report.get("overall_status", "unknown"))
    if not should_discord_notify(when=when, overall_status=status):
        return {"sent": False, "reason": "policy_skip", "status": status}

    resolved_webhook = resolve_discord_webhook(webhook=webhook_url, webhook_env=webhook_env)
    if not resolved_webhook:
        return {"sent": False, "reason": "webhook_missing", "status": status}

    payload = build_discord_scorecard_payload(report=report)
    payload["content"] = render_summary_line(report)
    sender = post_func or _post_json_webhook
    try:
        ok, detail = sender(resolved_webhook, payload, 15)
    except Exception as exc:
        return {
            "sent": False,
            "reason": "post_exception",
            "status": status,
            "detail": f"{type(exc).__name__}:{exc}",
        }

    if ok:
        return {"sent": True, "reason": "sent", "status": status, "detail": str(detail)}
    return {"sent": False, "reason": "post_failed", "status": status, "detail": str(detail)}


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME", "").strip()
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


def queue_discord_notification_via_notifier(
    *,
    webhook_url: str,
    payload: Mapping[str, Any],
    zmq_host: str,
    zmq_port: int,
) -> None:
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
        sock.setsockopt(zmq.LINGER, 1000)
        sock.connect(f"tcp://{zmq_host}:{int(zmq_port)}")
        time.sleep(0.05)
        sock.send_string(message)
        sock.close()
    finally:
        ctx.term()


def maybe_queue_discord_scorecard_notification(
    *,
    when: str,
    webhook: str,
    webhook_env: str,
    report: Mapping[str, Any],
    zmq_host: str,
    zmq_port: int,
) -> bool:
    status = str(report.get("overall_status", "unknown"))
    if not should_discord_notify(when=when, overall_status=status):
        return False

    webhook_url = resolve_discord_webhook(webhook=webhook, webhook_env=webhook_env)
    if not webhook_url:
        return False

    discord_payload = build_discord_scorecard_payload(report=report)
    try:
        queue_discord_notification_via_notifier(
            webhook_url=webhook_url,
            payload=discord_payload,
            zmq_host=zmq_host,
            zmq_port=zmq_port,
        )
    except Exception as exc:
        msg = str(exc)
        msg = msg if len(msg) <= 200 else msg[:197] + "..."
        print(
            f"[WARN] Edge scorecard Discord notify failed: {type(exc).__name__}: {msg}",
            file=sys.stderr,
            flush=True,
        )
    return True


def main() -> None:
    parser = argparse.ArgumentParser(description="Generate Swimmy edge scorecard")
    parser.add_argument("--db", default="data/memory/swimmy.db")
    parser.add_argument("--out", default="data/reports/edge_scorecard_latest.json")
    parser.add_argument("--history-dir", default="data/reports/edge_scorecard")
    parser.add_argument("--rank-report", default="data/reports/rank_conformance_latest.json")
    parser.add_argument("--now-ts", type=int, default=None, help="Unix timestamp override for deterministic runs")
    parser.add_argument("--short-days", type=int, default=7)
    parser.add_argument("--long-days", type=int, default=30)
    parser.add_argument(
        "--discord-when",
        choices=["never", "problem", "always"],
        default=_env_str("EDGE_SCORECARD_DISCORD_WHEN", "never"),
    )
    parser.add_argument("--discord-webhook", default=_env_str("EDGE_SCORECARD_DISCORD_WEBHOOK", ""))
    parser.add_argument(
        "--discord-webhook-env",
        default=_env_str("EDGE_SCORECARD_DISCORD_WEBHOOK_ENV", "SWIMMY_DISCORD_ALERTS"),
    )
    parser.add_argument("--discord-zmq-host", default=_env_str("EDGE_SCORECARD_DISCORD_ZMQ_HOST", "localhost"))
    parser.add_argument(
        "--discord-zmq-port",
        type=int,
        default=_env_int("EDGE_SCORECARD_DISCORD_ZMQ_PORT", _env_int("SWIMMY_PORT_NOTIFIER", 5562)),
    )
    args = parser.parse_args()

    report = run_edge_scorecard(
        db_path=Path(args.db),
        out_path=Path(args.out),
        history_dir=Path(args.history_dir),
        rank_report_path=Path(args.rank_report),
        now_ts=args.now_ts,
        short_days=args.short_days,
        long_days=args.long_days,
    )
    print(json.dumps(report, ensure_ascii=False))
    print(render_summary_line(report))
    if maybe_queue_discord_scorecard_notification(
        when=args.discord_when,
        webhook=args.discord_webhook,
        webhook_env=args.discord_webhook_env,
        report=report,
        zmq_host=args.discord_zmq_host,
        zmq_port=args.discord_zmq_port,
    ):
        print("[INFO] Edge scorecard Discord notification queued")


if __name__ == "__main__":
    main()
