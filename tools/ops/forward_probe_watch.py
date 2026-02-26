#!/usr/bin/env python3
"""Watch FORWARD_RUNNING trade growth and alert when probes stall."""

from __future__ import annotations

import argparse
import json
import sqlite3
import time
import urllib.error
import urllib.request
from pathlib import Path
from typing import Any, Mapping


DEFAULT_DB_PATH = Path("data/memory/swimmy.db")
DEFAULT_STATE_PATH = Path("data/memory/forward_probe_watch_state.json")
DEFAULT_INTERVAL_SECONDS = 3600
DEFAULT_WEBHOOK_ENV = "SWIMMY_DISCORD_ALERTS"


def _to_int(value: Any, default: int = 0) -> int:
    try:
        if value is None:
            return default
        return int(value)
    except (TypeError, ValueError):
        return default


def load_state(path: Path) -> dict[str, Any]:
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return {}
    if isinstance(payload, dict):
        return payload
    return {}


def save_state(path: Path, payload: Mapping[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    tmp.replace(path)


def fetch_strategy_snapshot(db_path: Path, strategy_name: str) -> dict[str, Any]:
    with sqlite3.connect(str(db_path)) as conn:
        conn.row_factory = sqlite3.Row
        row = conn.execute(
            """
            SELECT d.strategy_name,
                   COALESCE(d.forward_trades, 0) AS forward_trades,
                   COALESCE(d.decision, '') AS decision,
                   COALESCE(d.reason, '') AS reason,
                   COALESCE(d.updated_at, 0) AS updated_at,
                   COALESCE(s.symbol, '') AS symbol
              FROM deployment_gate_status d
              LEFT JOIN strategies s
                ON s.name = d.strategy_name
             WHERE d.strategy_name = ?
            """,
            (strategy_name,),
        ).fetchone()
    if row is None:
        raise RuntimeError(f"strategy not found in deployment_gate_status: {strategy_name}")
    return {
        "strategy_name": str(row["strategy_name"]),
        "symbol": str(row["symbol"] or ""),
        "forward_trades": _to_int(row["forward_trades"], 0),
        "decision": str(row["decision"] or ""),
        "reason": str(row["reason"] or ""),
        "updated_at": _to_int(row["updated_at"], 0),
    }


def assess_probe_growth(
    *,
    previous_trades: int,
    current_trades: int,
    elapsed_seconds: int,
    interval_seconds: int,
) -> dict[str, Any]:
    prev = _to_int(previous_trades, 0)
    curr = _to_int(current_trades, 0)
    elapsed = max(0, _to_int(elapsed_seconds, 0))
    interval = max(1, _to_int(interval_seconds, DEFAULT_INTERVAL_SECONDS))
    delta = curr - prev

    if elapsed < interval:
        return {
            "status": "SKIP",
            "delta": delta,
            "reason": f"interval_not_reached: elapsed={elapsed}s < interval={interval}s",
        }
    if delta <= 0:
        return {
            "status": "ALERT",
            "delta": delta,
            "reason": f"probe_growth_stalled: delta={delta} (prev={prev}, curr={curr})",
        }
    return {
        "status": "OK",
        "delta": delta,
        "reason": f"probe_growth_ok: delta={delta} (prev={prev}, curr={curr})",
    }


def build_alert_message(
    *,
    strategy_name: str,
    symbol: str,
    current_trades: int,
    elapsed_seconds: int,
    decision: str,
    reason: str,
) -> str:
    elapsed_hours = max(0.0, float(_to_int(elapsed_seconds, 0))) / 3600.0
    return (
        "ALERT: FORWARD probe stalled\n"
        f"strategy={strategy_name}\n"
        f"symbol={symbol or 'UNKNOWN'}\n"
        f"decision={decision or 'UNKNOWN'}\n"
        f"forward_trades={_to_int(current_trades, 0)}\n"
        f"elapsed_hours={elapsed_hours:.2f}\n"
        f"gate_reason={reason or 'N/A'}"
    )


def _resolve_webhook(explicit: str, env_name: str) -> str:
    token = (explicit or "").strip()
    if token:
        return token
    import os

    return (os.getenv(env_name) or "").strip()


def send_discord_alert(webhook_url: str, message: str, *, timeout_seconds: int = 10) -> dict[str, Any]:
    payload = {
        "embeds": [
            {
                "title": "FORWARD Probe Watch",
                "description": message,
                "color": 15158332,
            }
        ]
    }
    data = json.dumps(payload).encode("utf-8")
    req = urllib.request.Request(
        webhook_url,
        data=data,
        headers={"Content-Type": "application/json; charset=utf-8", "User-Agent": "swimmy-forward-probe-watch/1.0"},
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=timeout_seconds) as resp:  # noqa: S310
            code = getattr(resp, "status", 0) or 0
    except urllib.error.URLError as exc:
        return {"sent": False, "status_code": 0, "error": str(exc)}

    if code in (200, 204):
        return {"sent": True, "status_code": code, "error": ""}
    return {"sent": False, "status_code": code, "error": f"unexpected status {code}"}


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Watch forward probe growth and alert on stalls.")
    p.add_argument("--strategy", required=True, help="strategy_name in deployment_gate_status")
    p.add_argument("--db-path", default=str(DEFAULT_DB_PATH))
    p.add_argument("--state-path", default=str(DEFAULT_STATE_PATH))
    p.add_argument("--interval-seconds", type=int, default=DEFAULT_INTERVAL_SECONDS)
    p.add_argument("--notify", action="store_true", help="send Discord alert when stalled")
    p.add_argument("--webhook", default="", help="Discord webhook URL (optional)")
    p.add_argument("--webhook-env", default=DEFAULT_WEBHOOK_ENV, help="env var used when --webhook is empty")
    p.add_argument("--fail-on-alert", action="store_true", help="exit 1 when ALERT is detected")
    return p.parse_args()


def main() -> int:
    args = parse_args()
    db_path = Path(args.db_path)
    state_path = Path(args.state_path)
    now = int(time.time())

    snapshot = fetch_strategy_snapshot(db_path, args.strategy)
    state = load_state(state_path)
    prev = state.get(args.strategy)

    if not isinstance(prev, dict):
        state[args.strategy] = {
            "observed_at_epoch": now,
            "forward_trades": snapshot["forward_trades"],
            "symbol": snapshot["symbol"],
            "decision": snapshot["decision"],
            "reason": snapshot["reason"],
            "updated_at": snapshot["updated_at"],
        }
        save_state(state_path, state)
        print(
            json.dumps(
                {
                    "status": "BOOTSTRAP",
                    "strategy_name": args.strategy,
                    "forward_trades": snapshot["forward_trades"],
                    "state_path": str(state_path),
                },
                ensure_ascii=False,
            )
        )
        return 0

    elapsed = max(0, now - _to_int(prev.get("observed_at_epoch"), 0))
    verdict = assess_probe_growth(
        previous_trades=_to_int(prev.get("forward_trades"), 0),
        current_trades=_to_int(snapshot.get("forward_trades"), 0),
        elapsed_seconds=elapsed,
        interval_seconds=_to_int(args.interval_seconds, DEFAULT_INTERVAL_SECONDS),
    )

    alert_message = ""
    notify_result: dict[str, Any] | None = None
    if verdict["status"] == "ALERT":
        alert_message = build_alert_message(
            strategy_name=args.strategy,
            symbol=str(snapshot.get("symbol") or ""),
            current_trades=_to_int(snapshot.get("forward_trades"), 0),
            elapsed_seconds=elapsed,
            decision=str(snapshot.get("decision") or ""),
            reason=str(snapshot.get("reason") or ""),
        )
        if args.notify:
            webhook = _resolve_webhook(args.webhook, args.webhook_env)
            if webhook:
                notify_result = send_discord_alert(webhook, alert_message)
            else:
                notify_result = {"sent": False, "status_code": 0, "error": f"webhook not configured ({args.webhook_env})"}

    state[args.strategy] = {
        "observed_at_epoch": now,
        "forward_trades": snapshot["forward_trades"],
        "symbol": snapshot["symbol"],
        "decision": snapshot["decision"],
        "reason": snapshot["reason"],
        "updated_at": snapshot["updated_at"],
    }
    save_state(state_path, state)

    output = {
        "status": verdict["status"],
        "strategy_name": args.strategy,
        "symbol": snapshot["symbol"],
        "decision": snapshot["decision"],
        "gate_reason": snapshot["reason"],
        "forward_trades": snapshot["forward_trades"],
        "delta": verdict["delta"],
        "elapsed_seconds": elapsed,
        "reason": verdict["reason"],
        "state_path": str(state_path),
    }
    if alert_message:
        output["alert_message"] = alert_message
    if notify_result is not None:
        output["notify"] = notify_result
    print(json.dumps(output, ensure_ascii=False))

    if verdict["status"] == "ALERT" and args.fail_on_alert:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
