#!/usr/bin/env python3
"""Audit backtest heartbeat freshness from logs/backtest.log."""

from __future__ import annotations

import argparse
import json
import re
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict


_HEARTBEAT_RE = re.compile(
    r"HEARTBEAT\s+"
    r"inflight=(?P<inflight>\S+)\s+"
    r"recv=(?P<recv>\d+)\s+"
    r"submit=(?P<submit>\d+)\s+"
    r"done=(?P<done>\d+)\s+"
    r"sent=(?P<sent>\d+)\s+"
    r"rx_age=(?P<rx>[0-9]+s|-)\s+"
    r"tx_age=(?P<tx>[0-9]+s|-)"
)


def _parse_age_seconds(token: str) -> int | None:
    text = str(token or "").strip()
    if not text or text == "-":
        return None
    if text.endswith("s"):
        text = text[:-1]
    try:
        value = int(text)
    except ValueError:
        return None
    return max(0, value)


def parse_heartbeat_line(line: str) -> Dict[str, Any] | None:
    match = _HEARTBEAT_RE.search(line or "")
    if not match:
        return None
    return {
        "line": line.rstrip("\n"),
        "inflight": match.group("inflight"),
        "recv": int(match.group("recv")),
        "submit": int(match.group("submit")),
        "done": int(match.group("done")),
        "sent": int(match.group("sent")),
        "rx_age_seconds": _parse_age_seconds(match.group("rx")),
        "tx_age_seconds": _parse_age_seconds(match.group("tx")),
    }


def _find_last_heartbeat(log_path: Path) -> Dict[str, Any] | None:
    try:
        lines = log_path.read_text(encoding="utf-8", errors="replace").splitlines()
    except Exception:
        return None
    for line in reversed(lines):
        parsed = parse_heartbeat_line(line)
        if parsed is not None:
            return parsed
    return None


def check_status(
    *,
    log_path: Path,
    max_log_age_seconds: int,
    max_rx_age_seconds: int,
    max_tx_age_seconds: int,
) -> Dict[str, Any]:
    out: Dict[str, Any] = {
        "ok": True,
        "log_path": str(log_path),
        "checked_at": datetime.now(timezone.utc).replace(microsecond=0).isoformat(),
        "log_age_seconds": None,
        "max_log_age_seconds": max(0, int(max_log_age_seconds)),
        "max_rx_age_seconds": max(0, int(max_rx_age_seconds)),
        "max_tx_age_seconds": max(0, int(max_tx_age_seconds)),
        "rx_age_seconds": None,
        "tx_age_seconds": None,
        "inflight": None,
        "issues": [],
    }
    issues = out["issues"]

    if not log_path.exists():
        out["ok"] = False
        issues.append("backtest log not found")
        return out

    now_ts = datetime.now(timezone.utc).timestamp()
    try:
        log_age_seconds = int(max(0.0, now_ts - float(log_path.stat().st_mtime)))
    except Exception:
        log_age_seconds = None
    out["log_age_seconds"] = log_age_seconds
    if log_age_seconds is None:
        out["ok"] = False
        issues.append("failed to read backtest log mtime")
    elif log_age_seconds > out["max_log_age_seconds"]:
        out["ok"] = False
        issues.append(f"log too old: age={log_age_seconds}s > max={out['max_log_age_seconds']}s")

    heartbeat = _find_last_heartbeat(log_path)
    if heartbeat is None:
        out["ok"] = False
        issues.append("heartbeat line not found")
        return out

    out["inflight"] = heartbeat.get("inflight")
    out["rx_age_seconds"] = heartbeat.get("rx_age_seconds")
    out["tx_age_seconds"] = heartbeat.get("tx_age_seconds")

    rx_age = heartbeat.get("rx_age_seconds")
    tx_age = heartbeat.get("tx_age_seconds")
    if rx_age is None:
        out["ok"] = False
        issues.append("rx_age missing")
    elif rx_age > out["max_rx_age_seconds"]:
        out["ok"] = False
        issues.append(f"rx_age too high: {rx_age}s > max={out['max_rx_age_seconds']}s")

    if tx_age is None:
        out["ok"] = False
        issues.append("tx_age missing")
    elif tx_age > out["max_tx_age_seconds"]:
        out["ok"] = False
        issues.append(f"tx_age too high: {tx_age}s > max={out['max_tx_age_seconds']}s")

    return out


def main() -> None:
    parser = argparse.ArgumentParser(description="Check backtest heartbeat freshness")
    parser.add_argument("--log", default="logs/backtest.log", help="Path to backtest log")
    parser.add_argument("--max-log-age-seconds", type=int, default=600)
    parser.add_argument("--max-rx-age-seconds", type=int, default=300)
    parser.add_argument("--max-tx-age-seconds", type=int, default=300)
    parser.add_argument("--fail-on-problem", action="store_true")
    args = parser.parse_args()

    status = check_status(
        log_path=Path(args.log),
        max_log_age_seconds=max(0, args.max_log_age_seconds),
        max_rx_age_seconds=max(0, args.max_rx_age_seconds),
        max_tx_age_seconds=max(0, args.max_tx_age_seconds),
    )
    print(json.dumps(status, ensure_ascii=False))
    if args.fail_on_problem and not status.get("ok", False):
        raise SystemExit(1)


if __name__ == "__main__":
    main()
