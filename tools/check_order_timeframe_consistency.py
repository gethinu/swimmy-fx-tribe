#!/usr/bin/env python3
"""Check live order timeframe consistency between telemetry log and strategy DB."""

from __future__ import annotations

import argparse
import json
import sqlite3
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional


def parse_iso8601(value: str) -> datetime:
    text = value.strip()
    if text.endswith("Z"):
        text = text[:-1] + "+00:00"
    dt = datetime.fromisoformat(text)
    if dt.tzinfo is not None:
        dt = dt.astimezone(timezone.utc).replace(tzinfo=None)
    return dt


def parse_tf_minutes(value: Any) -> Optional[int]:
    if value is None:
        return None
    if isinstance(value, (int, float)):
        minutes = int(round(value))
        return minutes if minutes > 0 else None

    text = str(value).strip().upper()
    if not text or text == "NIL":
        return None
    if text in {"MN", "MN1"}:
        return 43200
    if text.isdigit():
        minutes = int(text)
        return minutes if minutes > 0 else None
    if len(text) >= 2 and text[0] in {"M", "H", "D", "W"} and text[1:].isdigit():
        mag = int(text[1:])
        if mag <= 0:
            return None
        if text[0] == "M":
            return mag
        if text[0] == "H":
            return 60 * mag
        if text[0] == "D":
            return 1440 * mag
        if text[0] == "W":
            return 10080 * mag
    return None


def load_expected_timeframes(db_path: Path) -> Dict[str, int]:
    conn = sqlite3.connect(str(db_path))
    try:
        cur = conn.cursor()
        cur.execute("SELECT name, timeframe FROM strategies")
        rows = cur.fetchall()
    finally:
        conn.close()

    result: Dict[str, int] = {}
    for name, timeframe in rows:
        if not isinstance(name, str):
            continue
        tf_min = parse_tf_minutes(timeframe)
        if tf_min is not None:
            result[name] = tf_min
    return result


def iter_order_submitted(log_path: Path, since: Optional[datetime]) -> Iterable[Dict[str, Any]]:
    with log_path.open("r", encoding="utf-8") as f:
        for line_no, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
            except json.JSONDecodeError:
                continue
            if obj.get("event_type") != "execution.order_submitted":
                continue
            ts_text = obj.get("timestamp")
            if not isinstance(ts_text, str):
                continue
            try:
                ts = parse_iso8601(ts_text)
            except ValueError:
                continue
            if since is not None and ts < since:
                continue
            data = obj.get("data") or {}
            yield {
                "line": line_no,
                "timestamp": ts_text,
                "strategy": data.get("strategy"),
                "timeframe": data.get("timeframe"),
                "symbol": data.get("symbol"),
                "direction": data.get("direction"),
            }


def detect_timeframe_issues(
    events: Iterable[Dict[str, Any]], expected_timeframes: Dict[str, int]
) -> List[Dict[str, Any]]:
    issues: List[Dict[str, Any]] = []
    for event in events:
        strategy = str(event.get("strategy") or "").strip()
        actual_tf = event.get("timeframe")
        actual_minutes = parse_tf_minutes(actual_tf)

        if strategy not in expected_timeframes:
            issues.append(
                {
                    "type": "strategy_not_found",
                    "strategy": strategy,
                    "actual_timeframe": actual_tf,
                    "actual_minutes": actual_minutes,
                    "line": event.get("line"),
                    "timestamp": event.get("timestamp"),
                    "symbol": event.get("symbol"),
                    "direction": event.get("direction"),
                }
            )
            continue

        expected_minutes = expected_timeframes[strategy]
        if actual_minutes is None:
            issues.append(
                {
                    "type": "invalid_timeframe",
                    "strategy": strategy,
                    "actual_timeframe": actual_tf,
                    "actual_minutes": actual_minutes,
                    "expected_minutes": expected_minutes,
                    "line": event.get("line"),
                    "timestamp": event.get("timestamp"),
                    "symbol": event.get("symbol"),
                    "direction": event.get("direction"),
                }
            )
            continue

        if actual_minutes != expected_minutes:
            issues.append(
                {
                    "type": "timeframe_mismatch",
                    "strategy": strategy,
                    "actual_timeframe": actual_tf,
                    "actual_minutes": actual_minutes,
                    "expected_minutes": expected_minutes,
                    "line": event.get("line"),
                    "timestamp": event.get("timestamp"),
                    "symbol": event.get("symbol"),
                    "direction": event.get("direction"),
                }
            )
    return issues


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--db", type=Path, default=Path("data/memory/swimmy.db"), help="SQLite DB path")
    parser.add_argument("--log", type=Path, default=Path("logs/swimmy.json.log"), help="Telemetry log path")
    time_window = parser.add_mutually_exclusive_group()
    time_window.add_argument("--since", type=str, default=None, help="ISO8601 lower bound for log timestamp")
    time_window.add_argument(
        "--lookback-minutes",
        type=int,
        default=None,
        help="Use only events newer than local now - N minutes",
    )
    parser.add_argument(
        "--strategy",
        type=str,
        default=None,
        help="Optional exact strategy-name filter for event scan",
    )
    parser.add_argument(
        "--fail-on-issues",
        action="store_true",
        help="Return non-zero when any issue is detected",
    )
    return parser


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()

    if not args.db.exists():
        print(f"[ERROR] db not found: {args.db}")
        return 2
    if not args.log.exists():
        print(f"[ERROR] log not found: {args.log}")
        return 2

    since_dt = parse_iso8601(args.since) if args.since else None
    since_label = args.since or "beginning"
    if args.lookback_minutes is not None:
        if args.lookback_minutes <= 0:
            print("[ERROR] --lookback-minutes must be > 0")
            return 2
        # Use local naive time to match telemetry rows that are usually emitted
        # without timezone offsets.
        since_dt = datetime.now() - timedelta(minutes=args.lookback_minutes)
        since_label = since_dt.isoformat(timespec="seconds")
    expected = load_expected_timeframes(args.db)
    events = list(iter_order_submitted(args.log, since_dt))
    if args.strategy:
        events = [e for e in events if str(e.get("strategy") or "") == args.strategy]

    issues = detect_timeframe_issues(events, expected)

    print(
        f"[SUMMARY] orders={len(events)} issues={len(issues)} "
        f"since={since_label} strategy={args.strategy or '*'}"
    )
    for issue in issues:
        print(
            "[ISSUE] "
            f"type={issue.get('type')} line={issue.get('line')} ts={issue.get('timestamp')} "
            f"strategy={issue.get('strategy')} actual={issue.get('actual_timeframe')} "
            f"expected_minutes={issue.get('expected_minutes')} symbol={issue.get('symbol')} "
            f"dir={issue.get('direction')}"
        )

    if args.fail_on_issues and issues:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
