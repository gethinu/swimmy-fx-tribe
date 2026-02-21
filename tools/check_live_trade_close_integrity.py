#!/usr/bin/env python3
"""
Check live trade-close integrity signals:
- Recent trade_logs context quality (strategy/category/direction not Unknown/NIL)
- Tail log markers for invalid POSITIONS SEXP and legacy narrative output

Exit code:
  0: healthy (or no recent trades and no hard errors)
  1: contract violation detected
"""

from __future__ import annotations

import argparse
import sqlite3
import sys
import time
from pathlib import Path

CL_UNIVERSAL_OFFSET = 2208988800  # unix epoch -> Common Lisp universal time


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Check live trade-close integrity")
    p.add_argument(
        "--db",
        default="data/memory/swimmy.db",
        help="Path to SQLite DB (default: data/memory/swimmy.db)",
    )
    p.add_argument(
        "--log",
        default="logs/swimmy.log",
        help="Path to Swimmy brain log (default: logs/swimmy.log)",
    )
    p.add_argument(
        "--lookback-minutes",
        type=int,
        default=240,
        help="Lookback window for trade_logs query (default: 240)",
    )
    p.add_argument(
        "--tail-lines",
        type=int,
        default=5000,
        help="Tail lines scanned in log for parser/legacy markers (default: 5000)",
    )
    p.add_argument(
        "--require-recent-trades",
        action="store_true",
        help="Fail when no recent trade_logs rows exist in lookback window",
    )
    p.add_argument(
        "--after-id",
        type=int,
        default=0,
        help="Only evaluate trade_logs rows with id > after-id (default: 0)",
    )
    return p.parse_args()


def nil_like(value: object) -> bool:
    if value is None:
        return True
    token = str(value).strip().upper()
    return token in {"", "NIL", "NULL", "NONE", "UNKNOWN"}


def read_tail_lines(path: Path, n: int) -> list[str]:
    if not path.exists():
        return []
    with path.open("r", encoding="utf-8", errors="ignore") as f:
        lines = f.readlines()
    if n <= 0:
        return lines
    return lines[-n:]


def check_trade_logs(
    db_path: Path, lookback_minutes: int, after_id: int
) -> tuple[int, int, list[str], list[str]]:
    if not db_path.exists():
        return (0, 0, [], [f"DB not found: {db_path}"])
    cutoff = int(time.time()) + CL_UNIVERSAL_OFFSET - lookback_minutes * 60
    bad = 0
    total = 0
    details: list[str] = []
    try:
        with sqlite3.connect(db_path) as con:
            cur = con.cursor()
            cur.execute(
                """
                SELECT id, timestamp, strategy_name, symbol, direction, category, pnl
                FROM trade_logs
                WHERE timestamp >= ? AND id > ?
                ORDER BY id DESC
                """,
                (cutoff, max(0, int(after_id))),
            )
            rows = cur.fetchall()
    except sqlite3.Error as e:
        return (0, 0, [], [f"trade_logs query failed: {e}"])
    for row in rows:
        total += 1
        trade_id, ts, strategy_name, symbol, direction, category, pnl = row
        issues = []
        if nil_like(strategy_name):
            issues.append("strategy_name")
        if nil_like(direction):
            issues.append("direction")
        if nil_like(category):
            issues.append("category")
        if issues:
            bad += 1
            details.append(
                f"id={trade_id} ts={ts} symbol={symbol} pnl={pnl} missing={','.join(issues)} "
                f"raw=({strategy_name},{direction},{category})"
            )
    return (total, bad, details, [])


def main() -> int:
    args = parse_args()
    db_path = Path(args.db)
    log_path = Path(args.log)

    failures = 0
    warnings = 0

    total, bad, bad_details, infra_errors = check_trade_logs(
        db_path, args.lookback_minutes, args.after_id
    )
    print(
        "[CHECK] trade_logs lookback={}m after_id={} rows={} bad={}".format(
            args.lookback_minutes, args.after_id, total, bad
        )
    )
    for e in infra_errors:
        print(f"[FAIL] {e}")
    for d in bad_details[:20]:
        print(f"[BAD] {d}")
    if len(bad_details) > 20:
        print(f"[BAD] ... and {len(bad_details) - 20} more")

    if infra_errors:
        failures += 1
    elif total == 0:
        msg = "[WARN] no recent trade_logs rows in lookback window"
        if args.require_recent_trades:
            msg = msg.replace("[WARN]", "[FAIL]")
            failures += 1
        else:
            warnings += 1
        print(msg)
    elif bad > 0:
        failures += 1

    tail = read_tail_lines(log_path, args.tail_lines)
    invalid_positions = [
        line.strip()
        for line in tail
        if "Unsafe/invalid SEXP ignored head=((type . \"POSITIONS\")" in line
    ]
    legacy_nil_notice = [
        line.strip() for line in tail if "【NIL】" in line or "戦略: **Unknown** (NIL)" in line
    ]
    fixed_template_hits = [line.strip() for line in tail if "Trade Closed | " in line]

    print(
        "[CHECK] log tail lines={} invalid_positions={} legacy_nil_notice={} fixed_template_hits={}".format(
            args.tail_lines,
            len(invalid_positions),
            len(legacy_nil_notice),
            len(fixed_template_hits),
        )
    )
    if invalid_positions:
        failures += 1
        print(f"[FAIL] invalid POSITIONS SEXP detected in log tail (showing up to 5)")
        for line in invalid_positions[:5]:
            print(f"[BAD] {line}")

    if legacy_nil_notice:
        failures += 1
        print("[FAIL] legacy NIL/Unknown narrative markers detected in log tail (showing up to 5)")
        for line in legacy_nil_notice[:5]:
            print(f"[BAD] {line}")

    if failures == 0:
        print(f"[OK] live trade-close integrity check passed (warn={warnings})")
        return 0
    print(f"[FAIL] live trade-close integrity check failed (fail={failures} warn={warnings})")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
