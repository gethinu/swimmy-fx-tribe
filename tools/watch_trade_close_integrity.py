#!/usr/bin/env python3
"""
Watch live logs/DB until a new TRADE_CLOSED-derived trade_logs row appears,
then validate context quality.

Exit code:
  0: new trades observed and all context fields valid
  1: violation detected (invalid POSITIONS SEXP in new log tail OR bad trade context)
  2: timeout without new trades
"""

from __future__ import annotations

import argparse
import sqlite3
import time
from pathlib import Path


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Watch for next trade-close integrity")
    p.add_argument("--db", default="data/memory/swimmy.db")
    p.add_argument("--log", default="logs/swimmy.log")
    p.add_argument("--timeout-sec", type=int, default=900)
    p.add_argument("--poll-sec", type=float, default=5.0)
    return p.parse_args()


def nil_like(v: object) -> bool:
    if v is None:
        return True
    s = str(v).strip().upper()
    return s in {"", "NIL", "NULL", "NONE", "UNKNOWN"}


def get_latest_trade_id(db_path: Path) -> int:
    with sqlite3.connect(db_path) as con:
        cur = con.cursor()
        cur.execute("SELECT COALESCE(MAX(id), 0) FROM trade_logs")
        row = cur.fetchone()
    return int(row[0] or 0)


def get_new_trades(db_path: Path, after_id: int) -> list[tuple]:
    with sqlite3.connect(db_path) as con:
        cur = con.cursor()
        cur.execute(
            """
            SELECT id, timestamp, strategy_name, symbol, direction, category, pnl
            FROM trade_logs
            WHERE id > ?
            ORDER BY id ASC
            """,
            (after_id,),
        )
        return cur.fetchall()


def main() -> int:
    args = parse_args()
    db_path = Path(args.db)
    log_path = Path(args.log)

    if not db_path.exists():
        print(f"[FAIL] db not found: {db_path}")
        return 1
    if not log_path.exists():
        print(f"[FAIL] log not found: {log_path}")
        return 1

    baseline_id = get_latest_trade_id(db_path)
    print(f"[WATCH] baseline trade_logs id={baseline_id}")

    with log_path.open("r", encoding="utf-8", errors="ignore") as f:
        f.seek(0, 2)
        offset = f.tell()

    deadline = time.time() + max(1, args.timeout_sec)
    invalid_positions_seen = 0

    while time.time() < deadline:
        # Read appended log chunk and fail fast on parser regression.
        with log_path.open("r", encoding="utf-8", errors="ignore") as f:
            f.seek(offset)
            chunk = f.read()
            offset = f.tell()
        if chunk:
            hits = chunk.count('Unsafe/invalid SEXP ignored head=((type . "POSITIONS")')
            if hits > 0:
                invalid_positions_seen += hits
                print(f"[FAIL] detected invalid POSITIONS SEXP in new log chunk (hits={hits})")
                return 1

        new_rows = get_new_trades(db_path, baseline_id)
        if new_rows:
            bad = 0
            print(f"[WATCH] new trade rows={len(new_rows)}")
            for r in new_rows:
                trade_id, ts, strategy_name, symbol, direction, category, pnl = r
                fields = []
                if nil_like(strategy_name):
                    fields.append("strategy_name")
                if nil_like(direction):
                    fields.append("direction")
                if nil_like(category):
                    fields.append("category")
                if fields:
                    bad += 1
                    print(
                        "[BAD] id={} ts={} symbol={} pnl={} missing={} raw=({},{},{})".format(
                            trade_id,
                            ts,
                            symbol,
                            pnl,
                            ",".join(fields),
                            strategy_name,
                            direction,
                            category,
                        )
                    )
                else:
                    print(
                        "[OK] id={} ts={} symbol={} pnl={} ctx=({},{},{})".format(
                            trade_id,
                            ts,
                            symbol,
                            pnl,
                            strategy_name,
                            direction,
                            category,
                        )
                    )
            if bad > 0:
                print(f"[FAIL] trade-close context violation rows={bad}/{len(new_rows)}")
                return 1
            print("[OK] trade-close integrity validated for new rows")
            return 0

        time.sleep(max(0.1, args.poll_sec))

    print(
        f"[TIMEOUT] no new trades within {args.timeout_sec}s "
        f"(invalid_positions_seen={invalid_positions_seen})"
    )
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
