#!/usr/bin/env python3
"""P1 (Thread A) one-shot migration: make ``strategies.oos_sharpe`` honest.

Two classes of dishonest OOS values are normalised to SQL NULL ("unvalidated"):

  1. **Mirror OOS** — ``oos_sharpe == sharpe`` (and non-zero). These are fake
     out-of-sample numbers: the in-sample Sharpe was copied into the OOS slot
     (e.g. ``SHARPE 17.264853 / OOS-SHARPE 17.264853 / TRADES 35``). They must
     never be treated as validation evidence.

  2. **Zero OOS** — ``|oos_sharpe| <= eps``. The strategy struct used to default
     ``oos-sharpe`` to 0.0, so "never validated" was stored as 0.0 and became
     indistinguishable from a genuine OOS score of 0. After P1a the struct
     defaults to nil and upsert writes NULL, but historical rows still carry 0.0.

Both become NULL so the honest gate's ``oos_sharpe is not None and > 0`` test,
and any rank/breeding logic, treat them as unvalidated rather than measured.

Default is a DRY RUN. Pass ``--apply`` to commit (inside a single transaction).
A durable log of affected rows is written under ``data/reports/`` unless
``--no-log`` is given.

Usage:
    python -m tools.tribe.migrate_p1_oos_nil                 # dry-run
    python -m tools.tribe.migrate_p1_oos_nil --apply         # commit
    python -m tools.tribe.migrate_p1_oos_nil --db path/to.db --eps 1e-6
"""

from __future__ import annotations

import argparse
import os
import sqlite3
from datetime import datetime, timezone

DEFAULT_DB = os.path.join("data", "memory", "swimmy.db")

MIRROR_WHERE = (
    "oos_sharpe IS NOT NULL AND sharpe IS NOT NULL "
    "AND oos_sharpe = sharpe AND oos_sharpe <> 0"
)
ZERO_WHERE = "oos_sharpe IS NOT NULL AND ABS(oos_sharpe) <= ?"


def _fetch(cur, where, params, cols="name, rank, sharpe, oos_sharpe, trades", limit=None):
    sql = f"SELECT {cols} FROM strategies WHERE {where}"
    if limit is not None:
        sql += f" LIMIT {int(limit)}"
    cur.execute(sql, params)
    return cur.fetchall()


def _count(cur, where, params):
    cur.execute(f"SELECT COUNT(*) FROM strategies WHERE {where}", params)
    return cur.fetchone()[0]


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description="P1 OOS-nil migration (mirror + zero -> NULL)")
    ap.add_argument("--db", default=DEFAULT_DB, help="Path to swimmy.db")
    ap.add_argument("--eps", type=float, default=1e-6, help="Abs threshold treated as zero")
    ap.add_argument("--sample", type=int, default=20, help="Preview N affected rows per class")
    ap.add_argument("--apply", action="store_true", help="Commit changes (default: dry-run)")
    ap.add_argument("--no-log", action="store_true", help="Do not write a durable report file")
    args = ap.parse_args(argv)

    if not os.path.exists(args.db):
        print(f"ERROR: DB not found: {args.db}")
        return 2

    conn = sqlite3.connect(args.db)
    cur = conn.cursor()

    mirror_n = _count(cur, MIRROR_WHERE, [])
    zero_n = _count(cur, ZERO_WHERE, [args.eps])
    total_rows = _count(cur, "1=1", [])

    lines = []

    def log(s=""):
        print(s)
        lines.append(s)

    log(f"P1 OOS-nil migration  ({'APPLY' if args.apply else 'DRY-RUN'})")
    log(f"DB: {args.db}")
    log(f"total strategies: {total_rows}")
    log(f"[mirror] oos_sharpe == sharpe (nonzero): {mirror_n}")
    log(f"[zero]   |oos_sharpe| <= {args.eps}:      {zero_n}")

    if args.sample > 0 and mirror_n:
        log(f"\n-- mirror sample (up to {args.sample}) --")
        for r in _fetch(cur, MIRROR_WHERE, [], limit=args.sample):
            log(f"  name={r[0]!r} rank={r[1]} sharpe={r[2]} oos={r[3]} trades={r[4]}")
    if args.sample > 0 and zero_n:
        log(f"\n-- zero sample (up to {args.sample}) --")
        for r in _fetch(cur, ZERO_WHERE, [args.eps], limit=args.sample):
            log(f"  name={r[0]!r} rank={r[1]} sharpe={r[2]} oos={r[3]} trades={r[4]}")

    if not args.apply:
        log("\nDRY-RUN only. No rows changed. Re-run with --apply to commit.")
    else:
        try:
            cur.execute("BEGIN")
            cur.execute(f"UPDATE strategies SET oos_sharpe=NULL WHERE {MIRROR_WHERE}")
            m_upd = cur.rowcount
            cur.execute(f"UPDATE strategies SET oos_sharpe=NULL WHERE {ZERO_WHERE}", [args.eps])
            z_upd = cur.rowcount
            conn.commit()
        except Exception as e:  # pragma: no cover
            conn.rollback()
            log(f"\nERROR during apply, rolled back: {e}")
            return 1
        log(f"\nAPPLIED: mirror->NULL={m_upd}, zero->NULL={z_upd}")
        # post-conditions
        null_n = _count(cur, "oos_sharpe IS NULL", [])
        pos_n = _count(cur, "oos_sharpe > 0", [])
        remaining_mirror = _count(cur, MIRROR_WHERE, [])
        log(f"post: oos_sharpe NULL={null_n}, >0={pos_n}, mirror remaining={remaining_mirror}")
        assert remaining_mirror == 0, "mirror rows remain after migration"

    conn.close()

    if not args.no_log:
        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        report_dir = os.path.join(os.path.dirname(args.db) or ".", "..", "reports")
        report_dir = os.path.normpath(os.path.join("data", "reports"))
        os.makedirs(report_dir, exist_ok=True)
        mode = "apply" if args.apply else "dryrun"
        path = os.path.join(report_dir, f"p1_oos_nil_migration_{mode}_{stamp}.log")
        with open(path, "w", encoding="utf-8") as f:
            f.write("\n".join(lines) + "\n")
        print(f"\nwrote report: {path}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
