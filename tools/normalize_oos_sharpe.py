#!/usr/bin/env python3
import argparse
import sqlite3
from typing import List

DEFAULT_DB = "/home/swimmy/swimmy/data/memory/swimmy.db"


def _normalize_rank(r: str) -> str:
    if not r:
        return r
    r = r.strip()
    if r.upper() == "NIL":
        return "NIL"
    if r.startswith(":"):
        return r.upper()
    return f":{r.upper()}"


def _build_where(ranks: List[str], eps: float) -> (str, List[str]):
    where = "oos_sharpe IS NOT NULL AND ABS(oos_sharpe) <= ?"
    params: List[str] = [eps]
    if ranks:
        where += " AND rank IN ({})".format(",".join(["?"] * len(ranks)))
        params.extend(ranks)
    return where, params


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Normalize oos_sharpe defaults (0.0) to NULL in strategies table."
    )
    ap.add_argument("--db", default=DEFAULT_DB, help="Path to swimmy.db")
    ap.add_argument(
        "--ranks",
        default="",
        help="Comma-separated rank filter (e.g. A,B,S,LEGEND,NIL). Empty=all ranks",
    )
    ap.add_argument(
        "--eps",
        type=float,
        default=1e-6,
        help="Absolute threshold treated as zero (default: 1e-6)",
    )
    ap.add_argument(
        "--sample",
        type=int,
        default=10,
        help="Sample size to preview affected rows (default: 10)",
    )
    ap.add_argument(
        "--apply",
        action="store_true",
        help="Apply changes (default: dry-run)",
    )

    args = ap.parse_args()

    ranks = [r for r in (args.ranks.split(",") if args.ranks else []) if r.strip()]
    ranks = [_normalize_rank(r) for r in ranks]

    conn = sqlite3.connect(args.db)
    cur = conn.cursor()

    where, params = _build_where(ranks, args.eps)

    cur.execute(f"SELECT COUNT(*) FROM strategies WHERE {where}", params)
    count = cur.fetchone()[0]

    print("DB:", args.db)
    print("Filter:", f"ranks={ranks if ranks else 'ALL'}", f"eps={args.eps}")
    print("Will normalize rows:", count)

    if count > 0 and args.sample > 0:
        cur.execute(
            f"SELECT name, rank, oos_sharpe FROM strategies WHERE {where} LIMIT ?",
            params + [args.sample],
        )
        rows = cur.fetchall()
        print("Sample:")
        for name, rank, oos in rows:
            print(f"  {name} | {rank} | oos_sharpe={oos}")

    if not args.apply:
        print("Dry-run only. Use --apply to update.")
        return 0

    cur.execute(f"UPDATE strategies SET oos_sharpe=NULL WHERE {where}", params)
    updated = cur.rowcount
    conn.commit()

    print("Updated rows:", updated)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
