#!/usr/bin/env python3
"""P2 (Thread A) migration: retire the legacy LEGEND class to ``:LEGEND-ARCHIVE``.

Owner decision (Thread A P2 close):

  * Decouple LEGEND from the execution/capital path *completely* — already done
    in P2b (``*execution-ineligible-ranks*`` includes ``:legend``).
  * Do **not** hard-delete: quarantine = preserve the row/count for audit and
    learning (no-hard-delete policy + durable persistence).
  * Concretely: demote LEGEND to a *learning-only archive, zero capital,
    execution-forbidden* state, and sever the path by which
    ``restore_legend_61`` immortalizes loss-making strategies.

The codebase already ships a purpose-built rank for exactly that state,
``:LEGEND-ARCHIVE``:

  * ``*execution-ineligible-ranks*`` (school-execution.lisp) includes it, so it
    can never take capital or open a slot.
  * It is grouped with the archived ranks (GRAVEYARD/RETIRED/ARCHIVE) in the
    library/persistence layer, yet keeps its *legend* provenance — unlike
    ``:GRAVEYARD`` it does not conflate a retired legend with a failed recruit,
    so the "61 legends" audit trail (count) is preserved.
  * It is excluded from the active-rank cascade, breeding, and revalidation
    queue (school-rank-system.lisp), so an archived legend is inert.

This is a **class retirement**, not a per-criteria re-rank. The 61-strategy
legend seed (median Sharpe ~ -0.025, ~half negative) has no honest execution
role post KILL_CRITERIA §4 (FAIL -> fold). We are not promoting/demoting each
legend onto the active S/A/B ladder; we are moving the whole deprecated class
to the archive bucket while preserving every record.

Companion code changes (committed with P2e) make the demotion *stick*:
  * ``restore-legend-61`` skips any name whose authoritative DB rank is already
    ``:LEGEND-ARCHIVE`` (deliberate archival is not resurrected).
  * ``%ensure-rank-no-lock`` routes a LEGEND that earns graveyard to
    ``:LEGEND-ARCHIVE`` (honest, preserved) instead of freezing it as :LEGEND.

Never loosens a gate (prime directive). Default is a DRY RUN. Pass ``--apply``
to commit (single transaction). A durable before/after log is written under
``data/reports/`` unless ``--no-log`` is given.

Reverse operation (restore the class label; run against the same DB):

    UPDATE strategies SET rank=':LEGEND' WHERE rank=':LEGEND-ARCHIVE';

(There are no pre-existing ``:LEGEND-ARCHIVE`` rows before this migration — see
the pre-flight guard below — so the reverse is exact.)

Usage:
    python -m tools.tribe.migrate_p2_legend_archive              # dry-run
    python -m tools.tribe.migrate_p2_legend_archive --apply      # commit
    python -m tools.tribe.migrate_p2_legend_archive --db path/to.db
"""

from __future__ import annotations

import argparse
import os
import sqlite3
from collections import Counter
from datetime import datetime, timezone

DEFAULT_DB = os.path.join("data", "memory", "swimmy.db")

SRC_RANK = ":LEGEND"
DST_RANK = ":LEGEND-ARCHIVE"


def _f(v, default=0.0):
    return default if v is None else float(v)


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description="P2 LEGEND -> LEGEND-ARCHIVE class retirement")
    ap.add_argument("--db", default=DEFAULT_DB, help="Path to swimmy.db")
    ap.add_argument("--apply", action="store_true", help="Commit changes (default: dry-run)")
    ap.add_argument("--no-log", action="store_true", help="Do not write a durable report file")
    args = ap.parse_args(argv)

    if not os.path.exists(args.db):
        print(f"ERROR: DB not found: {args.db}")
        return 2

    conn = sqlite3.connect(args.db)
    conn.row_factory = sqlite3.Row
    cur = conn.cursor()

    lines = []

    def log(s=""):
        print(s)
        lines.append(s)

    log(f"P2 LEGEND -> LEGEND-ARCHIVE class retirement  ({'APPLY' if args.apply else 'DRY-RUN'})")
    log(f"DB: {args.db}")

    # Pre-flight: the reverse operation is only exact if there are no existing
    # archived-legend rows to confuse with the ones we create.
    pre_archive = cur.execute(
        "SELECT COUNT(*) FROM strategies WHERE rank = ?", (DST_RANK,)
    ).fetchone()[0]
    log(f"pre-existing {DST_RANK} rows: {pre_archive}")
    if pre_archive:
        log(f"NOTE: {pre_archive} rows are already {DST_RANK}; the documented reverse "
            f"SQL would over-revert them. Inspect before reversing.")

    rows = cur.execute(
        "SELECT name, sharpe, profit_factor, win_rate, trades, max_dd, "
        "symbol, direction, timeframe "
        "FROM strategies WHERE rank = ? ORDER BY name",
        (SRC_RANK,),
    ).fetchall()

    log(f"current {SRC_RANK} rows: {len(rows)}")
    if not rows:
        log(f"\nNothing to do — no {SRC_RANK} rows. (Idempotent: already retired.)")
        conn.close()
        return 0

    neg_sharpe = 0
    for r in rows:
        sh = _f(r["sharpe"])
        if sh < 0:
            neg_sharpe += 1
        log(f"  {r['name']:34} [{r['symbol']}/{r['direction']}/{r['timeframe']}] "
            f"sharpe={sh:.3f} pf={_f(r['profit_factor']):.2f} "
            f"wr={_f(r['win_rate']):.2f} maxdd={_f(r['max_dd']):.2f} "
            f"trades={r['trades']}   -> {DST_RANK}")

    log("")
    log(f"summary: retire {len(rows)} {SRC_RANK} rows -> {DST_RANK} "
        f"(negative-sharpe among them: {neg_sharpe})")

    if not args.apply:
        log(f"\nDRY-RUN only. No rows changed. Re-run with --apply to commit.")
    else:
        try:
            cur.execute("BEGIN")
            cur.execute(
                "UPDATE strategies SET rank = ? WHERE rank = ?",
                (DST_RANK, SRC_RANK),
            )
            changed = cur.rowcount
            conn.commit()
        except Exception as e:  # pragma: no cover
            conn.rollback()
            log(f"\nERROR during apply, rolled back: {e}")
            conn.close()
            return 1
        log(f"\nAPPLIED: retired {changed} rows {SRC_RANK} -> {DST_RANK}.")
        # post-conditions
        remaining = cur.execute(
            "SELECT COUNT(*) FROM strategies WHERE rank = ?", (SRC_RANK,)
        ).fetchone()[0]
        post_archive = cur.execute(
            "SELECT COUNT(*) FROM strategies WHERE rank = ?", (DST_RANK,)
        ).fetchone()[0]
        log(f"post: remaining {SRC_RANK} = {remaining}")
        log(f"post: total {DST_RANK} = {post_archive} (was {pre_archive}, +{post_archive - pre_archive})")
        assert remaining == 0, f"expected 0 {SRC_RANK} after apply, found {remaining}"
        assert post_archive - pre_archive == len(rows), "archive delta mismatch"
        # full distribution snapshot
        log("post: rank distribution:")
        for row in cur.execute(
            "SELECT rank, COUNT(*) c FROM strategies GROUP BY rank ORDER BY c DESC"
        ).fetchall():
            log(f"  {row['rank']:20} {row['c']}")

    conn.close()

    if not args.no_log:
        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        report_dir = os.path.normpath(os.path.join("data", "reports"))
        os.makedirs(report_dir, exist_ok=True)
        mode = "apply" if args.apply else "dryrun"
        path = os.path.join(report_dir, f"p2_legend_archive_{mode}_{stamp}.log")
        with open(path, "w", encoding="utf-8") as f:
            f.write("\n".join(lines) + "\n")
        print(f"\nwrote report: {path}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
