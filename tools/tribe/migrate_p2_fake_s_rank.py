#!/usr/bin/env python3
"""P2 (Thread A) one-shot migration: sanitize dishonest ``:S`` ranks.

The design memo (§2 P2) calls out a single fake S (``TestStrat``, all-zero
metrics). The live DB actually carries **10** ``rank=':S'`` rows, none of which
earns the title:

  * 6 all-zero rows (never backtested): ``sharpe=pf=wr=trades=0``.
  * 4 ``RECRUIT-RND-*`` rows with a lone ``sharpe`` (~0.46-0.54) but incoherent
    ``pf=0 / wr=0 / maxdd=0`` and ``trades`` 37-61.

Every one has **zero** trade evidence (``backtest_trade_logs``/``trade_logs`` = 0)
and a ``BLOCKED_OOS`` deployment gate, so none is live-eligible — this is a
data-honesty fix, not a live-safety fix. But an unearned ``:S`` title violates
the P2 invariant that *rank is a pure function of ``*rank-criteria*``*.

This migration recomputes the honest rank for every ``:S`` row by replicating
the canonical Lisp cascade (``run-rank-evaluation`` ->
``enforce-rank-trade-evidence-floors`` -> ``enforce-s-rank-criteria-conformance``
-> ``enforce-a-b-rank-criteria-conformance``), then writes the recomputed rank.
The recompute mirrors ``check-rank-criteria`` (school-rank-system.lisp:546) and
the rank floors unified to 200 in P2a:

  * S: evidence>=200, sharpe>=0.75, pf>=1.70, wr>=0.50, maxdd<0.10,
       cpcv_pass_rate>=0.70, cpcv_median_maxdd<0.12
  * A: evidence>=200, sharpe>=0.45, pf>=1.30, wr>=0.43, maxdd<0.16,
       oos_sharpe is not None and >=0.35
  * B: sharpe>=0.15, pf>=1.05, wr>=0.35, maxdd<0.25  (B floor = 0 evidence)
  * else -> :GRAVEYARD

"Trade evidence" is the ``rank-trade-evidence`` the floor gate uses:
``COUNT(*)`` from ``backtest_trade_logs`` for the strategy (see
``fetch-backtest-trade-count-map``, school-db.lisp:958). SHADOW-only evidence is
excluded from floor satisfaction, matching ``strategy-trade-evidence-count``.

Never loosens a gate (prime directive): a genuine :S that still meets the strict
criteria is preserved; only unearned titles are demoted. Default is a DRY RUN.
Pass ``--apply`` to commit (single transaction). A durable before/after log is
written under ``data/reports/`` unless ``--no-log`` is given.

Usage:
    python -m tools.tribe.migrate_p2_fake_s_rank              # dry-run
    python -m tools.tribe.migrate_p2_fake_s_rank --apply      # commit
    python -m tools.tribe.migrate_p2_fake_s_rank --db path/to.db
"""

from __future__ import annotations

import argparse
import os
import sqlite3
from datetime import datetime, timezone

DEFAULT_DB = os.path.join("data", "memory", "swimmy.db")

# Rank thresholds — mirror *rank-criteria* (school-rank-system.lisp:23-28) and
# the P2a-unified trade-evidence floors (A=S=200, B=0).
S_MIN_EVIDENCE = 200
A_MIN_EVIDENCE = 200
S = dict(sharpe=0.75, pf=1.70, wr=0.50, maxdd=0.10, cpcv_pass=0.70, cpcv_maxdd=0.12)
A = dict(sharpe=0.45, pf=1.30, wr=0.43, maxdd=0.16, oos=0.35)
B = dict(sharpe=0.15, pf=1.05, wr=0.35, maxdd=0.25)


def _f(v, default=0.0):
    return default if v is None else float(v)


def honest_rank(row, evidence):
    """Replicate the canonical rank cascade. Returns the highest earned rank."""
    sharpe = _f(row["sharpe"])
    pf = _f(row["profit_factor"])
    wr = _f(row["win_rate"])
    maxdd = _f(row["max_dd"], 1.0)
    oos = row["oos_sharpe"]  # None == unvalidated (post P1); do NOT coerce to 0
    cpcv_pass = _f(row["cpcv_pass_rate"])
    cpcv_maxdd = _f(row["cpcv_median_maxdd"], 1.0)

    if (evidence >= S_MIN_EVIDENCE and sharpe >= S["sharpe"] and pf >= S["pf"]
            and wr >= S["wr"] and maxdd < S["maxdd"]
            and cpcv_pass >= S["cpcv_pass"] and cpcv_maxdd < S["cpcv_maxdd"]):
        return ":S"
    if (evidence >= A_MIN_EVIDENCE and sharpe >= A["sharpe"] and pf >= A["pf"]
            and wr >= A["wr"] and maxdd < A["maxdd"]
            and (oos is not None and float(oos) >= A["oos"])):
        return ":A"
    if (sharpe >= B["sharpe"] and pf >= B["pf"] and wr >= B["wr"]
            and maxdd < B["maxdd"]):
        return ":B"
    return ":GRAVEYARD"


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description="P2 fake :S rank sanitization")
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

    log(f"P2 fake-:S rank sanitization  ({'APPLY' if args.apply else 'DRY-RUN'})")
    log(f"DB: {args.db}")

    rows = cur.execute(
        "SELECT name, sharpe, profit_factor, win_rate, trades, max_dd, "
        "oos_sharpe, cpcv_pass_rate, cpcv_median_maxdd, symbol, direction, timeframe "
        "FROM strategies WHERE rank = ':S' ORDER BY name"
    ).fetchall()

    log(f"current :S rows: {len(rows)}")

    plan = []  # (name, new_rank, evidence)
    for r in rows:
        ev = cur.execute(
            "SELECT COUNT(*) FROM backtest_trade_logs WHERE strategy_name = ?",
            (r["name"],),
        ).fetchone()[0]
        new = honest_rank(r, ev)
        plan.append((r["name"], new, ev))
        changed = "KEEP" if new == ":S" else f"-> {new}"
        log(f"  {r['name']:34} [{r['symbol']}/{r['direction']}/{r['timeframe']}] "
            f"sharpe={_f(r['sharpe']):.3f} pf={_f(r['profit_factor']):.2f} "
            f"wr={_f(r['win_rate']):.2f} maxdd={_f(r['max_dd']):.2f} "
            f"oos={r['oos_sharpe']} cpcv_pass={_f(r['cpcv_pass_rate']):.2f} "
            f"evidence={ev}   {changed}")

    to_change = [(n, nr) for (n, nr, _ev) in plan if nr != ":S"]
    kept = len(plan) - len(to_change)
    log("")
    log(f"summary: keep :S = {kept}, demote = {len(to_change)}")
    from collections import Counter
    breakdown = Counter(nr for (_n, nr) in to_change)
    for tgt, c in sorted(breakdown.items()):
        log(f"  -> {tgt}: {c}")

    if not args.apply:
        log("\nDRY-RUN only. No rows changed. Re-run with --apply to commit.")
    else:
        try:
            cur.execute("BEGIN")
            for name, new_rank in to_change:
                cur.execute(
                    "UPDATE strategies SET rank = ? WHERE name = ? AND rank = ':S'",
                    (new_rank, name),
                )
            conn.commit()
        except Exception as e:  # pragma: no cover
            conn.rollback()
            log(f"\nERROR during apply, rolled back: {e}")
            return 1
        log(f"\nAPPLIED: demoted {len(to_change)} rows out of {len(plan)} :S rows.")
        # post-conditions
        remaining_s = cur.execute("SELECT COUNT(*) FROM strategies WHERE rank = ':S'").fetchone()[0]
        log(f"post: remaining :S = {remaining_s}")
        # every surviving :S must genuinely meet S criteria
        for r in cur.execute(
            "SELECT name, sharpe, profit_factor, win_rate, max_dd, cpcv_pass_rate, "
            "cpcv_median_maxdd FROM strategies WHERE rank = ':S'"
        ).fetchall():
            ev = cur.execute(
                "SELECT COUNT(*) FROM backtest_trade_logs WHERE strategy_name = ?",
                (r["name"],),
            ).fetchone()[0]
            assert honest_rank(r, ev) == ":S", f"surviving :S does not meet criteria: {r['name']}"

    conn.close()

    if not args.no_log:
        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        report_dir = os.path.normpath(os.path.join("data", "reports"))
        os.makedirs(report_dir, exist_ok=True)
        mode = "apply" if args.apply else "dryrun"
        path = os.path.join(report_dir, f"p2_fake_s_rank_{mode}_{stamp}.log")
        with open(path, "w", encoding="utf-8") as f:
            f.write("\n".join(lines) + "\n")
        print(f"\nwrote report: {path}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
