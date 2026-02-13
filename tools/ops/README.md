# Ops Scripts

This directory contains operator-facing scripts for backtest, reporting, and maintenance tasks.

## Quick list

| Script | Purpose |
| --- | --- |
| `all_strategy_backtest.lisp` | Enqueue backtests for a broad strategy set. |
| `monitor_backtest_progress.lisp` | Print backtest progress snapshots. |
| `finalize_rank_report.lisp` | Refresh metrics and generate evolution report (rank eval is opt-in). |
| `finalize_rank_report.sh` | Wrapper for `finalize_rank_report.lisp` with SBCL env setup. |
| `reconcile_archive_db.py` | Reconcile archive folders and DB rank/state mismatches. |
| `reseed_active_b.py` | Reseed `:B` from archive (`:GRAVEYARD/:RETIRED`) by top-N per category. |
| `cpcv_smoke.py` | Send one-off CPCV smoke messages (`runtime` / `criteria`). |

## CPCV smoke checks

The `cpcv_smoke.py` script helps distinguish:
- `runtime` failures (execution error path)
- `criteria` failures (valid execution, failed gate path)

Examples:

```bash
# Runtime error path (expected notifier label: ERROR)
.venv/bin/python3 tools/ops/cpcv_smoke.py --mode runtime

# Criteria fail path (expected notifier label: FAILED)
.venv/bin/python3 tools/ops/cpcv_smoke.py --mode criteria --send-count 3

# Print request only (no send)
.venv/bin/python3 tools/ops/cpcv_smoke.py --mode runtime --dry-run
```

Useful flags:

```bash
.venv/bin/python3 tools/ops/cpcv_smoke.py --mode criteria \
  --symbol USDJPY \
  --endpoint tcp://127.0.0.1:5559 \
  --send-count 5 \
  --interval-sec 0.4 \
  --settle-sec 1.0
```

## Confirm results

```bash
watch -n 2 "sed -n '1,2p' data/reports/cpcv_status.txt"
rg -n "CPCV Validation: ERROR|CPCV Validation: FAILED" logs/notifier.log | tail -n 20
```

## Finalize report (safe default)

`finalize_rank_report.sh` runs in **snapshot mode by default**:
- Refresh metrics from DB
- Generate report
- Skip rank evaluation/culling

Run rank evaluation only when explicitly requested:

```bash
tools/ops/finalize_rank_report.sh --with-rank-eval
```

## Reseed active B from archive

Use when `B/A/S` are starved after archive-heavy reconciliation.
The script applies Stage1 B thresholds, picks top-N per category
(`timeframe x direction x symbol`), moves files into `data/library/B/`,
and updates DB rank to `:B`.
File metadata rewrite uses atomic replace (`tmp -> os.replace`) so root-owned
archive artifacts can be normalized when directory write permission exists.
If the same strategy exists in both `B/` and archive, reseed keeps `B/` as the
source of truth and still promotes DB rank to `:B`.

```bash
# preview only
python3 tools/ops/reseed_active_b.py --dry-run --per-category 20

# apply (creates DB backup under data/memory/backup by default)
python3 tools/ops/reseed_active_b.py --per-category 20
```
