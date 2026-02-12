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
