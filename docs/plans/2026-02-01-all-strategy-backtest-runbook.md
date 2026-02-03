# All-Strategy Backtest Runbook Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Run a full backtest sweep for all strategies, refresh ranks, and regenerate the Evolution Factory Report.

**Architecture:** Add small ops scripts (SBCL) that load the Swimmy system, enqueue backtests for every strategy with CSV-based data, monitor completion using SQLite `last_bt_time`, then run rank evaluation and generate the report. Operations are coordinated with systemd services for brain/backtest.

**Tech Stack:** Common Lisp (SBCL), ASDF, SQLite, systemd, ZMQ.

---

### Task 1: Create the all-strategy backtest enqueue script

**Files:**
- Create: `tools/ops/all_strategy_backtest.lisp`

**Step 1: Write the failing test**

Create a placeholder script that immediately errors, then verify it fails:

```lisp
(error "TODO: implement all_strategy_backtest")
```

Run: `sbcl --script tools/ops/all_strategy_backtest.lisp`
Expected: FAIL with "TODO: implement all_strategy_backtest".

**Step 2: Run test to verify it fails**

Run: `sbcl --script tools/ops/all_strategy_backtest.lisp`
Expected: FAIL with the TODO error.

**Step 3: Write minimal implementation**

Implement script logic:
- `asdf:load-system :swimmy`
- `(in-package :swimmy.school)`
- Force `swimmy.core:*backtest-service-enabled*` to `t` and call `init-backtest-zmq`
- `init-knowledge-base`
- For each strategy in `*strategy-knowledge-base*`:
  - resolve symbol (default "USDJPY")
  - verify `data/historical/<SYM>_M1.csv` exists
  - if exists: `(request-backtest strat :candles nil :symbol sym :suffix "-FULL")`, sleep 0.01
  - else: record missing symbol for report
- Write start time to `data/reports/backtest_all_start.txt`
- Write missing symbols list to `data/reports/backtest_all_missing_symbols.txt`
- Print summary counts

**Step 4: Run test to verify it passes**

Run: `sbcl --script tools/ops/all_strategy_backtest.lisp`
Expected: PASS; prints queued/skipped counts and writes the two report files.

**Step 5: Commit**

```bash
git add tools/ops/all_strategy_backtest.lisp data/reports/backtest_all_start.txt data/reports/backtest_all_missing_symbols.txt
git commit -m "ops: add full backtest enqueue script"
```

---

### Task 2: Create the progress monitor script

**Files:**
- Create: `tools/ops/monitor_backtest_progress.lisp`

**Step 1: Write the failing test**

Create a placeholder script that errors, then verify it fails:

```lisp
(error "TODO: implement monitor_backtest_progress")
```

Run: `sbcl --script tools/ops/monitor_backtest_progress.lisp`
Expected: FAIL with "TODO: implement monitor_backtest_progress".

**Step 2: Run test to verify it fails**

Run: `sbcl --script tools/ops/monitor_backtest_progress.lisp`
Expected: FAIL with the TODO error.

**Step 3: Write minimal implementation**

Implement script logic:
- `asdf:load-system :swimmy`
- `(in-package :swimmy.school)`
- Read start time from `data/reports/backtest_all_start.txt`
- Query SQLite:
  - total strategies: `SELECT count(*) FROM strategies`
  - completed: `SELECT count(*) FROM strategies WHERE last_bt_time >= ?`
- Print counts and percent

**Step 4: Run test to verify it passes**

Run: `sbcl --script tools/ops/monitor_backtest_progress.lisp`
Expected: PASS; prints total/completed/percent.

**Step 5: Commit**

```bash
git add tools/ops/monitor_backtest_progress.lisp
git commit -m "ops: add backtest progress monitor"
```

---

### Task 3: Create the finalize (rank + report) script

**Files:**
- Create: `tools/ops/finalize_rank_report.lisp`

**Step 1: Write the failing test**

Create a placeholder script that errors, then verify it fails:

```lisp
(error "TODO: implement finalize_rank_report")
```

Run: `sbcl --script tools/ops/finalize_rank_report.lisp`
Expected: FAIL with "TODO: implement finalize_rank_report".

**Step 2: Run test to verify it fails**

Run: `sbcl --script tools/ops/finalize_rank_report.lisp`
Expected: FAIL with the TODO error.

**Step 3: Write minimal implementation**

Implement script logic:
- `asdf:load-system :swimmy`
- `(in-package :swimmy.school)`
- `init-knowledge-base`
- `refresh-strategy-metrics-from-db :force t`
- `run-rank-evaluation`
- `notify-evolution-report`

**Step 4: Run test to verify it passes**

Run: `sbcl --script tools/ops/finalize_rank_report.lisp`
Expected: PASS; prints rank cycle summary and writes report files.

**Step 5: Commit**

```bash
git add tools/ops/finalize_rank_report.lisp
git commit -m "ops: add rank + report finalizer"
```

---

### Task 4: Execute the full run (services → enqueue → monitor → finalize)

**Files:**
- Modify: none

**Step 1: Restart required services**

Run:
```bash
sudo systemctl restart swimmy-backtest swimmy-brain
```
Expected: both services active.

**Step 2: Enqueue all backtests**

Run:
```bash
SWIMMY_BACKTEST_SERVICE=1 sbcl --script tools/ops/all_strategy_backtest.lisp | tee /tmp/backtest_all_enqueue.log
```
Expected: queued/skipped summary printed; start/missing-symbols files written.

**Step 3: Monitor progress until completion**

Run (repeat as needed):
```bash
sbcl --script tools/ops/monitor_backtest_progress.lisp
```
Expected: completed count grows; eventually ~= total.

**Step 4: Finalize ranks + report**

Run:
```bash
sbcl --script tools/ops/finalize_rank_report.lisp
```
Expected: Evolution Factory Report updated in `data/reports/evolution_factory_report.txt`.

**Step 5: Commit (optional)**

If you want the runbook scripts in git:
```bash
git status -sb
git log -1 --stat
```
Decide whether to merge these ops scripts to `master`.

---

## Notes / Risks
- If `data/historical/<SYM>_M1.csv` is missing, those strategies will be skipped. Missing symbols are logged in `data/reports/backtest_all_missing_symbols.txt`.
- Backtest results require `swimmy-brain` to be running because it binds the results port and applies metrics.
- Expect long runtime (tens of thousands of strategies). Use the monitor script to track completion.
