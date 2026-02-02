# OOS Auto Dispatch Fix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Ensure A/B-rank strategies trigger out-of-sample backtests instead of being treated as cached because `strategy-oos-sharpe` defaults to `0.0`.

**Architecture:** Represent unknown OOS Sharpe as `nil` (persisted as NULL). Request an OOS backtest when the value is unset; treat any numeric value (including zero) as a completed result. Run a one-time in-memory normalization to clear legacy `0.0` defaults so pending strategies get revalidated. Add a regression test that proves OOS dispatch occurs when OOS Sharpe is unset.

**Tech Stack:** Common Lisp (SBCL), SQLite persistence, existing minimal test harness in `tests/engine-tests.lisp`.

### Task 1: Add failing OOS dispatch test
**Files:**
- Modify: `tests/engine-tests.lisp`

**Steps:**
1) Write a test `oos-validation-dispatches-when-unset` that:
   - Creates a temporary `data/historical/USDJPY_M1.csv` fixture.
   - Rebinds `request-backtest` to a stub that records calls.
   - Sets a fresh `*oos-pending*` hash table.
   - Builds a strategy with `:oos-sharpe nil`.
   - Calls `run-oos-validation` and asserts it returns "OOS pending (async)" and the stub was invoked.
2) Update `run-all-tests` to execute the new test.
3) Run the test file to confirm the new test fails on current code.

### Task 2: Make unknown OOS sharpe nil by default
**Files:**
- Modify: `src/lisp/dsl.lisp`

**Steps:**
1) Change `defstruct strategy` default for `oos-sharpe` to `nil`.
2) Change `defstrategy` macro default for `oos-sharpe` to `nil`.

### Task 3: Persist unset OOS as NULL instead of 0.0
**Files:**
- Modify: `src/lisp/school/school-db.lisp`

**Steps:**
1) In the `INSERT OR REPLACE` statement, pass `strategy-oos-sharpe` directly (remove `or ... 0.0`) so `nil` becomes NULL.
2) When refreshing from DB, allow NULL to keep `strategy-oos-sharpe` as `nil`.

### Task 4: Trigger OOS only when value is known
**Files:**
- Modify: `src/lisp/school/school-validation.lisp`
- Modify: `src/lisp/school/school-rank-system.lisp` (for one-time normalization hook)

**Steps:**
1) Update `run-oos-validation` to treat only numeric, non-nil values as cached; otherwise call `maybe-request-oos-backtest`.
2) Add a one-time normalization (e.g., `normalize-oos-defaults`) that converts `0.0` defaults to `nil` for in-memory strategies; invoke it early in `run-rank-evaluation` so legacy strategies get revalidated once.

### Task 5: GREEN and regression
**Files:**
- `tests/engine-tests.lisp`

**Steps:**
1) Re-run the test suite (`sbcl --load tests/engine-tests.lisp --eval '(run-all-tests)' --quit`).
2) Ensure the new OOS test now passes and no regressions occur.

### Task 6: Optional data cleanup (manual)
**Steps (if needed):**
- Run a one-off SQL to set `oos_sharpe` to NULL where it equals 0 for active ranks, or rely on normalization to trigger revalidation in-process.
