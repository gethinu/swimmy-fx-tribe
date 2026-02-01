# Data SEXP Sync Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Ensure `apply-backtest-result` updates `data_sexp` in fallback paths so KB loads accurate metrics.

**Architecture:** Update the fallback path to deserialize `data_sexp`, apply metrics, and call `upsert-strategy`; keep the column-only update as a safe fallback when parsing fails. Add a regression test that asserts `data_sexp` reflects updated Sharpe when no in-memory strategy exists.

**Tech Stack:** Common Lisp (SBCL), SQLite, Swimmy Lisp test harness.

---

### Task 1: Add a regression test for data_sexp sync

**Files:**
- Create: `src/lisp/tests/backtest-db-tests.lisp`
- Modify: `swimmy.asd`

**Step 1: Write the failing test**

Create `src/lisp/tests/backtest-db-tests.lisp`:

```lisp
;;; backtest-db-tests.lisp - DB regression tests for backtest updates

(in-package :swimmy.tests)

(deftest test-apply-backtest-result-updates-data-sexp
  "Ensure fallback path updates data_sexp when strategy is not in memory."
  (let* ((name "TEST-APPLY-BT-UPDATE")
         (initial-sharpe 0.12)
         (updated-sharpe 1.23)
         (metrics (list :sharpe updated-sharpe
                        :profit-factor 1.5
                        :win-rate 0.6
                        :trades 10
                        :max-dd 0.2
                        :oos-sharpe 1.1
                        :cpcv-median 0.9
                        :cpcv-pass-rate 0.8)))
    (init-db)
    (let ((strat (make-strategy
                  :name name
                  :indicators '((sma 20))
                  :entry '(> close (sma 20))
                  :exit '(< close (sma 20))
                  :sharpe initial-sharpe
                  :symbol "USDJPY"
                  :timeframe 1
                  :direction :BOTH)))
      (upsert-strategy strat))
    (unwind-protect
        (let ((*strategy-knowledge-base* nil)
              (swimmy.globals:*evolved-strategies* nil))
          (apply-backtest-result name metrics)
          (let* ((sexp-str (execute-single "SELECT data_sexp FROM strategies WHERE name = ?" name))
                 (*package* (find-package :swimmy.school))
                 (obj (read-from-string sexp-str)))
            (assert-true (strategy-p obj) "Stored object should be a strategy")
            (assert-true (< (abs (- (strategy-sharpe obj) updated-sharpe)) 0.0001)
                         "data_sexp should reflect updated sharpe")))
      (execute-non-query "DELETE FROM strategies WHERE name = ?" name))))
```

Update `swimmy.asd` by adding the test file to the TESTS section:

```lisp
(:file "src/lisp/tests/backtest-db-tests")
```

**Step 2: Run test to verify it fails**

Run: `sbcl --script test_runner.lisp`

Expected: FAIL in `test-apply-backtest-result-updates-data-sexp` because `data_sexp` still has the old Sharpe.

**Step 3: Commit**

```bash
git add src/lisp/tests/backtest-db-tests.lisp swimmy.asd
git commit -m "test: add regression for data_sexp backtest sync"
```

---

### Task 2: Update fallback path to sync data_sexp

**Files:**
- Modify: `src/lisp/school/school-rank-system.lisp`

**Step 1: Write minimal implementation**

Replace the fallback block in `apply-backtest-result` with:

```lisp
        ;; Fallback: update DB even if in-memory strategy is missing
        (progn
          (let ((updated nil)
                (sexp-str (ignore-errors (execute-single "SELECT data_sexp FROM strategies WHERE name = ?" name))))
            (when (and sexp-str (stringp sexp-str))
              (handler-case
                  (let* ((*package* (find-package :swimmy.school))
                         (obj (read-from-string sexp-str)))
                    (when (strategy-p obj)
                      (setf (strategy-sharpe obj) (float (getf metrics :sharpe 0.0))
                            (strategy-profit-factor obj) (float (getf metrics :profit-factor 0.0))
                            (strategy-win-rate obj) (float (getf metrics :win-rate 0.0))
                            (strategy-trades obj) (getf metrics :trades 0)
                            (strategy-max-dd obj) (float (getf metrics :max-dd 0.0))
                            (strategy-oos-sharpe obj) (float (getf metrics :oos-sharpe 0.0))
                            (strategy-cpcv-median-sharpe obj) (float (getf metrics :cpcv-median 0.0))
                            (strategy-cpcv-pass-rate obj) (float (getf metrics :cpcv-pass-rate 0.0))
                            (strategy-revalidation-pending obj) nil)
                      (upsert-strategy obj)
                      (setf updated t)))
                (error (e)
                  (format t "[DB] ⚠️ Failed to parse data_sexp for ~a: ~a~%" name e))))
            (unless updated
              (execute-non-query
               "UPDATE strategies SET sharpe=?, profit_factor=?, win_rate=?, trades=?, max_dd=?, last_bt_time=? WHERE name=?"
               (float (getf metrics :sharpe 0.0))
               (float (getf metrics :profit-factor 0.0))
               (float (getf metrics :win-rate 0.0))
               (getf metrics :trades 0)
               (float (getf metrics :max-dd 0.0))
               (get-universal-time)
               name))
            nil))
```

**Step 2: Run tests to verify they pass**

Run: `sbcl --script test_runner.lisp`

Expected: PASS (36 existing tests + new regression test).

**Step 3: Commit**

```bash
git add src/lisp/school/school-rank-system.lisp
git commit -m "fix: sync data_sexp on backtest fallback"
```

---

### Task 3: Sanity check (optional)

**Files:**
- None

**Step 1: Quick DB sanity (optional)**

Run an ad-hoc script or REPL check to confirm the updated Sharpe appears in `data_sexp` for a known strategy.

**Step 2: Commit (if script added)**

Only if any scripts are saved to the repo.
