# Phase2 Removal Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove Backtest V2 Phase2 validation and its report line while keeping Phase1 screening and the OOS/CPCV pipeline intact.

**Architecture:** Phase2 code paths (request, handler, constants, and report state) are deleted. Evolution report no longer renders Phase2 EndTime. Tests are updated to assert Phase2 is absent and Phase1 payload behavior remains.

**Tech Stack:** Common Lisp (SBCL), ASDF/Quicklisp, SQLite-backed tests.

### Task 1: Remove Phase2 EndTime From Evolution Report

**Files:**
- Modify: `src/lisp/tests/school-split-tests.lisp`
- Modify: `src/lisp/school/school-narrative.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Rename and invert the assertion to ensure Phase2 is NOT present.

```lisp
(deftest test-evolution-report-omits-phase2-end-time
  "Evolution report should omit Phase2 end_time line."
  (let* ((tmp-db (format nil "/tmp/swimmy-report-phase2-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let ((orig-refresh (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)
                            (lambda (&rest _args) (declare (ignore _args)) nil))
                      (let ((report (swimmy.school::generate-evolution-report)))
                        (assert-false (search "Phase2 EndTime:" report)
                                      "Report should NOT contain Phase2 EndTime line")))
                 (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db) orig-refresh))))
        (swimmy.core:close-db-connection)
        (when (probe-file tmp-db) (delete-file tmp-db))))))
```

Update the test registry in `src/lisp/tests.lisp` to use the new test name.

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-evolution-report-omits-phase2-end-time)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL because the report still includes the Phase2 line.

**Step 3: Write minimal implementation**

Remove Phase2 text rendering from `generate-evolution-report`.

```lisp
;; Remove phase2-end-text binding and the format placeholder:
;; - delete the let binding for phase2-end-text
;; - delete the "🕒 Phase2 EndTime: ~a" line
;; - remove the corresponding argument in (format nil ...)
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-evolution-report-omits-phase2-end-time)' \
  --eval '(sb-ext:quit)'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/tests/school-split-tests.lisp src/lisp/tests.lisp src/lisp/school/school-narrative.lisp
git commit -m "remove: drop Phase2 EndTime from evolution report"
```

### Task 2: Remove Phase2 Validation Path And State

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/school/school-backtest-v2.lisp`
- Modify: `src/lisp/school/school-constants.lisp`
- Modify: `src/lisp/core/globals.lisp`
- Modify: `src/lisp/packages.lisp`

**Step 1: Write the failing test**

Replace the Phase2 promotion test with an ignore test.

```lisp
(deftest test-backtest-v2-ignores-phase2-results
  "Phase2 results should be ignored after Phase2 removal"
  (let* ((s (swimmy.school:make-strategy :name "Phase2" :symbol "USDJPY"))
         (swimmy.school::*strategy-knowledge-base* (list s))
         (called nil))
    (let ((orig (symbol-function 'swimmy.school:ensure-rank)))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:ensure-rank)
                  (lambda (strat rank &optional reason)
                    (declare (ignore reason))
                    (setf called (list strat rank))
                    rank))
            (swimmy.school::handle-v2-result "Phase2_P2" (list :sharpe 1.0)))
        (setf (symbol-function 'swimmy.school:ensure-rank) orig)))
    (assert-true (null called) "Phase2 results should not trigger rank changes")))
```

Update the test registry in `src/lisp/tests.lisp` to use the new test name.

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-backtest-v2-ignores-phase2-results)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL because Phase2 currently promotes to A.

**Step 3: Write minimal implementation**

Remove Phase2 logic from V2 backtest handler:

```lisp
;; src/lisp/school/school-backtest-v2.lisp
;; - remove phase2 end_time tracking in request-backtest-v2
;; - delete run-phase-2-validation
;; - remove the (search "_P2" ...) branch from handle-v2-result
;; - update header/comments to reflect Phase1-only screening
```

Remove Phase2 constants:

```lisp
;; src/lisp/school/school-constants.lisp
;; - remove *backtest-range-2*
;; - remove *phase2-min-sharpe*
;; - update comments to Phase1-only
```

Remove Phase2 global state and export:

```lisp
;; src/lisp/core/globals.lisp
;; - delete *phase2-last-end-unix*

;; src/lisp/packages.lisp
;; - remove #:*phase2-last-end-unix* from exports
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-backtest-v2-ignores-phase2-results)' \
  --eval '(sb-ext:quit)'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/tests.lisp src/lisp/school/school-backtest-v2.lisp \
  src/lisp/school/school-constants.lisp src/lisp/core/globals.lisp src/lisp/packages.lisp
git commit -m "remove: drop Phase2 validation and state"
```

### Task 3: Update V2 Payload Test To Phase1

**Files:**
- Modify: `src/lisp/tests/backtest-payload-tests.lisp`

**Step 1: Write the failing test**

Change expectations to Phase1 but leave the call as Phase2 to force a failure.

```lisp
(assert-equal "phase1" (field 'phase payload) "Expected phase=phase1")
(assert-equal "P1" (field 'range_id payload) "Expected range_id=P1")
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-request-backtest-v2-emits-phase-range-id)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL because the test still sends Phase2.

**Step 3: Write minimal implementation**

Update the test call to send Phase1:

```lisp
(swimmy.school::request-backtest-v2 strat
                                    :start-date "2021.01.01"
                                    :end-date "2021.12.31"
                                    :phase "phase1"
                                    :range-id "P1")
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-request-backtest-v2-emits-phase-range-id)' \
  --eval '(sb-ext:quit)'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/tests/backtest-payload-tests.lisp
git commit -m "test: align v2 payload phase/range with phase1"
```

### Task 4: Regression Smoke

**Files:**
- None

**Step 1: Run targeted suite**

Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests:run-all-tests)' \
  --eval '(sb-ext:quit)'
```
Expected: PASS (all tests green).
