# Backtest Queue Periodic Flush Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a periodic flush hook so queued backtest requests are drained once the requester is available.

**Architecture:** Add a lightweight time-gated flush function in `school-backtest-utils.lisp`, then call it from the main school loop. Keep existing pending/rate protections intact.

**Tech Stack:** Common Lisp (SBCL), Swimmy school modules, existing unit test harness in `src/lisp/tests.lisp`.

### Task 1: Fix/Verify the periodic flush test

**Files:**
- Modify: `src/lisp/tests.lisp`
- Test: `src/lisp/tests.lisp`

**Step 1: Write/adjust the failing test**

```lisp
(deftest test-backtest-queue-periodic-flush
  "maybe-flush-backtest-send-queue should trigger flush when interval elapsed"
  (let* ((orig-flush (when (fboundp 'swimmy.school::flush-backtest-send-queue)
                       (symbol-function 'swimmy.school::flush-backtest-send-queue)))
         (orig-queue swimmy.school::*backtest-send-queue*)
         (orig-last (when (boundp 'swimmy.school::*backtest-queue-last-flush*)
                      swimmy.school::*backtest-queue-last-flush*))
         (orig-interval (when (boundp 'swimmy.school::*backtest-queue-flush-interval-sec*)
                          swimmy.school::*backtest-queue-flush-interval-sec*))
         (flushed nil)
         (now (get-universal-time)))
    (unwind-protect
        (progn
          (setf swimmy.school::*backtest-send-queue* (list "(m1)"))
          (setf swimmy.school::*backtest-queue-last-flush* (- now 10))
          (setf swimmy.school::*backtest-queue-flush-interval-sec* 1)
          (setf (symbol-function 'swimmy.school::flush-backtest-send-queue)
                (lambda ()
                  (setf flushed t)
                  1))
          (swimmy.school::maybe-flush-backtest-send-queue)
          (assert-true flushed "expected flush to run")
          (assert-true (>= swimmy.school::*backtest-queue-last-flush* now)
                       "last flush timestamp should update"))
      (setf swimmy.school::*backtest-send-queue* orig-queue)
      (when (boundp 'swimmy.school::*backtest-queue-last-flush*)
        (setf swimmy.school::*backtest-queue-last-flush* orig-last))
      (when (boundp 'swimmy.school::*backtest-queue-flush-interval-sec*)
        (setf swimmy.school::*backtest-queue-flush-interval-sec* orig-interval))
      (when orig-flush
        (setf (symbol-function 'swimmy.school::flush-backtest-send-queue) orig-flush)))))
```

**Step 2: Run test to verify it fails**

Run:
```
./scripts/ci-test.sh
```
Expected: `TEST-BACKTEST-QUEUE-PERIODIC-FLUSH` fails because `maybe-flush-backtest-send-queue` is undefined.

**Step 3: Commit**

```bash
git add src/lisp/tests.lisp

git commit -m "test: add periodic backtest queue flush test"
```

### Task 2: Implement periodic flush utility

**Files:**
- Modify: `src/lisp/school/school-backtest-utils.lisp`

**Step 1: Write minimal implementation**

```lisp
(defparameter *backtest-queue-last-flush* 0
  "Last time the backtest send queue was flushed.")

(defparameter *backtest-queue-flush-interval-sec* 1
  "Minimum seconds between periodic flush checks for backtest send queue.")

(defun maybe-flush-backtest-send-queue (&optional (now (get-universal-time)))
  "Flush queued backtest messages when interval has elapsed."
  (when (and *backtest-send-queue*
             (> (- now *backtest-queue-last-flush*)
                *backtest-queue-flush-interval-sec*))
    (setf *backtest-queue-last-flush* now)
    (flush-backtest-queue)))
```

**Step 2: Run test to verify it passes**

Run:
```
./scripts/ci-test.sh
```
Expected: `TEST-BACKTEST-QUEUE-PERIODIC-FLUSH` passes.

**Step 3: Commit**

```bash
git add src/lisp/school/school-backtest-utils.lisp

git commit -m "feat: add periodic backtest queue flush helper"
```

### Task 3: Wire periodic flush into school loop

**Files:**
- Modify: `src/lisp/school/school-connector.lisp`

**Step 1: Call periodic flush in loop**

```lisp
(when (fboundp 'swimmy.school::maybe-flush-backtest-send-queue)
  (swimmy.school::maybe-flush-backtest-send-queue))
```

Insert this near the end of `start-evolution-service` before the cycle completes.

**Step 2: Run tests**

Run:
```
./scripts/ci-test.sh
```
Expected: All tests pass.

**Step 3: Commit**

```bash
git add src/lisp/school/school-connector.lisp

git commit -m "feat: flush queued backtests periodically in school loop"
```

### Task 4: Ensure test list includes new test (if needed)

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: Add test to run-all-tests list**

```lisp
test-backtest-queue-periodic-flush
```

**Step 2: Run tests**

Run:
```
./scripts/ci-test.sh
```
Expected: All tests pass.

**Step 3: Commit**

```bash
git add src/lisp/tests.lisp

git commit -m "test: include periodic flush test in run-all-tests"
```
