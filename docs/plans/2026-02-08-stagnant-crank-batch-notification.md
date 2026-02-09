# Stagnant C-Rank Batch Notification Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Suppress individual “Stagnant C-Rank after 10 days” alerts and emit a separate hourly batch summary, mirroring the existing max-age batching behavior.

**Architecture:** Add a dedicated buffer + timer in `src/lisp/core/discord.lisp`, queue entries from `kill-strategy` when the reason matches “Stagnant C-Rank”, and flush via `check-timeout-flushes` alongside the max-age batch.

**Tech Stack:** Common Lisp (SBCL), Swimmy core/discord notifier, existing test runner `./scripts/ci-test.sh`.

---

### Task 1: Add failing test for Stagnant C-Rank batching

**Files:**
- Modify: `src/lisp/tests/backtest-db-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add this test (patterned after the max-age test) to `src/lisp/tests/backtest-db-tests.lisp`:

```lisp
(deftest test-stagnant-crank-batched-notification
  "Stagnant C-Rank soft-kill should batch notifications and flush hourly."
  (let* ((name "TEST-STAGNANT-C-RANK")
         (tmp-db (format nil "/tmp/swimmy-stagnant-crank-~a.db" (get-universal-time)))
         (sent-messages nil))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (let ((orig-notify (and (fboundp 'swimmy.school::notify-discord-alert)
                              (symbol-function 'swimmy.school::notify-discord-alert))))
        (unwind-protect
            (progn
              (setf swimmy.core::*stagnant-crank-retire-buffer* nil)
              (setf swimmy.core::*stagnant-crank-retire-first-seen* 0)
              (when orig-notify
                (setf (symbol-function 'swimmy.school::notify-discord-alert)
                      (lambda (msg &key color)
                        (declare (ignore color))
                        (push msg sent-messages)
                        t)))
              (swimmy.school::init-db)
              (let ((strat (make-strategy :name name :symbol "USDJPY")))
                (setf *strategy-knowledge-base* (list strat))
                (upsert-strategy strat)
                (kill-strategy name "Cull: Stagnant C-Rank (0.15) after 10 days")
                (assert-equal 0 (length sent-messages) "Should suppress individual alert")
                (assert-equal 1 (length swimmy.core::*stagnant-crank-retire-buffer*)
                              "Should buffer stagnant C-rank notification")
                (let ((start swimmy.core::*stagnant-crank-retire-first-seen*))
                  (swimmy.core::maybe-flush-stagnant-crank-retire (+ start 3601))
                  (assert-equal 1 (length sent-messages) "Should flush summary after 1 hour")
                  (assert-true (search "Stagnant C-Rank Summary" (car sent-messages))
                               "Summary title should be included")
                  (assert-true (search name (car sent-messages))
                               "Summary should include strategy name"))))
          (when orig-notify
            (setf (symbol-function 'swimmy.school::notify-discord-alert) orig-notify))
          (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
          (ignore-errors (close-db-connection))
          (ignore-errors (delete-file tmp-db)))))))
```

**Step 2: Register the test in the test list**

Add `test-stagnant-crank-batched-notification` near other backtest DB tests in `src/lisp/tests.lisp`:

```lisp
                  test-max-age-retire-batched-notification
                  test-stagnant-crank-batched-notification
```

**Step 3: Run test to verify it fails**

Run: `./scripts/ci-test.sh`

Expected: FAIL with undefined vars/functions for `*stagnant-crank-retire-*` or `maybe-flush-stagnant-crank-retire`.

**Step 4: Commit**

```bash
git add src/lisp/tests/backtest-db-tests.lisp src/lisp/tests.lisp
git commit -m "test: add stagnant C-rank batch notification test"
```

---

### Task 2: Implement Stagnant C-Rank batch buffer + flush

**Files:**
- Modify: `src/lisp/core/discord.lisp`

**Step 1: Write minimal implementation**

Add constants + buffer + functions (mirroring max-age) near the existing max-age batch section:

```lisp
(defparameter *stagnant-crank-batch-interval* 3600 "Seconds to batch Stagnant C-Rank alerts")
(defvar *stagnant-crank-retire-buffer* nil "Buffered strategy names for Stagnant C-Rank alerts")
(defvar *stagnant-crank-retire-first-seen* 0 "Timestamp when current Stagnant C-Rank batch started")

(defun queue-stagnant-crank-retire (strategy-name &key (now (get-universal-time)))
  "Queue a Stagnant C-Rank alert for hourly batching."
  (unless (and (stringp strategy-name) (> (length strategy-name) 0))
    (return-from queue-stagnant-crank-retire nil))
  (push strategy-name *stagnant-crank-retire-buffer*)
  (when (<= *stagnant-crank-retire-first-seen* 0)
    (setf *stagnant-crank-retire-first-seen* now))
  t)

(defun maybe-flush-stagnant-crank-retire (&optional (now (get-universal-time)))
  "Flush Stagnant C-Rank summary if the batch interval has elapsed."
  (when (and *stagnant-crank-retire-buffer*
             (> (- now *stagnant-crank-retire-first-seen*) *stagnant-crank-batch-interval*))
    (let* ((total (length *stagnant-crank-retire-buffer*))
           (ordered (reverse *stagnant-crank-retire-buffer*))
           (top (subseq ordered 0 (min 5 (length ordered))))
           (msg (format nil "🧊 **Stagnant C-Rank Summary (Last 1h)**~%Total: ~d~%Top: ~{`~a`~^, ~}~%Action: Individual alerts suppressed."
                        total top)))
      (setf *stagnant-crank-retire-buffer* nil)
      (setf *stagnant-crank-retire-first-seen* 0)
      (notify-discord-alert msg :color +color-alert+))))
```

Also extend `check-timeout-flushes` to flush this new batch:

```lisp
    ;; 5. Stagnant C-Rank (Hourly batch)
    (maybe-flush-stagnant-crank-retire now)
```

**Step 2: Run test to verify it still fails**

Run: `./scripts/ci-test.sh`

Expected: FAIL (because `kill-strategy` still sends individual alerts instead of queuing for this reason).

**Step 3: Commit**

```bash
git add src/lisp/core/discord.lisp
git commit -m "feat: add stagnant C-rank batch buffer and flush"
```

---

### Task 3: Wire kill-strategy to use the new batch path

**Files:**
- Modify: `src/lisp/strategies/strategies.lisp`

**Step 1: Implement routing in kill-strategy**

In `kill-strategy`, extend the notification branch to detect Stagnant C-Rank reasons:

```lisp
      (when (fboundp 'notify-discord-alert)
        (cond
          ((and (stringp reason)
                (search "Max Age Retirement" reason :test #'char-equal))
           (swimmy.core::queue-max-age-retire name))
          ((and (stringp reason)
                (search "Stagnant C-Rank" reason :test #'char-equal))
           (swimmy.core::queue-stagnant-crank-retire name))
          (t
           (notify-discord-alert
            (format nil "🛡️ **Strategy Soft-Killed (Cooldown)**~%Name: ~a~%Reason: ~a~%Action: Shelved for future review" name reason)
            :color 15158332))))
```

**Step 2: Run test to verify it passes**

Run: `./scripts/ci-test.sh`

Expected: PASS with the new test included.

**Step 3: Commit**

```bash
git add src/lisp/strategies/strategies.lisp
git commit -m "feat: batch stagnant C-rank alerts like max-age"
```

---

### Task 4: Final verification

**Files:**
- None (verification only)

**Step 1: Run full test suite**

Run: `./scripts/ci-test.sh`

Expected: PASS (144+ tests, 0 failures)

**Step 2: Final commit check**

```bash
git status -sb
```

Expected: Clean working tree.
