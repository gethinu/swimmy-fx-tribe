# Stagnant C-Rank Batch Alerts Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Batch “Stagnant C-Rank (after 10 days)” soft-kill alerts into an hourly summary, mirroring Max Age Retirement batching.

**Architecture:** Add a dedicated buffer + flush function in `src/lisp/core/discord.lisp` and route matching soft-kill reasons in `kill-strategy` to the queue function. Flush from `check-timeout-flushes` alongside Max Age batching. Tests mirror existing Max Age batched notification test.

**Tech Stack:** Common Lisp (SBCL), existing test runner in `tests/test_runner.lisp`.

### Task 1: Add Failing Test (TDD)

**Files:**
- Modify: `src/lisp/tests/backtest-db-tests.lisp`

**Step 1: Write failing test**

Add below `test-max-age-retire-batched-notification`:

```lisp
(deftest test-stagnant-c-rank-batched-notification
  "Stagnant C-Rank soft-kill should batch notifications and flush hourly."
  (let* ((name "TEST-STAGNANT-C-RANK")
         (tmp-db (format nil "/tmp/swimmy-stagnant-c-rank-~a.db" (get-universal-time)))
         (sent-messages nil))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (let ((orig-notify (and (fboundp 'swimmy.school::notify-discord-alert)
                              (symbol-function 'swimmy.school::notify-discord-alert))))
        (unwind-protect
            (progn
              (setf swimmy.core::*stagnant-c-rank-buffer* nil)
              (setf swimmy.core::*stagnant-c-rank-first-seen* 0)
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
                (kill-strategy name "Cull: Stagnant C-Rank (0.00) after 10 days")
                (assert-equal 0 (length sent-messages) "Should suppress individual alert")
                (assert-equal 1 (length swimmy.core::*stagnant-c-rank-buffer*)
                              "Should buffer stagnant C-rank notification")
                (let ((start swimmy.core::*stagnant-c-rank-first-seen*))
                  (swimmy.core::maybe-flush-stagnant-c-rank (+ start 3601))
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

**Step 2: Run test to verify failure**

Run: `sbcl --script tests/test_runner.lisp`

Expected: FAIL (undefined `*stagnant-c-rank-buffer*` or `maybe-flush-stagnant-c-rank`, or assertions fail).

### Task 2: Implement Batch Queue + Flush

**Files:**
- Modify: `src/lisp/core/discord.lisp`
- Modify: `src/lisp/strategies/strategies.lisp`

**Step 1: Add buffer + queue + flush to discord.lisp**

Add near the Max Age section:

```lisp
;;; ==========================================
;;; BATCHED ALERTS (Stagnant C-Rank)
;;; ==========================================
(defparameter *stagnant-c-rank-batch-interval* 3600 "Seconds to batch Stagnant C-Rank alerts")
(defvar *stagnant-c-rank-buffer* nil "Buffered strategy names for Stagnant C-Rank alerts")
(defvar *stagnant-c-rank-first-seen* 0 "Timestamp when current Stagnant C-Rank batch started")

(defun queue-stagnant-c-rank (strategy-name &key (now (get-universal-time)))
  "Queue a Stagnant C-Rank alert for hourly batching."
  (unless (and (stringp strategy-name) (> (length strategy-name) 0))
    (return-from queue-stagnant-c-rank nil))
  (push strategy-name *stagnant-c-rank-buffer*)
  (when (<= *stagnant-c-rank-first-seen* 0)
    (setf *stagnant-c-rank-first-seen* now))
  t)

(defun maybe-flush-stagnant-c-rank (&optional (now (get-universal-time)))
  "Flush Stagnant C-Rank summary if the batch interval has elapsed."
  (when (and *stagnant-c-rank-buffer*
             (> (- now *stagnant-c-rank-first-seen*) *stagnant-c-rank-batch-interval*))
    (let* ((total (length *stagnant-c-rank-buffer*))
           (ordered (reverse *stagnant-c-rank-buffer*))
           (top (subseq ordered 0 (min 5 (length ordered))))
           (msg (format nil "🧊 **Stagnant C-Rank Summary (Last 1h)**~%Total: ~d~%Top: ~{`~a`~^, ~}~%Action: Individual alerts suppressed."
                        total top)))
      (setf *stagnant-c-rank-buffer* nil)
      (setf *stagnant-c-rank-first-seen* 0)
      (notify-discord-alert msg :color +color-alert+))))
```

Then in `check-timeout-flushes` add:

```lisp
;; 5. Stagnant C-Rank (Hourly batch)
(maybe-flush-stagnant-c-rank now)
```

**Step 2: Route soft-kill reason to new queue**

In `src/lisp/strategies/strategies.lisp` in `kill-strategy`, change the notify block to:

```lisp
(when (fboundp 'notify-discord-alert)
  (cond
    ((and (stringp reason)
          (search "Max Age Retirement" reason :test #'char-equal))
     (swimmy.core::queue-max-age-retire name))
    ((and (stringp reason)
          (search "Stagnant C-Rank" reason :test #'char-equal))
     (swimmy.core::queue-stagnant-c-rank name))
    (t
     (notify-discord-alert
       (format nil "🛡️ **Strategy Soft-Killed (Cooldown)**~%Name: ~a~%Reason: ~a~%Action: Shelved for future review" name reason)
       :color 15158332))))
```

**Step 3: Run tests to verify green**

Run: `sbcl --script tests/test_runner.lisp`

Expected: PASS

### Task 3: Update State Documentation (Optional but Recommended)

**Files:**
- Modify: `docs/llm/STATE.md`

**Step 1: Update Notifications bullet**

Extend the existing Notifications bullet to mention Stagnant C-Rank batching alongside Max Age.

**Step 2: Commit doc change**

```bash
git add docs/llm/STATE.md
```

### Task 4: Commit Feature

**Files:**
- Modify: `src/lisp/core/discord.lisp`
- Modify: `src/lisp/strategies/strategies.lisp`
- Modify: `src/lisp/tests/backtest-db-tests.lisp`
- (Optional) `docs/llm/STATE.md`

**Step 1: Commit**

```bash
git add src/lisp/core/discord.lisp src/lisp/strategies/strategies.lisp src/lisp/tests/backtest-db-tests.lisp
# add docs/llm/STATE.md if updated

git commit -m "feat: batch stagnant C-rank soft-kill alerts"
```
