# Stagnant C‑Rank Daily Cull + Independent Flush Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Stagnant C‑Rank を日次で実行し、バッチ通知フラッシュを進化ループ完走に依存させず、滞留状況をテレメトリで可視化する。

**Architecture:** `school-breeder.lisp` に日次カリングガードを追加し、`kill-strategy` を reason-code 対応に拡張する。`school-connector.lisp` に軽量フラッシュループを追加し、`core/discord.lisp` の Stagnant C‑Rank キューでテレメトリを出す。

**Tech Stack:** Common Lisp (SBCL), Swimmy test runner (ASDF + `swimmy.tests::*`), Structured Telemetry (JSONL via `emit-telemetry-event`).

---

### Task 1: 日次カリングガードのテスト追加

**Files:**
- Modify: `src/lisp/tests/scheduler-tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-daily-cull-guard
  "Daily cull should run only once per day."
  (let* ((calls 0)
         (orig-cull (and (fboundp 'swimmy.school::cull-weak-strategies)
                         (symbol-function 'swimmy.school::cull-weak-strategies)))
         (orig-key (and (boundp 'swimmy.school::*last-cull-day-key*)
                        swimmy.school::*last-cull-day-key*)))
    (unwind-protect
        (progn
          (when orig-cull
            (setf (symbol-function 'swimmy.school::cull-weak-strategies)
                  (lambda () (incf calls))))
          (setf swimmy.school::*last-cull-day-key* 0)
          (swimmy.school::maybe-cull-weak-strategies
           :now (encode-universal-time 0 0 0 9 2 2026))
          (swimmy.school::maybe-cull-weak-strategies
           :now (encode-universal-time 0 0 1 9 2 2026))
          (swimmy.school::maybe-cull-weak-strategies
           :now (encode-universal-time 0 0 0 10 2 2026))
          (assert-equal 2 calls "Should cull once per day"))
      (when orig-cull
        (setf (symbol-function 'swimmy.school::cull-weak-strategies) orig-cull))
      (when (boundp 'swimmy.school::*last-cull-day-key*)
        (setf swimmy.school::*last-cull-day-key* orig-key)))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-daily-cull-guard)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL (undefined `maybe-cull-weak-strategies` / `*last-cull-day-key*`).

---

### Task 2: 日次カリングガードの実装

**Files:**
- Modify: `src/lisp/school/school-breeder.lisp`

**Step 1: Write minimal implementation**

```lisp
(defparameter *last-cull-day-key* 0 "Day key of last cull execution")

(defun day-key-from-time (now)
  (multiple-value-bind (_s _m _h date month year) (decode-universal-time now)
    (declare (ignore _s _m _h))
    (+ (* year 10000) (* month 100) date)))

(defun maybe-cull-weak-strategies (&key (now (get-universal-time)))
  (let ((day-key (day-key-from-time now)))
    (unless (= day-key *last-cull-day-key*)
      (setf *last-cull-day-key* day-key)
      (cull-weak-strategies))))
```

Then in `process-breeding-cycle`, replace the Saturday-only block with:
```lisp
(maybe-cull-weak-strategies)
```

**Step 2: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-daily-cull-guard)' \
  --eval '(sb-ext:quit)'
```
Expected: PASS

**Step 3: Commit**
```bash
git add src/lisp/school/school-breeder.lisp src/lisp/tests/scheduler-tests.lisp
git commit -m "feat: run stagnant c-rank cull daily with guard"
```

---

### Task 3: reason-code でバッチ投入できるテスト追加

**Files:**
- Modify: `src/lisp/tests/backtest-db-tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-kill-strategy-reason-code-queues-stagnant
  "kill-strategy should queue stagnant batch via reason-code even without string match."
  (let* ((name "TEST-REASON-CODE")
         (tmp-db (format nil "/tmp/swimmy-reason-code-~a.db" (get-universal-time)))
         (queued nil)
         (orig-queue (and (fboundp 'swimmy.core::queue-stagnant-c-rank)
                          (symbol-function 'swimmy.core::queue-stagnant-c-rank))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*strategy-knowledge-base* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (setf swimmy.core::*stagnant-c-rank-buffer* nil)
            (setf swimmy.core::*stagnant-c-rank-first-seen* 0)
            (when orig-queue
              (setf (symbol-function 'swimmy.core::queue-stagnant-c-rank)
                    (lambda (s &key now)
                      (declare (ignore now))
                      (push s queued)
                      t)))
            (swimmy.school::init-db)
            (let ((strat (make-strategy :name name :symbol "USDJPY")))
              (setf *strategy-knowledge-base* (list strat))
              (upsert-strategy strat)
              (kill-strategy name "Manual Reason" :reason-code :stagnant-c-rank)
              (assert-equal 1 (length queued) "Should queue via reason-code")))
        (when orig-queue
          (setf (symbol-function 'swimmy.core::queue-stagnant-c-rank) orig-queue))
        (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-kill-strategy-reason-code-queues-stagnant)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL (unknown keyword or queue not called).

---

### Task 4: reason-code 対応の実装

**Files:**
- Modify: `src/lisp/strategies/strategies.lisp`
- Modify: `src/lisp/school/school-breeder.lisp`

**Step 1: Implement minimal code**

In `kill-strategy` signature:
```lisp
(defun kill-strategy (name reason &key reason-code)
```

And notify section:
```lisp
(when (fboundp 'notify-discord-alert)
  (cond
    ((eq reason-code :stagnant-c-rank)
     (swimmy.core::queue-stagnant-c-rank name))
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

In `cull-weak-strategies`, pass reason-code:
```lisp
(kill-strategy (strategy-name s)
               (format nil "Cull: Stagnant C-Rank (~,2f) after 10 days" sharpe)
               :reason-code :stagnant-c-rank)
```

**Step 2: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-kill-strategy-reason-code-queues-stagnant)' \
  --eval '(sb-ext:quit)'
```
Expected: PASS

**Step 3: Commit**
```bash
git add src/lisp/strategies/strategies.lisp src/lisp/school/school-breeder.lisp src/lisp/tests/backtest-db-tests.lisp
git commit -m "feat: add reason-code for stagnant c-rank batching"
```

---

### Task 5: 独立フラッシュループ + テレメトリのテスト追加

**Files:**
- Modify: `src/lisp/tests/scheduler-tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-stagnant-flush-tick-invokes-timeout-flush
  "Flush tick should invoke check-timeout-flushes without evolution loop."
  (let ((called 0)
        (orig (and (fboundp 'swimmy.core:check-timeout-flushes)
                   (symbol-function 'swimmy.core:check-timeout-flushes))))
    (unwind-protect
        (progn
          (when orig
            (setf (symbol-function 'swimmy.core:check-timeout-flushes)
                  (lambda () (incf called))))
          (swimmy.school::run-stagnant-flush-tick)
          (assert-equal 1 called "Should call check-timeout-flushes"))
      (when orig
        (setf (symbol-function 'swimmy.core:check-timeout-flushes) orig)))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-stagnant-flush-tick-invokes-timeout-flush)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL (undefined `run-stagnant-flush-tick`).

---

### Task 6: 独立フラッシュループ + テレメトリの実装

**Files:**
- Modify: `src/lisp/school/school-connector.lisp`
- Modify: `src/lisp/core/discord.lisp`

**Step 1: Implement flush tick + background loop**

In `school-connector.lisp`:
```lisp
(defparameter *stagnant-flush-interval* 60)
(defvar *stagnant-flush-thread* nil)
(defvar *stagnant-flush-stop* nil)

(defun run-stagnant-flush-tick ()
  (when (fboundp 'swimmy.core:check-timeout-flushes)
    (swimmy.core:check-timeout-flushes)))

(defun start-stagnant-flush-loop (&key (interval *stagnant-flush-interval*))
  (unless (and *stagnant-flush-thread*
               (sb-thread:thread-alive-p *stagnant-flush-thread*))
    (setf *stagnant-flush-stop* nil)
    (setf *stagnant-flush-thread*
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (loop until *stagnant-flush-stop* do
                   (run-stagnant-flush-tick)
                   (sleep interval))
               (error (e)
                 (format *error-output* "[CONNECTOR] ⚠️ Stagnant flush loop failed: ~a~%" e))))
           :name "stagnant-flush-loop"))))
```

Call `start-stagnant-flush-loop` once during startup (e.g. at top of `start-evolution-service` before loop).

**Step 2: Emit telemetry on queue**

In `core/discord.lisp` inside `queue-stagnant-c-rank`:
```lisp
(let* ((now (or now (get-universal-time)))
       (first (if (<= *stagnant-c-rank-first-seen* 0) now *stagnant-c-rank-first-seen*))
       (oldest (- now first))
       (data (list :buffer-len (length *stagnant-c-rank-buffer*)
                   :oldest-secs oldest)))
  (when (fboundp 'swimmy.core::emit-telemetry-event)
    (swimmy.core::emit-telemetry-event "stagnant_c_rank.buffer"
      :service "school"
      :severity "info"
      :data data)))
```

**Step 3: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-stagnant-flush-tick-invokes-timeout-flush)' \
  --eval '(sb-ext:quit)'
```
Expected: PASS

**Step 4: Commit**
```bash
git add src/lisp/school/school-connector.lisp src/lisp/core/discord.lisp src/lisp/tests/scheduler-tests.lisp
git commit -m "feat: run stagnant c-rank flush loop independent of evolution"
```

---

### Task 7: ドキュメント更新

**Files:**
- Modify: `doc/owners_guide.md`

**Step 1: Update weekly → daily**

Update the culling bullet to say daily execution.

**Step 2: Commit**
```bash
git add doc/owners_guide.md
git commit -m "docs: clarify stagnant c-rank cull runs daily"
```

---

### Task 8: Full Test Run

**Step 1: Run full test suite**

Run:
```bash
sbcl --script tests/test_runner.lisp
```
Expected: `Passed: 185, Failed: 0` (or updated count) and exit 0.

---

## Execution Handoff

Plan complete and saved to `docs/plans/2026-02-09-stagnant-c-rank-daily-flush-implementation-plan.md`.

Two execution options:

1. Subagent-Driven (this session) — use `superpowers:subagent-driven-development`
2. Parallel Session (separate) — new session with `superpowers:executing-plans`

Which approach?
