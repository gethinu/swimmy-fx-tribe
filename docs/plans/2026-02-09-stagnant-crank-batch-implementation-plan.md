# Stagnant C-Rank Batch Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Stagnant C-Rankを日次cullに統一し、理由コード化・短周期フラッシュ・テレメトリ（バッファ長/最古滞留）を追加する。

**Architecture:** `kill-strategy` に `:reason-code` を導入して通知経路を構造化。`run-periodic-maintenance` から `check-timeout-flushes` を呼び、ループ終端依存を解消。日次cullは日付キーで1日1回だけ走るようにガードする。

**Tech Stack:** Common Lisp (SBCL), ASDF tests, Markdown

---

### Task 1: 理由コードでStagnant C-Rank通知を駆動する

**Files:**
- Modify: `src/lisp/strategies/strategies.lisp`
- Test: `src/lisp/tests/backtest-db-tests.lisp`
- Test list: `src/lisp/tests.lisp`

**Step 1: Write the failing test**
Add a test that uses a reason code (string is unrelated) and asserts the stagnant buffer is used.
```lisp
(deftest test-kill-strategy-reason-code-stagnant-crank
  "Reason code should route Stagnant C-Rank without string search"
  (let* ((name "TEST-STAGNANT-C-RANK-CODE")
         (tmp-db (format nil "/tmp/swimmy-stagnant-crank-code-~a.db" (get-universal-time)))
         (sent-messages nil)
         (ok nil))
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
                (handler-case
                    (progn
                      (kill-strategy name "Unrelated reason" :reason-code :stagnant-crank)
                      (setf ok t))
                  (error () (setf ok nil)))
                (assert-true ok "kill-strategy should accept reason-code")
                (assert-equal 0 (length sent-messages) "Should suppress individual alert")
                (assert-equal 1 (length swimmy.core::*stagnant-crank-retire-buffer*)
                              "Should buffer stagnant C-rank notification")))
          (when orig-notify
            (setf (symbol-function 'swimmy.school::notify-discord-alert) orig-notify))
          (ignore-errors (execute-non-query "DELETE FROM strategies WHERE name = ?" name))
          (ignore-errors (close-db-connection))
          (ignore-errors (delete-file tmp-db)))))))
```

**Step 2: Run test to verify it fails**
Run: `sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :test-kill-strategy-reason-code-stagnant-crank) (sb-ext:quit))'`
Expected: FAIL with "kill-strategy should accept reason-code"

**Step 3: Write minimal implementation**
- Update `kill-strategy` signature to accept `&key reason-code`.
- Add a small helper to normalize reason-code from legacy strings.
- Route notification by code first, then fallback to string search.
- Update Stagnant C-Rank cull call to pass `:reason-code :stagnant-crank`.

**Step 4: Run test to verify it passes**
Run: `sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :test-kill-strategy-reason-code-stagnant-crank) (sb-ext:quit))'`
Expected: PASS

**Step 5: Commit**
```bash
git add src/lisp/strategies/strategies.lisp src/lisp/school/school-breeder.lisp src/lisp/tests/backtest-db-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add reason codes for stagnant c-rank"
```

---

### Task 2: Stagnant C-Rankバッファのテレメトリ

**Files:**
- Modify: `src/lisp/core/discord.lisp`
- Test: `src/lisp/tests/telemetry-tests.lisp`
- Test list: `src/lisp/tests.lisp`

**Step 1: Write the failing test**
```lisp
(deftest test-stagnant-crank-telemetry-buffer
  "Queueing stagnant C-Rank should emit buffer length + oldest age telemetry"
  (let ((events nil)
        (orig-emit (symbol-function 'swimmy.core::emit-telemetry-event)))
    (unwind-protect
        (progn
          (setf swimmy.core::*stagnant-crank-retire-buffer* nil)
          (setf swimmy.core::*stagnant-crank-retire-first-seen* 0)
          (setf (symbol-function 'swimmy.core::emit-telemetry-event)
                (lambda (event-type &key data &allow-other-keys)
                  (push (list event-type data) events)))
          (queue-stagnant-crank-retire "STRAT-A" :now 100)
          (let* ((ev (find "stagnant_crank.buffer" events :key #'first :test #'string=))
                 (data (second ev)))
            (assert-true ev "Expected telemetry event")
            (assert-true (jsown:val data "buffer_len"))
            (assert-true (jsown:val data "oldest_age_seconds"))))
      (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit))))
```

**Step 2: Run test to verify it fails**
Run: `sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :test-stagnant-crank-telemetry-buffer) (sb-ext:quit))'`
Expected: FAIL (no telemetry event)

**Step 3: Write minimal implementation**
- In `queue-stagnant-crank-retire`, compute buffer length and oldest age.
- Emit `stagnant_crank.buffer` telemetry with `:data` containing `buffer_len` and `oldest_age_seconds`.
- Add a simple time-based throttle to avoid spam (e.g., 60s).

**Step 4: Run test to verify it passes**
Run: `sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :test-stagnant-crank-telemetry-buffer) (sb-ext:quit))'`
Expected: PASS

**Step 5: Commit**
```bash
git add src/lisp/core/discord.lisp src/lisp/tests/telemetry-tests.lisp src/lisp/tests.lisp
git commit -m "feat: emit stagnant c-rank buffer telemetry"
```

---

### Task 3: フラッシュ経路の短周期フック

**Files:**
- Modify: `src/lisp/core/scheduler.lisp`
- Test: `src/lisp/tests/scheduler-tests.lisp`
- Test list: `src/lisp/tests.lisp`

**Step 1: Write the failing test**
```lisp
(deftest test-scheduler-calls-timeout-flushes
  "run-periodic-maintenance should call check-timeout-flushes"
  (let ((called nil)
        (orig-flush (symbol-function 'swimmy.core::check-timeout-flushes))
        (orig-check (symbol-function 'swimmy.main::check-scheduled-tasks)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core::check-timeout-flushes)
                (lambda (&rest args) (declare (ignore args)) (setf called t)))
          (setf (symbol-function 'swimmy.main::check-scheduled-tasks)
                (lambda (&rest args) (declare (ignore args)) nil))
          (let ((*candle-history* nil))
            (swimmy.main:run-periodic-maintenance))
          (assert-true called "Expected timeout flushes to be invoked"))
      (setf (symbol-function 'swimmy.core::check-timeout-flushes) orig-flush)
      (setf (symbol-function 'swimmy.main::check-scheduled-tasks) orig-check))))
```

**Step 2: Run test to verify it fails**
Run: `sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :test-scheduler-calls-timeout-flushes) (sb-ext:quit))'`
Expected: FAIL

**Step 3: Write minimal implementation**
- Call `(swimmy.core:check-timeout-flushes)` in `run-periodic-maintenance` (Section 3), guarded by `fboundp`.

**Step 4: Run test to verify it passes**
Run: `sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :test-scheduler-calls-timeout-flushes) (sb-ext:quit))'`
Expected: PASS

**Step 5: Commit**
```bash
git add src/lisp/core/scheduler.lisp src/lisp/tests/scheduler-tests.lisp src/lisp/tests.lisp
git commit -m "feat: call timeout flushes from scheduler"
```

---

### Task 4: 日次cullガード + ドキュメント明記

**Files:**
- Modify: `src/lisp/school/school-breeder.lisp`
- Modify: `doc/owners_guide.md`
- Test: `src/lisp/tests.lisp`

**Step 1: Write the failing test**
Add a small unit for day-key guard.
```lisp
(deftest test-stagnant-crank-daily-guard
  "Stagnant C-Rank cull should run once per day"
  (let ((swimmy.school::*last-stagnant-crank-cull-day* nil))
    (assert-true (swimmy.school::should-run-stagnant-crank-cull-p 20260209))
    (assert-false (swimmy.school::should-run-stagnant-crank-cull-p 20260209))
    (assert-true (swimmy.school::should-run-stagnant-crank-cull-p 20260210))))
```

**Step 2: Run test to verify it fails**
Run: `sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :test-stagnant-crank-daily-guard) (sb-ext:quit))'`
Expected: FAIL

**Step 3: Write minimal implementation**
- Add `*last-stagnant-crank-cull-day*` and helper `should-run-stagnant-crank-cull-p` in `school-breeder.lisp`.
- Split/adjust cull logic so Stagnant C-Rank cull runs daily with the guard.

**Step 4: Update documentation**
Add to `doc/owners_guide.md`:
- “Stagnant C-Rank のcullは日次（day-keyガードで1日1回）”
- “通知は1時間バッチでサマリ送信（個別通知抑制）”

**Step 5: Run test to verify it passes**
Run: `sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :test-stagnant-crank-daily-guard) (sb-ext:quit))'`
Expected: PASS

**Step 6: Commit**
```bash
git add src/lisp/school/school-breeder.lisp doc/owners_guide.md src/lisp/tests.lisp
git commit -m "feat: run stagnant c-rank cull daily"
```

---

### Final Verification
```bash
sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :run-all-tests) (sb-ext:quit))'
```

---

Plan complete and saved to `docs/plans/2026-02-09-stagnant-crank-batch-implementation-plan.md`.

Two execution options:
1. **Subagent-Driven (this session)** - I dispatch a fresh subagent per task, review between tasks
2. **Parallel Session (separate)** - Open new session with executing-plans

Which approach?
