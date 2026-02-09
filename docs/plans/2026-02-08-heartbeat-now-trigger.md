# Heartbeat Now Trigger Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan.

**Goal:** Add a trigger-file mechanism so operators can force an immediate Discord heartbeat without restarting services.

**Architecture:** The Brain checks for `.opus/heartbeat.now` inside the periodic maintenance loop. If present, it calls `swimmy.engine::send-discord-heartbeat` and deletes the trigger file. A unit test verifies call + cleanup.

**Tech Stack:** Common Lisp (SBCL), Swimmy test framework (`deftest`).

---

### Task 1: Add a Failing Test (Trigger File)

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**
Add this test near the other heartbeat tests:

```lisp
(deftest test-heartbeat-now-trigger-file
  "Trigger file should send heartbeat and delete trigger"
  (let* ((trigger (swimmy.core::swimmy-path ".opus/heartbeat.now"))
         (orig-send (and (fboundp 'swimmy.engine::send-discord-heartbeat)
                         (symbol-function 'swimmy.engine::send-discord-heartbeat)))
         (called nil))
    (assert-true (fboundp 'swimmy.main::maybe-trigger-heartbeat-now)
                 "Expected trigger checker to exist")
    (unwind-protect
        (progn
          (ensure-directories-exist trigger)
          (with-open-file (out trigger :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
            (write-line "1" out))
          (when orig-send
            (setf (symbol-function 'swimmy.engine::send-discord-heartbeat)
                  (lambda () (setf called t))))
          (swimmy.main::maybe-trigger-heartbeat-now)
          (assert-true called "Expected heartbeat send to be invoked")
          (assert-false (probe-file trigger) "Trigger file should be removed"))
      (when orig-send
        (setf (symbol-function 'swimmy.engine::send-discord-heartbeat) orig-send))
      (ignore-errors (delete-file trigger)))))
```

**Step 2: Run test to verify it fails**
Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-asd "/home/swimmy/swimmy/.worktrees/heartbeat-now-trigger/swimmy.asd") (asdf:load-system :swimmy) (swimmy.tests::test-heartbeat-now-trigger-file) (sb-ext:quit))'
```
Expected: FAIL with assertion like `Expected trigger checker to exist`.

---

### Task 2: Implement Trigger Check + Wire into Scheduler

**Files:**
- Modify: `src/lisp/core/scheduler.lisp`

**Step 1: Add the trigger function**
Add near other scheduler helpers:

```lisp
(defun maybe-trigger-heartbeat-now ()
  "If .opus/heartbeat.now exists, send a heartbeat and delete the trigger file."
  (let ((trigger (swimmy.core::swimmy-path ".opus/heartbeat.now")))
    (when (probe-file trigger)
      (when (fboundp 'swimmy.engine::send-discord-heartbeat)
        (ignore-errors (swimmy.engine::send-discord-heartbeat)))
      (ignore-errors (delete-file trigger)))))
```

**Step 2: Call it from periodic maintenance**
Inside `run-periodic-maintenance` **Section 3** (Heartbeat area), add:

```lisp
(when (fboundp 'swimmy.main::maybe-trigger-heartbeat-now)
  (swimmy.main::maybe-trigger-heartbeat-now))
```
Place it before the existing `check-discord-heartbeat` call.

**Step 3: Run test to verify it passes**
Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-asd "/home/swimmy/swimmy/.worktrees/heartbeat-now-trigger/swimmy.asd") (asdf:load-system :swimmy) (swimmy.tests::test-heartbeat-now-trigger-file) (sb-ext:quit))'
```
Expected: PASS (no error output).

---

### Task 3: Add Test to the Suite

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: Add to run-all-tests list**
Insert `test-heartbeat-now-trigger-file` next to other heartbeat tests.

**Step 2: (Optional) Run full suite**
Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-asd "/home/swimmy/swimmy/.worktrees/heartbeat-now-trigger/swimmy.asd") (asdf:load-system :swimmy) (swimmy-tests:run-all-tests) (sb-ext:quit))'
```
Expected: All tests pass (warnings acceptable if pre-existing).

**Step 3: Commit**
```bash
git add src/lisp/tests.lisp src/lisp/core/scheduler.lisp
git commit -m "feat: add heartbeat trigger file for on-demand send"
```

---

## Notes for Ops
To trigger an immediate heartbeat:
```bash
touch /home/swimmy/swimmy/.opus/heartbeat.now
```
The Brain will send a heartbeat on the next maintenance tick and remove the file.
