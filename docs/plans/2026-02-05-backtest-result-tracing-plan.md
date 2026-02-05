# Backtest Result Tracing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 5581で受信した BACKTEST_RESULT の「適用→DB upsert」経路を、`SWIMMY_BACKTEST_DEBUG_RECV=1` 時のみ可観測にする。

**Architecture:** `src/lisp/core/message-dispatcher.lisp` に限定してログを追加し、`SWIMMY_BACKTEST_DEBUG_RECV=1` のときだけ `backtest_debug.log` に summary を追記する。エラー時は `%dlq-record` に記録し、例外は再送出して既存の `Msg Error` を維持する。

**Tech Stack:** Common Lisp (SBCL), existing test runner (`sbcl --script tests/test_runner.lisp`).

---

### Task 1: Debug Log の回帰テストを追加（RED）

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add below test near other `internal-process-msg` tests (around `test-internal-process-msg-backtest-request-id-bound`):

```lisp
(deftest test-backtest-debug-log-records-apply
  "BACKTEST_RESULT should write debug log around apply when debug enabled"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-DBG\") (sharpe . 0.2) (trades . 1) (pnl . 0.1) (request_id . \"RID-DBG\"))))")
           (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
           (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
           (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
           (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                         (symbol-function 'swimmy.school::handle-v2-result)))
           (orig-env (uiop:getenv "SWIMMY_BACKTEST_DEBUG_RECV"))
           (tmp-path (merge-pathnames
                      (format nil "backtest_debug_test_~a.log" (get-universal-time))
                      (uiop:temporary-directory))))
      (unwind-protect
          (progn
            (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" "1" 1)
            (setf (symbol-function 'swimmy.school:cache-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:apply-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:lookup-oos-request)
                  (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
            (when (fboundp 'swimmy.school::handle-v2-result)
              (setf (symbol-function 'swimmy.school::handle-v2-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (when (probe-file tmp-path)
              (ignore-errors (delete-file tmp-path)))
            (let ((swimmy.main::*backtest-debug-log* (namestring tmp-path)))
              (funcall fn msg)
              (assert-true (probe-file tmp-path) "Expected debug log file")
              (let ((content (uiop:read-file-string tmp-path)))
                (assert-true (search "apply-backtest-result start" content)
                             "Expected apply start log")
                (assert-true (search "apply-backtest-result end" content)
                             "Expected apply end log"))))
        (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
        (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
        (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
        (when orig-v2
          (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2))
        (if orig-env
            (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" orig-env 1)
            (sb-posix:unsetenv "SWIMMY_BACKTEST_DEBUG_RECV"))
        (when (probe-file tmp-path)
          (ignore-errors (delete-file tmp-path)))))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --script tests/test_runner.lisp`

Expected: FAIL with missing debug log markers (apply start/end not present).

---

### Task 2: Debug Log 追加（GREEN）

**Files:**
- Modify: `src/lisp/core/message-dispatcher.lisp:213-253`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write minimal implementation**

Update `BACKTEST_RESULT` handling to log start/end and record errors.
Add around `apply-backtest-result` and `handle-v2-result` calls (inside the `(t ...)` branch):

```lisp
(backtest-debug-log "apply-backtest-result start name=~a request_id=~a" name request-id)
(handler-case
    (progn
      (swimmy.school:apply-backtest-result name metrics)
      (backtest-debug-log "apply-backtest-result end name=~a request_id=~a" name request-id))
  (error (e)
    (backtest-debug-log "apply-backtest-result error name=~a request_id=~a err=~a"
                        name request-id e)
    (%dlq-record "apply-backtest-result error" msg result)
    (error e)))

(when (fboundp 'swimmy.school::handle-v2-result)
  (backtest-debug-log "handle-v2-result start name=~a request_id=~a" full-name request-id)
  (handler-case
      (progn
        (swimmy.school::handle-v2-result full-name metrics)
        (backtest-debug-log "handle-v2-result end name=~a request_id=~a" full-name request-id))
    (error (e)
      (backtest-debug-log "handle-v2-result error name=~a request_id=~a err=~a"
                          full-name request-id e)
      (%dlq-record "handle-v2-result error" msg result)
      (error e))))
```

**Step 2: Run test to verify it passes**

Run: `sbcl --script tests/test_runner.lisp`

Expected: PASS (new debug log markers present).

**Step 3: Commit**

```bash
git add src/lisp/core/message-dispatcher.lisp src/lisp/tests.lisp
git commit -m "feat: trace backtest result apply flow"
```

---

### Task 3: Final Verification

**Files:**
- None

**Step 1: Run full test suite**

Run: `sbcl --script tests/test_runner.lisp`

Expected: PASS (89 passed, 0 failed)

