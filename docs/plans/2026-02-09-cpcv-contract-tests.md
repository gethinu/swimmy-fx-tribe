# CPCV Contract Tests Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add Lisp-side contract tests for CPCV request/response, and preserve optional trade metadata fields in CPCV_RESULT notifications.

**Architecture:** Extend existing `src/lisp/tests.lisp` CPCV tests to validate required request keys and optional response fields. Update the CPCV_RESULT handler in `src/lisp/core/message-dispatcher.lisp` to include `:trades-truncated` and `:trades-ref` in the result plist passed downstream.

**Tech Stack:** Common Lisp (SBCL), existing Swimmy test runner (`tests/test_runner.lisp`).

### Task 1: Add CPCV Contract Tests

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing tests**

1) Extend `test-request-cpcv-includes-request-id` to assert required keys:

```lisp
(assert-true (search "(action" captured) "action should be present in payload")
(assert-true (search "CPCV_VALIDATE" captured) "action should be CPCV_VALIDATE")
(assert-true (search "strategy_name" captured) "strategy_name should be present")
(assert-true (search "symbol" captured) "symbol should be present")
(assert-true (search "candles_file" captured) "candles_file should be present")
(assert-true (search "strategy_params" captured) "strategy_params should be present")
```

2) Add a new test `test-cpcv-result-preserves-trade-meta` near the other CPCV tests:

```lisp
(deftest test-cpcv-result-preserves-trade-meta
  "CPCV_RESULT should preserve trade_list metadata fields"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"AAA\") (request_id . \"RID-CPCV\") (trade_list . (((timestamp . 1) (pnl . 1.0) (symbol . \"USDJPY\")))) (trades_truncated . t) (trades_ref . \"RID-CPCV\"))))")
           (called nil)
           (captured nil)
           (orig-record (symbol-function 'swimmy.school:record-backtest-trades))
           (orig-notify (symbol-function 'swimmy.core:notify-cpcv-result)))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:record-backtest-trades)
                  (lambda (rid name kind trades)
                    (setf called (list rid name kind trades))
                    nil))
            (setf (symbol-function 'swimmy.core:notify-cpcv-result)
                  (lambda (data)
                    (setf captured data)
                    nil))
            (funcall fn msg)
            (assert-true called "record-backtest-trades should be called")
            (assert-true captured "notify-cpcv-result should be called")
            (assert-equal t (getf captured :trades-truncated) "trades_truncated should be preserved")
            (assert-equal "RID-CPCV" (getf captured :trades-ref) "trades_ref should be preserved"))
        (setf (symbol-function 'swimmy.school:record-backtest-trades) orig-record)
        (setf (symbol-function 'swimmy.core:notify-cpcv-result) orig-notify)))))
```

3) Add the new test symbol to `run-all-tests` near the other CPCV tests:

```lisp
test-cpcv-result-preserves-trade-meta
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --script tests/test_runner.lisp
```

Expected: FAIL at `test-cpcv-result-preserves-trade-meta` because `:trades-truncated` / `:trades-ref` are missing from the CPCV result payload.

### Task 2: Preserve Optional Trade Metadata in CPCV_RESULT

**Files:**
- Modify: `src/lisp/core/message-dispatcher.lisp`

**Step 1: Write minimal implementation**

In the CPCV_RESULT handler, extract optional fields and add them to `result-plist`:

```lisp
(trades-truncated (%result-val-normalized result '(trades_truncated trades-truncated) nil))
(trades-ref (%result-val-normalized result '(trades_ref trades-ref) nil))
...
(let ((result-plist (list ...
                          :is-passed is-passed
                          :request-id request-id
                          :error error-msg
                          :trades-truncated trades-truncated
                          :trades-ref trades-ref)))
  ...)
```

**Step 2: Run tests to verify it passes**

Run:
```bash
sbcl --script tests/test_runner.lisp
```

Expected: PASS (all tests green).

**Step 3: Commit**

```bash
git add src/lisp/tests.lisp src/lisp/core/message-dispatcher.lisp
git commit -m "feat: preserve CPCV trade metadata in results"
```
