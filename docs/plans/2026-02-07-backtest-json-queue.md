# Backtest JSON + Send Queue Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 5555番でJSONの`BACKTEST_RESULT`を受けられるようにし、Backtest Service起動前の送信ドロップをキューで防止し、初期化失敗時は明示的に失敗させる。

**Architecture:** `message-dispatcher.lisp`でJSONを`BACKTEST_RESULT`のみS式へ変換し既存ハンドラへ流す。`school-backtest-utils.lisp`に送信キューを追加し、`SWIMMY_BACKTEST_SERVICE=1`時はrequester未準備ならキューに積む。`init-backtest-zmq`は接続成功後にキューをフラッシュし、失敗時は明示的にエラーで停止する。

**Tech Stack:** Common Lisp (SBCL), jsown, pzmq (ZeroMQ), Swimmy内蔵テストフレームワーク

---

### Task 1: JSON BACKTEST_RESULTの失敗テストを追加

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-internal-process-msg-backtest-json-applies
  "JSON BACKTEST_RESULT should call apply-backtest-result"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (called nil)
         (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
         (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
         (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
         (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                       (symbol-function 'swimmy.school::handle-v2-result))))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:apply-backtest-result)
                (lambda (name metrics) (setf called (list name metrics))))
          (setf (symbol-function 'swimmy.school:cache-backtest-result)
                (lambda (&rest _args) (declare (ignore _args)) nil))
          (setf (symbol-function 'swimmy.school:lookup-oos-request)
                (lambda (&rest _args) (declare (ignore _args)) (values nil nil nil)))
          (when (fboundp 'swimmy.school::handle-v2-result)
            (setf (symbol-function 'swimmy.school::handle-v2-result)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (funcall fn "{\"type\":\"BACKTEST_RESULT\",\"result\":{\"strategy_name\":\"UT-JSON-1\",\"sharpe\":1.2,\"trades\":3,\"pnl\":0.4,\"request_id\":\"RID-J1\"}}")
          (assert-not-nil called "Expected apply-backtest-result to be called")
          (assert-equal "UT-JSON-1" (first called) "Expected strategy name")
          (assert-equal "RID-J1" (getf (second called) :request-id)
                        "Expected request-id in metrics"))
      (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
      (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
      (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
      (when orig-v2
        (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2)))))
```

Also add `test-internal-process-msg-backtest-json-applies` to the test list in `run-all-tests`.

**Step 2: Run test to verify it fails**

Run: `sbcl --script tests/test_runner.lisp`

Expected: FAIL with `Expected apply-backtest-result to be called`.

**Step 3: Commit**

```bash
git add src/lisp/tests.lisp
git commit -m "test: add failing JSON BACKTEST_RESULT test"
```

---

### Task 2: Backtest送信キューの失敗テストを追加

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing tests**

```lisp
(deftest test-backtest-queue-enqueues-when-requester-missing
  "Backtest sends should enqueue before requester is ready"
  (let* ((orig-enabled swimmy.core:*backtest-service-enabled*)
         (orig-req (and (boundp 'swimmy.globals:*backtest-requester*)
                        swimmy.globals:*backtest-requester*))
         (orig-cmd (and (boundp 'swimmy.globals:*cmd-publisher*)
                        swimmy.globals:*cmd-publisher*))
         (orig-send (symbol-function 'pzmq:send))
         (sent nil))
    (unwind-protect
        (progn
          (setf swimmy.core:*backtest-service-enabled* t)
          (setf swimmy.globals:*backtest-requester* nil)
          (setf swimmy.globals:*cmd-publisher* :dummy)
          (setf swimmy.school::*backtest-send-queue* nil)
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest args) (setf sent args)))
          (swimmy.school:send-zmq-msg "(PING)" :target :backtest)
          (assert-true (null sent) "Should not send before requester")
          (assert-equal 1 (length swimmy.school::*backtest-send-queue*)
                        "Expected queued message"))
      (setf swimmy.core:*backtest-service-enabled* orig-enabled)
      (setf swimmy.globals:*backtest-requester* orig-req)
      (setf swimmy.globals:*cmd-publisher* orig-cmd)
      (setf (symbol-function 'pzmq:send) orig-send))))

(deftest test-backtest-queue-flushes-after-requester
  "Queued backtests should flush in order once requester is ready"
  (let* ((orig-req (and (boundp 'swimmy.globals:*backtest-requester*)
                        swimmy.globals:*backtest-requester*))
         (orig-send (symbol-function 'pzmq:send))
         (sent nil))
    (unwind-protect
        (progn
          (setf swimmy.school::*backtest-send-queue* (list "m1" "m2"))
          (setf swimmy.globals:*backtest-requester* :dummy)
          (setf (symbol-function 'pzmq:send)
                (lambda (_sock msg) (push msg sent)))
          (swimmy.school:flush-backtest-queue)
          (assert-equal '("m1" "m2") (nreverse sent) "Expected FIFO send")
          (assert-true (null swimmy.school::*backtest-send-queue*)
                       "Queue should be empty"))
      (setf swimmy.globals:*backtest-requester* orig-req)
      (setf (symbol-function 'pzmq:send) orig-send))))
```

Also add these tests to the `run-all-tests` list.

**Step 2: Run test to verify it fails**

Run: `sbcl --script tests/test_runner.lisp`

Expected: FAIL with `Expected queued message` and/or `Expected FIFO send`.

**Step 3: Commit**

```bash
git add src/lisp/tests.lisp
git commit -m "test: add failing backtest queue tests"
```

---

### Task 3: Backtest初期化Fail-Fastの失敗テストを追加

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-init-backtest-zmq-fails-when-requester-missing
  "init-backtest-zmq should fail fast when enabled but requester is missing"
  (let* ((orig-enabled swimmy.core:*backtest-service-enabled*)
         (orig-req (and (boundp 'swimmy.globals:*backtest-requester*)
                        swimmy.globals:*backtest-requester*))
         (orig-ctx (symbol-function 'pzmq:ctx-new))
         (orig-socket (symbol-function 'pzmq:socket))
         (orig-connect (symbol-function 'pzmq:connect))
         (signaled nil))
    (unwind-protect
        (progn
          (setf swimmy.core:*backtest-service-enabled* t)
          (setf swimmy.globals:*backtest-requester* nil)
          (setf (symbol-function 'pzmq:ctx-new) (lambda () :ctx))
          (setf (symbol-function 'pzmq:socket) (lambda (&rest _args) (declare (ignore _args)) :sock))
          (setf (symbol-function 'pzmq:connect) (lambda (&rest _args) (declare (ignore _args)) (error "boom")))
          (handler-case
              (swimmy.school::init-backtest-zmq)
            (error () (setf signaled t)))
          (assert-true signaled "Expected init-backtest-zmq to signal error"))
      (setf swimmy.core:*backtest-service-enabled* orig-enabled)
      (setf swimmy.globals:*backtest-requester* orig-req)
      (setf (symbol-function 'pzmq:ctx-new) orig-ctx)
      (setf (symbol-function 'pzmq:socket) orig-socket)
      (setf (symbol-function 'pzmq:connect) orig-connect))))
```

Also add this test to the `run-all-tests` list.

**Step 2: Run test to verify it fails**

Run: `sbcl --script tests/test_runner.lisp`

Expected: FAIL with `Expected init-backtest-zmq to signal error`.

**Step 3: Commit**

```bash
git add src/lisp/tests.lisp
git commit -m "test: add failing init-backtest-zmq fail-fast test"
```

---

### Task 4: JSON BACKTEST_RESULTをS式に変換して処理

**Files:**
- Modify: `src/lisp/core/message-dispatcher.lisp`

**Step 1: Write minimal implementation**

Add helpers to convert JSON -> alist, then branch in `internal-process-msg`:

```lisp
(defun %json->alist (obj)
  (cond
    ((null obj) nil)
    ((listp obj) (mapcar #'%json->alist obj))
    (t
     (let ((out nil))
       (dolist (k (jsown:keys obj) (nreverse out))
         (let* ((val (jsown:val obj k))
                (sym (intern (string-upcase k) :swimmy.main)))
           (push (cons sym (%json->alist val)) out))))))

(defun %json->sexp-backtest (json)
  (let* ((result (jsown:val json "result"))
         (alist (%json->alist result)))
    `((type . "BACKTEST_RESULT") (result . ,alist))))
```

Then in `internal-process-msg`, for non-S-exp:

```lisp
(let ((json (handler-case (jsown:parse msg) (error () nil))))
  (if json
      (let ((type (or (%json-val json '("type") "") "")))
        (cond
          ((string-equal type "BACKTEST_RESULT")
           (let ((sexp (%json->sexp-backtest json)))
             (return-from internal-process-msg
               (internal-process-msg (swimmy.core:encode-sexp sexp)))))
          (t
           (format t "[DISPATCH] ⚠️ JSON payload ignored (type=~a)~%" type)
           (return-from internal-process-msg nil))))
      (progn
        (format t "[DISPATCH] ⚠️ JSON payload ignored (parse failed)~%")
        (return-from internal-process-msg nil))))
```

**Step 2: Run tests to verify they pass**

Run: `sbcl --script tests/test_runner.lisp`

Expected: The JSON test should pass (other new tests still fail).

**Step 3: Commit**

```bash
git add src/lisp/core/message-dispatcher.lisp
git commit -m "feat: accept JSON BACKTEST_RESULT in dispatcher"
```

---

### Task 5: Backtest送信キュー + Fail-Fast実装

**Files:**
- Modify: `src/lisp/school/school-backtest-utils.lisp`
- Modify: `src/lisp/school/school-backtest.lisp`

**Step 1: Write minimal implementation**

In `school-backtest-utils.lisp`, add queue + flush:

```lisp
(defvar *backtest-send-queue* nil)
(defvar *backtest-send-queue-max* 5000)

(defun enqueue-backtest-msg (msg)
  (when (< (length *backtest-send-queue*) *backtest-send-queue-max*)
    (setf *backtest-send-queue* (nconc *backtest-send-queue* (list msg)))
    t))

(defun flush-backtest-queue ()
  (when (and (boundp 'swimmy.globals:*backtest-requester*)
             swimmy.globals:*backtest-requester*)
    (loop while (and *backtest-send-queue* (backtest-send-allowed-p))
          do (let ((msg (pop *backtest-send-queue*)))
               (pzmq:send swimmy.globals:*backtest-requester* msg)))))
```

Update `send-zmq-msg` to queue when enabled but requester missing (no fallback to CMD):

```lisp
(when (and (eq target :backtest) swimmy.core:*backtest-service-enabled*
           (or (not (boundp 'swimmy.globals:*backtest-requester*))
               (null swimmy.globals:*backtest-requester*)))
  (enqueue-backtest-msg msg)
  (format t "[BACKTEST] 📥 Queued until requester is ready.~%")
  (return-from send-zmq-msg :queued))
```

In `school-backtest.lisp` `init-backtest-zmq`, after connect:

```lisp
(unless (and (boundp 'swimmy.globals:*backtest-requester*)
             swimmy.globals:*backtest-requester*)
  (error "Backtest service enabled but requester not initialized"))
(skimmy.school:flush-backtest-queue)
```

And in the error handler, re-`error` after logging so the fail-fast test passes.

**Step 2: Run tests to verify they pass**

Run: `sbcl --script tests/test_runner.lisp`

Expected: All new tests pass.

**Step 3: Commit**

```bash
git add src/lisp/school/school-backtest-utils.lisp src/lisp/school/school-backtest.lisp
git commit -m "feat: queue backtests before requester + fail fast on init"
```

---

### Task 6: Full verification

**Files:**
- None

**Step 1: Run full test suite**

Run: `sbcl --script tests/test_runner.lisp`

Expected: PASS (0 failures).

**Step 2: Optional smoke check**

Run: `rg -n "JSON payload ignored" logs/swimmy.log | head -20`

Expected: JSON ignored should only appear for non-BACKTEST_RESULT types.

---

Plan complete.
