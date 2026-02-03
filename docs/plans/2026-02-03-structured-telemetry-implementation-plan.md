# Structured Telemetry Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** OOS/WFV/Heartbeat/System metrics を構造化イベント(JSONL)として統一し、可観測性と運用の信頼性を上げる。

**Architecture:** `core/telemetry.lisp` にイベント生成/出力を集約し、`logger.lisp` の JSONL へ1本化する。各サブシステムは `emit-telemetry-event` を呼ぶだけにする。

**Tech Stack:** Common Lisp (SBCL), ASDF, jsown, 既存テストフレームワーク。

---

### Task 0: Worktree Preflight (local-only file sync)

**Files:**
- Local-only copy: `src/lisp/school/school-optimized-params.lisp` (gitignored)

**Step 1: Copy ignored file into worktree**

Run:
```bash
cp /home/swimmy/swimmy/src/lisp/school/school-optimized-params.lisp \
  /home/swimmy/swimmy/.worktrees/structured-telemetry/src/lisp/school/school-optimized-params.lisp
```

**Step 2: Verify ASDF load**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(format t "OK~%")'
```
Expected: prints `OK` without missing-file error.

**Step 3: Commit**
No commit (gitignored, local-only).

---

### Task 1: Telemetry Core + Schema Test

**Files:**
- Create: `src/lisp/core/telemetry.lisp`
- Modify: `src/lisp/logger.lisp`
- Modify: `src/lisp/core/config.lisp`
- Modify: `swimmy.asd`
- Create: `src/lisp/tests/telemetry-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add to `src/lisp/tests/telemetry-tests.lisp`:
```lisp
(in-package :swimmy.tests)

(deftest test-telemetry-event-schema
  (let* ((tmp "data/memory/telemetry-test.jsonl")
         (orig swimmy.core::*log-file-path*))
    (unwind-protect
        (progn
          (setf swimmy.core::*log-file-path* tmp)
          (swimmy.core::emit-telemetry-event "test.event"
            :service "school" :severity "info" :correlation-id "CID-1"
            :data (list :foo 1))
          (with-open-file (in tmp)
            (let* ((line (read-line in nil nil))
                   (obj (jsown:parse line)))
              (assert-true (jsown:val obj "schema_version"))
              (assert-true (jsown:val obj "timestamp"))
              (assert-equal "telemetry" (jsown:val obj "log_type"))
              (assert-equal "test.event" (jsown:val obj "event_type"))
              (assert-equal "school" (jsown:val obj "service"))
              (assert-equal "info" (jsown:val obj "severity"))
              (assert-equal "CID-1" (jsown:val obj "correlation_id")))))
      (setf swimmy.core::*log-file-path* orig))))
```

Add the test name to `run-all-tests` list in `src/lisp/tests.lisp`.

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests:test-telemetry-event-schema)'
```
Expected: FAIL (missing function emit-telemetry-event).

**Step 3: Write minimal implementation**

Create `src/lisp/core/telemetry.lisp`:
```lisp
(in-package :swimmy.core)

(defparameter *telemetry-schema-version* 1)
(defparameter *telemetry-fail-count* 0)

(defun emit-telemetry-event (event-type &key service severity correlation-id data)
  (handler-case
      (log-telemetry event-type
                     :service service
                     :severity severity
                     :correlation-id correlation-id
                     :data data)
    (error (e)
      (incf *telemetry-fail-count*)
      (safe-format-t "[TELEMETRY] ⚠️ emit failed: ~a~%" e))))
```

Extend `src/lisp/logger.lisp` with `log-telemetry`:
```lisp
(defun log-telemetry (event-type &key service severity correlation-id data)
  (let ((entry (jsown:new-js
                 ("schema_version" *telemetry-schema-version*)
                 ("timestamp" (get-iso-8601-time))
                 ("log_type" "telemetry")
                 ("event_type" event-type)
                 ("service" (or service "unknown"))
                 ("severity" (or severity "info"))
                 ("correlation_id" (or correlation-id "unknown"))
                 ("data" data))))
    (write-line (jsown:to-json entry)
                (open *log-file-path* :direction :output :if-exists :append :if-does-not-exist :create))))
```

Add `src/lisp/core/telemetry.lisp` to `swimmy.asd` load list.

**Step 4: Run test to verify it passes**

Run same command as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/core/telemetry.lisp src/lisp/logger.lisp src/lisp/tests/telemetry-tests.lisp src/lisp/tests.lisp swimmy.asd src/lisp/core/config.lisp
git commit -m "feat: add core telemetry event emitter"
```

---

### Task 2: OOS Telemetry Emission

**Files:**
- Modify: `src/lisp/school/school-validation.lisp`
- Modify: `src/lisp/tests/telemetry-tests.lisp`

**Step 1: Write the failing test**

Add test capturing telemetry calls:
```lisp
(deftest test-oos-telemetry-requested
  (let ((events nil))
    (flet ((swimmy.core::emit-telemetry-event (event-type &key data &allow-other-keys)
             (push (list event-type data) events)))
      (flet ((swimmy.school::init-db () nil)
             (swimmy.school::lookup-oos-request (name) (declare (ignore name)) (values nil nil nil))
             (swimmy.school::enqueue-oos-request (&rest args) (declare (ignore args)) nil)
             (swimmy.school::request-backtest (&rest args) (declare (ignore args)) nil))
        (swimmy.school::maybe-request-oos-backtest (swimmy.school:make-strategy :name "UT-OOS" :symbol "USDJPY")))
      (assert-true (find "oos.requested" events :key #'first :test #'string=)))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests:test-oos-telemetry-requested)'
```
Expected: FAIL (no emit call).

**Step 3: Write minimal implementation**

In `school-validation.lisp`, add emits:
- In `maybe-request-oos-backtest`:
  - `oos.requested` / `oos.throttled` / `oos.dispatch_failed`
- In `handle-oos-backtest-result`:
  - `oos.result`
Include data: `request_id`, `oos_kind:"a-rank"`, `latency_sec`, `status`.

**Step 4: Run test to verify it passes**

Run same as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/school/school-validation.lisp src/lisp/tests/telemetry-tests.lisp
git commit -m "feat: emit OOS telemetry events"
```

---

### Task 3: WFV Scheduling Guard + Pending Monitoring

**Files:**
- Modify: `src/lisp/school/school-connector.lisp`
- Modify: `src/lisp/school/school-backtest.lisp`
- Modify: `src/lisp/core/config.lisp`
- Modify: `src/lisp/tests/school-split-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing tests**

Add to `src/lisp/tests/school-split-tests.lisp`:
```lisp
(deftest test-wfv-scheduling-respects-interval-and-pending
  (let ((calls 0)
        (orig-start (symbol-function 'swimmy.school::start-walk-forward-validation))
        (orig-top (symbol-function 'swimmy.school::get-top-strategies)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::start-walk-forward-validation)
                (lambda (strat) (declare (ignore strat)) (incf calls)))
          (setf (symbol-function 'swimmy.school::get-top-strategies)
                (lambda (n) (declare (ignore n)) (list (swimmy.school:make-strategy :name "WFV-1"))))
          (let ((swimmy.school::*wfv-pending-strategies* (make-hash-table :test 'equal))
                (swimmy.school::*last-wfv-qualify-time* (get-universal-time))
                (swimmy.core::*wfv-enabled* t)
                (swimmy.core::*wfv-interval-sec* 3600)
                (swimmy.core::*wfv-max-pending* 0)
                (swimmy.core::*wfv-max-per-run* 1))
            (setf (gethash "PENDING" swimmy.school::*wfv-pending-strategies*)
                  (list :started-at (- (get-universal-time) 10)))
            (swimmy.school::maybe-run-wfv-qualification)
            (assert-equal 0 calls)))
      (setf (symbol-function 'swimmy.school::start-walk-forward-validation) orig-start)
      (setf (symbol-function 'swimmy.school::get-top-strategies) orig-top))))

(deftest test-wfv-pending-stats-oldest-age
  (let* ((now 1000)
         (swimmy.school::*wfv-pending-strategies* (make-hash-table :test 'equal)))
    (setf (gethash "A" swimmy.school::*wfv-pending-strategies*) (list :started-at 900))
    (setf (gethash "B" swimmy.school::*wfv-pending-strategies*) (list :started-at 950))
    (multiple-value-bind (count oldest-age) (swimmy.school::wfv-pending-stats :now now)
      (assert-equal 2 count)
      (assert-equal 100 oldest-age))))
```

Add the test names to `run-all-tests` list in `src/lisp/tests.lisp`.

**Step 2: Run tests to verify they fail**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests:test-wfv-scheduling-respects-interval-and-pending)' \
  --eval '(swimmy.tests:test-wfv-pending-stats-oldest-age)'
```
Expected: FAIL (missing functions/guards).

**Step 3: Write minimal implementation**

In `src/lisp/core/config.lisp`, add WFV scheduling config:
```lisp
(defparameter *wfv-enabled* t)
(defparameter *wfv-interval-sec* (* 60 60)) ; default 1h
(defparameter *wfv-max-pending* 2)
(defparameter *wfv-max-per-run* 1)
```

In `src/lisp/school/school-backtest.lisp`:
- Add `:started-at` to the pending entry in `start-walk-forward-validation`.
- Add a helper:
```lisp
(defun wfv-pending-stats (&key (now (get-universal-time)))
  (let ((count 0)
        (oldest nil))
    (maphash (lambda (_ entry)
               (declare (ignore _))
               (incf count)
               (let ((started (getf entry :started-at)))
                 (when started
                   (setf oldest (if oldest (min oldest started) started)))))
             *wfv-pending-strategies*)
    (values count (and oldest (- now oldest)))))
```

In `src/lisp/school/school-connector.lisp`:
- Add `*last-wfv-qualify-time*`.
- Add `maybe-run-wfv-qualification` that:
  - returns early if `*wfv-enabled*` is false (log or `wfv.deferred` telemetry if available).
  - checks interval and pending count (`wfv-pending-stats`).
  - selects candidates via `(get-top-strategies *wfv-max-per-run*)`, skips already-pending by name, and calls `start-walk-forward-validation`.
  - updates `*last-wfv-qualify-time*` only when at least one WFV is started.
- Update `phase-3-qualify` to call `maybe-run-wfv-qualification`.

**Step 4: Run tests to verify they pass**

Run same command as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/core/config.lisp src/lisp/school/school-backtest.lisp src/lisp/school/school-connector.lisp src/lisp/tests/school-split-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add WFV scheduling guards"
```

---

### Task 4: WFV Telemetry Emission

**Files:**
- Modify: `src/lisp/school/school-backtest.lisp`
- Modify: `src/lisp/tests/telemetry-tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-wfv-telemetry-result-emitted-on-complete
  (let ((events nil))
    (flet ((swimmy.core::emit-telemetry-event (event-type &key data &allow-other-keys)
             (push (list event-type data) events)))
      (let ((swimmy.school::*wfv-pending-strategies* (make-hash-table :test 'equal)))
        (setf (gethash "WFV" swimmy.school::*wfv-pending-strategies*)
              (list :is-result nil :oos-result nil :strategy (swimmy.school:make-strategy :name "WFV" :generation 2)
                    :wfv-id "WFV-1" :split-ratio 0.2))
        (swimmy.school::process-wfv-result "WFV_IS" (list :sharpe 1.2))
        (swimmy.school::process-wfv-result "WFV_OOS" (list :sharpe 0.8)))
      (let* ((ev (find "wfv.result" events :key #'first :test #'string=))
             (data (second ev)))
        (assert-true (jsown:val data "wfv_id"))
        (assert-true (jsown:val data "split_ratio"))
        (assert-true (jsown:val data "generation"))
        (assert-true (jsown:val data "required_oos"))
        (assert-true (jsown:val data "degradation"))
        (assert-true (jsown:val data "decision"))))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests:test-wfv-telemetry-result-emitted-on-complete)'
```
Expected: FAIL.

**Step 3: Write minimal implementation**

Emit:
- `wfv.started` in `start-walk-forward-validation` with `wfv_id`, `split_ratio`, `is_bars`, `oos_bars`.
- `wfv.part_received` in `process-wfv-result` with `wfv_id`, `part` ("IS"/"OOS"), `sharpe`.
- `wfv.result` in `complete-wfv` with `wfv_id`, `generation`, `required_oos`, `degradation`, `decision`, `is_sharpe`, `oos_sharpe`.

Store `:wfv-id` and `:split-ratio` in the pending entry to reuse across events; generate `wfv_id` via `swimmy.core:generate-uuid`.

**Step 4: Run test to verify it passes**

Run same as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/school/school-backtest.lisp src/lisp/tests/telemetry-tests.lisp
git commit -m "feat: emit WFV telemetry events"
```

---

### Task 5: Heartbeat Telemetry Emission

**Files:**
- Modify: `src/lisp/core/executor.lisp`
- Modify: `src/lisp/engine/heartbeat.lisp`
- Modify: `src/lisp/core/message-dispatcher.lisp`
- Modify: `src/lisp/tests/telemetry-tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-heartbeat-emits-telemetry
  (let ((events nil))
    (flet ((swimmy.core::emit-telemetry-event (event-type &key data &allow-other-keys)
             (push (list event-type data) events))
           (swimmy.core::make-heartbeat-message (&optional (status "OK"))
             (jsown:new-js ("type" "HEARTBEAT") ("id" "HB-1") ("status" status))))
      (let ((swimmy.executor::*last-heartbeat-sent* 0)
            (swimmy.executor::*cmd-publisher* nil))
        (swimmy.executor::send-heartbeat))
      (assert-true (find "heartbeat.sent" events :key #'first :test #'string=)))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests:test-heartbeat-emits-telemetry)'
```
Expected: FAIL.

**Step 3: Write minimal implementation**

- `executor.lisp`: emit `heartbeat.sent` with correlation_id = heartbeat message id.
- `message-dispatcher.lisp`: on HEARTBEAT recv emit `heartbeat.recv` with correlation_id.
- `engine/heartbeat.lisp`: emit `heartbeat.discord_sent` when queued.

**Step 4: Run test to verify it passes**

Run same as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/core/executor.lisp src/lisp/engine/heartbeat.lisp src/lisp/core/message-dispatcher.lisp src/lisp/tests/telemetry-tests.lisp
git commit -m "feat: emit heartbeat telemetry events"
```

---

### Task 6: Metrics/Status Telemetry + Atomic Writes

**Files:**
- Modify: `src/lisp/school/school-telemetry.lisp`
- Modify: `src/lisp/shell/notifications.lisp`
- Modify: `src/lisp/core/telemetry.lisp`
- Modify: `src/lisp/tests/telemetry-tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-atomic-write-json
  (let ((path "data/memory/atomic-test.json"))
    (swimmy.core::atomic-write-text path "{\"ok\":true}")
    (with-open-file (in path)
      (assert-equal "{\"ok\":true}" (read-line in nil nil)))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests:test-atomic-write-json)'
```
Expected: FAIL (function missing).

**Step 3: Write minimal implementation**

Add to `core/telemetry.lisp`:
```lisp
(defun atomic-write-text (path content)
  (let* ((tmp (format nil "~a.tmp" path)))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede)
      (write-string content out))
    (rename-file tmp path)))
```

Update `school-telemetry.lisp` and `notifications.lisp` to use `atomic-write-text` instead of direct `with-open-file`.
Emit `metrics.snapshot` and `status.snapshot` events after writes.

**Step 4: Run test to verify it passes**

Run same as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/core/telemetry.lisp src/lisp/school/school-telemetry.lisp src/lisp/shell/notifications.lisp src/lisp/tests/telemetry-tests.lisp
git commit -m "feat: atomic writes + metrics/status telemetry"
```

---

### Task 7: Telemetry Log Rotation + Failure Counter

**Files:**
- Modify: `src/lisp/logger.lisp`
- Modify: `src/lisp/core/config.lisp`
- Modify: `src/lisp/tests/telemetry-tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-telemetry-rotation
  (let* ((path "data/memory/telemetry-rotate.jsonl")
         (orig swimmy.core::*log-file-path*))
    (unwind-protect
        (progn
          (setf swimmy.core::*log-file-path* path)
          (setf swimmy.core::*telemetry-max-bytes* 10)
          (swimmy.core::log-telemetry "rotate.test" :service "core" :severity "info" :correlation-id "CID" :data (list :x 1))
          (assert-true (probe-file (format nil "~a.1" path))))
      (setf swimmy.core::*log-file-path* orig))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests:test-telemetry-rotation)'
```
Expected: FAIL.

**Step 3: Write minimal implementation**

In `logger.lisp`, add:
- `*telemetry-max-bytes*` config (default e.g. 10MB)
- `maybe-rotate-log` before appending (rename to `.1` if size exceeds).
- increment telemetry failure counter if rotate/write fails.

**Step 4: Run test to verify it passes**

Run same as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/logger.lisp src/lisp/core/config.lisp src/lisp/tests/telemetry-tests.lisp
git commit -m "feat: telemetry log rotation"
```

---

### Task 8: Documentation + Runbook Update

**Files:**
- Modify: `doc/runbook.md`
- Modify: `doc/owners_guide.md` (if needed)

**Step 1: Write doc updates**
Add telemetry log location, schema_version policy, and monitoring steps.

**Step 2: Commit**
```bash
git add doc/runbook.md doc/owners_guide.md
git commit -m "docs: document telemetry monitoring"
```

---

## Notes / Constraints
- Tests require local copy of `school-optimized-params.lisp` in this worktree.
- JSONL is single source of truth; S-expression ミラーは後回し (OFF by default)。
- WFV scheduling is gated by `*wfv-enabled*` / interval / pending limits. Manual or ad-hoc WFV triggers remain as emergency tools (no removal).
