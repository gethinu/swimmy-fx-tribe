# OOS Pipeline Integrity Fix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** OOS相関ID欠落・UUID衝突・oos_queue不整合を修正し、A→S昇格の停滞を解消する。

**Architecture:** Backtest Serviceで`request_id`を結果へ伝播し、Lisp側のUUID生成を高エントロピー化。OOS再送のID運用を調整し、起動時に安全な全クリア＋再キューを実行する。契約は`docs/llm/INTERFACES.md`と`docs/llm/STATE.md`に反映する。

**Tech Stack:** Common Lisp (SBCL), Python, SQLite, ZeroMQ, repo docs.

### Task 1: Backtest結果に`request_id`が伝播されることのテストを追加

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/tests/backtest-db-tests.lisp` (必要なら)

**Step 1: Write the failing test**

```lisp
(deftest test-backtest-result-preserves-request-id
  "BACKTEST_RESULT should carry request_id through the pipeline"
  (let* ((msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-REQ\") (sharpe . 0.2) (trades . 1) (request_id . \"RID-1\"))))"))
    (setf swimmy.main::*backtest-recv-count* 0)
    (swimmy.main::internal-process-msg msg)
    (assert-equal "RID-1" swimmy.main::*backtest-recv-last-id* "request_id should be preserved")))
```

**Step 2: Run test to verify it fails**

Run: `./run.sh --test tests.lisp`
Expected: FAIL with missing/incorrect `request_id` assertion.

**Step 3: Write minimal implementation**

No code change in Lisp; this should pass after Task 2 (Backtest Service fix). If it still fails, add minimal bridging logic in dispatcher to read missing `request_id` from outer message.

**Step 4: Run test to verify it passes**

Run: `./run.sh --test tests.lisp`
Expected: PASS

**Step 5: Commit**

```bash
git add src/lisp/tests.lisp
git commit -m "test: assert request_id is preserved in backtest results"
```

### Task 2: Backtest Serviceで`request_id`を結果へ付与

**Files:**
- Modify: `tools/backtest_service.py`

**Step 1: Write the failing test**

Add a small unit-style test helper in `tools/backtest_service.py` or create `tests/test_backtest_service_request_id.py` (if python tests exist) that feeds a sexpr containing `(request_id . "RID-TEST")` and asserts outgoing result contains `request_id`.

```python
# tests/test_backtest_service_request_id.py
from tools.backtest_service import BacktestService

def test_backtest_service_propagates_request_id():
    svc = BacktestService(use_zmq=False)
    msg = '((action . "BACKTEST") (request_id . "RID-TEST") (strategy . ((name . "UT"))))'
    out = svc._handle_sexpr(msg)
    assert "RID-TEST" in str(out)
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/test_backtest_service_request_id.py -v`
Expected: FAIL (request_id not found).

**Step 3: Write minimal implementation**

- Parse input sexpr to extract `request_id`.
- Ensure result dict (or sexpr string) includes `request_id`.
- If Guardian returns raw sexpr string, inject `(request_id . ...)` into result.

**Step 4: Run test to verify it passes**

Run: `pytest tests/test_backtest_service_request_id.py -v`
Expected: PASS

**Step 5: Commit**

```bash
git add tools/backtest_service.py tests/test_backtest_service_request_id.py
git commit -m "fix: propagate request_id in backtest results"
```

### Task 3: UUID生成の高エントロピー化

**Files:**
- Modify: `src/lisp/core/execution-protocol.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-generate-uuid-unique
  "generate-uuid should not collide across short bursts"
  (let ((ids (make-hash-table :test 'equal)))
    (dotimes (i 1000)
      (setf (gethash (swimmy.core:generate-uuid) ids) t))
    (assert-equal 1000 (hash-table-count ids) "UUIDs should be unique in a burst")))
```

**Step 2: Run test to verify it fails**

Run: `./run.sh --test tests.lisp`
Expected: FAIL if collisions occur.

**Step 3: Write minimal implementation**

- Seed `*random-state*` once using `(make-random-state t)` and time-based jitter.
- Mix in `(get-universal-time)` and `(get-internal-real-time)` into UUID string if needed.

**Step 4: Run test to verify it passes**

Run: `./run.sh --test tests.lisp`
Expected: PASS

**Step 5: Commit**

```bash
git add src/lisp/core/execution-protocol.lisp src/lisp/tests.lisp
git commit -m "fix: harden uuid generation to avoid collisions"
```

### Task 4: OOSリトライID運用の見直し

**Files:**
- Modify: `src/lisp/school/school-validation.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-oos-retry-uses-new-request-id
  "OOS retry should generate a new request_id"
  ;; Arrange: enqueue a sent request
  (swimmy.school:enqueue-oos-request "UT-OOS" "RID-OLD" :status "sent")
  ;; Act: maybe-request-oos-backtest should return a new id on retry
  (let ((id (swimmy.school:maybe-request-oos-backtest (swimmy.school:find-strategy "UT-OOS"))))
    (assert-true (and id (not (string= id "RID-OLD"))) "retry should issue new request_id")))
```

**Step 2: Run test to verify it fails**

Run: `./run.sh --test tests.lisp`
Expected: FAIL (old ID reused).

**Step 3: Write minimal implementation**

- On retry, always generate a new `request_id`.
- Store previous ID in `last_error` or telemetry if needed.

**Step 4: Run test to verify it passes**

Run: `./run.sh --test tests.lisp`
Expected: PASS

**Step 5: Commit**

```bash
git add src/lisp/school/school-validation.lisp src/lisp/tests.lisp
git commit -m "fix: issue new request_id on oos retry"
```

### Task 5: 起動時のOOSキュー全クリア＆再キュー

**Files:**
- Modify: `src/lisp/school/school-connector.lisp` or startup path (TBD)
- Modify: `src/lisp/school/school-db-oos.lisp`
- Modify: `docs/llm/STATE.md`

**Step 1: Write the failing test**

```lisp
(deftest test-oos-queue-clear-on-startup
  "startup cleanup should clear oos_queue"
  (swimmy.school:enqueue-oos-request "UT-OOS" "RID-X" :status "sent")
  (swimmy.school:cleanup-oos-queue-on-startup)
  (multiple-value-bind (rid _ status) (swimmy.school:lookup-oos-request "UT-OOS")
    (declare (ignore _ status))
    (assert-true (null rid) "oos_queue should be cleared")))
```

**Step 2: Run test to verify it fails**

Run: `./run.sh --test tests.lisp`
Expected: FAIL (function missing).

**Step 3: Write minimal implementation**

- Add `cleanup-oos-queue-on-startup` to delete all rows from `oos_queue`.
- Call it from startup (connector/runner) once.
- Ensure it logs and is safe to run idempotently.

**Step 4: Run test to verify it passes**

Run: `./run.sh --test tests.lisp`
Expected: PASS

**Step 5: Commit**

```bash
git add src/lisp/school/school-db-oos.lisp src/lisp/school/school-connector.lisp src/lisp/tests.lisp docs/llm/STATE.md
git commit -m "fix: clear oos_queue on startup"
```

### Task 6: プロトコル/状態ドキュメント更新

**Files:**
- Modify: `docs/llm/INTERFACES.md`
- Modify: `docs/llm/STATE.md`

**Step 1: Write the doc changes**

- `BACKTEST_RESULT` に `request_id` 必須を追記。
- OOSキュー起動時クリアの運用方針をSTATEに追記。

**Step 2: Verify doc consistency**

Run: `rg -n "BACKTEST_RESULT|oos_queue" docs/llm/INTERFACES.md docs/llm/STATE.md`
Expected: updated references present.

**Step 3: Commit**

```bash
git add docs/llm/INTERFACES.md docs/llm/STATE.md
git commit -m "docs: update backtest result contract and oos startup cleanup"
```

