# MCP/MT5 Protocol Alignment Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align MCP JSON-RPC request_id handling and MT5 ORDER_OPEN strictness with docs, and persist submit/receive request IDs in backtest status.

**Architecture:** Use a single request_id across MCP → ZMQ → Lisp backtest results, track both submit/receive IDs in Lisp status output, and enforce strict ORDER_OPEN fields in MT5. Update docs to match runtime behavior and add contract tests to prevent regressions.

**Tech Stack:** Common Lisp (SBCL, jsown), Python (JSON-RPC), MQL5 (MT5 EA), Markdown docs.

---

### Task 1: Backtest last_request_id tracking in Lisp status output

**Files:**
- Modify: `src/lisp/core/globals.lisp`
- Modify: `src/lisp/core/message-dispatcher.lisp`
- Modify: `src/lisp/school/school-backtest.lisp`
- Modify: `src/lisp/packages.lisp`
- Test: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add a test that forces a BACKTEST_RESULT with request_id and verifies `data/reports/backtest_status.txt` contains `last_request_id` only.

```lisp
(deftest test-backtest-status-includes-last-request-id
  (let* ((status-path "data/reports/backtest_status.txt")
         (msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-REQ-STATUS\") (sharpe . 0.2) (trades . 1) (pnl . 0.1) (request_id . \"RID-123\"))))"))
    (setf swimmy.globals:*backtest-submit-last-id* "SUB-999")
    (setf swimmy.core::*backtest-recv-last-log* 0)
    (swimmy.core::internal-process-msg msg)
    (let ((content (with-open-file (s status-path :direction :input)
                     (let ((text (make-string (file-length s))))
                       (read-sequence text s)
                       text))))
      (assert-true (search "last_request_id: RID-123" content))
      (assert-true (null (search "last_submit_id" content)))
      (assert-true (null (search "last_recv_id" content))))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --script test_runner.lisp`
Expected: FAIL with missing `last_request_id` in status output.

**Step 3: Write minimal implementation**

1) Add new globals and export:

```lisp
;; src/lisp/core/globals.lisp
(defparameter *backtest-submit-last-id* nil)
```

```lisp
;; src/lisp/packages.lisp (exports)
:export
  ;; ...
  *backtest-submit-last-id*
```

2) Track receive request_id + write last_request_id:

```lisp
;; src/lisp/core/message-dispatcher.lisp
(defparameter *backtest-recv-last-id* nil)

;; when processing BACKTEST_RESULT
(setf *backtest-recv-last-id* request-id)

;; when writing backtest_status.txt
(format s "last_request_id: ~a~%" (or *backtest-recv-last-id* "N/A"))
```

3) Ensure submit IDs are generated and recorded:

```lisp
;; src/lisp/school/school-backtest.lisp
(let ((request-id (or request-id (swimmy.core:generate-uuid))))
  (setf swimmy.globals:*backtest-submit-last-id* request-id)
  ;; use request-id in payload
  ...)
```

**Step 4: Run tests to verify they pass**

Run: `sbcl --script test_runner.lisp`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/core/globals.lisp src/lisp/packages.lisp src/lisp/core/message-dispatcher.lisp src/lisp/school/school-backtest.lisp src/lisp/tests.lisp
git commit -m "feat: track backtest last_request_id in status"
```

---

### Task 2: MCP request_id propagation + contract tests

**Files:**
- Modify: `tools/mcp_gateway.py`
- Modify: `tools/mcp_stdio_server.py`
- Test: `tools/test_mcp_gateway.py`
- Test: `tools/test_mcp_stdio_server.py`

**Step 1: Write failing tests**

Update tests to assert deterministic request_id:

```python
# tools/test_mcp_gateway.py
ok = handle_backtest_submit({"symbol": "USDJPY", "timeframe": 1, "candles_file": "x.csv", "request_id": "RID-1"})
assert ok["request_id"] == "RID-1"
```

```python
# tools/test_mcp_stdio_server.py
req = {
    "jsonrpc": "2.0",
    "id": "REQ-42",
    "method": "backtest.submit",
    "params": {"api_key": "k", "symbol": "USDJPY", "timeframe": 1, "candles_file": "x.csv"},
}
res = handle_jsonrpc_request(req, api_key="k")
assert res["result"]["request_id"] == "REQ-42"
```

**Step 2: Run tests to verify they fail**

Run: `python3 tools/test_mcp_gateway.py`
Expected: FAIL (request_id mismatch)

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: FAIL (request_id not defaulted)

**Step 3: Write minimal implementation**

```python
# tools/mcp_gateway.py
request_id = payload.get("request_id") or str(uuid.uuid4())
```

```python
# tools/mcp_stdio_server.py
payload = dict(params)
payload.pop("api_key", None)
if not payload.get("request_id"):
    payload["request_id"] = str(req["id"])
```

**Step 4: Run tests to verify they pass**

Run: `python3 tools/test_mcp_gateway.py`
Expected: PASS

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: PASS

**Step 5: Commit**

```bash
git add tools/mcp_gateway.py tools/mcp_stdio_server.py tools/test_mcp_gateway.py tools/test_mcp_stdio_server.py
git commit -m "feat: propagate MCP request_id deterministically"
```

---

### Task 3: MT5 S-expression strictness + sender contract test

**Files:**
- Modify: `src/mt5/SwimmyBridge.mq5`
- Test: `src/lisp/tests.lisp`
- Modify: `src/lisp/core/execution-protocol.lisp`
- Modify: `src/lisp/core/executor.lisp`
- Modify: `src/lisp/risk-manager.lisp`
- Modify: MT5 command senders to use S-expression (`school-danger.lisp`, `school-execution.lisp`, `school-allocation.lisp`, `system/runner.lisp`, `engine/positions.lisp`)

**Step 1: Write failing test**

Add a Lisp contract test that asserts ORDER_OPEN is emitted as S-expression with `instrument` + `side`:

```lisp
(deftest test-order-open-uses-instrument-side
  (let* ((msg (swimmy.core:make-order-message "UT" "USDJPY" :buy 0.1 0 0 0))
         (sexp (swimmy.core:encode-sexp msg))
         (parsed (swimmy.core:safe-read-sexp sexp :package :swimmy.core)))
    (assert-equal "USDJPY" (swimmy.core:sexp-alist-get parsed "instrument"))
    (assert-equal "BUY" (swimmy.core:sexp-alist-get parsed "side"))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --script test_runner.lisp`
Expected: FAIL if S-expression keys mismatch or encoding missing.

**Step 3: Write minimal implementation**

1) Enforce strict S-expression in MT5:
- Remove `StringFind` fallback for BUY/SELL and `type`.
- Require `type == "ORDER_OPEN"`.
- Require non-empty `instrument` (no symbol fallback for ORDER_OPEN).
- Require `side` in {BUY, SELL}; otherwise log error and return.

2) Align Lisp sender to S-expression:
- `make-protocol-message` returns an alist (symbols in :swimmy.core) and `encode-sexp` serializes it.
- `make-order-message` and `make-heartbeat-message` use alist + `encode-sexp` for ZMQ.
- Update `risk-manager.lisp` and `executor.lisp` to use `encode-sexp` and `sexp-alist-get` instead of jsown.
- Update command senders to use S-expression with `type` keys (replace JSON `action`).

3) WSL IP guard:
- Set default `InpWSL_IP` to empty string.
- In `OnInit`, if empty → `LogError` and return `INIT_FAILED`.

**Step 4: Run tests to verify they pass**

Run: `sbcl --script test_runner.lisp`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/mt5/SwimmyBridge.mq5 src/lisp/tests.lisp
git commit -m "feat: enforce strict ORDER_OPEN and WSL IP guard"
```

---

### Task 4: Documentation alignment

**Files:**
- Modify: `docs/llm/INTERFACES.md`
- Modify: `docs/llm/STATE.md`
- Modify: `doc/SYSTEM_ARCHITECTURE.md`
- Modify: `doc/runbook.md`
- Modify: `doc/owners_guide.md`

**Step 1: Write doc deltas**

Update protocol docs to match strict behavior:
- ORDER_OPEN uses `instrument` + `side` (BUY/SELL). `action`/`symbol` removed.
- Mention request_id propagation and `backtest_status.txt` field: `last_request_id`.
- Add MT5 encoding note (UTF-8 JSON, ASCII keys).
- Add WSL IP must be configured (no default) in runbook/owners guide.

**Step 2: Verify references**

Check for any remaining `action`/`symbol` mention that contradicts new spec.

**Step 3: Commit**

```bash
git add docs/llm/INTERFACES.md docs/llm/STATE.md doc/SYSTEM_ARCHITECTURE.md doc/runbook.md doc/owners_guide.md
git commit -m "docs: align MCP/MT5 protocol and status fields"
```

---

### Task 5: Final verification

**Step 1: Run full test suite**

Run: `sbcl --script test_runner.lisp`
Expected: PASS (warnings acceptable if pre-existing)

Run: `python3 tools/test_mcp_gateway.py`
Expected: PASS

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: PASS

**Step 2: Summarize changes and provide handoff**

List modified files and note any warnings observed.

**Step 3: Commit if any uncommitted changes remain**

```bash
git status -sb
git add -A
git commit -m "chore: finalize MCP/MT5 protocol alignment"
```

---

Plan complete.
