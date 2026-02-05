# Aux Services S-Expression Migration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Data Keeper / Risk Gateway / Notifier のZMQプロトコルを S-expression（alist）に統一し、外部API境界はJSONのまま維持する。

**Architecture:** ZMQメッセージは `type` + `schema_version` 必須の alist。Python側は `src/python/sexp_utils.py` でパースし、`src/python/sexp_serialize.py` でS式を生成する（Option用の特殊キーは無効化）。Lisp側は `encode-sexp` と `safe-read-sexp` で送受信する。

**Tech Stack:** Python (tools/*), Common Lisp (SBCL), ZeroMQ, `src/python/sexp_utils.py`, `src/python/sexp_serialize.py`, Lisp test runner (`sbcl --script tests/test_runner.lisp`).

---

### Task 1: Aux S-expression Helpers (Python)

**Files:**
- Create: `src/python/aux_sexp.py`
- Test: `tools/test_aux_sexp.py`

**Step 1: Write the failing test**

```python
# tools/test_aux_sexp.py
from aux_sexp import parse_aux_request, sexp_response


def _assert_raises(fn):
    ok = False
    try:
        fn()
    except Exception:
        ok = True
    assert ok, "expected error"


def main():
    ok = parse_aux_request('((type . "DATA_KEEPER") (schema_version . 1) (action . "STATUS"))')
    assert ok["type"] == "DATA_KEEPER"
    _assert_raises(lambda: parse_aux_request('((schema_version . 1))'))
    out = sexp_response({"type": "DATA_KEEPER_RESULT", "schema_version": 1, "status": "ok"})
    assert "schema_version" in out


if __name__ == "__main__":
    main()
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_aux_sexp.py`
Expected: FAIL with `ModuleNotFoundError: No module named 'aux_sexp'`

**Step 3: Write minimal implementation**

```python
# src/python/aux_sexp.py
from sexp_utils import parse_sexp_alist
from sexp_serialize import sexp_serialize

REQUIRED_KEYS = ("type", "schema_version")


def parse_aux_request(text: str) -> dict:
    data = parse_sexp_alist(text)
    missing = [k for k in REQUIRED_KEYS if k not in data]
    if missing:
        raise ValueError(f"missing required keys: {missing}")
    return data


def sexp_response(payload: dict) -> str:
    if "schema_version" not in payload:
        payload = dict(payload)
        payload["schema_version"] = 1
    return sexp_serialize(
        payload,
        symbol_value_keys=set(),
        bool_value_keys=set(),
        optional_list_keys=set(),
    )
```

**Step 4: Run test to verify it passes**

Run: `python3 tools/test_aux_sexp.py`
Expected: PASS (no output)

**Step 5: Commit**

```bash
git add src/python/aux_sexp.py tools/test_aux_sexp.py
git commit -m "feat: add aux s-expression helpers"
```

---

### Task 2: Data Keeper S-expression Protocol

**Files:**
- Modify: `tools/data_keeper.py`
- Create: `tools/test_data_keeper_sexp.py`

**Step 1: Write the failing test**

```python
# tools/test_data_keeper_sexp.py
import os
import zmq
from sexp_utils import parse_sexp_alist


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default


port = _env_int("SWIMMY_PORT_DATA_KEEPER", 5561)
ctx = zmq.Context()
sock = ctx.socket(zmq.REQ)
sock.connect(f"tcp://localhost:{port}")
sock.setsockopt(zmq.RCVTIMEO, 2000)

sexp = '((type . "DATA_KEEPER") (schema_version . 1) (action . "STATUS"))'
print("Sending:", sexp)
sock.send_string(sexp)
resp = sock.recv_string()
print("Received:", resp)
parsed = parse_sexp_alist(resp)
assert parsed["type"] == "DATA_KEEPER_RESULT"
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_data_keeper_sexp.py`
Expected: FAIL with `SexpParseError` (JSON response)

**Step 3: Write minimal implementation**

```python
# tools/data_keeper.py (imports)
from aux_sexp import parse_aux_request, sexp_response

# inside run_server loop
message = socket.recv_string()
try:
    request = parse_aux_request(message)
except Exception as e:
    socket.send_string(sexp_response({"type": "DATA_KEEPER_RESULT", "status": "error", "error": str(e)}))
    continue

action = str(request.get("action", "")).upper()
if action == "GET_HISTORY":
    symbol = str(request.get("symbol", "")).upper()
    timeframe = str(request.get("timeframe", "M1")).upper()
    count = int(request.get("count", 0))
    response = handle_get_history_params(symbol, timeframe, count)
elif action == "GET_FILE_PATH":
    response = handle_get_file_path_params(request)
# ... STATUS / ADD_CANDLE / SAVE_ALL
socket.send_string(sexp_response({"type": "DATA_KEEPER_RESULT", **response}))
```

**Step 4: Run test to verify it passes**

Run: `python3 tools/test_data_keeper_sexp.py`
Expected: PASS (parses S-expression)

**Step 5: Commit**

```bash
git add tools/data_keeper.py tools/test_data_keeper_sexp.py
git commit -m "feat: add data keeper s-expression protocol"
```

---

### Task 3: Risk Gateway S-expression Protocol

**Files:**
- Modify: `tools/risk_gateway.py`
- Create: `tools/test_risk_gateway_sexp.py`

**Step 1: Write the failing test**

```python
# tools/test_risk_gateway_sexp.py
import os
import zmq
from sexp_utils import parse_sexp_alist


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default


port = _env_int("SWIMMY_RISK_GATEWAY_PORT", 5563)
ctx = zmq.Context()
sock = ctx.socket(zmq.REQ)
sock.connect(f"tcp://localhost:{port}")
sock.setsockopt(zmq.RCVTIMEO, 2000)

sexp = '((type . "CHECK_RISK") (schema_version . 1) (action . "BUY") (symbol . "USDJPY") (lot . 0.01) (daily_pnl . -10.0) (equity . 50000.0) (consecutive_losses . 0))'
print("Sending:", sexp)
sock.send_string(sexp)
resp = sock.recv_string()
print("Received:", resp)
parsed = parse_sexp_alist(resp)
assert parsed["type"] == "RISK_DECISION"
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_risk_gateway_sexp.py`
Expected: FAIL with `SexpParseError` (JSON response)

**Step 3: Write minimal implementation**

```python
# tools/risk_gateway.py (imports)
from aux_sexp import parse_aux_request, sexp_response

# inside loop
msg_str = socket.recv_string()
try:
    data = parse_aux_request(msg_str)
except Exception as e:
    socket.send_string(sexp_response({"type": "RISK_DECISION", "status": "ERROR", "reason": str(e)}))
    continue

if data.get("type") == "CHECK_RISK":
    response = handle_check_risk(data)
    response = {"type": "RISK_DECISION", **response}
else:
    response = {"type": "RISK_DECISION", "status": "ERROR", "reason": "Unknown type"}

socket.send_string(sexp_response(response))
```

**Step 4: Run test to verify it passes**

Run: `python3 tools/test_risk_gateway_sexp.py`
Expected: PASS (parses S-expression)

**Step 5: Commit**

```bash
git add tools/risk_gateway.py tools/test_risk_gateway_sexp.py
git commit -m "feat: add risk gateway s-expression protocol"
```

---

### Task 4: Notifier S-expression Protocol

**Files:**
- Modify: `tools/notifier.py`
- Create: `tools/test_notifier_sexp.py`

**Step 1: Write the failing test**

```python
# tools/test_notifier_sexp.py
from aux_sexp import parse_aux_request


def main():
    sexp = '((type . "NOTIFY") (schema_version . 1) (webhook . "http://example") (content_type . "embed") (data . ((content . "") (embeds . (((title . "T") (description . "D")))))))'
    data = parse_aux_request(sexp)
    assert data["type"] == "NOTIFY"
    assert data["webhook"] == "http://example"


if __name__ == "__main__":
    main()
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_notifier_sexp.py`
Expected: FAIL with `ModuleNotFoundError: No module named 'aux_sexp'` (if Task 1 not done) or PASS after Task 1.

**Step 3: Write minimal implementation**

```python
# tools/notifier.py (imports)
from aux_sexp import parse_aux_request

# inside loop
msg_str = socket.recv_string()
try:
    msg = parse_aux_request(msg_str)
except Exception:
    print("[NOTIFIER] Invalid S-expression received")
    continue

if msg.get("type") != "NOTIFY":
    print("[NOTIFIER] Unsupported type")
    continue

webhook = msg.get("webhook")
payload = msg.get("data")
```

**Step 4: Run test to verify it passes**

Run: `python3 tools/test_notifier_sexp.py`
Expected: PASS

**Step 5: Commit**

```bash
git add tools/notifier.py tools/test_notifier_sexp.py
git commit -m "feat: add notifier s-expression protocol"
```

---

### Task 5: Lisp Data Keeper Client S-expression

**Files:**
- Modify: `src/lisp/core/data-client.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-data-keeper-request-sexp
  "Data Keeper requests should be S-expression with schema_version"
  (let ((req (swimmy.core::build-data-keeper-request "STATUS")))
    (assert-true (search "(type . \"DATA_KEEPER\")" req))
    (assert-true (search "(schema_version . 1)" req))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --script tests/test_runner.lisp`
Expected: FAIL with undefined function `build-data-keeper-request`

**Step 3: Write minimal implementation**

```lisp
(defun build-data-keeper-request (action &key symbol timeframe count candle)
  (swimmy.core:encode-sexp
   `((type . "DATA_KEEPER")
     (schema_version . 1)
     (action . ,action)
     (symbol . ,symbol)
     (timeframe . ,timeframe)
     (count . ,count)
     (candle . ,candle))))

(defun data-keeper-query (action &key symbol timeframe count candle)
  (when *data-keeper-socket*
    (handler-case
        (progn
          (pzmq:send *data-keeper-socket*
                     (build-data-keeper-request action
                                                :symbol symbol
                                                :timeframe timeframe
                                                :count count
                                                :candle candle))
          (let* ((response (pzmq:recv-string *data-keeper-socket*))
                 (sexp (swimmy.core:safe-read-sexp response :package :swimmy.core)))
            sexp))
      (error (e)
        (format t "[DATA-CLIENT] Query failed: ~a~%" e)
        nil))))
```

**Step 4: Run test to verify it passes**

Run: `sbcl --script tests/test_runner.lisp`
Expected: PASS

**Step 5: Commit**

```bash
git add src/lisp/core/data-client.lisp src/lisp/tests.lisp
git commit -m "feat: send data keeper requests as s-expression"
```

---

### Task 6: Update Integration Scripts to S-expression

**Files:**
- Modify: `tools/test_keeper_ping.py`
- Modify: `tools/test_persistence.py`
- Modify: `tools/test_notifier_direct.py`

**Step 1: Write the failing test**

```python
# tools/test_keeper_ping.py (update to S-expression)
sexp = '((type . "DATA_KEEPER") (schema_version . 1) (action . "STATUS"))'
socket.send_string(sexp)
```

**Step 2: Run test to verify it fails (before code migration)**

Run: `python3 tools/test_keeper_ping.py`
Expected: FAIL if Data Keeper still JSON

**Step 3: Write minimal implementation**

Update each script to send S-expression payloads and parse S-expression responses using `sexp_utils.parse_sexp_alist`.

**Step 4: Run test to verify it passes**

Run: `python3 tools/test_keeper_ping.py`
Expected: PASS

**Step 5: Commit**

```bash
git add tools/test_keeper_ping.py tools/test_persistence.py tools/test_notifier_direct.py
git commit -m "chore: update aux service tests to s-expression"
```
