# MCP stdio JSON-RPC Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add an MCP-standard stdio JSON-RPC 2.0 server that exposes read-only + backtest-exec methods and routes through the existing gateway core.

**Architecture:** A single-process stdio server parses Content-Length framed JSON-RPC, validates API keys, dispatches methods to existing handlers, and reads status/metrics from local files. Internal ZMQ/S-expression contracts remain unchanged.

**Tech Stack:** Python 3, JSON-RPC 2.0 over stdio (Content-Length), existing `tools/mcp_gateway.py`, `src/python/sexp_utils.py`.

---

### Task 1: JSON-RPC framing + validation utilities

**Files:**
- Create: `tools/mcp_stdio_server.py`
- Test: `tools/test_mcp_stdio_server.py`

**Step 1: Write the failing test**

```python
# tools/test_mcp_stdio_server.py
import io
import json
from mcp_stdio_server import read_jsonrpc_message, write_jsonrpc_message, validate_jsonrpc_request


def test_read_jsonrpc_message():
    payload = {"jsonrpc": "2.0", "id": 1, "method": "health.ping"}
    body = json.dumps(payload).encode("utf-8")
    buf = io.BytesIO(b"Content-Length: %d\r\n\r\n" % len(body) + body)
    msg = read_jsonrpc_message(buf)
    assert msg["method"] == "health.ping"


def test_write_jsonrpc_message():
    buf = io.BytesIO()
    write_jsonrpc_message(buf, {"jsonrpc": "2.0", "id": 1, "result": {"ok": True}})
    out = buf.getvalue()
    assert out.startswith(b"Content-Length: ")
    assert b"\r\n\r\n" in out


def test_validate_jsonrpc_request():
    req = {"jsonrpc": "2.0", "id": 1, "method": "health.ping", "params": {"api_key": "x"}}
    assert validate_jsonrpc_request(req) is None


def main():
    test_read_jsonrpc_message()
    test_write_jsonrpc_message()
    test_validate_jsonrpc_request()


if __name__ == "__main__":
    main()
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: FAIL with `ModuleNotFoundError: No module named 'mcp_stdio_server'`

**Step 3: Write minimal implementation**

```python
# tools/mcp_stdio_server.py
import json


def read_jsonrpc_message(stream):
    headers = {}
    while True:
        line = stream.readline()
        if not line:
            return None
        if line in (b"\r\n", b"\n"):
            break
        key, value = line.decode("utf-8").split(":", 1)
        headers[key.strip().lower()] = value.strip()
    length = int(headers.get("content-length", "0"))
    body = stream.read(length)
    return json.loads(body.decode("utf-8"))


def write_jsonrpc_message(stream, payload):
    body = json.dumps(payload).encode("utf-8")
    header = f"Content-Length: {len(body)}\r\n\r\n".encode("utf-8")
    stream.write(header + body)
    stream.flush()


def validate_jsonrpc_request(req):
    if not isinstance(req, dict):
        return "invalid"
    if req.get("jsonrpc") != "2.0":
        return "invalid"
    if "method" not in req or "id" not in req:
        return "invalid"
    return None
```

**Step 4: Run test to verify it passes**

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: PASS

**Step 5: Commit**

```bash
git add tools/mcp_stdio_server.py tools/test_mcp_stdio_server.py
git commit -m "feat: add jsonrpc framing helpers"
```

---

### Task 2: Dispatch + auth + gateway integration

**Files:**
- Modify: `tools/mcp_stdio_server.py`
- Test: `tools/test_mcp_stdio_server.py`

**Step 1: Write the failing test**

```python
# tools/test_mcp_stdio_server.py (append)
from mcp_stdio_server import handle_jsonrpc_request


def test_auth_missing():
    req = {"jsonrpc": "2.0", "id": 1, "method": "health.ping", "params": {}}
    res = handle_jsonrpc_request(req, api_key="secret")
    assert res["error"]["code"] == -32001


def test_trade_disabled():
    req = {"jsonrpc": "2.0", "id": 1, "method": "trade.submit", "params": {"api_key": "k"}}
    res = handle_jsonrpc_request(req, api_key="k")
    assert res["error"]["code"] == -32003


def test_backtest_submit_returns_request_id():
    req = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "backtest.submit",
        "params": {"api_key": "k", "symbol": "USDJPY", "timeframe": 1, "candles_file": "x.csv"},
    }
    res = handle_jsonrpc_request(req, api_key="k")
    assert "request_id" in res["result"]


def main():
    test_read_jsonrpc_message()
    test_write_jsonrpc_message()
    test_validate_jsonrpc_request()
    test_auth_missing()
    test_trade_disabled()
    test_backtest_submit_returns_request_id()
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: FAIL with `AttributeError` or `NameError` (missing `handle_jsonrpc_request`)

**Step 3: Write minimal implementation**

```python
# tools/mcp_stdio_server.py (append)
import os
import time
from mcp_gateway import handle_backtest_submit, handle_trade_submit


def jsonrpc_error(req_id, code, message, data=None):
    payload = {"jsonrpc": "2.0", "id": req_id, "error": {"code": code, "message": message}}
    if data is not None:
        payload["error"]["data"] = data
    return payload


def jsonrpc_result(req_id, result):
    return {"jsonrpc": "2.0", "id": req_id, "result": result}


def handle_jsonrpc_request(req, api_key=None):
    err = validate_jsonrpc_request(req)
    if err:
        return jsonrpc_error(req.get("id"), -32600, "Invalid Request")
    params = req.get("params") or {}
    if api_key and params.get("api_key") != api_key:
        return jsonrpc_error(req.get("id"), -32001, "Unauthorized")
    method = req.get("method")
    if method == "health.ping":
        return jsonrpc_result(req["id"], {"status": "ok"})
    if method == "trade.submit":
        res = handle_trade_submit(params)
        return jsonrpc_error(req["id"], -32003, res.get("error", "trade-capable disabled"))
    if method == "backtest.submit":
        payload = dict(params)
        payload.pop("api_key", None)
        res = handle_backtest_submit(payload)
        return jsonrpc_result(req["id"], res)
    return jsonrpc_error(req.get("id"), -32601, "Method not found")
```

**Step 4: Run test to verify it passes**

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: PASS

**Step 5: Commit**

```bash
git add tools/mcp_stdio_server.py tools/test_mcp_stdio_server.py
git commit -m "feat: add mcp jsonrpc dispatch"
```

---

### Task 3: system.status / system.metrics / backtest.status

**Files:**
- Modify: `tools/mcp_stdio_server.py`
- Test: `tools/test_mcp_stdio_server.py`

**Step 1: Write the failing test**

```python
# tools/test_mcp_stdio_server.py (append)
import os
import tempfile
from pathlib import Path
from mcp_stdio_server import load_system_status, load_system_metrics


def test_system_status_reads_files():
    with tempfile.TemporaryDirectory() as d:
        backtest = Path(d) / "backtest_status.txt"
        backtest.write_text("timestamp: 1\ncount: 2\n", encoding="utf-8")
        swimmy_status = Path(d) / "swimmy_status"
        swimmy_status.write_text("TIME: now\nTICKS: 3\n", encoding="utf-8")
        data = load_system_status(str(swimmy_status), str(backtest))
        assert data["backtest"]["count"] == "2"
        assert data["swimmy"]["ticks"] == "3"


def test_system_metrics_reads_sexp():
    with tempfile.TemporaryDirectory() as d:
        metrics = Path(d) / "system_metrics.sexp"
        metrics.write_text("((uptime_seconds . 10) (strategy_count . 2))", encoding="utf-8")
        data = load_system_metrics(str(metrics))
        assert data["uptime_seconds"] == 10


def main():
    test_read_jsonrpc_message()
    test_write_jsonrpc_message()
    test_validate_jsonrpc_request()
    test_auth_missing()
    test_trade_disabled()
    test_backtest_submit_returns_request_id()
    test_system_status_reads_files()
    test_system_metrics_reads_sexp()
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: FAIL with missing functions `load_system_status` / `load_system_metrics`

**Step 3: Write minimal implementation**

```python
# tools/mcp_stdio_server.py (append)
from pathlib import Path
from mcp_gateway_observer import read_status_file

BASE_DIR = Path(__file__).resolve().parents[1]
PYTHON_SRC = BASE_DIR / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))
from sexp_utils import load_sexp_alist


def load_system_status(swimmy_status_path="/tmp/swimmy_status", backtest_status_path=None):
    backtest_status_path = backtest_status_path or str(BASE_DIR / "data" / "reports" / "backtest_status.txt")
    swimmy = read_status_file(swimmy_status_path) if Path(swimmy_status_path).exists() else {}
    backtest = read_status_file(backtest_status_path) if Path(backtest_status_path).exists() else {}
    return {"swimmy": {k.lower(): v for k, v in swimmy.items()}, "backtest": {k.lower(): v for k, v in backtest.items()}}


def load_system_metrics(metrics_path=None):
    metrics_path = metrics_path or str(BASE_DIR / "data" / "system_metrics.sexp")
    if not Path(metrics_path).exists():
        return {}
    return load_sexp_alist(metrics_path)
```

Then update dispatcher to handle methods:

```python
if method == "system.status":
    return jsonrpc_result(req["id"], load_system_status())
if method == "system.metrics":
    return jsonrpc_result(req["id"], load_system_metrics())
if method == "backtest.status":
    data = load_system_status()["backtest"]
    data["mode"] = "latest"
    return jsonrpc_result(req["id"], data)
```

**Step 4: Run test to verify it passes**

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: PASS

**Step 5: Commit**

```bash
git add tools/mcp_stdio_server.py tools/test_mcp_stdio_server.py
git commit -m "feat: add mcp status and metrics methods"
```

---

### Task 4: stdio main loop + logging

**Files:**
- Modify: `tools/mcp_stdio_server.py`
- Test: `tools/test_mcp_stdio_server.py` (optional log smoke)

**Step 1: Write the failing test**

```python
# tools/test_mcp_stdio_server.py (append)
from mcp_stdio_server import build_log_entry


def test_build_log_entry():
    entry = build_log_entry("req-1", "health.ping", 12, "ok")
    assert entry["request_id"] == "req-1"


def main():
    test_read_jsonrpc_message()
    test_write_jsonrpc_message()
    test_validate_jsonrpc_request()
    test_auth_missing()
    test_trade_disabled()
    test_backtest_submit_returns_request_id()
    test_system_status_reads_files()
    test_system_metrics_reads_sexp()
    test_build_log_entry()
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: FAIL with missing `build_log_entry`

**Step 3: Write minimal implementation**

```python
# tools/mcp_stdio_server.py (append)
import datetime


def build_log_entry(request_id, method, duration_ms, status, error=None):
    entry = {
        "timestamp": datetime.datetime.utcnow().isoformat() + "Z",
        "request_id": request_id,
        "method": method,
        "duration_ms": duration_ms,
        "status": status,
    }
    if error:
        entry["error"] = error
    return entry


def append_log(entry, path=None):
    path = path or str(BASE_DIR / "logs" / "mcp_gateway.jsonl")
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    with open(path, "a", encoding="utf-8") as f:
        f.write(json.dumps(entry, ensure_ascii=False) + "\n")


def main():
    api_key = os.getenv("SWIMMY_MCP_API_KEY", "")
    while True:
        req = read_jsonrpc_message(sys.stdin.buffer)
        if req is None:
            break
        start = time.time()
        res = handle_jsonrpc_request(req, api_key=api_key)
        duration_ms = int((time.time() - start) * 1000)
        status = "ok" if "result" in res else "error"
        append_log(build_log_entry(req.get("id"), req.get("method"), duration_ms, status, res.get("error")))
        write_jsonrpc_message(sys.stdout.buffer, res)

if __name__ == "__main__":
    main()
```

**Step 4: Run test to verify it passes**

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: PASS

**Step 5: Commit**

```bash
git add tools/mcp_stdio_server.py tools/test_mcp_stdio_server.py
git commit -m "feat: add mcp stdio main loop and logging"
```

---

### Task 5: Documentation updates

**Files:**
- Modify: `docs/llm/INTERFACES.md`
- Modify: `doc/SYSTEM_ARCHITECTURE.md`
- Modify: `doc/runbook.md`

**Step 1: Update INTERFACES.md**

Add a new MCP stdio JSON-RPC section:
```markdown
## MCP stdio (JSON-RPC 2.0)
- Transport: stdio (Content-Length framing)
- Methods: health.ping, system.status, system.metrics, backtest.submit, backtest.status, trade.submit (403)
- Params: api_key required
```

**Step 2: Update SYSTEM_ARCHITECTURE.md**

Add MCP boundary in architecture diagram and mention stdio server.

**Step 3: Update runbook**

Add a small section for running the stdio server manually:
```bash
SWIMMY_MCP_API_KEY=... python3 tools/mcp_stdio_server.py
```

**Step 4: Commit**

```bash
git add docs/llm/INTERFACES.md doc/SYSTEM_ARCHITECTURE.md doc/runbook.md
git commit -m "docs: document mcp stdio jsonrpc"
```

---

### Task 6: Full verification

**Step 1: Run Python tests**

Run: `python3 tools/test_mcp_stdio_server.py`
Expected: PASS

**Step 2: Run CI test suite**

Run: `./ci-test.sh`
Expected: PASS

**Step 3: Commit any remaining changes**

```bash
git status --short
```

