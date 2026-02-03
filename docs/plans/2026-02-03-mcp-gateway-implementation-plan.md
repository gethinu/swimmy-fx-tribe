# MCP Gateway Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** MCPゲートウェイをWSL2内に常駐させ、read-only/backtest-execを提供し、trade-capableは封印したままS式(ZMQ)へ橋渡しする。  

**Architecture:** MCP(JSON) → Gateway → ZMQ(5559, S式) を唯一の境界に固定し、観測系はファイル読み取りのみ。ZMQエンドポイントは差し替え可能にして本番ポートを叩かない統合テストを実現する。  

**Tech Stack:** Python 3, pyzmq, systemd, JSON/S-expression

---

### Task 1: 共通S式シリアライザの分離

**Files:**
- Create: `src/python/sexp_serialize.py`
- Modify: `tools/backtest_service.py`
- Test: `tools/test_sexp_serialize.py`

**Step 1: Write the failing test**

```python
# tools/test_sexp_serialize.py
from sexp_serialize import sexp_serialize

def _assert_in(needle, haystack, label):
    if needle not in haystack:
        raise AssertionError(f"{label} missing: {needle}\nGot: {haystack}")

def main():
    sexp = sexp_serialize({"filter_enabled": "false"})
    _assert_in("(filter_enabled . #f)", sexp, "bool coercion")

    sexp = sexp_serialize({"timeframe": 5})
    _assert_in("(timeframe . (5))", sexp, "timeframe option")

    sexp = sexp_serialize({"candles_file": "file.csv"})
    _assert_in('(candles_file . ("file.csv"))', sexp, "candles_file option")

if __name__ == "__main__":
    main()
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_sexp_serialize.py`  
Expected: FAIL with `ModuleNotFoundError: No module named 'sexp_serialize'`

**Step 3: Write minimal implementation**

```python
# src/python/sexp_serialize.py
def _sexp_key(key) -> str:
    return str(key).replace("-", "_")

def _sexp_symbol(value: str) -> str:
    return value.replace("-", "_")

def _coerce_bool(value):
    if isinstance(value, bool):
        return value
    if isinstance(value, str):
        return value.strip().lower() in ("1", "true", "yes", "on")
    return bool(value)

def sexp_serialize(value) -> str:
    if isinstance(value, dict):
        items = []
        for k, v in value.items():
            key_norm = _sexp_key(k)
            if isinstance(v, str) and v.lower() in ("true", "false"):
                v_str = "#t" if _coerce_bool(v) else "#f"
            elif isinstance(v, bool):
                v_str = "#t" if v else "#f"
            elif isinstance(v, (int, float)):
                v_str = str(v)
            elif isinstance(v, str):
                v_str = f"\"{v}\""
            elif isinstance(v, list):
                v_str = f"({sexp_serialize(v)})" if v and not isinstance(v[0], list) else f"({sexp_serialize(v)})"
            else:
                v_str = sexp_serialize(v)
            items.append(f"({key_norm} . {v_str})")
        return f"({' '.join(items)})"
    if isinstance(value, list):
        return " ".join(sexp_serialize(v) for v in value)
    if isinstance(value, str):
        return f"\"{value}\""
    return str(value)
```

**Step 4: Update backtest_service to import**

```python
# tools/backtest_service.py (top)
from sexp_serialize import sexp_serialize as _sexp_serialize
```

**Step 5: Run tests to verify they pass**

Run: `python3 tools/test_sexp_serialize.py`  
Expected: PASS (no output)

**Step 6: Commit**

```bash
git add src/python/sexp_serialize.py tools/backtest_service.py tools/test_sexp_serialize.py
git commit -m "feat: extract shared sexp serializer"
```

---

### Task 2: MCPゲートウェイ設定/認証レイヤー

**Files:**
- Create: `tools/mcp_gateway_config.py`
- Test: `tools/test_mcp_gateway_config.py`

**Step 1: Write the failing test**

```python
# tools/test_mcp_gateway_config.py
import os
from mcp_gateway_config import GatewayConfig

def main():
    os.environ["SWIMMY_MCP_API_KEY"] = "k"
    cfg = GatewayConfig.from_env()
    assert cfg.api_key == "k"
    assert cfg.zmq_endpoint == "tcp://localhost:5559"
    assert cfg.bind_host == "127.0.0.1"

if __name__ == "__main__":
    main()
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_mcp_gateway_config.py`  
Expected: FAIL with `ModuleNotFoundError: No module named 'mcp_gateway_config'`

**Step 3: Write minimal implementation**

```python
# tools/mcp_gateway_config.py
from dataclasses import dataclass
import os

@dataclass
class GatewayConfig:
    api_key: str
    zmq_endpoint: str
    bind_host: str
    bind_port: int

    @classmethod
    def from_env(cls):
        return cls(
            api_key=os.getenv("SWIMMY_MCP_API_KEY", ""),
            zmq_endpoint=os.getenv("SWIMMY_MCP_ZMQ_ENDPOINT", "tcp://localhost:5559"),
            bind_host=os.getenv("SWIMMY_MCP_BIND_HOST", "127.0.0.1"),
            bind_port=int(os.getenv("SWIMMY_MCP_BIND_PORT", "8790")),
        )
```

**Step 4: Run tests to verify they pass**

Run: `python3 tools/test_mcp_gateway_config.py`  
Expected: PASS

**Step 5: Commit**

```bash
git add tools/mcp_gateway_config.py tools/test_mcp_gateway_config.py
git commit -m "feat: add mcp gateway config"
```

---

### Task 3: ZMQ送信アダプタ（エンドポイント差し替え可）

**Files:**
- Create: `tools/mcp_gateway_zmq.py`
- Test: `tools/test_mcp_gateway_zmq.py`

**Step 1: Write the failing test**

```python
# tools/test_mcp_gateway_zmq.py
import zmq
from mcp_gateway_zmq import ZmqPublisher

def main():
    ctx = zmq.Context()
    sub = ctx.socket(zmq.SUB)
    sub.bind("tcp://127.0.0.1:5999")
    sub.setsockopt_string(zmq.SUBSCRIBE, "")

    pub = ZmqPublisher("tcp://127.0.0.1:5999")
    pub.send("(test . ok)")

    msg = sub.recv_string(flags=zmq.NOBLOCK)
    assert "test" in msg

if __name__ == "__main__":
    main()
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_mcp_gateway_zmq.py`  
Expected: FAIL with `ModuleNotFoundError`

**Step 3: Write minimal implementation**

```python
# tools/mcp_gateway_zmq.py
import zmq

class ZmqPublisher:
    def __init__(self, endpoint: str):
        self._ctx = zmq.Context.instance()
        self._socket = self._ctx.socket(zmq.PUB)
        self._socket.connect(endpoint)

    def send(self, payload: str):
        self._socket.send_string(payload)
```

**Step 4: Run tests to verify they pass**

Run: `python3 tools/test_mcp_gateway_zmq.py`  
Expected: PASS

**Step 5: Commit**

```bash
git add tools/mcp_gateway_zmq.py tools/test_mcp_gateway_zmq.py
git commit -m "feat: add mcp gateway zmq publisher"
```

---

### Task 4: 観測リーダ（status/metrics）

**Files:**
- Create: `tools/mcp_gateway_observer.py`
- Test: `tools/test_mcp_gateway_observer.py`

**Step 1: Write the failing test**

```python
# tools/test_mcp_gateway_observer.py
import tempfile
from pathlib import Path
from mcp_gateway_observer import read_status_file

def main():
    with tempfile.TemporaryDirectory() as d:
        p = Path(d) / "swimmy_status"
        p.write_text("TIME: now\nTICKS: 1\n")
        data = read_status_file(str(p))
        assert data["TIME"] == "now"
        assert data["TICKS"] == "1"

if __name__ == "__main__":
    main()
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_mcp_gateway_observer.py`  
Expected: FAIL with `ModuleNotFoundError`

**Step 3: Write minimal implementation**

```python
# tools/mcp_gateway_observer.py
def read_status_file(path: str) -> dict:
    data = {}
    with open(path, "r") as f:
        for line in f:
            if ":" in line:
                k, v = line.split(":", 1)
                data[k.strip()] = v.strip()
    return data
```

**Step 4: Run tests to verify they pass**

Run: `python3 tools/test_mcp_gateway_observer.py`  
Expected: PASS

**Step 5: Commit**

```bash
git add tools/mcp_gateway_observer.py tools/test_mcp_gateway_observer.py
git commit -m "feat: add mcp gateway observer"
```

---

### Task 5: MCPゲートウェイ本体（read-only/backtest-exec）

**Files:**
- Create: `tools/mcp_gateway.py`
- Test: `tools/test_mcp_gateway.py`

**Step 1: Write the failing test**

```python
# tools/test_mcp_gateway.py
from mcp_gateway import handle_trade_submit, handle_backtest_submit

def main():
    res = handle_trade_submit({"symbol": "USDJPY"})
    assert res["status"] == 403

    ok = handle_backtest_submit({"symbol": "USDJPY", "timeframe": 1, "candles_file": "x.csv"})
    assert ok["status"] == 202

if __name__ == "__main__":
    main()
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_mcp_gateway.py`  
Expected: FAIL with `ModuleNotFoundError`

**Step 3: Write minimal implementation**

```python
# tools/mcp_gateway.py
import uuid
from mcp_gateway_zmq import ZmqPublisher
from sexp_serialize import sexp_serialize

_pub = None

def _publisher(endpoint):
    global _pub
    if _pub is None:
        _pub = ZmqPublisher(endpoint)
    return _pub

def handle_trade_submit(_payload):
    return {"status": 403, "error": "trade-capable disabled"}

def handle_backtest_submit(payload, endpoint="tcp://localhost:5559"):
    request_id = str(uuid.uuid4())
    sexp = sexp_serialize({"action": "BACKTEST", "request_id": request_id, **payload})
    _publisher(endpoint).send(sexp)
    return {"status": 202, "request_id": request_id}
```

**Step 4: Run tests to verify they pass**

Run: `python3 tools/test_mcp_gateway.py`  
Expected: PASS

**Step 5: Commit**

```bash
git add tools/mcp_gateway.py tools/test_mcp_gateway.py
git commit -m "feat: add mcp gateway core handlers"
```

---

### Task 6: systemdユニット & Runbook

**Files:**
- Create: `systemd/swimmy-mcp-gateway.service`
- Modify: `doc/runbook.md`

**Step 1: Write failing doc check (manual)**

Run: `rg -n "mcp-gateway" doc/runbook.md`  
Expected: no matches

**Step 2: Add systemd unit**

```ini
# systemd/swimmy-mcp-gateway.service
[Unit]
Description=Swimmy MCP Gateway
After=network.target

[Service]
Type=simple
User=swimmy
WorkingDirectory=/home/swimmy/swimmy
EnvironmentFile=/home/swimmy/swimmy/.env
ExecStart=/usr/bin/python3 /home/swimmy/swimmy/tools/mcp_gateway.py
Restart=always

[Install]
WantedBy=multi-user.target
```

**Step 3: Update runbook**

```markdown
## MCP Gateway
- 起動: `sudo systemctl start swimmy-mcp-gateway`
- 停止: `sudo systemctl stop swimmy-mcp-gateway`
- 状態: `sudo systemctl status swimmy-mcp-gateway`
```

**Step 4: Commit**

```bash
git add systemd/swimmy-mcp-gateway.service doc/runbook.md
git commit -m "docs: add mcp gateway service/runbook"
```

---

### Task 7: ドキュメント整備（INTERFACES / STATE）

**Files:**
- Modify: `docs/llm/INTERFACES.md`
- Modify: `docs/llm/STATE.md`

**Step 1: Update INTERFACES**

```markdown
## MCP Tools (Gateway)
- health.ping (read-only)
- system.status / system.metrics (read-only)
- backtest.submit / backtest.status (backtest-exec)
- trade.submit (trade-capable; disabled)
```

**Step 2: Update STATE**

```markdown
- **MCP Gateway**: read-only/backtest-execのみ有効。trade-capableは封印（403固定）。
```

**Step 3: Commit**

```bash
git add docs/llm/INTERFACES.md docs/llm/STATE.md
git commit -m "docs: document mcp gateway permissions"
```
