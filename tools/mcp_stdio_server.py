import datetime
import json
import os
import sys
import time
from pathlib import Path

from mcp_gateway import handle_backtest_submit, handle_trade_submit
from mcp_gateway_observer import read_status_file


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
        if not payload.get("request_id"):
            payload["request_id"] = str(req["id"])
        res = handle_backtest_submit(payload)
        return jsonrpc_result(req["id"], res)
    if method == "system.status":
        return jsonrpc_result(req["id"], load_system_status())
    if method == "system.metrics":
        return jsonrpc_result(req["id"], load_system_metrics())
    if method == "backtest.status":
        data = load_system_status()["backtest"]
        data["mode"] = "latest"
        return jsonrpc_result(req["id"], data)
    return jsonrpc_error(req.get("id"), -32601, "Method not found")


BASE_DIR = Path(__file__).resolve().parents[1]
PYTHON_SRC = BASE_DIR / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from sexp_utils import load_sexp_alist


def load_system_status(swimmy_status_path="/tmp/swimmy_status", backtest_status_path=None):
    backtest_status_path = backtest_status_path or str(
        BASE_DIR / "data" / "reports" / "backtest_status.txt"
    )
    swimmy = read_status_file(swimmy_status_path) if Path(swimmy_status_path).exists() else {}
    backtest = (
        read_status_file(backtest_status_path) if Path(backtest_status_path).exists() else {}
    )
    return {
        "swimmy": {k.lower(): v for k, v in swimmy.items()},
        "backtest": {k.lower(): v for k, v in backtest.items()},
    }


def load_system_metrics(metrics_path=None):
    metrics_path = metrics_path or str(BASE_DIR / "data" / "system_metrics.sexp")
    if not Path(metrics_path).exists():
        return {}
    return load_sexp_alist(metrics_path)


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
        append_log(
            build_log_entry(
                req.get("id"), req.get("method"), duration_ms, status, res.get("error")
            )
        )
        write_jsonrpc_message(sys.stdout.buffer, res)


if __name__ == "__main__":
    main()
