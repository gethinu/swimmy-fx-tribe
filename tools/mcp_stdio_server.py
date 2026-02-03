import json

from mcp_gateway import handle_backtest_submit, handle_trade_submit


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
        res = handle_backtest_submit(payload)
        return jsonrpc_result(req["id"], res)
    return jsonrpc_error(req.get("id"), -32601, "Method not found")
