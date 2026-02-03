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
