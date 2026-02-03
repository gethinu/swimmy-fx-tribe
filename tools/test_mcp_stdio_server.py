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
