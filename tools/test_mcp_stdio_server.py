import io
import json
import tempfile
from pathlib import Path

from mcp_stdio_server import (
    read_jsonrpc_message,
    write_jsonrpc_message,
    validate_jsonrpc_request,
    handle_jsonrpc_request,
    load_system_status,
    load_system_metrics,
    build_log_entry,
)


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


if __name__ == "__main__":
    main()
