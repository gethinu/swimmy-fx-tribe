from mcp_gateway import handle_trade_submit, handle_backtest_submit


def main():
    res = handle_trade_submit({"symbol": "USDJPY"})
    assert res["status"] == 403

    ok = handle_backtest_submit({"symbol": "USDJPY", "timeframe": 1, "candles_file": "x.csv", "request_id": "RID-1"})
    assert ok["status"] == 202
    assert ok["request_id"] == "RID-1"


if __name__ == "__main__":
    main()
