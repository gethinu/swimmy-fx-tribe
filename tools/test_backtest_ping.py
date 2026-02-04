#!/usr/bin/env python3
"""
S-expression版 Backtest Service 疎通チェック
 - Brain/School が使う PUSH -> PULL (5580) を模倣
 - 結果は Brain の PULL(5581) に流れるため、このスクリプトは送信のみ（受信しない）。
 - 実環境で Brain が動いている場合でも衝突しない（PUSH connect は増えてもOK）。
"""
import os
import zmq


def _env_int(key: str, default: int) -> int:
    try:
        return int(os.getenv(key, default))
    except ValueError:
        return default


def build_sexp(strategy_name: str, symbol: str, timeframe: int = 1) -> str:
    """最小限のS式バックテストリクエストを生成（CSV data_id 使用）。"""
    return (
        f'((action . "BACKTEST") '
        f'(strategy . ((name . "{strategy_name}") (timeframe . {timeframe}) (sma_short . 5) (sma_long . 20) '
        f'(sl . 0.001) (tp . 0.0015) (volume . 0.02))) '
        f'(data_id "{symbol}_M1") '
        f'(candles_file "data/historical/{symbol}_M1.csv") '
        f'(symbol . "{symbol}") '
        f'(timeframe {timeframe}))'
    )


def main():
    port = _env_int("SWIMMY_PORT_BACKTEST_REQ", 5580)
    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    sock.connect(f"tcp://localhost:{port}")

    sexp = build_sexp("Test-Sexpr-Ping", "USDJPY", 1)
    print(f"Sending S-expression BACKTEST to tcp://localhost:{port}")
    print(sexp)
    sock.send_string(sexp)
    print("Sent. 結果はBrainのPULL(5581)側で処理されます。")


if __name__ == "__main__":
    main()
