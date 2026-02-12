#!/usr/bin/env python3
"""
request_history_zmq.py

Sends "REQ_HISTORY" commands via the Brain motor nerve path (Port 5556).
Guardian subscribes to 5556 and forwards non-trading commands to MT5 (5560).

Usage:
    python3 tools/request_history_zmq.py

Example:
    python3 tools/request_history_zmq.py --symbols USDJPY EURUSD GBPUSD --count 2000
"""

import argparse
import time

import zmq

DEFAULT_ENDPOINT = "tcp://localhost:5556"
DEFAULT_SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]
DEFAULT_TF = "M1"
DEFAULT_COUNT = 2000
DEFAULT_CONNECT_WAIT_SEC = 1.0
DEFAULT_SEND_INTERVAL_SEC = 0.5


def build_req_history_message(symbol, tf, count):
    return (
        f'((type . "REQ_HISTORY") (symbol . "{symbol}") '
        f'(tf . "{tf}") (count . {int(count)}))'
    )


def parse_args():
    parser = argparse.ArgumentParser(description="Send REQ_HISTORY for selected symbols.")
    parser.add_argument(
        "--endpoint",
        default=DEFAULT_ENDPOINT,
        help=f"ZMQ endpoint to publish commands (default: {DEFAULT_ENDPOINT})",
    )
    parser.add_argument(
        "--symbols",
        nargs="+",
        default=DEFAULT_SYMBOLS,
        help="Symbols to request (space separated).",
    )
    parser.add_argument(
        "--tf",
        default=DEFAULT_TF,
        help=f"Timeframe (default: {DEFAULT_TF})",
    )
    parser.add_argument(
        "--count",
        type=int,
        default=DEFAULT_COUNT,
        help=f"Number of bars to request per symbol (default: {DEFAULT_COUNT})",
    )
    parser.add_argument(
        "--connect-wait",
        type=float,
        default=DEFAULT_CONNECT_WAIT_SEC,
        help=f"Seconds to wait after connect (default: {DEFAULT_CONNECT_WAIT_SEC})",
    )
    parser.add_argument(
        "--send-interval",
        type=float,
        default=DEFAULT_SEND_INTERVAL_SEC,
        help=f"Seconds between symbol sends (default: {DEFAULT_SEND_INTERVAL_SEC})",
    )
    return parser.parse_args()


def send_history_requests(endpoint, symbols, tf, count, connect_wait, send_interval):
    context = zmq.Context()
    socket = context.socket(zmq.PUB)
    socket.connect(endpoint)

    try:
        time.sleep(max(0.0, connect_wait))

        print(f"🔌 Connected: {endpoint}")
        print(f"📤 Requesting {count} {tf} bars for {symbols}...")

        for symbol in symbols:
            sexp = build_req_history_message(symbol, tf, count)
            socket.send_string(sexp)
            print(f"   -> Sent {symbol}")
            time.sleep(max(0.0, send_interval))

        print("✅ Requests sent. Check Guardian/Brain logs for HISTORY batches.")
    finally:
        socket.close(0)
        context.term()


def main():
    args = parse_args()
    send_history_requests(
        endpoint=args.endpoint,
        symbols=args.symbols,
        tf=args.tf,
        count=args.count,
        connect_wait=args.connect_wait,
        send_interval=args.send_interval,
    )


if __name__ == "__main__":
    main()
