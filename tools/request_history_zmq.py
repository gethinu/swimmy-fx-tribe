#!/usr/bin/env python3
"""
request_history_zmq.py

Sends a "REQ_HISTORY" command to Guardian (Port 5559) via ZMQ.
Guardian forwards this command to the MT5 Bridge (Port 5560).

Usage:
    python3 tools/request_history_zmq.py

Note: Requires Guardian to be running.
"""

import zmq
import time

SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]
# 10 Million bars ~ 20 years of M1 data
COUNT = 10_000_000


def main():
    context = zmq.Context()
    # Guardian listens on 5559 so we connect
    # Guardian is SUB, so we are PUB
    socket = context.socket(zmq.PUB)
    socket.connect("tcp://localhost:5559")

    # Allow connection time
    time.sleep(1)

    print(f"🔌 Connected to Guardian (PUB 5559)")
    print(f"📤 Requesting {COUNT} M1 bars for {SYMBOLS}...")

    for symbol in SYMBOLS:
        sexp = f'((type . "REQ_HISTORY") (symbol . "{symbol}") (tf . "M1") (count . {COUNT}))'
        socket.send_string(sexp)
        print(f"   -> Sent {symbol}")
        time.sleep(0.5)  # small delay between requests

    print("✅ Requests sent. Check MT5 / Guardian logs.")


if __name__ == "__main__":
    main()
