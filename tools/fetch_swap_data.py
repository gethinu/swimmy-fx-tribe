#!/usr/bin/env python3
"""
fetch_swap_data.py (Phase 29: Live Data)

Connects to the ZMQ Bridge (Guardian Port 5559) and sends `GET_SWAP` commands.
The MT5 Bridge (if updated with V30 code) will respond with `SWAP_DATA` messages.
These messages are received by the Swimmy Brain (listening on 5557/5558) and stored in the Data Lake.

Usage:
    python3 tools/fetch_swap_data.py
"""

import zmq
import json
import time

# List of symbols to fetch swap data for.
# In a real scenario, this might be dynamic or loaded from a config.
SYMBOLS = [
    "USDJPY",
    "EURUSD",
    "GBPUSD",
    "AUDJPY",
    "NZDJPY",
    "CADJPY",
    "CHFJPY",
    "EURJPY",
    "GBPJPY",
    "AUDUSD",
    "NZDUSD",
    "USDCAD",
    "USDCHF",
    "XAUUSD",
]

# Bridge Connection
BRIDGE_REQ_PORT = 5559  # We PUSH/PUB requests here (Guardian)


def main():
    print(f"🌊 Swimmy Data Gatherer (Live Swaps) Starting...")

    context = zmq.Context()

    # Guardian listens on 5559 (SUB), so we use PUB to send commands.
    # Note: request_history_zmq.py uses PUB, so we follow that pattern.
    print(f"🔌 Connecting to Bridge Request Port (PUB {BRIDGE_REQ_PORT})...")
    socket = context.socket(zmq.PUB)
    socket.connect(f"tcp://localhost:{BRIDGE_REQ_PORT}")

    # Allow connection time (ZMQ Slow Joiner)
    time.sleep(1)

    print(f"📨 Sending GET_SWAP commands for {len(SYMBOLS)} symbols...")

    for symbol in SYMBOLS:
        # Construct command matching SwimmyBridge.mq5 V30
        cmd = {"type": "GET_SWAP", "symbol": symbol, "magic": 0}  # System command
        json_str = json.dumps(cmd)

        # Send
        socket.send_string(json_str)
        print(f"   -> Sent GET_SWAP for {symbol}")

        # Throttle slightly to avoid flooding
        time.sleep(0.1)

    print("✅ All requests sent.")
    print("ℹ️  Check 'swimmy.log' or 'brain.log' to confirm SWAP_DATA receipt.")

    # Wait a bit to ensure async delivery before closing
    time.sleep(2)
    socket.close()
    context.term()


if __name__ == "__main__":
    main()
