#!/usr/bin/env python3
"""
data_keeper.py - Persistent Historical Data Service
====================================================
Expert Panel Approved (2026-01-07)

Purpose:
    Hold historical candle data persistently, allowing Swimmy to restart
    without losing data. Provides ZMQ REP socket for queries.

Architecture:
    [Broker/MT5] --> [data_keeper.py] --> [ZMQ] --> [Swimmy (Lisp)]
                          |
                    (Redis or Memory)

Usage:
    python3 tools/data_keeper.py

Commands (via ZMQ):
    GET_HISTORY:USDJPY:1000  -> Returns last 1000 M1 candles for USDJPY
    GET_HISTORY:EURUSD:500   -> Returns last 500 M1 candles for EURUSD
    ADD_CANDLE:USDJPY:JSON   -> Adds a new candle to USDJPY history
    STATUS                   -> Returns service status

Author: Antigravity (Expert Panel Approved)
"""

import zmq
import json
import time
import os
from datetime import datetime
from collections import defaultdict, deque

# Configuration
ZMQ_PORT = 5561  # Data Keeper Port (separate from Guardian 5559)
MAX_CANDLES_PER_SYMBOL = 50000  # ~35 days of M1 data (enough for D1 strategies)
SUPPORTED_SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]

# In-memory storage (future: Redis/SQLite)
candle_histories = defaultdict(lambda: deque(maxlen=MAX_CANDLES_PER_SYMBOL))


def load_historical_data():
    """Load historical data from CSV files if available."""
    data_dir = os.path.join(os.path.dirname(__file__), "..", "data", "historical")

    for symbol in SUPPORTED_SYMBOLS:
        csv_path = os.path.join(data_dir, f"{symbol}_M1.csv")
        if os.path.exists(csv_path):
            print(f"[DATA-KEEPER] Loading {csv_path}...")
            count = 0
            try:
                with open(csv_path, "r") as f:
                    # Skip header
                    next(f, None)
                    for line in f:
                        parts = line.strip().split(",")
                        if len(parts) >= 6:
                            candle = {
                                "timestamp": parts[0],
                                "open": float(parts[1]),
                                "high": float(parts[2]),
                                "low": float(parts[3]),
                                "close": float(parts[4]),
                                "volume": int(float(parts[5])) if len(parts) > 5 else 0,
                            }
                            candle_histories[symbol].append(candle)
                            count += 1
                print(f"[DATA-KEEPER] Loaded {count} candles for {symbol}")
            except Exception as e:
                print(f"[DATA-KEEPER] Error loading {csv_path}: {e}")
        else:
            print(f"[DATA-KEEPER] No historical data for {symbol} at {csv_path}")


def handle_get_history(parts):
    """Handle GET_HISTORY:SYMBOL:COUNT request."""
    if len(parts) < 3:
        return {"error": "Usage: GET_HISTORY:SYMBOL:COUNT"}

    symbol = parts[1].upper()
    try:
        count = int(parts[2])
    except ValueError:
        return {"error": "Invalid count"}

    if symbol not in SUPPORTED_SYMBOLS:
        return {"error": f"Unsupported symbol: {symbol}"}

    history = list(candle_histories[symbol])
    # Return newest first (matching Swimmy's *candle-history* format)
    result = history[-count:] if count < len(history) else history
    result.reverse()  # Newest first

    return {"symbol": symbol, "count": len(result), "candles": result}


def handle_add_candle(parts):
    """Handle ADD_CANDLE:SYMBOL:JSON request."""
    if len(parts) < 3:
        return {"error": "Usage: ADD_CANDLE:SYMBOL:{json}"}

    symbol = parts[1].upper()
    try:
        candle = json.loads(parts[2])
    except json.JSONDecodeError as e:
        return {"error": f"Invalid JSON: {e}"}

    candle_histories[symbol].append(candle)
    return {"status": "ok", "symbol": symbol, "total": len(candle_histories[symbol])}


def handle_status():
    """Return service status."""
    status = {
        "service": "data-keeper",
        "uptime": time.time() - start_time,
        "symbols": {},
    }
    for symbol in SUPPORTED_SYMBOLS:
        status["symbols"][symbol] = len(candle_histories[symbol])
    return status


def main():
    global start_time
    start_time = time.time()

    print("=" * 60)
    print("  📚 DATA KEEPER - Persistent Historical Data Service")
    print("  Expert Panel Approved (2026-01-07)")
    print("=" * 60)

    # Load historical data from CSV
    load_historical_data()

    # Setup ZMQ REP socket
    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind(f"tcp://*:{ZMQ_PORT}")
    print(f"[DATA-KEEPER] Listening on port {ZMQ_PORT}")

    print("[DATA-KEEPER] Ready for queries.")
    print(
        "[DATA-KEEPER] Commands: GET_HISTORY:SYMBOL:COUNT, ADD_CANDLE:SYMBOL:JSON, STATUS"
    )

    while True:
        try:
            message = socket.recv_string()
            print(f"[DATA-KEEPER] Received: {message[:50]}...")

            parts = message.split(":", 2)  # Max 3 parts (command:symbol:data)
            command = parts[0].upper()

            if command == "GET_HISTORY":
                response = handle_get_history(parts)
            elif command == "ADD_CANDLE":
                response = handle_add_candle(parts)
            elif command == "STATUS":
                response = handle_status()
            else:
                response = {"error": f"Unknown command: {command}"}

            socket.send_string(json.dumps(response))

        except KeyboardInterrupt:
            print("\n[DATA-KEEPER] Shutting down...")
            break
        except Exception as e:
            print(f"[DATA-KEEPER] Error: {e}")
            try:
                socket.send_string(json.dumps({"error": str(e)}))
            except:
                pass

    socket.close()
    context.term()
    print("[DATA-KEEPER] Goodbye.")


if __name__ == "__main__":
    main()
