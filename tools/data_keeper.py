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

# In-memory storage: history[symbol][timeframe] = deque
candle_histories = defaultdict(
    lambda: defaultdict(lambda: deque(maxlen=MAX_CANDLES_PER_SYMBOL))
)
TIMEOUT_SEC = 5


def load_historical_data():
    """Load historical data from CSV files for all timeframes."""
    data_dir = os.path.join(os.path.dirname(__file__), "..", "data", "historical")

    # Timeframe suffixes to look for
    timeframes = ["M1", "M5", "M15", "M30", "H1", "H4", "D1", "W1", "MN"]

    for symbol in SUPPORTED_SYMBOLS:
        for tf in timeframes:
            # Try both naming conventions: USDJPY_M1.csv and USDJPY.a_M1.csv
            candidates = [f"{symbol}_{tf}.csv", f"{symbol}.a_{tf}.csv"]

            csv_path = None
            for c in candidates:
                p = os.path.join(data_dir, c)
                if os.path.exists(p):
                    csv_path = p
                    break

            if csv_path:
                print(f"[DATA-KEEPER] Loading {csv_path} ({tf})...")
                count = 0
                try:
                    with open(csv_path, "r") as f:
                        # Skip header checking if first line is actually a header
                        # Some files have headers, some don't. We try to detect.
                        first_line = f.readline()
                        if not first_line:
                            continue

                        # Check if header
                        if (
                            "timestamp" in first_line.lower()
                            or "time" in first_line.lower()
                        ):
                            pass  # Headers consumed
                        else:
                            # Not a header, reset pointer (seek not always safe with iterators, but here ok)
                            f.seek(0)

                        for line in f:
                            parts = line.strip().split(",")
                            if len(parts) >= 6:
                                try:
                                    candle = {
                                        # Handle float timestamp if present
                                        "timestamp": int(float(parts[0])),
                                        "open": float(parts[1]),
                                        "high": float(parts[2]),
                                        "low": float(parts[3]),
                                        "close": float(parts[4]),
                                        "volume": (
                                            int(float(parts[5]))
                                            if len(parts) > 5
                                            else 0
                                        ),
                                    }
                                    candle_histories[symbol][tf].append(candle)
                                    count += 1
                                except ValueError:
                                    continue

                    print(f"[DATA-KEEPER] Loaded {count} candles for {symbol} ({tf})")
                except Exception as e:
                    print(f"[DATA-KEEPER] Error loading {csv_path}: {e}")
            else:
                # M1 is mandatory-ish, others optional
                if tf == "M1":
                    print(f"[DATA-KEEPER] No M1 historical data for {symbol}")


def save_historical_data():
    """Save in-memory data back to CSV files."""
    data_dir = os.path.join(os.path.dirname(__file__), "..", "data", "historical")
    os.makedirs(data_dir, exist_ok=True)

    print("[DATA-KEEPER] 💾 Saving data to disk...")
    saved_count = 0

    for symbol in SUPPORTED_SYMBOLS:
        for tf, candles in candle_histories[symbol].items():
            if not candles:
                continue

            # Use standard naming convention: SYMBOL_TF.csv
            filename = f"{symbol}_{tf}.csv"
            filepath = os.path.join(data_dir, filename)

            try:
                # Sort by timestamp to ensure order
                sorted_candles = sorted(list(candles), key=lambda c: c["timestamp"])

                with open(filepath, "w") as f:
                    # Write header?? standard format implies no header usually,
                    # but our loader checks for it. Let's write a standard header.
                    # "timestamp,open,high,low,close,volume"
                    f.write("timestamp,open,high,low,close,volume\n")

                    for c in sorted_candles:
                        line = f"{c['timestamp']},{c['open']},{c['high']},{c['low']},{c['close']},{c['volume']}\n"
                        f.write(line)

                print(
                    f"[DATA-KEEPER] Saved {len(sorted_candles)} candles to {filename}"
                )
                saved_count += 1
            except Exception as e:
                print(f"[DATA-KEEPER] Error saving {filename}: {e}")

    return saved_count


def handle_save_all():
    """Handle SAVE_ALL request."""
    count = save_historical_data()
    return {"status": "ok", "files_saved": count}


def handle_get_history(parts):
    """
    Handle GET_HISTORY request.
    Format 1: GET_HISTORY:SYMBOL:COUNT (Defaults to M1)
    Format 2: GET_HISTORY:SYMBOL:TIMEFRAME:COUNT
    """
    if len(parts) < 3:
        return {"error": "Usage: GET_HISTORY:SYMBOL:[TIMEFRAME:]COUNT"}

    symbol = parts[1].upper()

    # Parse based on length
    if len(parts) == 3:
        # Legacy: SYMBOL:COUNT -> M1
        timeframe = "M1"
        try:
            count = int(parts[2])
        except ValueError:
            return {"error": "Invalid count"}
    else:
        # New: SYMBOL:TIMEFRAME:COUNT
        timeframe = parts[2].upper()
        try:
            count = int(parts[3])
        except ValueError:
            return {"error": "Invalid count"}

    if symbol not in SUPPORTED_SYMBOLS:
        return {"error": f"Unsupported symbol: {symbol}"}

    # Retrieve data
    if timeframe in candle_histories[symbol]:
        history = list(candle_histories[symbol][timeframe])
        # Return newest first (matching Swimmy's *candle-history* format)
        result = history[-count:] if count < len(history) else history
        result.reverse()  # Newest first
        return {
            "symbol": symbol,
            "timeframe": timeframe,
            "count": len(result),
            "candles": result,
        }
    else:
        return {
            "symbol": symbol,
            "timeframe": timeframe,
            "count": 0,
            "candles": [],
            "error": "No data for timeframe",
        }


def handle_add_candle(parts):
    """
    Handle ADD_CANDLE request.
    Format 1: ADD_CANDLE:SYMBOL:JSON (Legacy -> M1)
    Format 2: ADD_CANDLE:SYMBOL:TIMEFRAME:JSON
    """
    if len(parts) < 3:
        return {"error": "Usage: ADD_CANDLE:SYMBOL:[TIMEFRAME:]JSON"}

    symbol = parts[1].upper()

    # Try parsing based on length/content
    # If 3 parts, it's Legacy or JSON (but JSON has colons... split limit was 2 in main loop)
    # The main loop does `parts = message.split(":", 2)`.
    # So parts[0]=CMD, parts[1]=SYMBOL, parts[2]=REMAINDER.
    # If REMAINDER starts with "M1:" or "H1:" etc... no, JSON starts with "{".
    # Timeframe doesn't start with "{".

    remainder = parts[2]
    timestamp_key = None
    timeframe = "M1"
    json_str = ""

    # Check if remainder has a timeframe prefix like "M5:{"
    # We look for the first colon.
    if ":" in remainder:
        # Potential split: TIMEFRAME:JSON
        subparts = remainder.split(":", 1)
        potential_tf = subparts[0].upper()
        if potential_tf in ["M1", "M5", "M15", "M30", "H1", "H4", "D1", "W1", "MN"]:
            timeframe = potential_tf
            json_str = subparts[1]
        else:
            # Not a recognized timeframe, assume Legacy JSON
            json_str = remainder
    else:
        json_str = remainder

    try:
        candle = json.loads(json_str)
        # Add to history
        if timeframe not in candle_histories[symbol]:
            # Ensure dict exists? defaultdict handles it.
            pass

        candle_histories[symbol][timeframe].append(candle)
        return {"status": "ok", "symbol": symbol, "timeframe": timeframe}
    except Exception as e:
        return {"error": f"Error adding candle: {e}"}


def handle_status():
    """Return service status."""
    status = {
        "service": "data-keeper",
        "symbols": {},
    }
    for symbol in SUPPORTED_SYMBOLS:
        status["symbols"][symbol] = {
            tf: len(candle_histories[symbol][tf]) for tf in candle_histories[symbol]
        }
    return status


def main():
    print("🐙 Swimmy Data Keeper Service (Multi-Timeframe + Persistence)")
    print("=============================================================")

    # Load historical data from CSV
    load_historical_data()

    # Setup ZMQ REP socket
    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind(f"tcp://*:{ZMQ_PORT}")

    print(f"[DATA-KEEPER] Listening on port {ZMQ_PORT}...")
    print("[DATA-KEEPER] Ready for queries.")
    print(
        "[DATA-KEEPER] Commands: GET_HISTORY:SYMBOL:[TIMEFRAME:]COUNT, ADD_CANDLE:SYMBOL:JSON, SAVE_ALL, STATUS"
    )

    last_save_time = time.time()

    while True:
        try:
            # Non-blocking check?? No, ZMQ REP is blocking.
            # We can use poller or just save when command received or use async.
            # For simplicity, we just process commands. Autosave implies we need a timeout or async loop.
            # Let's verify ZMQ timeout.

            # Simple approach: Wait for message. If timeout (doesn't exist in basic REP), we proceed.
            # Actually standard REP blocks. We can add a periodic thread or just rely on manual SAVE_ALL.
            # Let's rely on SAVE_ALL command and OS signals for now to keep it simple.
            # Or use Poller with timeout.

            if socket.poll(timeout=1000):  # Check every 1s
                message = socket.recv_string()
                print(f"[DATA-KEEPER] Received: {message[:50]}...")

                parts = message.split(":", 2)  # Max 3 parts (command:symbol:data)
                command = parts[0].upper()

                response = {}

                if command == "GET_HISTORY":
                    response = handle_get_history(parts)
                elif command == "ADD_CANDLE":
                    response = handle_add_candle(parts)
                elif command == "SAVE_ALL":
                    response = handle_save_all()
                elif command == "STATUS":
                    stats = {}
                    for sym in SUPPORTED_SYMBOLS:
                        stats[sym] = {
                            tf: len(candle_histories[sym][tf])
                            for tf in candle_histories[sym]
                        }
                    response = {"status": "running", "symbols": stats}
                else:
                    response = {"error": f"Unknown command: {command}"}

                socket.send_string(json.dumps(response))
            else:
                # Idle loop - check auto save?
                if time.time() - last_save_time > 3600:  # 1 hour
                    save_historical_data()
                    last_save_time = time.time()

        except KeyboardInterrupt:
            print("\n[DATA-KEEPER] Shutting down...")
            break
        except Exception as e:
            print(f"[DATA-KEEPER] Error: {e}")
            try:
                socket.send_string(json.dumps({"error": str(e)}))
            except:
                pass  # Socket might be broken.

    socket.close()
    context.term()
    print("[DATA-KEEPER] Goodbye.")


if __name__ == "__main__":
    main()
