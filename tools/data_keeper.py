#!/usr/bin/env python3
"""
data_keeper.py - Persistent Historical Data Service (Article 5 Compliant)
========================================================================
Expert Panel Approved (V2: 2026-01-09)

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
    ADD_CANDLE:USDJPY:JSON   -> Adds a new candle to USDJPY history
    STATUS                   -> Returns service status
    SAVE_ALL                 -> Save data to CSV immediately

Author: Swimmy Team (Article 5 Compliant)
"""

import zmq
import json
import time
import os
import sys
import fcntl
import requests
from datetime import datetime
from collections import defaultdict, deque

# === REQUIRED CONSTANTS (Article 5) ===
MAX_CONSECUTIVE_FAILURES = 5


def load_apex_webhook():
    """Load apex webhook URL from Environment Variable."""
    # 1. Environment Variable
    return os.getenv("SWIMMY_DISCORD_APEX", "")


APEX_WEBHOOK = load_apex_webhook()

# Configuration
ZMQ_PORT = 5561
MAX_CANDLES_PER_SYMBOL = 50000
SUPPORTED_SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]
TIMEOUT_SEC = 5

# In-memory storage: history[symbol][timeframe] = deque
candle_histories = defaultdict(
    lambda: defaultdict(lambda: deque(maxlen=MAX_CANDLES_PER_SYMBOL))
)


def send_discord_alert(message: str, is_error: bool = True):
    """Article 5: Send alert to Discord."""
    try:
        color = 15158332 if is_error else 3066993
        payload = {
            "embeds": [
                {"title": "🐙 Data Keeper", "description": message, "color": color}
            ]
        }
        requests.post(APEX_WEBHOOK, json=payload, timeout=5)
    except Exception as e:
        print(f"[ALERT FAILED] {e}")


def load_historical_data():
    """Load historical data from CSV files for all timeframes."""
    data_dir = os.path.join(os.path.dirname(__file__), "..", "data", "historical")
    timeframes = ["M1", "M5", "M15", "M30", "H1", "H4", "D1", "W1", "MN"]

    for symbol in SUPPORTED_SYMBOLS:
        for tf in timeframes:
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
                        first_line = f.readline()
                        if not first_line:
                            continue

                        if not (
                            "timestamp" in first_line.lower()
                            or "time" in first_line.lower()
                        ):
                            f.seek(0)

                        for line in f:
                            parts = line.strip().split(",")
                            if len(parts) >= 6:
                                try:
                                    candle = {
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
            elif tf == "M1":
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
            filename = f"{symbol}_{tf}.csv"
            filepath = os.path.join(data_dir, filename)
            try:
                sorted_candles = sorted(list(candles), key=lambda c: c["timestamp"])
                with open(filepath, "w") as f:
                    f.write("timestamp,open,high,low,close,volume\n")
                    for c in sorted_candles:
                        line = f"{c['timestamp']},{c['open']},{c['high']},{c['low']},{c['close']},{c['volume']}\n"
                        f.write(line)
                saved_count += 1
            except Exception as e:
                print(f"[DATA-KEEPER] Error saving {filename}: {e}")
    return saved_count


def handle_save_all():
    count = save_historical_data()
    return {"status": "ok", "files_saved": count}


def handle_get_history(parts):
    if len(parts) < 3:
        return {"error": "Usage: GET_HISTORY:SYMBOL:[TIMEFRAME:]COUNT"}

    symbol = parts[1].upper()
    if len(parts) == 3:
        timeframe = "M1"
        try:
            count = int(parts[2])
        except ValueError:
            return {"error": "Invalid count"}
    else:
        timeframe = parts[2].upper()
        try:
            count = int(parts[3])
        except ValueError:
            return {"error": "Invalid count"}

    if symbol not in SUPPORTED_SYMBOLS:
        return {"error": f"Unsupported symbol: {symbol}"}

    if timeframe in candle_histories[symbol]:
        history = list(candle_histories[symbol][timeframe])
        result = history[-count:] if count < len(history) else history
        result.reverse()
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
            "error": "No data",
        }


def handle_add_candle(parts):
    if len(parts) < 3:
        return {"error": "Usage: ADD_CANDLE:SYMBOL:[TIMEFRAME:]JSON"}
    symbol = parts[1].upper()
    remainder = parts[2]
    timeframe = "M1"
    json_str = ""

    if ":" in remainder:
        subparts = remainder.split(":", 1)
        potential_tf = subparts[0].upper()
        if potential_tf in ["M1", "M5", "M15", "M30", "H1", "H4", "D1", "W1", "MN"]:
            timeframe = potential_tf
            json_str = subparts[1]
        else:
            json_str = remainder
    else:
        json_str = remainder

    try:
        candle = json.loads(json_str)
        candle_histories[symbol][timeframe].append(candle)
        return {"status": "ok", "symbol": symbol, "timeframe": timeframe}
    except Exception as e:
        return {"error": f"Error adding candle: {e}"}


def run_server():
    """Main server loop with proper setup."""
    print("🐙 Swimmy Data Keeper Service (Multi-Timeframe + Persistence)")
    print("=============================================================")

    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind(f"tcp://*:{ZMQ_PORT}")

    # Load data ONCE at startup
    load_historical_data()

    print(f"[DATA-KEEPER] Listening on port {ZMQ_PORT}...")

    last_save_time = time.time()

    while True:
        # Check for message with timeout to allow periodic tasks
        if socket.poll(timeout=1000):
            message = socket.recv_string()
            parts = message.split(":", 2)
            command = parts[0].upper()
            response = {}

            if command == "GET_HISTORY":
                response = handle_get_history(parts)
            elif command == "ADD_CANDLE":
                response = handle_add_candle(parts)
            elif command == "SAVE_ALL":
                response = handle_save_all()
            elif command == "STATUS":
                stats = {
                    sym: {
                        tf: len(candle_histories[sym][tf])
                        for tf in candle_histories[sym]
                    }
                    for sym in SUPPORTED_SYMBOLS
                }
                response = {"status": "running", "symbols": stats}
            else:
                response = {"error": f"Unknown command: {command}"}

            socket.send_string(json.dumps(response))
        else:
            # Auto-save every hour
            if time.time() - last_save_time > 3600:
                save_historical_data()
                last_save_time = time.time()


def main():
    """Article 5 Compliant Main Loop."""
    # Singleton Check
    lock_file = open("/tmp/swimmy_data_keeper.lock", "w")
    try:
        fcntl.lockf(lock_file, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except IOError:
        print("[DATA-KEEPER] Another instance is already running. Exiting.")
        sys.exit(0)

    send_discord_alert("✅ Data Keeper Service Started", is_error=False)

    consecutive_failures = 0
    alert_sent = False

    while True:
        try:
            run_server()

            # If run_server returns normally (which it shouldn't unless interrupted), we exit
            break

        except KeyboardInterrupt:
            print("\n[DATA-KEEPER] Shutting down...")
            send_discord_alert("🛑 Data Keeper Stopped", is_error=False)
            break
        except zmq.error.ZMQError as e:
            if "Address already in use" in str(e):
                print(f"[FATAL] Port {ZMQ_PORT} in use. Waiting 5s...")
                time.sleep(5)
                consecutive_failures += 1
            else:
                print(f"[ERROR] ZMQ Error: {e}")
                consecutive_failures += 1
        except Exception as e:
            consecutive_failures += 1
            print(f"❌ Error: {e}")

            if consecutive_failures >= MAX_CONSECUTIVE_FAILURES and not alert_sent:
                send_discord_alert(f"🚨 Data Keeper CRASHED: {e}")
                alert_sent = True

            time.sleep(5)  # Backoff

        # Recovery detection: if we had failures but recovered
        if consecutive_failures > 0 and alert_sent:
            send_discord_alert("✅ Data Keeper Recovered", is_error=False)
        consecutive_failures = 0
        alert_sent = False


if __name__ == "__main__":
    main()
