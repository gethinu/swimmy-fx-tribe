#!/usr/bin/env python3
"""
update_history_smart.py

Smart History Updater for Swimmy System.
1. Stops Guardian (external service dependency).
2. Connecting to MT5 Bridge directly.
3. Checks existing CSV for last timestamp.
4. Requests only NEW data (Delta).
5. Appends to CSV maintaining format.

Usage:
    python3 tools/update_history_smart.py
"""

import os
import sys
import zmq
import json
import time
import datetime
import traceback
from pathlib import Path

# Config
SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]
DATA_DIR = Path(__file__).parent.parent / "data" / "historical"
ZMQ_PUB_PORT = 5560  # We publish commands TO MT5
ZMQ_SUB_PORT = 5557  # We subscribe market data FROM MT5
# Note: Guardian binds these. We must bind them since Guardian is stopped.
# Actually, Guardian binds 5557 (SUB) and 5560 (PUB).
# Wait, Guardian Binds 5557 (SUB) to receive FROM MT5?
# Yes: sub_market.bind(addr_market)
# So we must BIND 5557 (SUB).
# And Guardian Binds 5560 (PUB) to send TO MT5?
# Yes: pub_to_mt5.bind("tcp://*:5560")
# So we must BIND 5560 (PUB).
# MT5 Connects to both.


def detect_format(filepath):
    """Detect if file uses Unix Timestamp or String Date."""
    if not filepath.exists():
        return "unix", 0

    last_line = ""
    with open(filepath, "rb") as f:
        try:
            f.seek(-200, os.SEEK_END)
        except:
            pass  # File too small
        lines = f.readlines()
        if lines:
            last_line = lines[-1].decode().strip()

    if not last_line:
        return "unix", 0

    parts = last_line.split(",")
    first_col = parts[0]

    # Check if dot/colon exists (String Date)
    if "." in first_col and ":" in first_col:
        # 2006.01.02 00:00:00
        try:
            dt = datetime.datetime.strptime(first_col, "%Y.%m.%d %H:%M:%S")
            return "string", int(dt.replace(tzinfo=datetime.timezone.utc).timestamp())
        except:
            # Maybe standard format?
            pass

    # Try Unix
    try:
        ts = int(float(first_col))
        return "unix", ts
    except:
        pass

    return "unknown", 0


def main():
    print("🐙 Swimmy Smart History Updater")
    DATA_DIR.mkdir(parents=True, exist_ok=True)

    context = zmq.Context()

    # 1. Setup Sockets (Mimic Guardian)
    # PUB to send commands (MT5 connects to this)
    pub_socket = context.socket(zmq.PUB)
    pub_socket.bind(f"tcp://*:{ZMQ_PUB_PORT}")

    # SUB to receive data (MT5 connects to this)
    sub_socket = context.socket(zmq.SUB)
    sub_socket.bind(f"tcp://*:{ZMQ_SUB_PORT}")
    sub_socket.setsockopt(zmq.SUBSCRIBE, b"")

    print(f"🔌 Ports Bound: PUB {ZMQ_PUB_PORT}, SUB {ZMQ_SUB_PORT}")
    print("⏳ Waiting 10s for MT5 Bridge to reconnect...")
    time.sleep(10)  # Give MT5 Bridge time to reconnect to our new sockets

    for symbol in SYMBOLS:
        csv_file = DATA_DIR / f"{symbol}_M1.csv"
        fmt, last_ts = detect_format(csv_file)

        print(f"\n📈 Processing {symbol}...")
        print(f"   Using Format: {fmt}")
        print(
            f"   Last Timestamp: {last_ts} ({datetime.datetime.fromtimestamp(last_ts)})"
        )

        # Request from LastTS - Buffer (e.g. 1 hour overlap to be safe)
        start_request = last_ts
        if start_request > 0:
            start_request -= 3600

        # Send Request
        # Note: SwimmyBridge.mq5 expects "start" as datetime (seconds)
        # and "count" as int. If count is big, it relies on start.
        req = {
            "type": "REQ_HISTORY",
            "symbol": symbol,
            "tf": "M1",
            "start": start_request,
            "count": 10000000,  # Max count, but start takes precedence in MQ5 logic check
        }
        pub_socket.send_string(json.dumps(req))
        print(f"   📤 Request Sent: Start > {start_request}")

        # Receive Loop
        # We expect {"type":"HISTORY", "batch":N, "total":M, "data":[...]}
        timeout_start = time.time()
        received_batches = set()
        total_batches = 1  # Assume at least 1

        new_candles = []

        while len(received_batches) < total_batches:
            if time.time() - timeout_start > 30:
                print("   ❌ Timeout waiting for data.")
                break

            try:
                msg = sub_socket.recv_string(flags=zmq.NOBLOCK)
                if not msg:
                    continue

                # Check message type
                if "HISTORY" not in msg:
                    continue

                data = json.loads(msg)

                if data.get("type") != "HISTORY":
                    continue
                if data.get("symbol") != symbol:
                    continue
                if data.get("tf") != "M1":
                    continue

                # Process Data
                batch = data.get("batch", 0)
                total = data.get("total", 1)
                total_batches = total
                received_batches.add(batch)

                print(
                    f"   📥 Received Batch {batch+1}/{total_batches} ({len(data['data'])} bars)"
                )
                timeout_start = time.time()  # Reset timeout on activity

                new_candles.extend(data["data"])

            except zmq.Again:
                time.sleep(0.1)
                continue
            except Exception as e:
                print(f"   ⚠️ Error parsing msg: {e}")

        # Filter and Append
        if new_candles:
            # Sort by time
            new_candles.sort(key=lambda x: x["t"])

            # Filter duplicates > last_ts
            filtered = [c for c in new_candles if c["t"] > last_ts]

            print(f"   💾 Appending {len(filtered)} new bars to {csv_file.name}...")

            with open(csv_file, "a") as f:
                for c in filtered:
                    # Write format matching detected type
                    # MQ5 sends: t (unix), o, h, l, c, v (tick?), s, rv
                    # CSV Manual: YYYY.MM.DD HH:MM:SS,O,H,L,C,TV,S,RV
                    # CSV Script: unix,O,H,L,C,TV,S,RV

                    if fmt == "string":
                        # Convert to YYYY.MM.DD HH:MM:SS
                        ts_str = datetime.datetime.fromtimestamp(
                            c["t"], tz=datetime.timezone.utc
                        ).strftime("%Y.%m.%d %H:%M:%S")
                        line = f"{ts_str},{c['o']:.5f},{c['h']:.5f},{c['l']:.5f},{c['c']:.5f},{int(c.get('v',0) or c.get('tick_volume',0))},0,0\n"
                        f.write(line)
                    else:  # unix
                        line = f"{c['t']},{c['o']:.5f},{c['h']:.5f},{c['l']:.5f},{c['c']:.5f},{int(c.get('v',0))},0,0\n"
                        f.write(line)

            print("   ✅ Append Complete.")
        else:
            print("   ⚠️ No new data received.")

    print("\n✅ All updates completed.")
    pub_socket.close()
    sub_socket.close()
    context.term()


if __name__ == "__main__":
    main()
