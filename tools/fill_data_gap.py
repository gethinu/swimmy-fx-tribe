#!/usr/bin/env python3
"""
fill_data_gap.py - Auto-fetch missing historical data from MT5
==============================================================
Usage:
    Running on Windows (where MT5 is installed):
    python tools/fill_data_gap.py

    It connects to Data Keeper (on Linux/WSL via localhost) to check last timestamp.
    Then connects to MT5 to fetch missing data.
    Then pushes data to Data Keeper.
    Then triggers SAVE_ALL.
"""

import os
import sys
import time
from datetime import datetime, timedelta
import pytz
import zmq
from pathlib import Path

# Try importing MetaTrader5 (only works on Windows)
try:
    import MetaTrader5 as mt5
except ImportError:
    print(
        "MetaTrader5 package not found. This script must run on Windows with MT5 installed."
    )
    sys.exit(1)

# Configuration
DATA_KEEPER_HOST = (
    "localhost"  # Assuming WSL2 localhost forwarding works (usually does)
)
# If WSL2 is used, localhost on Windows might not map to WSL2 unless ports are forwarded.
# BUT, usually people access Windows FROM WSL via /mnt/c... or network.
# Accessing WSL FROM Windows: localhost works if service bound to 0.0.0.0.
def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default

DATA_KEEPER_PORT = _env_int("SWIMMY_PORT_DATA_KEEPER", 5561)
SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]
TIMEFRAMES = {
    "M1": mt5.TIMEFRAME_M1,
    "M5": mt5.TIMEFRAME_M5,
    "M15": mt5.TIMEFRAME_M15,
    "H1": mt5.TIMEFRAME_H1,
    "H4": mt5.TIMEFRAME_H4,
    "D1": mt5.TIMEFRAME_D1,
    "W1": mt5.TIMEFRAME_W1,
}


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
PYTHON_SRC = BASE_DIR / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from aux_sexp import sexp_request
from sexp_utils import parse_sexp_alist


def connect_to_data_keeper():
    context = zmq.Context()
    socket = context.socket(zmq.REQ)
    socket.connect(f"tcp://{DATA_KEEPER_HOST}:{DATA_KEEPER_PORT}")
    socket.setsockopt(zmq.RCVTIMEO, 5000)  # 5 sec timeout
    return socket, context


def get_last_timestamp(socket, symbol, tf):
    # Sends GET_HISTORY to get the very last candle
    socket.send_string(
        sexp_request(
            {
                "type": "DATA_KEEPER",
                "action": "GET_HISTORY",
                "symbol": symbol,
                "timeframe": tf,
                "count": 1,
            }
        )
    )
    try:
        msg = socket.recv_string()
        data = parse_sexp_alist(msg)
        if "candles" in data and len(data["candles"]) > 0:
            # Candles are Newest-First. Index 0 is newest.
            return data["candles"][0]["timestamp"]
        return 0
    except Exception as e:
        print(f"Error getting last timestamp for {symbol} {tf}: {e}")
        return 0


def fetch_and_push(socket, symbol, tf_name, tf_val, last_ts):
    # Calculate start time (last_ts + 1 min/period)
    # MT5 copy_rates_range takes datetimes.
    # last_ts is unix timestamp (seconds).

    if last_ts == 0:
        print(
            f"[{symbol} {tf_name}] No existing data. Skipping auto-fill (manual load recommended first)."
        )
        return

    start_dt = datetime.fromtimestamp(last_ts, tz=pytz.utc)
    # Add a bit to avoid overlap? Or request overlap and let app handle dups?
    # Data keeper uses deque append. It doesn't check dups by default (simple version).
    # We should avoid pushing duplicates.
    # Start from last_ts + 1 second.
    start_dt = start_dt + timedelta(seconds=1)

    now = datetime.now(pytz.utc)

    if start_dt >= now:
        print(f"[{symbol} {tf_name}] Up to date.")
        return

    print(f"[{symbol} {tf_name}] Gap found! Fetching from {start_dt} to {now}...")

    rates = mt5.copy_rates_range(symbol, tf_val, start_dt, now)

    if rates is None or len(rates) == 0:
        print(f"[{symbol} {tf_name}] No new data found.")
        return

    print(
        f"[{symbol} {tf_name}] Fetched {len(rates)} new bars. Pushing to Data Keeper..."
    )

    count = 0
    for rate in rates:
        # Convert MT5 tuple/struct to dict
        candle = {
            "timestamp": int(rate["time"]),
            "open": float(rate["open"]),
            "high": float(rate["high"]),
            "low": float(rate["low"]),
            "close": float(rate["close"]),
            "volume": int(rate["tick_volume"]),
        }

        cmd = sexp_request(
            {
                "type": "DATA_KEEPER",
                "action": "ADD_CANDLE",
                "symbol": symbol,
                "timeframe": tf_name,
                "candle": candle,
            }
        )

        socket.send_string(cmd)
        resp = socket.recv_string()  # Wait for ack
        count += 1

    print(f"[{symbol} {tf_name}] Pushed {count} candles.")


def main():
    if not mt5.initialize():
        print("MT5 Start failed")
        return

    try:
        socket, ctx = connect_to_data_keeper()
    except Exception as e:
        print(f"Failed to connect to Data Keeper: {e}")
        return

    print("--- Checking for Data Gaps ---")

    for symbol in SYMBOLS:
        for tf_name, tf_val in TIMEFRAMES.items():
            last_ts = get_last_timestamp(socket, symbol, tf_name)
            fetch_and_push(socket, symbol, tf_name, tf_val, last_ts)

    # Trigger Save
    print("--- Triggering Persistence Save ---")
    socket.send_string(sexp_request({"type": "DATA_KEEPER", "action": "SAVE_ALL"}))
    print(socket.recv_string())

    mt5.shutdown()


if __name__ == "__main__":
    main()
