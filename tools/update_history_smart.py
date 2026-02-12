#!/usr/bin/env python3
"""
update_history_smart.py

Smart History Updater for Swimmy System.
1. Stops Guardian (external service dependency).
2. Connects to MT5 Bridge directly.
3. Checks existing CSV for last timestamp.
4. Requests only NEW data (delta).
5. Appends to CSV while preserving timestamp style.
"""

import datetime
import os
import time
from pathlib import Path

import zmq

# Config
SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]
DATA_DIR = Path(__file__).parent.parent / "data" / "historical"
ZMQ_PUB_PORT = 5560  # Publish commands TO MT5
ZMQ_SUB_PORT = 5557  # Subscribe market data FROM MT5

# Timestamp conversion (Unix 1970 <-> Universal 1900)
UNIX_TO_UNIVERSAL = 2208988800


def infer_epoch_kind(raw_ts, now_unix=None):
    """Infer whether numeric timestamp is Unix(1970) or Universal(1900)."""
    if now_unix is None:
        now_unix = int(time.time())
    if raw_ts is None:
        return "unix"
    raw_ts = int(raw_ts)
    as_unix = abs(raw_ts - now_unix)
    as_universal = abs((raw_ts - UNIX_TO_UNIVERSAL) - now_unix)
    return "universal" if as_universal < as_unix else "unix"


def to_unix_timestamp(raw_ts, epoch_kind=None):
    """Normalize timestamp value to Unix seconds."""
    if raw_ts is None:
        return 0
    ts = int(float(raw_ts))
    if epoch_kind is None:
        epoch_kind = infer_epoch_kind(ts)
    return ts - UNIX_TO_UNIVERSAL if epoch_kind == "universal" else ts


def from_unix_timestamp(unix_ts, epoch_kind):
    """Convert Unix seconds to requested storage epoch."""
    if epoch_kind == "universal":
        return int(unix_ts + UNIX_TO_UNIVERSAL)
    return int(unix_ts)


def parse_sexp_bool(value):
    lowered = value.lower()
    if lowered in ("true", "t", "1"):
        return True
    if lowered in ("false", "nil", "0"):
        return False
    return False


def extract_sexp_value(sexp, key):
    search = f"({key} . "
    start = sexp.find(search)
    if start < 0:
        return None
    start += len(search)
    while start < len(sexp) and sexp[start] == " ":
        start += 1
    if start >= len(sexp):
        return None
    if sexp[start] == '"':
        start += 1
        end = sexp.find('"', start)
        if end < 0:
            return None
        return sexp[start:end]
    end = sexp.find(")", start)
    if end < 0:
        return None
    return sexp[start:end].strip()


def extract_sexp_number(sexp, key, default=0):
    raw = extract_sexp_value(sexp, key)
    if raw is None:
        return default
    try:
        if "." in raw:
            return float(raw)
        return int(raw)
    except Exception:
        return default


def extract_sexp_data(sexp):
    data_key = "(data . ("
    start = sexp.find(data_key)
    if start < 0:
        return []
    start += len(data_key)
    end = sexp.rfind("))")
    if end < start:
        end = len(sexp)
    payload = sexp[start:end].strip()
    if not payload:
        return []

    candles = []
    idx = 0
    while idx < len(payload):
        open_idx = payload.find("((", idx)
        if open_idx < 0:
            break
        close_idx = payload.find("))", open_idx)
        if close_idx < 0:
            break
        chunk = payload[open_idx + 2 : close_idx]
        candle = {}
        for pair in chunk.split(") ("):
            cleaned = pair.strip("() ")
            if " . " not in cleaned:
                continue
            key, value = cleaned.split(" . ", 1)
            key = key.strip()
            value = value.strip().strip('"')
            try:
                if "." in value:
                    candle[key] = float(value)
                else:
                    candle[key] = int(value)
            except Exception:
                candle[key] = value
        if candle:
            candles.append(candle)
        idx = close_idx + 2
    return candles


def detect_format(filepath):
    """
    Detect storage format and last timestamp.
    Returns: (fmt, last_ts_unix, numeric_epoch_kind)
      - fmt: "string" or "unix"
      - last_ts_unix: Unix seconds
      - numeric_epoch_kind: "unix" or "universal" (for numeric files)
    """
    if not filepath.exists():
        return "unix", 0, "unix"

    try:
        with open(filepath, "rb") as f:
            f.seek(0, os.SEEK_END)
            size = f.tell()
            f.seek(max(0, size - 32768), os.SEEK_SET)
            lines = f.read().decode(errors="ignore").splitlines()
    except Exception:
        lines = []

    for line in reversed(lines):
        row = line.strip()
        if not row:
            continue
        first_col = row.split(",", 1)[0].strip()
        if first_col.lower() == "timestamp":
            continue

        # String datetime format
        if "." in first_col and ":" in first_col:
            try:
                dt = datetime.datetime.strptime(first_col, "%Y.%m.%d %H:%M:%S")
                ts_unix = int(dt.replace(tzinfo=datetime.timezone.utc).timestamp())
                return "string", ts_unix, "unix"
            except Exception:
                continue

        # Numeric timestamp format
        try:
            ts_raw = int(float(first_col))
            epoch_kind = infer_epoch_kind(ts_raw)
            ts_unix = to_unix_timestamp(ts_raw, epoch_kind)
            return "unix", ts_unix, epoch_kind
        except Exception:
            continue

    return "unix", 0, "unix"


def compute_start_request(last_ts_unix, overlap_sec=3600):
    if last_ts_unix <= 0:
        return 0
    return max(0, int(last_ts_unix) - int(overlap_sec))


def main():
    print("🐙 Swimmy Smart History Updater")
    DATA_DIR.mkdir(parents=True, exist_ok=True)

    context = zmq.Context()

    # Setup sockets (expects Guardian stopped so this script can bind)
    pub_socket = context.socket(zmq.PUB)
    pub_socket.bind(f"tcp://*:{ZMQ_PUB_PORT}")

    sub_socket = context.socket(zmq.SUB)
    sub_socket.bind(f"tcp://*:{ZMQ_SUB_PORT}")
    sub_socket.setsockopt(zmq.SUBSCRIBE, b"")

    print(f"🔌 Ports Bound: PUB {ZMQ_PUB_PORT}, SUB {ZMQ_SUB_PORT}")
    print("⏳ Waiting 10s for MT5 Bridge to reconnect...")
    time.sleep(10)

    for symbol in SYMBOLS:
        csv_file = DATA_DIR / f"{symbol}_M1.csv"
        fmt, last_ts_unix, storage_epoch = detect_format(csv_file)

        print(f"\n📈 Processing {symbol}...")
        print(f"   Using Format: {fmt} ({storage_epoch})")
        if last_ts_unix > 0:
            dt_utc = datetime.datetime.fromtimestamp(
                last_ts_unix, tz=datetime.timezone.utc
            )
            print(f"   Last Timestamp: {last_ts_unix} ({dt_utc.isoformat()})")
        else:
            print("   Last Timestamp: 0")

        start_request = compute_start_request(last_ts_unix)
        req = (
            f'((type . "REQ_HISTORY") (symbol . "{symbol}") (tf . "M1") '
            f'(start . {start_request}) (count . 10000000))'
        )
        pub_socket.send_string(req)
        print(f"   📤 Request Sent: Start > {start_request}")

        timeout_start = time.time()
        received_batches = set()
        total_batches = 1
        new_candles = []

        while len(received_batches) < total_batches:
            if time.time() - timeout_start > 30:
                print("   ❌ Timeout waiting for data.")
                break

            try:
                msg = sub_socket.recv_string(flags=zmq.NOBLOCK)
                if not msg or "HISTORY" not in msg:
                    continue

                msg_type = extract_sexp_value(msg, "type")
                if msg_type != "HISTORY":
                    continue
                if extract_sexp_value(msg, "symbol") != symbol:
                    continue
                if extract_sexp_value(msg, "tf") != "M1":
                    continue

                batch = extract_sexp_number(msg, "batch", 0)
                total_batches = extract_sexp_number(msg, "total", 1)
                received_batches.add(batch)

                candles = extract_sexp_data(msg)
                print(
                    f"   📥 Received Batch {batch + 1}/{total_batches} ({len(candles)} bars)"
                )
                timeout_start = time.time()
                new_candles.extend(candles)

            except zmq.Again:
                time.sleep(0.1)
                continue
            except Exception as e:
                print(f"   ⚠️ Error parsing msg: {e}")

        if new_candles:
            filtered = []
            for candle in new_candles:
                if "t" not in candle:
                    continue
                t_unix = to_unix_timestamp(candle["t"])
                if t_unix <= last_ts_unix:
                    continue
                candle["_t_unix"] = t_unix
                filtered.append(candle)

            filtered.sort(key=lambda c: c["_t_unix"])
            print(f"   💾 Appending {len(filtered)} new bars to {csv_file.name}...")

            with open(csv_file, "a") as f:
                for candle in filtered:
                    if fmt == "string":
                        ts_str = datetime.datetime.fromtimestamp(
                            candle["_t_unix"], tz=datetime.timezone.utc
                        ).strftime("%Y.%m.%d %H:%M:%S")
                        line = (
                            f"{ts_str},{candle['o']:.5f},{candle['h']:.5f},"
                            f"{candle['l']:.5f},{candle['c']:.5f},"
                            f"{int(candle.get('v', 0) or candle.get('tick_volume', 0))},0,0\n"
                        )
                    else:
                        out_ts = from_unix_timestamp(candle["_t_unix"], storage_epoch)
                        line = (
                            f"{out_ts},{candle['o']:.5f},{candle['h']:.5f},"
                            f"{candle['l']:.5f},{candle['c']:.5f},{int(candle.get('v', 0))},0,0\n"
                        )
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
