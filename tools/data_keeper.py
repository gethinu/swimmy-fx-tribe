import threading
import os
import sys
import time
import zmq
import fcntl
import requests
from datetime import datetime
from pathlib import Path
from collections import defaultdict, deque, OrderedDict


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

from aux_sexp import parse_aux_request, sexp_response


# === REQUIRED CONSTANTS (Article 5) ===
MAX_CONSECUTIVE_FAILURES = 5


def load_apex_webhook():
    """Load apex webhook URL from Environment Variable."""
    # 1. Environment Variable
    webhook = os.getenv("SWIMMY_DISCORD_APEX", "")
    if webhook:
        webhook = webhook.strip('"').strip("'")
    return webhook


APEX_WEBHOOK = load_apex_webhook()

# Configuration
def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default

ZMQ_PORT = _env_int("SWIMMY_PORT_DATA_KEEPER", 5561)
# buffer to 5M candles (Sufficient for ~10 years M1)
# M1 was causing OOM with 10M limit.
MAX_CANDLES_PER_SYMBOL = 500_000
MAX_TICKS_PER_SYMBOL = _env_int("SWIMMY_MAX_TICKS_PER_SYMBOL", 200_000)
SUPPORTED_SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]
TIMEOUT_SEC = 5
CORE_TIMEFRAMES = {"M1", "M5", "M15", "M30", "H1", "H4", "H12", "D1", "W1", "MN"}

MAX_CUSTOM_TF_CACHE = _env_int("SWIMMY_MAX_CUSTOM_TF_CACHE", 32)
MAX_CUSTOM_TF_CANDLES = _env_int("SWIMMY_MAX_CUSTOM_TF_CANDLES", 50_000)

# Custom TF cache: (symbol, tf_minutes) -> {"m1_version": int, "candles": deque(oldest-first)}
custom_tf_cache = OrderedDict()
m1_versions = defaultdict(int)

# In-memory storage: history[symbol][timeframe] = deque
candle_histories = defaultdict(
    lambda: defaultdict(lambda: deque(maxlen=MAX_CANDLES_PER_SYMBOL))
)

# In-memory storage: ticks[symbol] = deque (append-only, newest at right)
tick_histories = defaultdict(lambda: deque(maxlen=MAX_TICKS_PER_SYMBOL))

TICKS_DIR = Path(os.getenv("SWIMMY_TICKS_DIR", str(BASE_DIR / "data" / "ticks")))

# Lock for thread safety during save
save_lock = threading.Lock()


def _coerce_int(value, default=None):
    if value is None:
        return default
    try:
        return int(float(value))
    except (ValueError, TypeError):
        return default


def _coerce_float(value, default=None):
    if value is None:
        return default
    try:
        return float(value)
    except (ValueError, TypeError):
        return default


def _normalize_symbol(value):
    if not value:
        return None
    return str(value).upper()


def _normalize_timeframe(value):
    """Normalize timeframe input to minutes and a canonical label.

    Supported inputs:
    - Core labels: M1/M5/M15/M30/H1/H4/H12/D1/W1/MN (+ aliases MN1/Monthly)
    - Arbitrary minute integers (e.g. 36, 210, 300, 3600)
    - Arbitrary labels like "M36", "H5", "H60", "D2", "W3"
    """
    minutes = _normalize_timeframe_minutes(value)
    if minutes is None:
        return None
    return _timeframe_label_from_minutes(minutes)


def _timeframe_label_from_minutes(minutes: int) -> str:
    m = int(minutes)
    if m <= 1:
        return "M1"
    if m == 43200:
        return "MN"
    if m >= 10080 and m % 10080 == 0:
        return f"W{m // 10080}"
    if m >= 1440 and m % 1440 == 0:
        return f"D{m // 1440}"
    if m >= 60 and m % 60 == 0:
        return f"H{m // 60}"
    return f"M{m}"


def _normalize_timeframe_minutes(value):
    if value is None:
        return 1
    if isinstance(value, bool):
        return None
    if isinstance(value, (int, float)):
        m = _coerce_int(value)
        if not m or m <= 0:
            return None
        return m
    tf = str(value).strip().upper()
    if not tf:
        return 1
    if tf in ("MN1", "MONTHLY"):
        tf = "MN"
    if tf == "MN":
        return 43200
    if tf.isdigit():
        m = int(tf)
        return m if m > 0 else None
    # Label forms: M36, H5, D2, W3
    if len(tf) >= 2 and tf[0] in ("M", "H", "D", "W"):
        n = tf[1:]
        if n.isdigit():
            base = int(n)
            if tf[0] == "M":
                return base
            if tf[0] == "H":
                return base * 60
            if tf[0] == "D":
                return base * 1440
            if tf[0] == "W":
                return base * 10080
    return None


def _normalize_core_timeframe_label(value):
    """Return canonical label for core TFs only, or None for custom TFs."""
    label = _normalize_timeframe(value)
    if not label:
        return None
    if label == "MN1":
        label = "MN"
    if label not in CORE_TIMEFRAMES:
        return None
    return label


def _resample_candles_aligned(candles: list[dict], tf_seconds: int) -> list[dict]:
    """Resample candles (oldest-first) into tf_seconds buckets, aligned to unix bucket start.

    This matches Guardian's resampler semantics to avoid "BT vs execution saw different bars".
    """
    if not candles or tf_seconds <= 60:
        return list(candles)

    resampled: list[dict] = []
    current_bucket_start = None

    open_ = 0.0
    high = float("-inf")
    low = float("inf")
    close = 0.0
    volume = 0
    has_data = False

    for c in candles:
        ts = _coerce_int(c.get("timestamp"))
        if ts is None:
            continue
        bucket_start = (ts // int(tf_seconds)) * int(tf_seconds)

        if current_bucket_start is None or bucket_start != current_bucket_start:
            if has_data:
                resampled.append(
                    {
                        "timestamp": int(current_bucket_start),
                        "open": float(open_),
                        "high": float(high),
                        "low": float(low),
                        "close": float(close),
                        "volume": int(volume),
                    }
                )
            current_bucket_start = bucket_start
            open_ = float(c.get("open", 0.0))
            high = float(c.get("high", open_))
            low = float(c.get("low", open_))
            close = float(c.get("close", open_))
            volume = int(_coerce_int(c.get("volume"), default=0) or 0)
            has_data = True
        else:
            high = max(high, float(c.get("high", high)))
            low = min(low, float(c.get("low", low)))
            close = float(c.get("close", close))
            volume += int(_coerce_int(c.get("volume"), default=0) or 0)

    if has_data:
        resampled.append(
            {
                "timestamp": int(current_bucket_start),
                "open": float(open_),
                "high": float(high),
                "low": float(low),
                "close": float(close),
                "volume": int(volume),
            }
        )

    return resampled


def _get_history_series(symbol: str, tf_minutes: int, count: int) -> list[dict]:
    """Return candles newest-first."""
    tf_minutes = int(tf_minutes)
    label = _timeframe_label_from_minutes(tf_minutes)

    # Fast path: core TFs (stored as-is)
    if label in CORE_TIMEFRAMES:
        history = list(candle_histories[symbol].get(label, []))
        if count < len(history):
            history = history[-count:]
        history.reverse()
        return history

    # Custom TF: resample from M1 with LRU cache + version invalidation.
    m1 = candle_histories[symbol].get("M1")
    if not m1:
        return []

    key = (symbol, tf_minutes)
    version = int(m1_versions.get(symbol, 0))
    cached = custom_tf_cache.get(key)
    if cached and cached.get("m1_version") == version:
        series = list(cached.get("candles", []))
        custom_tf_cache.move_to_end(key)
    else:
        # Build up to MAX_CUSTOM_TF_CANDLES (oldest-first)
        need_m1 = max(0, (MAX_CUSTOM_TF_CANDLES + 2) * tf_minutes)
        base = list(m1)
        if need_m1 and need_m1 < len(base):
            base = base[-need_m1:]
        series = _resample_candles_aligned(base, tf_seconds=tf_minutes * 60)
        if MAX_CUSTOM_TF_CANDLES and len(series) > MAX_CUSTOM_TF_CANDLES:
            series = series[-MAX_CUSTOM_TF_CANDLES:]
        custom_tf_cache[key] = {
            "m1_version": version,
            "candles": deque(series, maxlen=MAX_CUSTOM_TF_CANDLES),
        }
        custom_tf_cache.move_to_end(key)
        while len(custom_tf_cache) > MAX_CUSTOM_TF_CACHE:
            custom_tf_cache.popitem(last=False)

    if count < len(series):
        series = series[-count:]
    series = list(series)
    series.reverse()
    return series


def _normalize_candle(candle):
    if not isinstance(candle, dict):
        return None, "Invalid candle format"

    def pick(*keys):
        for key in keys:
            if key in candle:
                return candle[key]
        return None

    ts = _coerce_int(pick("timestamp", "t"))
    open_ = _coerce_float(pick("open", "o"))
    high = _coerce_float(pick("high", "h"))
    low = _coerce_float(pick("low", "l"))
    close = _coerce_float(pick("close", "c"))
    volume = _coerce_int(pick("volume", "v"), default=0)

    if None in (ts, open_, high, low, close):
        return None, "Missing required candle fields"

    return {
        "timestamp": ts,
        "open": open_,
        "high": high,
        "low": low,
        "close": close,
        "volume": volume,
    }, None


def _normalize_tick(tick):
    if not isinstance(tick, dict):
        return None, "Invalid tick format"

    def pick(*keys):
        for key in keys:
            if key in tick:
                return tick[key]
        return None

    ts = _coerce_int(pick("timestamp", "t"))
    bid = _coerce_float(pick("bid", "b"))
    ask = _coerce_float(pick("ask", "a"))
    volume = _coerce_int(pick("volume", "v"), default=0)

    if None in (ts, bid, ask):
        return None, "Missing required tick fields"
    if ask < bid:
        return None, "Invalid tick fields (ask < bid)"

    return {"timestamp": ts, "bid": bid, "ask": ask, "volume": volume}, None


def _tick_file_path(symbol: str, timestamp_unix: int) -> Path:
    day = datetime.utcfromtimestamp(int(timestamp_unix)).strftime("%Y%m%d")
    return (TICKS_DIR / symbol / f"{day}.csv").resolve()


def _append_tick_to_disk(symbol: str, tick: dict) -> None:
    path = _tick_file_path(symbol, tick["timestamp"])
    path.parent.mkdir(parents=True, exist_ok=True)
    is_new = not path.exists()
    with open(path, "a") as f:
        if is_new:
            f.write("timestamp,bid,ask,volume\n")
        f.write(f"{tick['timestamp']},{tick['bid']},{tick['ask']},{tick['volume']}\n")


def _tail_lines(path: Path, max_lines: int) -> list[str]:
    if max_lines <= 0:
        return []
    try:
        with open(path, "rb") as f:
            f.seek(0, os.SEEK_END)
            size = f.tell()
            block = 8192
            data = bytearray()
            pos = size
            while pos > 0 and data.count(b"\n") <= max_lines + 1:
                read_size = block if pos >= block else pos
                pos -= read_size
                f.seek(pos)
                chunk = f.read(read_size)
                data[:0] = chunk
            lines = data.splitlines()
            # Keep only the tail slice.
            tail = lines[-max_lines:] if len(lines) > max_lines else lines
            return [ln.decode("utf-8", errors="replace") for ln in tail]
    except FileNotFoundError:
        return []


def _read_ticks_from_disk(symbol: str, count: int, start_time: int | None, end_time: int | None) -> list[dict]:
    sym_dir = TICKS_DIR / symbol
    if not sym_dir.exists():
        return []

    files = sorted(sym_dir.glob("*.csv"), reverse=True)
    out: list[dict] = []
    for path in files:
        # Over-read to account for headers and range filtering.
        lines = _tail_lines(path, max_lines=max(200, count * 10))
        for line in reversed(lines):
            line = line.strip()
            if not line or line.startswith("timestamp"):
                continue
            parts = line.split(",")
            if len(parts) < 4:
                continue
            try:
                ts = int(float(parts[0]))
                bid = float(parts[1])
                ask = float(parts[2])
                vol = int(float(parts[3])) if parts[3] else 0
            except ValueError:
                continue

            if end_time is not None and ts > end_time:
                continue
            if start_time is not None and ts < start_time:
                # Older than the requested range; we can stop completely because we are scanning newest->oldest.
                return out

            out.append({"timestamp": ts, "bid": bid, "ask": ask, "volume": vol})
            if len(out) >= count:
                return out

    return out


def _error_response(message: str):
    return sexp_response(
        {"type": "DATA_KEEPER_RESULT", "status": "error", "error": message}
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
    # V9.5 Fix: Re-enable M1 for fast startup (Expert Panel 2026-01-14)
    # M1 CSVs are ~300MB each, but loading avoids slow MT5 backfill loop
    timeframes = ["M1", "M5", "M15", "M30", "H1", "H4", "H12", "D1", "W1", "MN"]

    for symbol in SUPPORTED_SYMBOLS:
        for tf in timeframes:
            candidates = [f"{symbol}_{tf}.csv", f"{symbol}.a_{tf}.csv"]
            if tf == "MN":
                candidates.extend(
                    [
                        f"{symbol}_MN1.csv",
                        f"{symbol}.a_MN1.csv",
                        f"{symbol}_Monthly.csv",
                        f"{symbol}.a_Monthly.csv",
                    ]
                )
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


def save_historical_data_worker():
    """Worker function for async saving."""
    with save_lock:
        save_historical_data_sync()


def save_historical_data_sync():
    """Save in-memory data back to CSV files (Append-Only Safe Mode)."""
    data_dir = os.path.join(os.path.dirname(__file__), "..", "data", "historical")
    os.makedirs(data_dir, exist_ok=True)
    print("[DATA-KEEPER] 💾 Saving data (Append-Only)...")
    saved_count = 0

    for symbol in SUPPORTED_SYMBOLS:
        for tf, candles in candle_histories[symbol].items():
            if not candles:
                continue
            filename = f"{symbol}_{tf}.csv"
            filepath = os.path.join(data_dir, filename)

            try:
                # 1. Determine last timestamp in file
                last_file_ts = 0
                if os.path.exists(filepath):
                    with open(filepath, "r") as f:
                        try:
                            f.seek(0, os.SEEK_END)
                            pos = f.tell()
                            # Safe seek: Ensure we don't go before start
                            seek_back = min(pos, 100)
                            f.seek(pos - seek_back)
                            lines = f.readlines()
                            if lines:
                                last_line = lines[-1].strip()
                                parts = last_line.split(",")
                                if (
                                    len(parts) >= 1
                                    and parts[0].replace(".", "", 1).isdigit()
                                ):
                                    last_file_ts = int(float(parts[0]))
                        except Exception:
                            pass  # If fail, assume 0 (or risk dupes)

                # 2. Filter candles to append
                # Make a shallow copy for thread safety during iteration if needed,
                # though we are just reading.
                candles_snapshot = list(candles)
                new_candles = [
                    c for c in candles_snapshot if c["timestamp"] > last_file_ts
                ]

                if not new_candles:
                    continue

                sorted_new = sorted(new_candles, key=lambda c: c["timestamp"])

                # 3. Append
                mode = "a" if os.path.exists(filepath) else "w"
                with open(filepath, mode) as f:
                    if mode == "w":
                        f.write("timestamp,open,high,low,close,volume\n")

                    for c in sorted_new:
                        line = f"{c['timestamp']},{c['open']},{c['high']},{c['low']},{c['close']},{c['volume']}\n"
                        f.write(line)

                saved_count += 1
                if saved_count % 5 == 0:
                    print(
                        f"[DATA-KEEPER] Appended {len(sorted_new)} candles to {filename}"
                    )

            except Exception as e:
                print(f"[DATA-KEEPER] Error saving {filename}: {e}")

    print("[DATA-KEEPER] ✅ Async Save Complete.")
    return saved_count


def save_historical_data():
    """Wrapper to start save in a thread."""
    threading.Thread(target=save_historical_data_worker).start()
    return 1  # Return dummy count, actual count unknown immediately


def handle_save_all():
    save_historical_data()
    return {"status": "ok", "message": "Saving started in background"}


def get_csv_path(symbol, tf):
    """Deep Validation: Helper to find CSV path."""
    data_dir = os.path.join(os.path.dirname(__file__), "..", "data", "historical")
    candidates = [f"{symbol}_{tf}.csv", f"{symbol}.a_{tf}.csv"]
    if str(tf).upper() in ("MN", "MN1"):
        candidates.extend(
            [
                f"{symbol}_MN1.csv",
                f"{symbol}.a_MN1.csv",
                f"{symbol}_Monthly.csv",
                f"{symbol}.a_Monthly.csv",
            ]
        )
    for c in candidates:
        p = os.path.join(data_dir, c)
        if os.path.exists(p):
            return os.path.abspath(p)
    return None


def handle_request_sexp(message: str) -> str:
    try:
        data = parse_aux_request(message)
    except Exception as e:
        return _error_response(str(e))

    msg_type = str(data.get("type", "")).upper()
    if msg_type != "DATA_KEEPER":
        return _error_response(f"Invalid type: {msg_type}")

    schema_version = _coerce_int(data.get("schema_version"))
    if schema_version != 1:
        return _error_response("Unsupported schema_version")

    action = str(data.get("action", "")).upper()
    if not action:
        return _error_response("Missing action")

    if action == "STATUS":
        symbols = []
        for sym in SUPPORTED_SYMBOLS:
            tf_entries = []
            for tf, candles in candle_histories[sym].items():
                tf_entries.append({"tf": tf, "count": len(candles)})
            if tf_entries:
                symbols.append({"symbol": sym, "timeframes": tf_entries})
        return sexp_response(
            {
                "type": "DATA_KEEPER_RESULT",
                "status": "running",
                "symbols": symbols,
            }
        )

    if action == "GET_HISTORY":
        symbol = _normalize_symbol(data.get("symbol"))
        if not symbol:
            return _error_response("Missing symbol")
        if symbol not in SUPPORTED_SYMBOLS:
            return _error_response(f"Unsupported symbol: {symbol}")
        tf_value = data.get("timeframe")
        if tf_value is None and "timeframe_minutes" in data:
            tf_value = data.get("timeframe_minutes")
        tf_minutes = _normalize_timeframe_minutes(tf_value)
        if not tf_minutes:
            return _error_response("Invalid timeframe")
        timeframe = _timeframe_label_from_minutes(tf_minutes)
        count = _coerce_int(data.get("count"))
        if not count or count <= 0:
            return _error_response("Invalid count")
        history = _get_history_series(symbol, tf_minutes, count)
        return sexp_response(
            {
                "type": "DATA_KEEPER_RESULT",
                "status": "ok",
                "symbol": symbol,
                "timeframe": timeframe,
                "count": len(history),
                "candles": history,
            }
        )

    if action == "GET_FILE_PATH":
        symbol = _normalize_symbol(data.get("symbol"))
        if not symbol:
            return _error_response("Missing symbol")
        tf_value = data.get("timeframe")
        if tf_value is None and "timeframe_minutes" in data:
            tf_value = data.get("timeframe_minutes")
        timeframe = _normalize_core_timeframe_label(tf_value)
        if not timeframe:
            return _error_response("Invalid timeframe")
        path = get_csv_path(symbol, timeframe)
        if not path:
            return _error_response("File not found")
        return sexp_response(
            {"type": "DATA_KEEPER_RESULT", "status": "ok", "path": path}
        )

    if action == "ADD_CANDLE":
        symbol = _normalize_symbol(data.get("symbol"))
        if not symbol:
            return _error_response("Missing symbol")
        if symbol not in SUPPORTED_SYMBOLS:
            return _error_response(f"Unsupported symbol: {symbol}")
        timeframe = _normalize_core_timeframe_label(data.get("timeframe"))
        if not timeframe:
            return _error_response("Invalid timeframe")
        candle, err = _normalize_candle(data.get("candle"))
        if err:
            return _error_response(err)
        with save_lock:
            candle_histories[symbol][timeframe].append(candle)
            if timeframe == "M1":
                m1_versions[symbol] += 1
        return sexp_response(
            {
                "type": "DATA_KEEPER_RESULT",
                "status": "ok",
                "symbol": symbol,
                "timeframe": timeframe,
            }
        )

    if action == "ADD_TICK":
        symbol = _normalize_symbol(data.get("symbol"))
        if not symbol:
            return _error_response("Missing symbol")
        if symbol not in SUPPORTED_SYMBOLS:
            return _error_response(f"Unsupported symbol: {symbol}")
        tick, err = _normalize_tick(data.get("tick"))
        if err:
            return _error_response(err)
        with save_lock:
            tick_histories[symbol].append(tick)
            try:
                _append_tick_to_disk(symbol, tick)
            except Exception as e:
                return _error_response(f"Tick persistence error: {e}")
        return sexp_response(
            {
                "type": "DATA_KEEPER_RESULT",
                "status": "ok",
                "symbol": symbol,
            }
        )

    if action == "GET_TICKS":
        symbol = _normalize_symbol(data.get("symbol"))
        if not symbol:
            return _error_response("Missing symbol")
        if symbol not in SUPPORTED_SYMBOLS:
            return _error_response(f"Unsupported symbol: {symbol}")
        count = _coerce_int(data.get("count"))
        if not count or count <= 0:
            return _error_response("Invalid count")
        start_time = _coerce_int(data.get("start_time"))
        end_time = _coerce_int(data.get("end_time"))

        # Prefer in-memory for newest-only queries.
        ticks: list[dict] = []
        if start_time is None and end_time is None:
            ticks = list(reversed(tick_histories[symbol]))[:count]
        else:
            for t in reversed(tick_histories[symbol]):
                ts = t.get("timestamp")
                if ts is None:
                    continue
                if end_time is not None and ts > end_time:
                    continue
                if start_time is not None and ts < start_time:
                    break
                ticks.append(t)
                if len(ticks) >= count:
                    break

        if len(ticks) < count:
            disk_ticks = _read_ticks_from_disk(symbol, count=count, start_time=start_time, end_time=end_time)
            ticks = disk_ticks[:count]

        return sexp_response(
            {
                "type": "DATA_KEEPER_RESULT",
                "status": "ok",
                "symbol": symbol,
                "count": len(ticks),
                "ticks": ticks,
            }
        )

    if action == "SAVE_ALL":
        response = handle_save_all()
        payload = {"type": "DATA_KEEPER_RESULT"}
        payload.update(response)
        return sexp_response(payload)

    return _error_response(f"Unknown action: {action}")


def run_server():
    """Main server loop with proper setup."""
    print("🐙 Swimmy Data Keeper Service (Multi-Timeframe + Persistence + Async)")
    print("===================================================================")

    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind(f"tcp://*:{ZMQ_PORT}")

    # Load data ONCE at startup
    load_historical_data()

    print(f"[DATA-KEEPER] Listening on port {ZMQ_PORT}...")

    last_save_time = time.time()

    while True:
        try:
            # Check for message with timeout to allow periodic tasks
            if socket.poll(timeout=1000):
                message = socket.recv_string()
                try:
                    response = handle_request_sexp(message)
                except Exception as e:
                    response = _error_response(f"Unhandled error: {e}")
                socket.send_string(response)
            else:
                # Auto-save every hour
                if time.time() - last_save_time > 3600:
                    save_historical_data()
                    last_save_time = time.time()

        except zmq.ZMQError as e:
            print(f"[ZMQ ERROR] {e}")
            # Recreate socket? ZMQ usually recovers.
            # If fatal, main loop catches it.
        except Exception as e:
            print(f"[LOOP ERROR] {e}")
            # Do NOT break main loop, just continue


def main():
    """Article 5 Compliant Main Loop."""
    # Singleton Check
    lock_file = open("/tmp/swimmy_data_keeper.lock", "w")
    try:
        fcntl.lockf(lock_file, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except IOError:
        print("[DATA-KEEPER] Another instance is already running. Exiting.")
        sys.exit(0)

    send_discord_alert("✅ Data Keeper Service Started (Optimized)", is_error=False)

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
