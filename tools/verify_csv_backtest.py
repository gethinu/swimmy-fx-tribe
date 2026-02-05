import json
import os
import sys
import time
from pathlib import Path

import zmq


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent

# Configuration
BACKTEST_PORT = _env_int("SWIMMY_PORT_BACKTEST_REQ", 5580)
BRAIN_PORT = _env_int("SWIMMY_PORT_SENSORY", 5555)
BASE_DIR = str(resolve_base_dir())
CSV_FILE = os.path.join(BASE_DIR, "data", "historical", "USDJPY_M1.csv")

PYTHON_SRC = Path(BASE_DIR) / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from sexp_serialize import sexp_serialize
from sexp_utils import parse_sexp_alist


def main():
    context = zmq.Context()

    # 1. Be the "Brain" receiver
    receiver = context.socket(zmq.PULL)
    print(f"Binding to tcp://*:{BRAIN_PORT} (Simulating Brain)...")
    try:
        receiver.bind(f"tcp://*:{BRAIN_PORT}")
    except zmq.error.ZMQError:
        print("❌ Port 5555 in use! Stop 'swimmy-brain' first.")
        return

    # 2. Sender to Backtest Service
    sender = context.socket(zmq.PUSH)
    print(f"Connecting to tcp://localhost:{BACKTEST_PORT}...")
    sender.connect(f"tcp://localhost:{BACKTEST_PORT}")

    # 3. Construct Request
    strategy = {
        "name": "Direct-CSV-Verification-Strategy",
        "sma_short": 20,
        "sma_long": 50,
        "sl": 0.5,
        "tp": 1.0,
        "volume": 0.1,
        "indicator_type": "sma",
        "filter_enabled": False,
    }

    request = {
        "action": "BACKTEST",
        "strategy": strategy,
        "candles_file": CSV_FILE,
        "timeframe": 1440,
    }

    print(f"🚀 Sending Backtest Request (File: {CSV_FILE})...")
    start_time = time.time()
    sender.send_string(sexp_serialize(request))

    # 4. Wait for Response (Loop to skip TICKS)
    print("⏳ Waiting for BACKTEST_RESULT...")

    # Poll with timeout
    poller = zmq.Poller()
    poller.register(receiver, zmq.POLLIN)

    start_wait = time.time()
    while time.time() - start_wait < 60:  # 60s timeout
        if poller.poll(100):  # 100ms poll
            msg = receiver.recv_string()
            try:
                result = parse_sexp_alist(msg)
                msg_type = result.get("type", "UNKNOWN")

                if msg_type == "BACKTEST_RESULT":
                    elapsed = time.time() - start_time
                    print(f"✅ BACKTEST RESULT Received in {elapsed:.2f}s!")
                    print(json.dumps(result, indent=2, ensure_ascii=False))
                    break
                else:
                    # Ignore ticks etc
                    # print(f"Skipping {msg_type}")
                    pass

            except Exception:
                print("❌ Invalid S-Expression Received:", msg)
        else:
            time.sleep(0.01)

    else:
        print("❌ Timeout waiting for BACKTEST_RESULT.")


if __name__ == "__main__":
    main()
