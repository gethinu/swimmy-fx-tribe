import zmq
import json
import time
import os
import sys
from pathlib import Path


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

# Test configuration
ZMQ_PORT = _env_int("SWIMMY_PORT_DATA_KEEPER", 5561)
SYMBOL = "USDJPY"
TF = "M5"
TEST_TIMESTAMP = int(time.time()) - (int(time.time()) % 300)  # Current M5 candle
TEST_OPEN = 150.00
TEST_CLOSE = 150.50
BASE_DIR = str(resolve_base_dir())
CSV_FILE = os.path.join(BASE_DIR, "data", "historical", f"{SYMBOL}_{TF}.csv")


def test_persistence():
    print(f"🧪 Testing persistence for {SYMBOL} {TF}...")

    # 1. Connect to Data Keeper
    context = zmq.Context()
    socket = context.socket(zmq.REQ)
    socket.connect(f"tcp://localhost:{ZMQ_PORT}")
    socket.setsockopt(zmq.RCVTIMEO, 2000)

    # 2. Prepare Candle JSON
    candle = {
        "timestamp": TEST_TIMESTAMP,
        "open": TEST_OPEN,
        "high": TEST_CLOSE + 0.1,
        "low": TEST_OPEN - 0.1,
        "close": TEST_CLOSE,
        "volume": 9999,
    }

    # 3. Send ADD_CANDLE command
    # Format matches data_keeper.py handle_add_candle logic
    # ADD_CANDLE:SYMBOL:[TF:]JSON
    cmd = f"ADD_CANDLE:{SYMBOL}:{TF}:{json.dumps(candle)}"
    print(f"📤 Sending: {cmd[:50]}...")
    socket.send_string(cmd)

    try:
        reply = socket.recv_string()
        print(f"📥 Received: {reply}")
    except zmq.error.Again:
        print("❌ No response from Data Keeper")
        sys.exit(1)

    # 4. Trigger Save (Data Keeper saves every hour, but has SAVE_ALL command)
    print("💾 Triggering SAVE_ALL...")
    socket.send_string("SAVE_ALL")
    try:
        reply = socket.recv_string()
        print(f"📥 Received: {reply}")
    except:
        pass

    # 5. Verify CSV
    print(f"📂 Checking {CSV_FILE}...")
    if not os.path.exists(CSV_FILE):
        print(f"❌ File {CSV_FILE} not found")
        sys.exit(1)

    found = False
    with open(CSV_FILE, "r") as f:
        for line in f:
            if str(TEST_TIMESTAMP) in line and str(9999) in line:  # Check TS and Volume
                print(f"✅ Found candle in CSV: {line.strip()}")
                found = True
                break

    if found:
        print("🎉 Persistence Logic Verified!")
        sys.exit(0)
    else:
        print("❌ Test candle NOT found in CSV")
        sys.exit(1)


if __name__ == "__main__":
    test_persistence()
