import zmq
import json
import time

# Configuration
BACKTEST_PORT = 5580
BRAIN_PORT = 5555
CSV_FILE = "/home/swimmy/swimmy/data/historical/USDJPY_M1.csv"


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
    sender.send_json(request)

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
                result = json.loads(msg)
                msg_type = result.get("type", "UNKNOWN")

                if msg_type == "BACKTEST_RESULT":
                    elapsed = time.time() - start_time
                    print(f"✅ BACKTEST RESULT Received in {elapsed:.2f}s!")
                    print(json.dumps(result, indent=2))
                    break
                else:
                    # Ignore ticks etc
                    # print(f"Skipping {msg_type}")
                    pass

            except json.JSONDecodeError:
                print("❌ Invalid JSON Received:", msg)
        else:
            time.sleep(0.01)

    else:
        print("❌ Timeout waiting for BACKTEST_RESULT.")


if __name__ == "__main__":
    main()
