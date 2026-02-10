import zmq
import time
import json
import sys


def main():
    context = zmq.Context()
    # Brain binds so we must bind if we replace Brain.
    # Guardian CONNECTS to 5556.
    # So we act as the Brain PUB socket.

    pub = context.socket(zmq.PUB)
    try:
        pub.bind("tcp://*:5556")
        print("Bound to tcp://*:5556")
    except zmq.error.ZMQError as e:
        print(f"Failed to bind: {e}")
        print("Did you stop swimmy-brain? Try: sudo systemctl stop swimmy-brain.service")
        return

    # Wait for subscriber (Guardian) to connect
    print("Waiting for Guardian to connect...")
    time.sleep(2)

    # Payload similar to what Lisp sends
    # school.lisp sends: action, symbol, lot, sl, tp, magic
    payload = {
        "action": "BUY",
        "symbol": "USDJPY",
        "lot": 0.01,
        "sl": 100.00,
        "tp": 105.00,
        "magic": 123456,
    }

    # IMPORTANT: Rust code naively checks msg.contains("\"action\":\"BUY\"")
    # Python json.dumps defaults to spaces: "action": "BUY".
    # Must use separators=(',', ':') to eliminate spaces.
    msg_str = json.dumps(payload, separators=(",", ":"))
    print(f"Sending: {msg_str}")

    for i in range(5):
        pub.send_string(msg_str)
        time.sleep(0.5)

    print("Sent. Checking if Guardian is still alive is up to you.")

    pub.close()
    context.term()


if __name__ == "__main__":
    main()
