import zmq
import json
import argparse


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--action", type=str, default="run_test_backtest")
    args = parser.parse_args()

    context = zmq.Context()
    # Brain's Sensory Input (Afferent Nerve) - PULL socket on 5555
    # Guardian forwards to this, or we can talk directly?
    # Actually Brain's server.lisp binds PULL 5555.
    socket = context.socket(zmq.PUSH)
    socket.connect("tcp://localhost:5555")

    payload = {"type": "admin_command", "action": args.action, "symbol": "USDJPY"}

    print(f"Sending command: {payload}")
    socket.send_string(json.dumps(payload))
    print("Done.")


if __name__ == "__main__":
    main()
