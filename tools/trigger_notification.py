import zmq
import json
import time


def trigger():
    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    # Brain listens on 5555 via PULL
    sock.connect("tcp://localhost:5555")

    # 1. Trigger QUAL Batch (Targeting the Incubator results)
    payload_qual = {"type": "SYSTEM_COMMAND", "action": "BACKTEST_SUMMARY_QUAL"}

    sock.send_string(json.dumps(payload_qual))
    print("Sent BACKTEST_SUMMARY_QUAL command.")


if __name__ == "__main__":
    trigger()
