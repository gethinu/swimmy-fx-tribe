#!/usr/bin/env python3
"""
Trigger Backtest Summary Manually
Usage: python3 tools/trigger_backtest_summary.py
"""
import zmq
import json
import time


def main():
    context = zmq.Context()
    socket = context.socket(zmq.PUSH)
    socket.connect("tcp://127.0.0.1:5555")

    time.sleep(0.5)

    cmd = json.dumps({"type": "SYSTEM_COMMAND", "action": "BACKTEST_SUMMARY"})

    socket.send_string(cmd)
    print("✅ Backtest Summary command sent!")
    print("   Check #system-logs channel in Discord.")

    socket.close()
    context.term()


if __name__ == "__main__":
    main()
