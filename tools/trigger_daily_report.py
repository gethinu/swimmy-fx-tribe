#!/usr/bin/env python3
"""
Trigger Daily Report Manually
Usage: python3 tools/trigger_daily_report.py
"""
import zmq
import json
import sys


def main():
    context = zmq.Context()
    socket = context.socket(zmq.PUSH)
    socket.connect("tcp://127.0.0.1:5555")

    # Allow connection to establish
    import time

    time.sleep(0.5)

    # Send SYSTEM_COMMAND to trigger daily report
    cmd = json.dumps({"type": "SYSTEM_COMMAND", "action": "DAILY_REPORT"})

    socket.send_string(cmd)
    print("✅ Daily Report command sent!")
    print("   Check #reports channel in Discord.")

    socket.close()
    context.term()


if __name__ == "__main__":
    main()
