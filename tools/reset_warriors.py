#!/usr/bin/env python3
"""
reset_warriors.py
=================
Emergency Tool (2026-01-10)

Sends a RESET_WARRIORS command to the running Brain via ZMQ.
This forces a clearance of the *warrior-allocation* hash table.
Use when the system thinks it has open positions but MT5 is flat.
"""

import zmq
import json
import sys
import time

BRAIN_PORT = 5555


def main():
    try:
        context = zmq.Context()
        socket = context.socket(zmq.PUSH)
        socket.connect(f"tcp://localhost:{BRAIN_PORT}")

        payload = {"type": "SYSTEM_COMMAND", "action": "RESET_WARRIORS"}

        print(f"🧹 Sending RESET_WARRIORS to Brain (Port {BRAIN_PORT})...")
        socket.send_json(payload)

        # Give ZMQ a moment to flush
        time.sleep(0.5)
        print("✅ Command sent. Check Brain logs for confirmation.")

    except Exception as e:
        print(f"❌ Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
