#!/usr/bin/env python3
"""
reload_brain.py
===============
Expert Panel Implemented (2026-01-08)

Sends a RELOAD_CONFIG command to the running Brain via ZMQ.
This allows updating configuration (webhooks, thresholds) without restarting the process.
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

        payload = {"type": "SYSTEM_COMMAND", "action": "RELOAD_CONFIG"}

        print(f"📦 Sending RELOAD_CONFIG to Brain (Port {BRAIN_PORT})...")
        socket.send_json(payload)

        # Give ZMQ a moment to flush
        time.sleep(0.5)
        print("✅ Command sent. Check Brain logs for confirmation.")

    except Exception as e:
        print(f"❌ Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
