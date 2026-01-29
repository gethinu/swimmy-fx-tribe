#!/usr/bin/env python3
"""
trigger_alchemy.py (Phase 29)

Triggers the 'Morning Ritual' in simple swimmy-brain.
Sends {"type": "admin_command", "command": "run_morning_ritual"} to Brain (Port 5555).

Usage:
    python3 tools/trigger_alchemy.py
"""

import zmq
import json
import time

BRAIN_PORT = 5555  # Brain PULL Socket


def main():
    print(f"🌅 Triggering Morning Ritual...")

    context = zmq.Context()

    # Brain is PULL, so we use PUSH
    print(f"🔌 Connecting to Brain (PUSH {BRAIN_PORT})...")
    socket = context.socket(zmq.PUSH)
    socket.connect(f"tcp://localhost:{BRAIN_PORT}")

    # Allow connection time
    time.sleep(1)

    cmd = {"type": "admin_command", "command": "run_morning_ritual"}
    json_str = json.dumps(cmd)

    socket.send_string(json_str)
    print(f"   -> Sent: {json_str}")

    print("✅ Command sent. Check Brain logs for execution.")

    # Wait before close
    time.sleep(1)
    socket.close()
    context.term()


if __name__ == "__main__":
    main()
