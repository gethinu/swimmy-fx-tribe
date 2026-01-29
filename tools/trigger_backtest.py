#!/usr/bin/env python3
"""
trigger_backtest.py (Phase 31)

Triggers the 'Test Backtest' in swimmy-brain.
Sends {"type": "run_test_backtest"} to Brain (Port 5555).

Usage:
    python3 tools/trigger_backtest.py
"""

import zmq
import json
import time

BRAIN_PORT = 5555  # Brain PULL Socket


def main():
    print(f"🏟️ Triggering Arena Verification (Backtest)...")

    context = zmq.Context()

    # Brain is PULL, so we use PUSH
    print(f"🔌 Connecting to Brain (PUSH {BRAIN_PORT})...")
    socket = context.socket(zmq.PUSH)
    socket.connect(f"tcp://localhost:{BRAIN_PORT}")

    # Allow connection time
    time.sleep(1)

    # Note: 'admin_command' expects 'command', but we added a top-level case for 'run_test_backtest'?
    # Wait, let's check server.lisp
    # ((string= msg-type "run_test_backtest") ...)
    # So msg-type MUST be "run_test_backtest".

    cmd = {"type": "run_test_backtest"}
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
