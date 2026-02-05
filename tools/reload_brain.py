#!/usr/bin/env python3
"""
reload_brain.py
===============
Expert Panel Implemented (2026-01-08)

Sends a RELOAD_CONFIG command to the running Brain via ZMQ.
This allows updating configuration (webhooks, thresholds) without restarting the process.
"""

import os
import sys
import time
from pathlib import Path

import zmq

BRAIN_PORT = 5555


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
PYTHON_SRC = BASE_DIR / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from aux_sexp import sexp_request


def main():
    try:
        context = zmq.Context()
        socket = context.socket(zmq.PUSH)
        socket.connect(f"tcp://localhost:{BRAIN_PORT}")

        payload = {"type": "SYSTEM_COMMAND", "action": "RELOAD_CONFIG"}

        print(f"📦 Sending RELOAD_CONFIG to Brain (Port {BRAIN_PORT})...")
        socket.send_string(sexp_request(payload))

        # Give ZMQ a moment to flush
        time.sleep(0.5)
        print("✅ Command sent. Check Brain logs for confirmation.")

    except Exception as e:
        print(f"❌ Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
