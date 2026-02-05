#!/usr/bin/env python3
"""
trigger_debug_entry.py
======================
Test Tool (2026-01-10)

Sends a DEBUG_ENTRY command to the running Brain via ZMQ.
This triggers a TEST trade (safe-order) and a Discord notification
to verify the end-to-end pipeline (Brain -> Guardian -> MT5 & Brain -> Discord).
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

        target_symbol = sys.argv[1] if len(sys.argv) > 1 else "USDJPY"
        payload = {
            "type": "SYSTEM_COMMAND",
            "action": "DEBUG_ENTRY",
            "symbol": target_symbol,
        }

        print(
            f"🧪 Sending DEBUG_ENTRY ({target_symbol}) to Brain (Port {BRAIN_PORT})..."
        )
        socket.send_string(sexp_request(payload))

        # Give ZMQ a moment to flush
        time.sleep(0.5)
        print("✅ Command sent. Check Discord and MT5 immediately.")

    except Exception as e:
        print(f"❌ Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
