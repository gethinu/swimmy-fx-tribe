#!/bin/bash
# Test heartbeat notification via running Brain (ZMQ SYSTEM_COMMAND)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SWIMMY_HOME="${SWIMMY_HOME:-$SCRIPT_DIR}"
PORT="${SWIMMY_PORT_SENSORY:-5555}"

cd "$SWIMMY_HOME"

python3 - <<'EOF'
import json
import os
import zmq

port = int(os.getenv("SWIMMY_PORT_SENSORY", "5555"))
ctx = zmq.Context()
sock = ctx.socket(zmq.PUSH)
sock.connect(f"tcp://localhost:{port}")

msg = {"type": "SYSTEM_COMMAND", "action": "HEARTBEAT_NOW"}
sock.send_string(json.dumps(msg))
print(f"✅ HEARTBEAT_NOW sent to Brain on tcp://localhost:{port}")
print("   Discord通知を確認してください（alerts/heartbeat webhook）。")
EOF
