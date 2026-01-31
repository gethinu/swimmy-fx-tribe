#!/bin/bash
# Send REPORT_STATUS to verify connectivity
echo "Sending REPORT_STATUS to Brain..."

python3 - <<'EOF'
import zmq
import json
import time
import os

try:
    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    port = int(os.getenv("SWIMMY_PORT_SENSORY", "5555"))
    sock.connect(f"tcp://localhost:{port}") 
    
    msg = {
        "type": "SYSTEM_COMMAND",
        "action": "REPORT_STATUS"
    }
    
    sock.send_string(json.dumps(msg))
    print("✅ Sent REPORT_STATUS command.")
    
except Exception as e:
    print(f"❌ Error: {e}")
EOF
