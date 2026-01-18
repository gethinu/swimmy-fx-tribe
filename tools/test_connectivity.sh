#!/bin/bash
# Send REPORT_STATUS to verify connectivity
echo "Sending REPORT_STATUS to Brain..."

python3 - <<EOF
import zmq
import json
import time

try:
    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    sock.connect("tcp://localhost:5555") 
    
    msg = {
        "type": "SYSTEM_COMMAND",
        "action": "REPORT_STATUS"
    }
    
    sock.send_string(json.dumps(msg))
    print("✅ Sent REPORT_STATUS command.")
    
except Exception as e:
    print(f"❌ Error: {e}")
EOF
