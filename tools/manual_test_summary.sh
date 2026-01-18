#!/bin/bash
# Send a manual test notification to trigger Backtest Summary
echo "Test Notification: Sending BACKTEST_SUMMARY to Brain..."

python3 - <<EOF
import zmq
import json
import time

try:
    ctx = zmq.Context()
    sock = ctx.socket(zmq.PUSH)
    sock.connect("tcp://localhost:5555") # Connect to Brain PULL port
    
    # 1. Send BACKTEST_SUMMARY command
    msg = {
        "type": "SYSTEM_COMMAND",
        "action": "BACKTEST_SUMMARY"
    }
    
    print(f"Connecting to port 5555...")
    sock.send_string(json.dumps(msg))
    print("✅ Sent BACKTEST_SUMMARY command.")
    
    # 2. Also send a direct TEST notification to Status channel to be sure
    # Using Notifier Service directly (Port 5562) if running
    # But usually Brain handles notifications. Let's just rely on the command.
    
    time.sleep(1) # Give it a moment to flush

except Exception as e:
    print(f"❌ Error: {e}")

EOF
