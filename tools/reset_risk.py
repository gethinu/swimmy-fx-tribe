import zmq
import json
import time

CONTEXT = zmq.Context()
# Correct: PUSH socket to speak to Brain's PULL socket
SOCKET = CONTEXT.socket(zmq.PUSH)
# Brain binds to *:5555
SOCKET.connect("tcp://127.0.0.1:5555")

print("Connect to Brain (PUSH 5555)...")
time.sleep(1)

command = {"type": "SYSTEM_COMMAND", "action": "RESET_RISK"}

print("Sending RESET_RISK command...")
# No need for burst, PUSH/PULL is reliable queue
SOCKET.send_string(json.dumps(command))
print("✅ Command sent.")
time.sleep(1)
