import zmq
import json

context = zmq.Context()
socket = context.socket(zmq.REQ)
socket.connect("tcp://localhost:5580")

print("Sending PING...")
socket.send_json({"action": "PING"})
print("Waiting for response...")
try:
    msg = socket.recv_json()
    print("Received:", msg)
except Exception as e:
    print("Error:", e)
