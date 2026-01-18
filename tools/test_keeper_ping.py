import zmq
import json
import time

context = zmq.Context()
socket = context.socket(zmq.REQ)
socket.connect("tcp://localhost:5561")
socket.setsockopt(zmq.RCVTIMEO, 2000)

print("Sending STATUS request to Data Keeper...")
try:
    socket.send_string("STATUS:DUMMY")
    msg = socket.recv_string()
    print("Received:", msg)
except Exception as e:
    print("Error/Timeout:", e)
