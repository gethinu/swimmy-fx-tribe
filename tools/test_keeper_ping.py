import zmq
import json
import time
import os


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default

context = zmq.Context()
socket = context.socket(zmq.REQ)
port = _env_int("SWIMMY_PORT_DATA_KEEPER", 5561)
socket.connect(f"tcp://localhost:{port}")
socket.setsockopt(zmq.RCVTIMEO, 2000)

print("Sending STATUS request to Data Keeper...")
try:
    socket.send_string("STATUS:DUMMY")
    msg = socket.recv_string()
    print("Received:", msg)
except Exception as e:
    print("Error/Timeout:", e)
