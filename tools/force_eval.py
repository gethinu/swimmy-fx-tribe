import zmq
import json
import time


def force_eval():
    context = zmq.Context()
    # Connect to Guardian Command Input (5559) -> Forwards to Brain
    print("Connecting to Guardian Command Input (5559)...")
    socket = context.socket(zmq.PUB)
    try:
        socket.connect("tcp://127.0.0.1:5559")
    except Exception as e:
        print(f"Connection failed: {e}")
        return

    time.sleep(1)

    cmd = {"type": "SYSTEM_COMMAND", "action": "DEBUG_FORCE_EVAL", "target": "ALL"}

    print(f"Sending Command: {json.dumps(cmd)}")
    socket.send_string(json.dumps(cmd))

    time.sleep(1)
    print("Sent. Next tick should trigger immediate re-evaluation.")

    socket.close()
    context.term()


if __name__ == "__main__":
    force_eval()
