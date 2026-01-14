import zmq
import json
import time


def trigger_status_report():
    context = zmq.Context()
    socket = context.socket(zmq.PUB)
    # Connect to Brain's SUB port
    socket.connect("tcp://127.0.0.1:5555")

    # Give time for connection to establish
    time.sleep(1)

    print("Requesting STATUS REPORT...")

    command = {"type": "SYSTEM_COMMAND", "action": "REPORT_STATUS"}

    socket.send_string(json.dumps(command))
    print("Command sent. Check Discord.")

    socket.close()
    context.term()


if __name__ == "__main__":
    trigger_status_report()
