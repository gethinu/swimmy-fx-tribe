import zmq
import json
import time


def reset_warriors():
    context = zmq.Context()
    # Connect to Brain Command Input (Brain binds PUB but listens on SUB?)
    # Wait, Brain usually listens on 5558 (SUB) and Publishes on 5559?
    # Let's check Brain runner.lisp or config.
    # In tick-handler, it processes messages.
    # Brain main loop usually polls a SUB socket.

    # Based on previous tools/send_test_order.py, we sent to 5559 which ended up in Guardian.
    # But RESET_WARRIORS is handled in tick-handler.lisp (Brain).
    # Brain listens on *command-subscriber*.

    # runner.lisp:
    # (defparameter *cmd-subscriber* (setup-subscriber "tcp://localhost:5558"))
    # So Brain SUBSCRIBES to 5558.
    # We must PUBLISH to 5558.
    # Wait, if Brain subscribes to localhost:5558, something must BIND 5558.
    # Usually the Guardian or a broker BINDS.
    # If Brain Connects to 5558, then Guardian Binds 5558.
    # Guardian main.rs: pub_to_brain.bind("tcp://*:5558")

    # So to send to Brain, we must be the Guardian? Or we can connect a PUB socket to 5558?
    # No, if Guardian binds 5558 (PUB), Brain connects (SUB).
    # We cannot inject into a PUB-SUB connection from the outside easily unless we are the PUB.
    # But checking send_test_order.py logic:
    # "Connect to Guardian Command Input (5559)"
    # Guardian listens on 5559.

    # Does Guardian forward "SYSTEM_COMMAND" to Brain?
    # Guardian main.rs:
    # if let Some(action) = json.get("action") {
    #    if action == "RESET_WARRIORS" {
    #        // Forward to Brain?
    #    }
    # }

    # If Guardian forwards unknown commands to Brain, we can send to Guardian (5559).
    # Guardian forwards "market data" and "commands".

    # Let's try sending to Guardian (5559) with action="RESET_WARRIORS".
    # If Guardian doesn't know it, does it forward?
    # Logic: "Unknown messages from Brain are forwarded to MT5".
    # Logic: "Messages from Command Input (5559)" -> ?

    print("Sending RESET_WARRIORS to Guardian (5559)...")
    socket = context.socket(zmq.PUB)
    try:
        socket.connect("tcp://127.0.0.1:5559")
    except Exception as e:
        print(f"Connection failed: {e}")
        return

    time.sleep(1)

    cmd = {"type": "SYSTEM_COMMAND", "action": "RESET_WARRIORS"}

    print(f"Sending Command: {json.dumps(cmd)}")
    socket.send_string(json.dumps(cmd))

    time.sleep(1)
    print("Sent.")

    socket.close()
    context.term()


if __name__ == "__main__":
    reset_warriors()
