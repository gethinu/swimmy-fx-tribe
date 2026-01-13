import zmq
import json
import time
import sys

# Port 5560 is where Guardian (PUB) sends to MT5 (SUB)
# BUT Guardian BINDS to 5560.
# So MT5 CONNECTS to 5560.
# To inject a message pretending to be Guardian, we cannot bind to 5560 if Guardian is running.
# WE MUST KILL GUARDIAN FIRST? Or we can use a different pattern.
# Actually, Guardian receives on 5559 from Brain.
# We can inject into Brain command stream (5559)?
# No, we want to test MT5 <-> Guardian or MT5 receiving.

# MT5 (Bridge) -> Connects SUB to tcp://IP:5560.
# Guardian -> Binds PUB to tcp://*:5560.

# If we want to test RECEPTION at MT5, we must BE the PUB socket or inject into Guardian.
# We can't bind 5560 because Guardian has it.
# We can send to Guardian's "Brain Command" input (5559)?
# Guardian listens on 5559 (REP? or SUB?).
# Guardian main.rs:
# let sub_from_brain = context.socket(zmq::SUB).unwrap();
# sub_from_brain.bind("tcp://*:5559").expect("Fail to bind Brain Command");
# Wait, Brain sends to 5559 via PUB?
# Brain runner.lisp: (defparameter *cmd-publisher* (setup-publisher "tcp://*:5559")) -> Incorrect.
# Brain usually CONNECTS if Guardian BINDS.
# Let's check Brain runner.lisp.

# If Guardian BINDS 5559 (SUB), Brain must CONNECT 5559 (PUB).
# If we want to send to Guardian, we implement a PUB socket connecting to tcp://localhost:5559?
# No, usually SUB binds? No, usually PUB binds, SUB connects.
# Guardian logs: "Fail to bind Brain Command". So Guardian BINDS 5559 (SUB).
# So Brain CONNECTS (PUB).

# Let's try sending to Guardian first.
# If Guardian receives it, it will forward to MT5 (if valid).


def send_test_order():
    context = zmq.Context()
    # Connect to Guardian's Command Input (Guardian binds 5559)
    # Actually Guardian binds SUB to 5559.
    # So we need a PUB socket connecting to 5559.

    # Wait, if Guardian binds 5559 (SUB), then we connect to 127.0.0.1:5559 (PUB).
    print("Connecting to Guardian Command Input (5559)...")
    socket = context.socket(zmq.PUB)
    try:
        socket.connect("tcp://127.0.0.1:5559")
    except Exception as e:
        print(f"Connection failed: {e}")
        return

    time.sleep(1)  # Allow connection to establish

    # Create a simple BUY command
    cmd = {
        "type": "SYSTEM_COMMAND",  # Or "action": "BUY"?
        # Guardian expects: {"action": "BUY", ...}
    }

    # Let's send a direct trade command that Guardian accepts
    # Guardian logic: parse JSON -> if action=BUY -> Risk Gate -> Send to MT5
    trade_cmd = {
        "action": "BUY",
        "symbol": "USDJPY",
        "lot": 0.01,
        "sl": 100.0,
        "tp": 150.0,
        "magic": 999999,
    }

    print(f"Sending Trade Command: {json.dumps(trade_cmd)}")
    socket.send_string(json.dumps(trade_cmd))

    time.sleep(1)
    print("Sent. Check Guardian logs for 'Brain Command Received' or 'APPROVED'")

    socket.close()
    context.term()


if __name__ == "__main__":
    send_test_order()
