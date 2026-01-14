import zmq
import time


def sniff():
    ctx = zmq.Context()
    sock = ctx.socket(zmq.SUB)
    sock.connect("tcp://127.0.0.1:5556")
    sock.setsockopt_string(zmq.SUBSCRIBE, "")

    print("👃 Sniffer connected to 5556. Waiting for messages...")

    start = time.time()
    while time.time() - start < 10:  # Listen for 10 seconds
        try:
            msg = sock.recv_string(flags=zmq.NOBLOCK)
            print(f"👃 CAUGHT MSG: {msg}")
        except zmq.Again:
            time.sleep(0.1)


if __name__ == "__main__":
    sniff()
