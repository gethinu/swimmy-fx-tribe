import time
import zmq
from mcp_gateway_zmq import ZmqPublisher


def main():
    ctx = zmq.Context()
    sub = ctx.socket(zmq.SUB)
    sub.bind("tcp://127.0.0.1:5999")
    sub.setsockopt_string(zmq.SUBSCRIBE, "")

    pub = ZmqPublisher("tcp://127.0.0.1:5999")
    time.sleep(0.1)

    poller = zmq.Poller()
    poller.register(sub, zmq.POLLIN)

    for _ in range(5):
        pub.send("(test . ok)")
        events = dict(poller.poll(500))
        if sub in events:
            msg = sub.recv_string()
            assert "test" in msg
            return
        time.sleep(0.05)

    raise AssertionError("No message received from ZMQ publisher")


if __name__ == "__main__":
    main()
