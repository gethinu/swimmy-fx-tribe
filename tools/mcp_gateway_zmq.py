import zmq


class ZmqPublisher:
    def __init__(self, endpoint: str):
        self._ctx = zmq.Context.instance()
        self._socket = self._ctx.socket(zmq.PUB)
        self._socket.connect(endpoint)

    def send(self, payload: str):
        self._socket.send_string(payload)
