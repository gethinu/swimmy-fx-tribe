#!/usr/bin/env python3
import importlib.util
import queue
import threading
import unittest
from pathlib import Path

import zmq


MODULE_PATH = Path(__file__).with_name("trigger_status.py")


def load_module():
    spec = importlib.util.spec_from_file_location("trigger_status", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


class TriggerStatusTests(unittest.TestCase):
    def test_defaults_include_three_fx_pairs(self):
        mod = load_module()
        self.assertEqual(mod.DEFAULT_SYMBOLS, ["USDJPY", "EURUSD", "GBPUSD"])

    def test_build_status_now_message(self):
        mod = load_module()
        msg = mod.build_status_now_message("gbpusd", 1.371)
        self.assertEqual(
            msg,
            '((type . "STATUS_NOW") (symbol . "GBPUSD") (bid . 1.371))',
        )

    def test_parse_pair_spec(self):
        mod = load_module()
        pair = mod.parse_pair_spec("eurusd:1.082")
        self.assertEqual(pair, ("EURUSD", 1.082))

    def test_resolve_pairs_requires_bid_input(self):
        mod = load_module()
        with self.assertRaises(ValueError):
            mod.resolve_pairs(None, mod.DEFAULT_SYMBOLS, None)

    def test_resolve_pairs_symbol_bid_mode(self):
        mod = load_module()
        pairs = mod.resolve_pairs(
            None,
            ["USDJPY", "EURUSD", "GBPUSD"],
            ["151.2", "1.082", "1.371"],
        )
        self.assertEqual(
            pairs,
            [("USDJPY", 151.2), ("EURUSD", 1.082), ("GBPUSD", 1.371)],
        )

    def test_send_status_now_pushes_all_messages(self):
        mod = load_module()
        context = zmq.Context()
        receiver = context.socket(zmq.PULL)
        receiver.setsockopt(zmq.RCVTIMEO, 3000)
        port = receiver.bind_to_random_port("tcp://127.0.0.1")
        endpoint = f"tcp://127.0.0.1:{port}"
        out = queue.Queue()
        expected = [
            ('((type . "STATUS_NOW") (symbol . "USDJPY") (bid . 151.2))'),
            ('((type . "STATUS_NOW") (symbol . "EURUSD") (bid . 1.082))'),
            ('((type . "STATUS_NOW") (symbol . "GBPUSD") (bid . 1.371))'),
        ]

        def _collector():
            try:
                for _ in range(3):
                    out.put(receiver.recv_string())
            except Exception as exc:  # pragma: no cover
                out.put(exc)

        thread = threading.Thread(target=_collector, daemon=True)
        thread.start()
        mod.send_status_now(
            endpoint=endpoint,
            pairs=[("USDJPY", 151.2), ("EURUSD", 1.082), ("GBPUSD", 1.371)],
            connect_wait=0.2,
            send_interval=0.0,
        )
        thread.join(timeout=5)
        receiver.close(0)
        context.term()

        received = [out.get_nowait(), out.get_nowait(), out.get_nowait()]
        self.assertEqual(received, expected)


if __name__ == "__main__":
    unittest.main()
