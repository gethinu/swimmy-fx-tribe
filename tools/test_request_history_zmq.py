#!/usr/bin/env python3
import importlib.util
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("request_history_zmq.py")


def load_module():
    spec = importlib.util.spec_from_file_location("request_history_zmq", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


class RequestHistoryZmqTests(unittest.TestCase):
    def test_default_endpoint_is_brain_motor_5556(self):
        mod = load_module()
        self.assertEqual(mod.DEFAULT_ENDPOINT, "tcp://localhost:5556")

    def test_default_symbols_are_three_fx_pairs(self):
        mod = load_module()
        self.assertEqual(mod.DEFAULT_SYMBOLS, ["USDJPY", "EURUSD", "GBPUSD"])

    def test_build_req_history_message(self):
        mod = load_module()
        msg = mod.build_req_history_message("GBPUSD", "M1", 2000)
        self.assertEqual(
            msg,
            '((type . "REQ_HISTORY") (symbol . "GBPUSD") (tf . "M1") (count . 2000))',
        )


if __name__ == "__main__":
    unittest.main()
