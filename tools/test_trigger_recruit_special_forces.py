#!/usr/bin/env python3
import importlib.util
import queue
import threading
import unittest
from pathlib import Path

import zmq


MODULE_PATH = Path(__file__).with_name("trigger_recruit_special_forces.py")


def load_module():
    spec = importlib.util.spec_from_file_location("trigger_recruit_special_forces", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


class TriggerRecruitSpecialForcesTests(unittest.TestCase):
    def test_build_message(self):
        mod = load_module()
        msg = mod.build_recruit_special_forces_message(
            symbols=["eurusd", "gbpusd"],
            disable_preflight=True,
            bypass_graveyard=True,
            flush_phase1=True,
            flush_limit=7,
            recruit_limit=9,
            founder_keys=["hunted-d1-adx", "hunted-w1-ema"],
        )
        self.assertEqual(
            msg,
            '((type . "RECRUIT_SPECIAL_FORCES") (symbols . ("EURUSD" "GBPUSD")) (founder_keys . ("HUNTED-D1-ADX" "HUNTED-W1-EMA")) (recruit_limit . 9) (disable_preflight . t) (bypass_graveyard . t) (flush_phase1 . t) (flush_limit . 7))',
        )

    def test_send_recruit_special_forces_pushes_one_message(self):
        mod = load_module()
        context = zmq.Context()
        receiver = context.socket(zmq.PULL)
        receiver.setsockopt(zmq.RCVTIMEO, 3000)
        port = receiver.bind_to_random_port("tcp://127.0.0.1")
        endpoint = f"tcp://127.0.0.1:{port}"
        out = queue.Queue()

        def _collector():
            try:
                out.put(receiver.recv_string())
            except Exception as exc:  # pragma: no cover
                out.put(exc)

        thread = threading.Thread(target=_collector, daemon=True)
        thread.start()
        mod.send_recruit_special_forces(
            endpoint=endpoint,
            symbols=["EURUSD", "GBPUSD"],
            disable_preflight=True,
            bypass_graveyard=True,
            flush_phase1=True,
            flush_limit=3,
            recruit_limit=5,
            founder_keys=["hunted-d1-adx"],
            connect_wait=0.2,
        )
        thread.join(timeout=5)
        receiver.close(0)
        context.term()

        received = out.get_nowait()
        self.assertEqual(
            received,
            '((type . "RECRUIT_SPECIAL_FORCES") (symbols . ("EURUSD" "GBPUSD")) (founder_keys . ("HUNTED-D1-ADX")) (recruit_limit . 5) (disable_preflight . t) (bypass_graveyard . t) (flush_phase1 . t) (flush_limit . 3))',
        )


if __name__ == "__main__":
    unittest.main()
