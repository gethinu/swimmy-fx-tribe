import importlib
import os
import sys
import unittest
from pathlib import Path


class DummyResponse:
    status_code = 204
    text = ""


class DummyRequests:
    def __init__(self):
        self.called = False
        self.last_args = None
        self.last_kwargs = None

    def post(self, *args, **kwargs):
        self.called = True
        self.last_args = args
        self.last_kwargs = kwargs
        return DummyResponse()


def _install_requests_stub(stub):
    sys.modules.pop("requests", None)
    sys.modules["requests"] = stub


class TestSendHeartbeatScript(unittest.TestCase):
    def setUp(self):
        self._orig_env = os.environ.copy()
        self._orig_sys_path = list(sys.path)
        self._dummy_requests = DummyRequests()
        _install_requests_stub(self._dummy_requests)
        sys.modules.pop("send_heartbeat", None)

        repo_root = Path(__file__).resolve().parents[1]
        self._scripts_path = str(repo_root / "scripts")
        if self._scripts_path not in sys.path:
            sys.path.insert(0, self._scripts_path)

    def tearDown(self):
        os.environ.clear()
        os.environ.update(self._orig_env)
        sys.path[:] = self._orig_sys_path
        sys.modules.pop("send_heartbeat", None)
        sys.modules.pop("requests", None)

    def test_test_heartbeat_file_removed(self):
        self.assertFalse(Path("test_heartbeat.py").exists())

    def test_send_heartbeat_uses_env_and_no_import_side_effect(self):
        os.environ["SWIMMY_HEARTBEAT_WEBHOOK"] = "https://example.com/webhook"
        module = importlib.import_module("send_heartbeat")
        self.assertFalse(self._dummy_requests.called)

        module.main()
        self.assertTrue(self._dummy_requests.called)
        self.assertEqual(self._dummy_requests.last_args[0], "https://example.com/webhook")


if __name__ == "__main__":
    unittest.main()
