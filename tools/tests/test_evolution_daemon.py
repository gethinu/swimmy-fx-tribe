import io
import unittest

from tools import evolution_daemon as mod


class _FakeProcess:
    def __init__(self, polls):
        self._polls = list(polls)
        self._idx = 0
        self.terminated = False
        self.killed = False

    def poll(self):
        if self._idx >= len(self._polls):
            return None
        v = self._polls[self._idx]
        self._idx += 1
        return v

    def terminate(self):
        self.terminated = True

    def wait(self, timeout=None):
        return 0

    def kill(self):
        self.killed = True


class TestEvolutionDaemon(unittest.TestCase):
    def test_monitor_child_returns_exit_code(self):
        proc = _FakeProcess([7])
        status, code = mod._monitor_child_process(  # type: ignore[attr-defined]
            proc,
            io.StringIO(),
            should_pause_fn=lambda: False,
            sleep_fn=lambda _secs: None,
        )
        self.assertEqual("exited", status)
        self.assertEqual(7, code)
        self.assertFalse(proc.terminated)

    def test_monitor_child_stops_when_school_appears(self):
        proc = _FakeProcess([None, None, None])
        should_pause_calls = {"n": 0}

        def _should_pause():
            should_pause_calls["n"] += 1
            return should_pause_calls["n"] >= 2

        status, code = mod._monitor_child_process(  # type: ignore[attr-defined]
            proc,
            io.StringIO(),
            should_pause_fn=_should_pause,
            sleep_fn=lambda _secs: None,
        )
        self.assertEqual("paused_for_school", status)
        self.assertEqual(0, code)
        self.assertTrue(proc.terminated)


if __name__ == "__main__":
    unittest.main()
