import unittest

from tools import systemd_drift_probe as probe


class TestSystemdDriftProbe(unittest.TestCase):
    def test_parse_listener_pid_from_ss(self) -> None:
        ss = (
            'tcp   LISTEN 0 100 0.0.0.0:5580 0.0.0.0:* '
            'users:(("python3",pid=428932,fd=9))'
        )
        self.assertEqual(428932, probe.parse_listener_pid_from_ss(ss))

    def test_assess_ok_when_service_active_and_pid_matches(self) -> None:
        ok, msg = probe.assess_backtest_drift(
            service_state="active",
            service_main_pid=428932,
            listener_pid=428932,
        )
        self.assertTrue(ok)
        self.assertIn("aligned", msg)

    def test_assess_fail_when_service_inactive_but_listener_exists(self) -> None:
        ok, msg = probe.assess_backtest_drift(
            service_state="inactive",
            service_main_pid=0,
            listener_pid=428932,
        )
        self.assertFalse(ok)
        self.assertIn("inactive", msg)
        self.assertIn("listener", msg)

    def test_assess_fail_when_service_active_without_listener(self) -> None:
        ok, msg = probe.assess_backtest_drift(
            service_state="active",
            service_main_pid=428932,
            listener_pid=None,
        )
        self.assertFalse(ok)
        self.assertIn("no 5580 listener", msg)

    def test_assess_fail_when_active_but_pid_mismatch(self) -> None:
        ok, msg = probe.assess_backtest_drift(
            service_state="active",
            service_main_pid=111111,
            listener_pid=222222,
        )
        self.assertFalse(ok)
        self.assertIn("mismatch", msg)


if __name__ == "__main__":
    unittest.main()
