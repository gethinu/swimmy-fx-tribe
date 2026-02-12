import os
import sys
import unittest
from unittest import mock

sys.path.insert(0, os.path.dirname(__file__))

import dashboard  # noqa: E402


class DashboardServiceStatusTests(unittest.TestCase):
    def test_get_service_status_uses_system_scope(self):
        observed = {}

        def fake_run(args, capture_output=True, text=True):
            observed["args"] = args
            return mock.Mock(stdout="active\n")

        with mock.patch("subprocess.run", side_effect=fake_run):
            status = dashboard.get_service_status("swimmy-brain")
            self.assertIn("Active", status)
        self.assertEqual(observed["args"][:2], ["systemctl", "is-active"])
        self.assertNotIn("--user", observed["args"])


class DashboardBacktestDriftTests(unittest.TestCase):
    def test_get_backtest_status_line_reports_ok(self):
        with (
            mock.patch.object(dashboard, "get_service_status", return_value="svc"),
            mock.patch.object(dashboard, "get_backtest_drift_state", return_value=(True, "aligned")),
        ):
            line = dashboard.get_backtest_status_line()
        self.assertIn("svc", line)
        self.assertIn("Drift", line)
        self.assertIn("OK", line)

    def test_get_backtest_status_line_reports_drift(self):
        with (
            mock.patch.object(dashboard, "get_service_status", return_value="svc"),
            mock.patch.object(
                dashboard,
                "get_backtest_drift_state",
                return_value=(False, "inactive but listener exists"),
            ),
        ):
            line = dashboard.get_backtest_status_line()
        self.assertIn("svc", line)
        self.assertIn("Drift", line)
        self.assertIn("DRIFT", line)


if __name__ == "__main__":
    unittest.main()
