import json
import tempfile
import unittest
from datetime import datetime, timezone
from pathlib import Path

from tools.xau_autobot_trial_judge import (
    evaluate_trial_report,
    resolve_live_report_path,
)


class TestXauAutoBotTrialJudge(unittest.TestCase):
    def test_resolve_live_report_path_prefers_latest_file(self):
        with tempfile.TemporaryDirectory() as td:
            reports_dir = Path(td)
            old = reports_dir / "xau_autobot_live_report_20260220_110000.json"
            new = reports_dir / "xau_autobot_live_report_20260221_110000.json"
            old.write_text("{}", encoding="utf-8")
            new.write_text("{}", encoding="utf-8")
            now = datetime(2026, 2, 22, 12, 0, tzinfo=timezone.utc)
            old_ts = (now.timestamp() - 4 * 3600)
            new_ts = (now.timestamp() - 1 * 3600)
            old.touch()
            new.touch()
            old.chmod(0o644)
            new.chmod(0o644)
            old_stat_time = float(old_ts)
            new_stat_time = float(new_ts)
            import os

            os.utime(old, (old_stat_time, old_stat_time))
            os.utime(new, (new_stat_time, new_stat_time))

            resolved = resolve_live_report_path(
                live_report="",
                reports_dir=reports_dir,
                max_age_hours=6.0,
                now_utc=now,
            )
            self.assertEqual(resolved, new)

    def test_evaluate_trial_report_go(self):
        report = {
            "start_utc": "2026-02-01T00:00:00+00:00",
            "end_utc": "2026-02-16T00:00:00+00:00",
            "summary": {
                "closed_positions": 40.0,
                "profit_factor": 1.2,
                "win_rate": 0.45,
                "net_profit": 2500.0,
            },
            "diagnostics": {
                "after_magic_filter": 12.0,
                "after_comment_prefix_filter": 8.0,
            },
        }
        result = evaluate_trial_report(
            report,
            min_days=14.0,
            min_closed_positions=30.0,
            min_profit_factor=1.1,
            min_win_rate=0.42,
            min_net_profit=0.0,
        )
        self.assertEqual(result["verdict"], "GO")
        self.assertEqual(result["trial_valid"], True)
        self.assertEqual(result["failed_checks"], [])
        self.assertTrue(result["readiness"]["checks"]["window_days"])
        self.assertTrue(result["readiness"]["checks"]["closed_positions"])
        self.assertTrue(result["performance"]["checks"]["profit_factor"])
        self.assertTrue(result["performance"]["checks"]["win_rate"])
        self.assertTrue(result["performance"]["checks"]["net_profit"])

    def test_evaluate_trial_report_no_go_when_metrics_fail(self):
        report = {
            "start_utc": "2026-02-10T00:00:00+00:00",
            "end_utc": "2026-02-15T00:00:00+00:00",
            "summary": {
                "closed_positions": 12.0,
                "profit_factor": 0.8,
                "win_rate": 0.30,
                "net_profit": -500.0,
            },
        }
        result = evaluate_trial_report(
            report,
            min_days=14.0,
            min_closed_positions=30.0,
            min_profit_factor=1.1,
            min_win_rate=0.42,
            min_net_profit=0.0,
        )
        self.assertEqual(result["verdict"], "NO_GO")
        self.assertIn("window_days", result["failed_checks"])
        self.assertIn("closed_positions", result["failed_checks"])
        self.assertIn("profit_factor", result["failed_checks"])
        self.assertIn("win_rate", result["failed_checks"])
        self.assertIn("net_profit", result["failed_checks"])
        self.assertIn("window_days", result["readiness"]["failed_checks"])
        self.assertIn("closed_positions", result["readiness"]["failed_checks"])
        self.assertIn("profit_factor", result["performance"]["failed_checks"])
        self.assertIn("win_rate", result["performance"]["failed_checks"])
        self.assertIn("net_profit", result["performance"]["failed_checks"])

    def test_evaluate_trial_report_invalid_when_magic_or_comment_unmatched(self):
        report = {
            "start_utc": "2026-02-01T00:00:00+00:00",
            "end_utc": "2026-02-16T00:00:00+00:00",
            "summary": {
                "closed_positions": 40.0,
                "profit_factor": 1.4,
                "win_rate": 0.53,
                "net_profit": 3500.0,
            },
            "diagnostics": {
                "after_magic_filter": 0.0,
                "after_comment_prefix_filter": 0.0,
            },
        }
        result = evaluate_trial_report(
            report,
            min_days=14.0,
            min_closed_positions=30.0,
            min_profit_factor=1.1,
            min_win_rate=0.42,
            min_net_profit=0.0,
        )
        self.assertEqual(result["verdict"], "INVALID_TRIAL")
        self.assertEqual(result["trial_valid"], False)
        self.assertIn("after_magic_filter", result["invalid_reasons"])
        self.assertIn("after_comment_prefix_filter", result["invalid_reasons"])

    def test_evaluate_trial_report_treats_zero_net_profit_as_pass(self):
        report = {
            "start_utc": "2026-02-01T00:00:00+00:00",
            "end_utc": "2026-02-16T00:00:00+00:00",
            "summary": {
                "closed_positions": 40.0,
                "profit_factor": 1.2,
                "win_rate": 0.45,
                "net_profit": 0.0,
            },
            "diagnostics": {
                "after_magic_filter": 12.0,
                "after_comment_prefix_filter": 8.0,
            },
        }
        result = evaluate_trial_report(
            report,
            min_days=14.0,
            min_closed_positions=12.0,
            min_profit_factor=1.1,
            min_win_rate=0.42,
            min_net_profit=0.0,
        )
        self.assertEqual(result["verdict"], "GO")
        self.assertTrue(result["performance"]["checks"]["net_profit"])

    def test_evaluate_trial_report_monthly_only_go_when_target_hit(self):
        report = {
            "start_utc": "2026-02-01T00:00:00+00:00",
            "end_utc": "2026-03-03T00:00:00+00:00",
            "summary": {
                "closed_positions": 1.0,
                "profit_factor": 0.7,
                "win_rate": 0.2,
                "net_profit": 3000.0,
            },
            "diagnostics": {
                "after_magic_filter": 10.0,
                "after_comment_prefix_filter": 10.0,
            },
        }
        result = evaluate_trial_report(
            report,
            min_days=30.0,
            min_closed_positions=999.0,
            min_profit_factor=9.9,
            min_win_rate=0.99,
            min_net_profit=999999.0,
            decision_mode="monthly_only",
            min_monthly_return_pct=3.0,
            monthly_account_balance=100000.0,
        )
        self.assertEqual(result["verdict"], "GO")
        self.assertEqual(result["trial_valid"], True)
        self.assertAlmostEqual(result["monthly"]["monthly_return_pct"], 3.0, places=6)
        self.assertEqual(result["failed_checks"], [])

    def test_evaluate_trial_report_monthly_only_no_go_when_target_miss(self):
        report = {
            "start_utc": "2026-02-01T00:00:00+00:00",
            "end_utc": "2026-03-03T00:00:00+00:00",
            "summary": {
                "closed_positions": 200.0,
                "profit_factor": 9.0,
                "win_rate": 0.95,
                "net_profit": 1000.0,
            },
            "diagnostics": {
                "after_magic_filter": 10.0,
                "after_comment_prefix_filter": 10.0,
            },
        }
        result = evaluate_trial_report(
            report,
            min_days=30.0,
            min_closed_positions=1.0,
            min_profit_factor=1.0,
            min_win_rate=0.1,
            min_net_profit=-999.0,
            decision_mode="monthly_only",
            min_monthly_return_pct=3.0,
            monthly_account_balance=100000.0,
        )
        self.assertEqual(result["verdict"], "NO_GO")
        self.assertIn("monthly_return_pct", result["failed_checks"])

    def test_evaluate_trial_report_rolling_45d_enforces_window_floor(self):
        report = {
            "start_utc": "2026-02-01T00:00:00+00:00",
            "end_utc": "2026-03-03T00:00:00+00:00",  # 30 days
            "summary": {
                "closed_positions": 20.0,
                "profit_factor": 1.5,
                "win_rate": 0.5,
                "net_profit": 1500.0,
            },
            "diagnostics": {
                "after_magic_filter": 10.0,
                "after_comment_prefix_filter": 10.0,
            },
        }
        result = evaluate_trial_report(
            report,
            min_days=14.0,
            min_closed_positions=8.0,
            min_profit_factor=1.2,
            min_win_rate=0.38,
            min_net_profit=0.0,
            decision_mode="rolling_45d",
        )
        self.assertEqual(result["verdict"], "NO_GO")
        self.assertIn("window_days", result["failed_checks"])

    def test_evaluate_trial_report_rolling_60d_go_when_all_checks_pass(self):
        report = {
            "start_utc": "2026-01-01T00:00:00+00:00",
            "end_utc": "2026-03-05T00:00:00+00:00",  # 63 days
            "summary": {
                "closed_positions": 6.0,
                "profit_factor": 1.3,
                "win_rate": 0.40,
                "net_profit": 1200.0,
            },
            "diagnostics": {
                "after_magic_filter": 10.0,
                "after_comment_prefix_filter": 10.0,
            },
        }
        result = evaluate_trial_report(
            report,
            min_days=14.0,
            min_closed_positions=4.0,
            min_profit_factor=1.2,
            min_win_rate=0.33,
            min_net_profit=0.0,
            decision_mode="rolling_60d",
        )
        self.assertEqual(result["verdict"], "GO")
        self.assertEqual(result["failed_checks"], [])

    def test_evaluate_trial_report_rolling_45d_boundary_allows_exact_45_days(self):
        report = {
            "start_utc": "2026-01-01T00:00:00+00:00",
            "end_utc": "2026-02-15T00:00:00+00:00",  # exact 45 days
            "summary": {
                "closed_positions": 8.0,
                "profit_factor": 1.2,
                "win_rate": 0.4,
                "net_profit": 100.0,
            },
            "diagnostics": {
                "after_magic_filter": 10.0,
                "after_comment_prefix_filter": 10.0,
            },
        }
        result = evaluate_trial_report(
            report,
            min_days=30.0,
            min_closed_positions=8.0,
            min_profit_factor=1.2,
            min_win_rate=0.38,
            min_net_profit=0.0,
            decision_mode="rolling_45d",
        )
        self.assertEqual(result["verdict"], "GO")
        self.assertEqual(result["failed_checks"], [])

    def test_evaluate_trial_report_rolling_60d_boundary_allows_exact_60_days(self):
        report = {
            "start_utc": "2026-01-01T00:00:00+00:00",
            "end_utc": "2026-03-02T00:00:00+00:00",  # exact 60 days
            "summary": {
                "closed_positions": 4.0,
                "profit_factor": 1.25,
                "win_rate": 0.35,
                "net_profit": 100.0,
            },
            "diagnostics": {
                "after_magic_filter": 10.0,
                "after_comment_prefix_filter": 10.0,
            },
        }
        result = evaluate_trial_report(
            report,
            min_days=30.0,
            min_closed_positions=4.0,
            min_profit_factor=1.2,
            min_win_rate=0.33,
            min_net_profit=0.0,
            decision_mode="rolling_60d",
        )
        self.assertEqual(result["verdict"], "GO")
        self.assertEqual(result["failed_checks"], [])


if __name__ == "__main__":
    unittest.main()
