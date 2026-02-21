import unittest
import json
import os
import tempfile
from datetime import datetime, timezone
from pathlib import Path

from tools.xau_autobot_promote_best import (
    choose_best_period,
    resolve_live_summary,
    score_period_summary,
)


class TestXauAutoBotPromoteBest(unittest.TestCase):
    def test_score_period_summary_prefers_higher_return_and_lower_dd(self):
        a = {
            "period": "45d",
            "backtest": {"pf": 1.30, "total_return": 0.08, "max_dd": 0.04},
            "readiness": {"verdict": "GO"},
            "cost_guard": {"verdict": "GO"},
        }
        b = {
            "period": "60d",
            "backtest": {"pf": 1.10, "total_return": 0.03, "max_dd": 0.07},
            "readiness": {"verdict": "GO"},
            "cost_guard": {"verdict": "GO"},
        }
        self.assertGreater(score_period_summary(a), score_period_summary(b))

    def test_score_period_summary_penalizes_non_go(self):
        go = {
            "period": "90d",
            "backtest": {"pf": 1.1, "total_return": 0.02, "max_dd": 0.05},
            "readiness": {"verdict": "GO"},
            "cost_guard": {"verdict": "GO"},
        }
        no_go = {
            "period": "90d",
            "backtest": {"pf": 2.0, "total_return": 0.2, "max_dd": 0.02},
            "readiness": {"verdict": "NO_GO"},
            "cost_guard": {"verdict": "GO"},
        }
        self.assertGreater(score_period_summary(go), score_period_summary(no_go))

    def test_choose_best_period(self):
        rows = [
            {
                "period": "45d",
                "backtest": {"pf": 1.37, "total_return": 0.07, "max_dd": 0.04},
                "readiness": {"verdict": "GO"},
                "cost_guard": {"verdict": "GO"},
            },
            {
                "period": "60d",
                "backtest": {"pf": 1.20, "total_return": 0.05, "max_dd": 0.06},
                "readiness": {"verdict": "GO"},
                "cost_guard": {"verdict": "GO"},
            },
        ]
        best = choose_best_period(rows)
        self.assertEqual(best["period"], "45d")

    def test_score_period_summary_penalizes_oos_instability(self):
        stable = {
            "period": "45d",
            "backtest": {"pf": 1.30, "total_return": 0.06, "max_dd": 0.04},
            "readiness": {
                "verdict": "GO",
                "robustness": {
                    "oos_count": 3.0,
                    "oos_worst_total_return": 0.01,
                    "oos_worst_pf": 1.10,
                    "oos_negative_return_ratio": 0.0,
                },
            },
            "cost_guard": {"verdict": "GO"},
        }
        unstable = {
            "period": "90d",
            "backtest": {"pf": 1.55, "total_return": 0.10, "max_dd": 0.03},
            "readiness": {
                "verdict": "GO",
                "robustness": {
                    "oos_count": 3.0,
                    "oos_worst_total_return": -0.02,
                    "oos_worst_pf": 0.63,
                    "oos_negative_return_ratio": 0.67,
                },
            },
            "cost_guard": {"verdict": "GO"},
        }
        self.assertGreater(score_period_summary(stable), score_period_summary(unstable))

    def test_choose_best_period_prefers_robust_candidate_when_live_is_weak(self):
        stable = {
            "period": "45d",
            "backtest": {"pf": 1.25, "total_return": 0.055, "max_dd": 0.035},
            "readiness": {
                "verdict": "GO",
                "robustness": {
                    "oos_count": 3.0,
                    "oos_worst_total_return": 0.005,
                    "oos_worst_pf": 1.05,
                    "oos_negative_return_ratio": 0.0,
                },
            },
            "cost_guard": {"verdict": "GO"},
        }
        unstable = {
            "period": "90d",
            "backtest": {"pf": 1.55, "total_return": 0.095, "max_dd": 0.03},
            "readiness": {
                "verdict": "GO",
                "robustness": {
                    "oos_count": 3.0,
                    "oos_worst_total_return": -0.02,
                    "oos_worst_pf": 0.63,
                    "oos_negative_return_ratio": 0.67,
                },
            },
            "cost_guard": {"verdict": "GO"},
        }
        weak_live = {
            "closed_positions": 20.0,
            "win_rate": 0.24,
            "profit_factor": 0.55,
            "net_profit": -9000.0,
        }
        best = choose_best_period([stable, unstable], live_summary=weak_live)
        self.assertEqual(best["period"], "45d")

    def test_resolve_live_summary_prefers_nonzero_closed_positions(self):
        with tempfile.TemporaryDirectory() as td:
            d = Path(td)
            newer_zero = d / "xau_autobot_live_report_20260220_120000_7d.json"
            older_nonzero = d / "xau_autobot_live_report_20260219_120000_7d.json"
            newer_zero.write_text(
                json.dumps({"summary": {"closed_positions": 0.0, "profit_factor": 0.0}}),
                encoding="utf-8",
            )
            older_nonzero.write_text(
                json.dumps({"summary": {"closed_positions": 12.0, "profit_factor": 1.1}}),
                encoding="utf-8",
            )
            now = datetime(2026, 2, 21, 0, 0, tzinfo=timezone.utc)
            os.utime(older_nonzero, (float(now.timestamp() - 120), float(now.timestamp() - 120)))
            os.utime(newer_zero, (float(now.timestamp() - 60), float(now.timestamp() - 60)))

            path, summary = resolve_live_summary(
                live_report="",
                live_reports_dir=d,
                max_age_hours=24.0,
                now_utc=now,
            )
            self.assertEqual(path, older_nonzero)
            self.assertEqual(summary.get("closed_positions"), 12.0)

    def test_resolve_live_summary_skips_stale_reports(self):
        with tempfile.TemporaryDirectory() as td:
            d = Path(td)
            stale_nonzero = d / "xau_autobot_live_report_20260219_120000_7d.json"
            fresh_zero = d / "xau_autobot_live_report_20260220_230000_7d.json"
            stale_nonzero.write_text(
                json.dumps({"summary": {"closed_positions": 18.0, "profit_factor": 1.2}}),
                encoding="utf-8",
            )
            fresh_zero.write_text(
                json.dumps({"summary": {"closed_positions": 0.0, "profit_factor": 0.0}}),
                encoding="utf-8",
            )

            now = datetime(2026, 2, 21, 0, 0, tzinfo=timezone.utc)
            os.utime(stale_nonzero, (float(now.timestamp() - 48 * 3600), float(now.timestamp() - 48 * 3600)))
            os.utime(fresh_zero, (float(now.timestamp() - 30 * 60), float(now.timestamp() - 30 * 60)))

            path, summary = resolve_live_summary(
                live_report="",
                live_reports_dir=d,
                max_age_hours=12.0,
                now_utc=now,
            )
            self.assertEqual(path, fresh_zero)
            self.assertEqual(summary.get("closed_positions"), 0.0)


if __name__ == "__main__":
    unittest.main()
