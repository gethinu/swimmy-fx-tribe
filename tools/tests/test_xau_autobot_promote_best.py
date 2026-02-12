import unittest

from tools.xau_autobot_promote_best import (
    choose_best_period,
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


if __name__ == "__main__":
    unittest.main()
