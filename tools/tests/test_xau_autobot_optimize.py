import unittest

from tools.xau_autobot_optimize import candidate_to_config, score_candidate


class TestXauAutoBotOptimize(unittest.TestCase):
    def test_score_candidate_prefers_better_oos(self):
        weak = [
            {"is_total": -0.04, "oos_total": 0.01, "oos_pf": 1.05, "oos_trades": 80, "oos_max_dd": 0.05},
            {"is_total": -0.03, "oos_total": 0.00, "oos_pf": 1.00, "oos_trades": 70, "oos_max_dd": 0.06},
        ]
        strong = [
            {"is_total": -0.02, "oos_total": 0.08, "oos_pf": 1.45, "oos_trades": 80, "oos_max_dd": 0.03},
            {"is_total": -0.01, "oos_total": 0.07, "oos_pf": 1.35, "oos_trades": 70, "oos_max_dd": 0.03},
        ]
        self.assertGreater(score_candidate(strong, min_oos_trades=40), score_candidate(weak, min_oos_trades=40))

    def test_score_candidate_penalizes_too_few_trades(self):
        enough = [
            {"is_total": -0.01, "oos_total": 0.04, "oos_pf": 1.2, "oos_trades": 60, "oos_max_dd": 0.02},
        ]
        too_few = [
            {"is_total": -0.01, "oos_total": 0.04, "oos_pf": 1.2, "oos_trades": 10, "oos_max_dd": 0.02},
        ]
        self.assertGreater(score_candidate(enough, min_oos_trades=40), score_candidate(too_few, min_oos_trades=40))

    def test_candidate_to_config(self):
        candidate = (24, 140, 0.2, 1.5, 2.5, 7, 19, 0.9, 1.4)
        cfg = candidate_to_config(candidate, magic=560099, comment="xau_autobot_opt")
        self.assertEqual(cfg["fast_ema"], 24)
        self.assertEqual(cfg["slow_ema"], 140)
        self.assertEqual(cfg["session_start_hour_utc"], 7)
        self.assertEqual(cfg["session_end_hour_utc"], 19)
        self.assertEqual(cfg["min_atr_ratio_to_median"], 0.9)
        self.assertEqual(cfg["max_atr_ratio_to_median"], 1.4)
        self.assertEqual(cfg["magic"], 560099)
        self.assertEqual(cfg["comment"], "xau_autobot_opt")


if __name__ == "__main__":
    unittest.main()
