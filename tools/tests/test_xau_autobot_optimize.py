import unittest
from unittest import mock

from tools.xau_autobot_optimize import _load_ohlc, candidate_to_config, interval_to_timeframe, score_candidate


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

    def test_candidate_to_config_rejects_comment_over_31_chars(self):
        candidate = (24, 140, 0.2, 1.5, 2.5, 7, 19, 0.9, 1.4)
        with self.assertRaises(ValueError):
            candidate_to_config(candidate, magic=560099, comment="x" * 32)

    def test_candidate_to_config_supports_hybrid_strategy_fields(self):
        candidate = (
            24,
            160,
            0.2,
            1.5,
            2.5,
            7,
            19,
            0.9,
            1.4,
            "hybrid",
            1.1,
            1.2,
            1.0,
            1.4,
        )
        cfg = candidate_to_config(candidate, magic=560100, comment="xau_hybrid_opt")
        self.assertEqual(cfg["strategy_mode"], "hybrid")
        self.assertEqual(cfg["regime_trend_threshold"], 1.1)
        self.assertEqual(cfg["reversion_atr"], 1.2)
        self.assertEqual(cfg["reversion_sl_atr"], 1.0)
        self.assertEqual(cfg["reversion_tp_atr"], 1.4)

    def test_candidate_to_config_allows_timeframe_override(self):
        candidate = (24, 140, 0.2, 1.5, 2.5, 7, 19, 0.9, 1.4)
        cfg = candidate_to_config(candidate, magic=560099, comment="xau_autobot_opt", timeframe="M15")
        self.assertEqual(cfg["timeframe"], "M15")

    def test_interval_to_timeframe_maps_supported_values(self):
        self.assertEqual(interval_to_timeframe("5m"), "M5")
        self.assertEqual(interval_to_timeframe("m20"), "M20")
        self.assertEqual(interval_to_timeframe("m45"), "M45")
        self.assertEqual(interval_to_timeframe("15m"), "M15")
        self.assertEqual(interval_to_timeframe("60m"), "H1")
        self.assertEqual(interval_to_timeframe("1h"), "H1")
        self.assertEqual(interval_to_timeframe("h2"), "H2")
        self.assertEqual(interval_to_timeframe("h3"), "H3")
        self.assertEqual(interval_to_timeframe("4h"), "H4")
        self.assertEqual(interval_to_timeframe("m60"), "H1")
        self.assertEqual(interval_to_timeframe("h4"), "H4")
        self.assertEqual(interval_to_timeframe("1d"), "D1")
        self.assertEqual(interval_to_timeframe("d1"), "D1")
        self.assertEqual(interval_to_timeframe("1wk"), "W1")
        self.assertEqual(interval_to_timeframe("w1"), "W1")
        self.assertEqual(interval_to_timeframe("1mo"), "MN")
        self.assertEqual(interval_to_timeframe("month"), "MN")
        self.assertEqual(interval_to_timeframe("h5"), "H5")

    def test_interval_to_timeframe_rejects_unsupported_value(self):
        with self.assertRaises(ValueError):
            interval_to_timeframe("7m")

    def test_candidate_to_config_allows_extended_research_timeframes(self):
        candidate = (24, 140, 0.2, 1.5, 2.5, 7, 19, 0.9, 1.4)
        d1 = candidate_to_config(candidate, magic=560099, comment="xau_autobot_opt", timeframe="D1")
        w1 = candidate_to_config(candidate, magic=560099, comment="xau_autobot_opt", timeframe="W1")
        mn = candidate_to_config(candidate, magic=560099, comment="xau_autobot_opt", timeframe="MN")
        h5 = candidate_to_config(candidate, magic=560099, comment="xau_autobot_opt", timeframe="H5")
        h2 = candidate_to_config(candidate, magic=560099, comment="xau_autobot_opt", timeframe="H2")
        h3 = candidate_to_config(candidate, magic=560099, comment="xau_autobot_opt", timeframe="H3")
        m20 = candidate_to_config(candidate, magic=560099, comment="xau_autobot_opt", timeframe="M20")
        m45 = candidate_to_config(candidate, magic=560099, comment="xau_autobot_opt", timeframe="M45")
        self.assertEqual(d1["timeframe"], "D1")
        self.assertEqual(w1["timeframe"], "W1")
        self.assertEqual(mn["timeframe"], "MN")
        self.assertEqual(h5["timeframe"], "H5")
        self.assertEqual(h2["timeframe"], "H2")
        self.assertEqual(h3["timeframe"], "H3")
        self.assertEqual(m20["timeframe"], "M20")
        self.assertEqual(m45["timeframe"], "M45")

    def test_load_ohlc_forwards_mt5_csv_source_to_data_loader(self):
        expected = (["t"], [1.0], [1.1], [0.9], [1.0])
        with mock.patch("tools.xau_autobot_optimize.load_ohlc", return_value=expected) as loader:
            out = _load_ohlc(
                "XAUUSD",
                "365d",
                "m45",
                data_source="mt5_csv",
                source_csv="/tmp/xau_m15.csv",
            )
        loader.assert_called_once_with(
            ticker="XAUUSD",
            period="365d",
            interval="m45",
            data_source="mt5_csv",
            source_csv_path="/tmp/xau_m15.csv",
        )
        self.assertEqual(out, expected)


if __name__ == "__main__":
    unittest.main()
