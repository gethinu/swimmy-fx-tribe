import json
import os
import tempfile
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest import mock

from tools import xau_autobot_readiness
from tools.xau_autobot_readiness import (
    estimate_break_even_cost,
    reversion_guard_allows,
    readiness_verdict,
    total_return_from_gross,
)


class TestXauAutoBotReadiness(unittest.TestCase):
    def test_total_return_from_gross(self):
        gross = [0.01, -0.005, 0.02]
        total = total_return_from_gross(gross, cost_per_side=0.0)
        expected = (1.01 * 0.995 * 1.02) - 1.0
        self.assertAlmostEqual(total, expected, places=10)

    def test_estimate_break_even_cost(self):
        # (1 + 0.02 - 2c)^2 - 1 = 0 => c = 0.01
        gross = [0.02, 0.02]
        fn = lambda c: total_return_from_gross(gross, c)
        breakeven = estimate_break_even_cost(fn, lo=0.0, hi=0.02, iterations=40)
        self.assertAlmostEqual(breakeven, 0.01, places=4)

    def test_readiness_verdict(self):
        self.assertEqual(readiness_verdict(assumed_cost_side=0.0002, break_even_cost_side=0.0004), "GO")
        self.assertEqual(readiness_verdict(assumed_cost_side=0.00035, break_even_cost_side=0.0004), "CAUTION")
        self.assertEqual(readiness_verdict(assumed_cost_side=0.0005, break_even_cost_side=0.0004), "NO_GO")

    def test_reversion_guard_allows_when_thresholds_satisfied(self):
        self.assertTrue(
            reversion_guard_allows(
                atr_ratio_to_median=1.10,
                ema_gap_over_atr=0.95,
                max_atr_ratio_to_median=1.25,
                max_ema_gap_over_atr=1.10,
            )
        )

    def test_reversion_guard_blocks_when_thresholds_exceeded(self):
        self.assertFalse(
            reversion_guard_allows(
                atr_ratio_to_median=1.30,
                ema_gap_over_atr=0.95,
                max_atr_ratio_to_median=1.25,
                max_ema_gap_over_atr=1.10,
            )
        )
        self.assertFalse(
            reversion_guard_allows(
                atr_ratio_to_median=1.10,
                ema_gap_over_atr=1.30,
                max_atr_ratio_to_median=1.25,
                max_ema_gap_over_atr=1.10,
            )
        )

    def test_main_writes_runtime_journal_with_research_counters(self):
        with tempfile.TemporaryDirectory() as td:
            tempdir = Path(td)
            config_path = tempdir / "cfg.json"
            report_path = tempdir / "readiness_report.json"
            journal_path = tempdir / "runtime_journal.jsonl"

            config_path.write_text(
                json.dumps(
                    {
                        "fast_ema": 2,
                        "slow_ema": 4,
                        "atr_period": 2,
                        "strategy_mode": "trend",
                        "regime_trend_threshold": 1.0,
                        "session_start_hour_utc": 0,
                        "session_end_hour_utc": 23,
                        "atr_filter_window": 3,
                        "atr_filter_min_samples": 1,
                        "min_atr_ratio_to_median": 0.0,
                        "max_atr_ratio_to_median": 999.0,
                        "pullback_atr": 0.1,
                        "reversion_atr": 0.2,
                        "sl_atr": 1.0,
                        "tp_atr": 1.2,
                        "reversion_sl_atr": 1.0,
                        "reversion_tp_atr": 1.2,
                    },
                    ensure_ascii=True,
                )
                + "\n",
                encoding="utf-8",
            )

            base = datetime(2026, 1, 1, tzinfo=timezone.utc)
            times = [base + timedelta(hours=i) for i in range(30)]
            opens = [100.0 + (0.1 * i) for i in range(30)]
            highs = [v + 0.5 for v in opens]
            lows = [v - 0.5 for v in opens]
            closes = [v + 0.2 for v in opens]

            with (
                mock.patch.object(xau_autobot_readiness, "_load_ohlc", return_value=(times, opens, highs, lows, closes)),
                mock.patch.dict(os.environ, {"XAU_AUTOBOT_RUNTIME_JOURNAL_PATH": str(journal_path)}, clear=False),
                mock.patch(
                    "sys.argv",
                    [
                        "xau_autobot_readiness.py",
                        "--config",
                        str(config_path),
                        "--write-report",
                        str(report_path),
                    ],
                ),
            ):
                xau_autobot_readiness.main()

            payload = json.loads(report_path.read_text(encoding="utf-8"))
            runtime_metrics = payload.get("runtime_metrics", {})
            self.assertIn("signal_eval_count", runtime_metrics)
            self.assertIn("gap_reject_count", runtime_metrics)
            self.assertIn("spread_reject_count", runtime_metrics)
            self.assertIn("session_reject_count", runtime_metrics)
            self.assertIn("maxpos_reject_count", runtime_metrics)
            self.assertIn("gap_reject_rate", runtime_metrics)
            self.assertTrue(journal_path.exists())

    def test_load_ohlc_forwards_mt5_csv_source_to_data_loader(self):
        expected = (["t"], [1.0], [1.1], [0.9], [1.0])
        with mock.patch("tools.xau_autobot_readiness.load_ohlc", return_value=expected) as loader:
            out = xau_autobot_readiness._load_ohlc(
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

    def test_main_rejects_mt5_csv_without_source_csv(self):
        with tempfile.TemporaryDirectory() as td:
            config_path = Path(td) / "cfg.json"
            config_path.write_text(
                json.dumps(
                    {
                        "fast_ema": 2,
                        "slow_ema": 4,
                        "atr_period": 2,
                        "strategy_mode": "trend",
                        "regime_trend_threshold": 1.0,
                        "session_start_hour_utc": 0,
                        "session_end_hour_utc": 23,
                        "atr_filter_window": 3,
                        "atr_filter_min_samples": 1,
                        "min_atr_ratio_to_median": 0.0,
                        "max_atr_ratio_to_median": 999.0,
                        "pullback_atr": 0.1,
                        "reversion_atr": 0.2,
                        "sl_atr": 1.0,
                        "tp_atr": 1.2,
                        "reversion_sl_atr": 1.0,
                        "reversion_tp_atr": 1.2,
                    },
                    ensure_ascii=True,
                )
                + "\n",
                encoding="utf-8",
            )
            with (
                mock.patch(
                    "sys.argv",
                    [
                        "xau_autobot_readiness.py",
                        "--config",
                        str(config_path),
                        "--data-source",
                        "mt5_csv",
                    ],
                ),
                self.assertRaises(SystemExit),
            ):
                xau_autobot_readiness.main()


if __name__ == "__main__":
    unittest.main()
