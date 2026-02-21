import json
import tempfile
import unittest
from pathlib import Path

from tools.xau_autobot_performance_summary import (
    derive_status,
    load_jsonl_rows,
    summarize_compare_history,
    summarize_promotion_history,
)


class TestXauAutoBotPerformanceSummary(unittest.TestCase):
    def test_load_jsonl_rows_parses_only_objects(self):
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / "rows.jsonl"
            path.write_text('{"a":1}\n[1,2]\n{"b":2}\n', encoding="utf-8")
            rows = load_jsonl_rows(path)
            self.assertEqual(len(rows), 2)
            self.assertEqual(rows[0]["a"], 1)
            self.assertEqual(rows[1]["b"], 2)

    def test_summarize_promotion_history_counts_block_and_reasons(self):
        rows = [
            {"selected_period": "45d", "selected_score": 3.1, "promotion_blocked": False},
            {
                "selected_period": "60d",
                "selected_score": 2.4,
                "promotion_blocked": True,
                "live_gap": {
                    "sample_quality": "ok",
                    "underperforming": True,
                    "underperforming_reasons": ["live_pf_below_1", "pf_gap_large"],
                },
            },
            {
                "selected_period": "60d",
                "selected_score": 2.2,
                "promotion_blocked": True,
                "live_gap": {
                    "sample_quality": "ok",
                    "underperforming": True,
                    "underperforming_reasons": ["pf_gap_large"],
                },
            },
        ]
        summary = summarize_promotion_history(rows, lookback=2)
        self.assertEqual(summary["runs"], 2)
        self.assertEqual(summary["blocked_runs"], 2)
        self.assertEqual(summary["underperforming_runs"], 2)
        self.assertEqual(summary["selected_period_counts"]["60d"], 2)
        self.assertEqual(summary["underperforming_reason_counts"]["pf_gap_large"], 2)

    def test_summarize_compare_history_counts_skip_runs(self):
        rows = [
            {"action": "SKIP", "reason": "market_closed", "periods": []},
            {"periods": [{"period": "45d"}]},
            {"periods": [{"period": "45d"}, {"period": "60d"}]},
        ]
        summary = summarize_compare_history(rows, lookback=3)
        self.assertEqual(summary["runs"], 3)
        self.assertEqual(summary["skip_runs"], 1)
        self.assertEqual(summary["non_skip_runs"], 2)
        self.assertAlmostEqual(summary["avg_period_rows"], 1.0)

    def test_derive_status_alert_on_latest_blocked(self):
        latest_promotion = {"promotion_blocked": True}
        latest_compare = {"action": "SKIP", "reason": "market_closed"}
        self.assertEqual(derive_status(latest_promotion, latest_compare), "ALERT")

    def test_derive_status_unknown_when_no_data(self):
        self.assertEqual(derive_status({}, {}), "UNKNOWN")


if __name__ == "__main__":
    unittest.main()
