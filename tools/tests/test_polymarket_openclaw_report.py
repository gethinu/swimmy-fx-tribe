import json
import tempfile
import unittest
from pathlib import Path

from tools import polymarket_openclaw_report as report


class TestPolymarketOpenClawReport(unittest.TestCase):
    def test_summarize_journal_totals(self) -> None:
        records = [
            {
                "type": "run_summary",
                "run_id": "r1",
                "date": "2026-02-12",
                "summary": {"entries": 2, "total_stake_usd": 3.0},
            },
            {
                "type": "entry",
                "run_id": "r1",
                "date": "2026-02-12",
                "side": "YES",
                "stake_usd": 1.5,
                "expected_value_usd": 0.12,
                "edge": 0.03,
            },
            {
                "type": "entry",
                "run_id": "r1",
                "date": "2026-02-12",
                "side": "NO",
                "stake_usd": 1.5,
                "expected_value_usd": 0.08,
                "edge": 0.02,
            },
        ]

        summary = report.summarize_journal(records)

        self.assertEqual(1, summary["runs"])
        self.assertEqual(2, summary["entries"])
        self.assertAlmostEqual(3.0, summary["total_stake_usd"])
        self.assertAlmostEqual(0.2, summary["total_expected_value_usd"])
        self.assertEqual(1, summary["side_counts"]["YES"])
        self.assertEqual(1, summary["side_counts"]["NO"])

    def test_load_journal_records_ignores_blank_and_invalid_json(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / "journal.jsonl"
            path.write_text(
                "\n".join(
                    [
                        json.dumps({"type": "entry", "date": "2026-02-12", "stake_usd": 1.0}),
                        "",
                        "{invalid json}",
                    ]
                ),
                encoding="utf-8",
            )

            records = report.load_journal_records(path)

        self.assertEqual(1, len(records))
        self.assertEqual("entry", records[0]["type"])

    def test_date_filter(self) -> None:
        records = [
            {"type": "run_summary", "run_id": "r1", "date": "2026-02-11", "summary": {"entries": 1}},
            {"type": "entry", "run_id": "r1", "date": "2026-02-11", "side": "YES", "stake_usd": 1.0, "expected_value_usd": 0.05},
            {"type": "run_summary", "run_id": "r2", "date": "2026-02-12", "summary": {"entries": 2}},
            {"type": "entry", "run_id": "r2", "date": "2026-02-12", "side": "NO", "stake_usd": 2.0, "expected_value_usd": 0.08},
        ]

        summary = report.summarize_journal(records, target_date="2026-02-12")

        self.assertEqual(1, summary["runs"])
        self.assertEqual(1, summary["entries"])
        self.assertAlmostEqual(2.0, summary["total_stake_usd"])

    def test_summarize_realized_with_settlements(self) -> None:
        records = [
            {
                "type": "entry",
                "run_id": "r1",
                "date": "2026-02-12",
                "market_id": "m1",
                "side": "YES",
                "entry_price": 0.5,
                "model_prob": 0.8,
                "stake_usd": 10.0,
                "expected_value_usd": 1.5,
            },
            {
                "type": "entry",
                "run_id": "r1",
                "date": "2026-02-12",
                "market_id": "m2",
                "side": "NO",
                "entry_price": 0.4,
                "model_prob": 0.7,
                "stake_usd": 10.0,
                "expected_value_usd": 0.5,
            },
        ]
        settlements = {"m1": "YES", "m2": "YES"}

        realized = report.summarize_realized(records, settlements, target_date="2026-02-12")

        self.assertEqual(2, realized["resolved_entries"])
        self.assertEqual(0, realized["unresolved_entries"])
        self.assertAlmostEqual(0.0, realized["realized_pnl_usd"])
        self.assertAlmostEqual(0.5, realized["realized_win_rate"])
        self.assertAlmostEqual(2.0, realized["expected_value_usd"])
        self.assertAlmostEqual(-2.0, realized["realized_minus_expected_usd"])
        self.assertAlmostEqual(0.75, realized["mean_model_win_prob"])
        self.assertAlmostEqual(0.265, realized["brier_score"])

    def test_summarize_realized_counts_unresolved(self) -> None:
        records = [
            {
                "type": "entry",
                "run_id": "r1",
                "date": "2026-02-12",
                "market_id": "m1",
                "side": "YES",
                "stake_usd": 10.0,
                "expected_value_usd": 1.5,
            },
            {
                "type": "entry",
                "run_id": "r1",
                "date": "2026-02-12",
                "market_id": "m2",
                "side": "NO",
                "entry_price": 0.4,
                "stake_usd": 10.0,
                "expected_value_usd": 0.5,
            },
        ]
        settlements = {"m1": "YES"}

        realized = report.summarize_realized(records, settlements, target_date="2026-02-12")

        self.assertEqual(0, realized["resolved_entries"])
        self.assertEqual(2, realized["unresolved_entries"])
        self.assertAlmostEqual(0.0, realized["realized_pnl_usd"])

    def test_load_settlements_from_dict_and_list(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            dict_path = Path(td) / "s1.json"
            list_path = Path(td) / "s2.json"
            dict_path.write_text(json.dumps({"m1": "YES", "m2": "n"}), encoding="utf-8")
            list_path.write_text(
                json.dumps([{"market_id": "m3", "winner": "YES"}, {"id": "m4", "outcome": "no"}]),
                encoding="utf-8",
            )

            s1 = report.load_settlements(dict_path)
            s2 = report.load_settlements(list_path)

        self.assertEqual({"m1": "YES", "m2": "NO"}, s1)
        self.assertEqual({"m3": "YES", "m4": "NO"}, s2)

    def test_summarize_realized_applies_costs(self) -> None:
        records = [
            {
                "type": "entry",
                "run_id": "r1",
                "date": "2026-02-12",
                "market_id": "m1",
                "side": "YES",
                "entry_price": 0.5,
                "stake_usd": 10.0,
                "expected_value_usd": 1.0,
            }
        ]
        settlements = {"m1": "YES"}

        no_cost = report.summarize_realized(records, settlements, target_date="2026-02-12")
        with_cost = report.summarize_realized(
            records,
            settlements,
            target_date="2026-02-12",
            fee_bps_per_side=100.0,
            slippage_bps_per_side=100.0,
        )

        self.assertGreater(no_cost["realized_pnl_usd"], with_cost["realized_pnl_usd"])


if __name__ == "__main__":
    unittest.main()
