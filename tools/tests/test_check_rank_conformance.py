import json
import tempfile
import unittest
from datetime import datetime, timezone
from pathlib import Path

from tools import check_rank_conformance as rc


class TestCheckRankConformance(unittest.TestCase):
    def test_evaluate_rank_conformance_detects_violations(self) -> None:
        rows = [
            {
                "name": "S-GOOD",
                "rank": "S",
                "trades": 140,
                "sharpe": 0.9,
                "profit_factor": 1.45,
                "win_rate": 0.50,
                "max_dd": 0.09,
                "oos_sharpe": 0.60,
                "cpcv_pass_rate": 0.80,
                "cpcv_median_maxdd": 0.10,
            },
            {
                "name": "S-FLOOR-BUT-A",
                "rank": "S",
                "trades": 80,
                "sharpe": 0.8,
                "profit_factor": 1.40,
                "win_rate": 0.52,
                "max_dd": 0.09,
                "oos_sharpe": 0.60,
                "cpcv_pass_rate": 0.80,
                "cpcv_median_maxdd": 0.10,
            },
            {
                "name": "A-FLOOR-FAIL",
                "rank": "A",
                "trades": 40,
                "sharpe": 0.7,
                "profit_factor": 1.50,
                "win_rate": 0.50,
                "max_dd": 0.10,
                "oos_sharpe": 0.55,
                "cpcv_pass_rate": 0.75,
                "cpcv_median_maxdd": 0.09,
            },
            {
                "name": "B-FAIL",
                "rank": "B",
                "trades": 120,
                "sharpe": 0.10,
                "profit_factor": 1.01,
                "win_rate": 0.20,
                "max_dd": 0.40,
                "oos_sharpe": 0.05,
                "cpcv_pass_rate": 0.20,
                "cpcv_median_maxdd": 0.40,
            },
        ]

        report = rc.evaluate_rank_conformance(rows, previous_rank_map={})
        floor = report["violations"]["floor"]
        conf = report["violations"]["conformance"]

        self.assertEqual(1, floor["S_lt_100"])
        self.assertEqual(1, floor["A_lt_50"])
        self.assertEqual(1, conf["S_fail_to_A"])
        self.assertEqual(1, conf["A_fail_to_B"])
        self.assertEqual(1, conf["B_fail_to_graveyard"])

    def test_build_transition_summary_counts_promotions_and_demotions(self) -> None:
        previous = {
            "alpha": "B",
            "beta": "S",
            "gamma": "A",
        }
        current = {
            "alpha": "A",  # promotion
            "beta": "A",  # demotion
            "gamma": "A",  # unchanged
            "delta": "B",  # new
        }
        transitions = rc.build_transition_summary(previous, current)
        self.assertEqual(1, transitions["promotion_count"])
        self.assertEqual(1, transitions["demotion_count"])
        self.assertEqual(1, transitions["new_count"])
        self.assertEqual(0, transitions["removed_count"])
        self.assertEqual(2, transitions["changed_count"])

    def test_render_summary_line(self) -> None:
        line = rc.render_summary_line(
            {
                "violations": {"total": 3},
                "transitions": {
                    "promotion_count": 1,
                    "demotion_count": 2,
                    "changed_count": 4,
                },
            }
        )
        self.assertIn("Rank conformance summary", line)
        self.assertIn("violations=3", line)
        self.assertIn("promotions=1", line)

    def test_load_previous_rank_map_prefers_previous_day_snapshot(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            latest_report = base / "rank_conformance_latest.json"
            history_dir = base / "history"
            history_dir.mkdir(parents=True, exist_ok=True)

            latest_report.write_text(
                json.dumps(
                    {
                        "rank_map": {
                            "alpha": "A",
                            "beta": "B",
                        }
                    }
                ),
                encoding="utf-8",
            )
            (history_dir / "rank_conformance_20260220_010000.json").write_text(
                json.dumps({"rank_map": {"alpha": "S", "beta": "A"}}),
                encoding="utf-8",
            )
            (history_dir / "rank_conformance_20260219_233000.json").write_text(
                json.dumps({"rank_map": {"alpha": "B", "beta": "S"}}),
                encoding="utf-8",
            )

            previous = rc._load_previous_rank_map(
                latest_report,
                history_dir=history_dir,
                now_utc=datetime(2026, 2, 20, 12, 0, tzinfo=timezone.utc),
            )
            self.assertEqual({"alpha": "B", "beta": "S"}, previous)

    def test_load_previous_rank_map_falls_back_to_latest_when_no_previous_day_snapshot(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            latest_report = base / "rank_conformance_latest.json"
            history_dir = base / "history"
            history_dir.mkdir(parents=True, exist_ok=True)

            latest_report.write_text(
                json.dumps({"rank_map": {"alpha": "A"}}),
                encoding="utf-8",
            )
            (history_dir / "rank_conformance_20260220_010000.json").write_text(
                json.dumps({"rank_map": {"alpha": "S"}}),
                encoding="utf-8",
            )

            previous = rc._load_previous_rank_map(
                latest_report,
                history_dir=history_dir,
                now_utc=datetime(2026, 2, 20, 12, 0, tzinfo=timezone.utc),
            )
            self.assertEqual({"alpha": "A"}, previous)


if __name__ == "__main__":
    unittest.main()
