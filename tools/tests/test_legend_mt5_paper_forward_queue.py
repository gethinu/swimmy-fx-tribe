import json
import tempfile
import unittest
from pathlib import Path

from tools import legend_mt5_paper_forward_queue as queue_tool


def _write_json(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


class TestLegendMt5PaperForwardQueue(unittest.TestCase):
    def test_build_queue_sorts_by_shortlist_rank_and_keeps_only_required_candidates(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            bridge_path = root / "legend_evidence_bridge.json"
            _write_json(
                bridge_path,
                {
                    "schema_version": 1,
                    "generated_at_utc": "2026-03-08T06:03:14Z",
                    "shortlist": [
                        "legend-macd-above-zero-cross",
                        "historical-s-bred940-trend-core",
                        "legend-pullback-breakout",
                    ],
                    "strategies": [
                        {
                            "strategy_name": "legend-pullback-breakout",
                            "shortlist_rank": 3,
                            "shortlist_role": "third_higher_turnover_line",
                            "job": {"symbol": "USDJPY", "period": "H1"},
                            "contract_alignment": {"requires_paper_forward": True},
                        },
                        {
                            "strategy_name": "legend-macd-above-zero-cross",
                            "shortlist_rank": 1,
                            "shortlist_role": "primary_stability_first",
                            "job": {"symbol": "USDJPY", "period": "H4"},
                            "contract_alignment": {"requires_paper_forward": True},
                        },
                        {
                            "strategy_name": "skip-me",
                            "shortlist_rank": 99,
                            "shortlist_role": "not_queued",
                            "job": {"symbol": "USDJPY", "period": "M15"},
                            "contract_alignment": {"requires_paper_forward": False},
                        },
                        {
                            "strategy_name": "historical-s-bred940-trend-core",
                            "shortlist_rank": 2,
                            "shortlist_role": "second_adaptive_contender",
                            "job": {"symbol": "USDJPY", "period": "H1"},
                            "contract_alignment": {"requires_paper_forward": True},
                        },
                    ],
                },
            )

            report = queue_tool.build_paper_forward_queue(bridge_path)

        items = report["items"]
        self.assertEqual(
            [
                "legend-macd-above-zero-cross",
                "historical-s-bred940-trend-core",
                "legend-pullback-breakout",
            ],
            [item["strategy_name"] for item in items],
        )
        self.assertEqual(["QUEUED", "QUEUED", "QUEUED"], [item["status"] for item in items])
        for item in items:
            self.assertEqual(30, item["target_forward_days"])
            self.assertEqual(300, item["target_forward_trades"])
            self.assertEqual(0.70, item["target_forward_sharpe"])
            self.assertEqual(1.50, item["target_forward_pf"])
            self.assertEqual("queue_paper_forward_probe", item["queue_reason"])


if __name__ == "__main__":
    unittest.main()
