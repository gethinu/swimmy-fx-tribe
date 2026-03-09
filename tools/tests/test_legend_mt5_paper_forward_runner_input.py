import json
import tempfile
import unittest
from pathlib import Path

from tools import legend_mt5_paper_forward_runner_input as runner_input


def _write_json(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


class TestLegendMt5PaperForwardRunnerInput(unittest.TestCase):
    def test_build_runner_input_keeps_rank_order_and_expands_paper_fields(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            queue_path = root / "legend_paper_forward_queue.json"
            _write_json(
                queue_path,
                {
                    "schema_version": 1,
                    "generated_at_utc": "2026-03-08T06:10:53Z",
                    "bridge_source": "/tmp/bridge.json",
                    "targets": {
                        "target_forward_days": 30,
                        "target_forward_trades": 300,
                        "target_forward_sharpe": 0.70,
                        "target_forward_pf": 1.50,
                    },
                    "items": [
                        {
                            "strategy_name": "legend-pullback-breakout",
                            "shortlist_rank": 3,
                            "shortlist_role": "third_higher_turnover_line",
                            "symbol": "USDJPY",
                            "timeframe": "H1",
                            "bridge_source": "/tmp/bridge.json",
                            "queue_reason": "queue_paper_forward_probe",
                            "status": "QUEUED",
                            "target_forward_days": 30,
                            "target_forward_trades": 300,
                            "target_forward_sharpe": 0.70,
                            "target_forward_pf": 1.50,
                        },
                        {
                            "strategy_name": "legend-macd-above-zero-cross",
                            "shortlist_rank": 1,
                            "shortlist_role": "primary_stability_first",
                            "symbol": "USDJPY",
                            "timeframe": "H4",
                            "bridge_source": "/tmp/bridge.json",
                            "queue_reason": "queue_paper_forward_probe",
                            "status": "QUEUED",
                            "target_forward_days": 30,
                            "target_forward_trades": 300,
                            "target_forward_sharpe": 0.70,
                            "target_forward_pf": 1.50,
                        },
                    ],
                },
            )

            report = runner_input.build_runner_input(queue_path)

        items = report["items"]
        self.assertEqual(
            ["legend-macd-above-zero-cross", "legend-pullback-breakout"],
            [item["strategy_name"] for item in items],
        )
        self.assertEqual(
            ["legend-pf-r1-legend-macd-above-zero-cross", "legend-pf-r3-legend-pullback-breakout"],
            [item["paper_run_id"] for item in items],
        )
        self.assertEqual(
            ["legend_paper_forward_legend_macd_above_zero_cross", "legend_paper_forward_legend_pullback_breakout"],
            [item["paper_comment_prefix"] for item in items],
        )
        self.assertEqual(["PAPER", "PAPER"], [item["execution_mode"] for item in items])
        self.assertEqual(
            ["READY_FOR_PAPER_FORWARD", "READY_FOR_PAPER_FORWARD"],
            [item["status"] for item in items],
        )


if __name__ == "__main__":
    unittest.main()
