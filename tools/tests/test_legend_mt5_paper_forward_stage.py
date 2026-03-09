import json
import tempfile
import unittest
from pathlib import Path

from tools import legend_mt5_paper_forward_stage as stage_tool


def _write_json(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


class TestLegendMt5PaperForwardStage(unittest.TestCase):
    def test_stage_writes_manifest_and_staged_report(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            runner_input_path = root / "legend_paper_forward_runner_input.json"
            report_output_path = root / "legend_paper_forward_stage.json"
            planned_run_root = root / "runs" / "legend-pf-r1-legend-macd-above-zero-cross"
            _write_json(
                runner_input_path,
                {
                    "schema_version": 1,
                    "generated_at_utc": "2026-03-08T06:14:39Z",
                    "queue_source": "/tmp/queue.json",
                    "bridge_source": "/tmp/bridge.json",
                    "items": [
                        {
                            "paper_run_id": "legend-pf-r1-legend-macd-above-zero-cross",
                            "strategy_name": "legend-macd-above-zero-cross",
                            "shortlist_rank": 1,
                            "shortlist_role": "primary_stability_first",
                            "symbol": "USDJPY",
                            "timeframe": "H4",
                            "execution_mode": "PAPER",
                            "paper_comment_prefix": "legend_paper_forward_legend_macd_above_zero_cross",
                            "planned_run_root": str(planned_run_root),
                            "target_forward_days": 30,
                            "target_forward_trades": 300,
                            "target_forward_sharpe": 0.70,
                            "target_forward_pf": 1.50,
                            "queue_source": "/tmp/queue.json",
                            "bridge_source": "/tmp/bridge.json",
                            "status": "READY_FOR_PAPER_FORWARD",
                        }
                    ],
                },
            )

            report = stage_tool.build_stage_report(runner_input_path)
            stage_tool.write_stage_report(report, report_output_path)

            manifest_path = planned_run_root / "run_manifest.json"
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))

        self.assertEqual("STAGED", report["items"][0]["status"])
        self.assertEqual({"STAGED": 1}, report["status_counts"])
        self.assertEqual("STAGED", manifest["status"])
        self.assertEqual("awaiting_paper_forward_executor", manifest["status_reason"])
        self.assertEqual(0, manifest["observed_forward_days"])
        self.assertEqual(0, manifest["observed_forward_trades"])
        self.assertIsNone(manifest["observed_forward_sharpe"])
        self.assertIsNone(manifest["observed_forward_pf"])


if __name__ == "__main__":
    unittest.main()
