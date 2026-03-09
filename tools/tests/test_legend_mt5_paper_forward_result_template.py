import json
import tempfile
import unittest
from pathlib import Path

from tools import legend_mt5_paper_forward_result_template as template_tool


def _write_json(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


class TestLegendMt5PaperForwardResultTemplate(unittest.TestCase):
    def test_build_result_template_sorts_by_rank_and_emits_apply_shape_defaults(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            stage_report_path = root / "legend_paper_forward_stage.json"
            _write_json(
                stage_report_path,
                {
                    "schema_version": 1,
                    "generated_at_utc": "2026-03-08T06:25:50Z",
                    "runner_input_source": "/tmp/runner.json",
                    "items": [
                        {
                            "paper_run_id": "legend-pf-r3-legend-pullback-breakout",
                            "strategy_name": "legend-pullback-breakout",
                            "shortlist_rank": 3,
                            "manifest_path": "/tmp/r3/run_manifest.json",
                            "planned_run_root": "/tmp/r3",
                            "execution_mode": "PAPER",
                            "target_forward_days": 30,
                            "target_forward_trades": 300,
                            "target_forward_sharpe": 0.70,
                            "target_forward_pf": 1.50,
                        },
                        {
                            "paper_run_id": "legend-pf-r1-legend-macd-above-zero-cross",
                            "strategy_name": "legend-macd-above-zero-cross",
                            "shortlist_rank": 1,
                            "manifest_path": "/tmp/r1/run_manifest.json",
                            "planned_run_root": "/tmp/r1",
                            "execution_mode": "PAPER",
                            "target_forward_days": 30,
                            "target_forward_trades": 300,
                            "target_forward_sharpe": 0.70,
                            "target_forward_pf": 1.50,
                        },
                    ],
                },
            )

            report = template_tool.build_result_template(stage_report_path)

        self.assertEqual(
            ["legend-pf-r1-legend-macd-above-zero-cross", "legend-pf-r3-legend-pullback-breakout"],
            [item["paper_run_id"] for item in report["items"]],
        )
        self.assertEqual("PAPER", report["items"][0]["execution_mode"])
        self.assertEqual("/tmp/r1/run_manifest.json", report["items"][0]["manifest_path"])
        self.assertEqual("/tmp/r1", report["items"][0]["planned_run_root"])
        self.assertEqual(0, report["items"][0]["observed_forward_days"])
        self.assertEqual(0, report["items"][0]["observed_forward_trades"])
        self.assertIsNone(report["items"][0]["observed_forward_sharpe"])
        self.assertIsNone(report["items"][0]["observed_forward_pf"])
        self.assertEqual("STAGED", report["items"][0]["status"])
        self.assertEqual("fill_from_executor", report["items"][0]["status_reason"])


if __name__ == "__main__":
    unittest.main()
