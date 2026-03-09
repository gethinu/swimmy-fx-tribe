import json
import tempfile
import unittest
from pathlib import Path

from tools import legend_mt5_paper_forward_status as status_tool


def _write_json(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


class TestLegendMt5PaperForwardStatus(unittest.TestCase):
    def test_status_aggregates_manifest_progress(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            manifest_path = root / "runs" / "legend-pf-r1-legend-macd-above-zero-cross" / "run_manifest.json"
            stage_report_path = root / "legend_paper_forward_stage.json"
            _write_json(
                manifest_path,
                {
                    "paper_run_id": "legend-pf-r1-legend-macd-above-zero-cross",
                    "strategy_name": "legend-macd-above-zero-cross",
                    "execution_mode": "PAPER",
                    "paper_comment_prefix": "legend_paper_forward_legend_macd_above_zero_cross",
                    "target_forward_days": 30,
                    "target_forward_trades": 300,
                    "target_forward_sharpe": 0.70,
                    "target_forward_pf": 1.50,
                    "observed_forward_days": 15,
                    "observed_forward_trades": 120,
                    "observed_forward_sharpe": 0.35,
                    "observed_forward_pf": 1.20,
                    "status": "RUNNING",
                    "status_reason": "paper_forward_in_progress",
                },
            )
            _write_json(
                stage_report_path,
                {
                    "schema_version": 1,
                    "generated_at_utc": "2026-03-08T06:20:00Z",
                    "runner_input_source": "/tmp/runner.json",
                    "items": [
                        {
                            "paper_run_id": "legend-pf-r1-legend-macd-above-zero-cross",
                            "strategy_name": "legend-macd-above-zero-cross",
                            "manifest_path": str(manifest_path),
                            "planned_run_root": str(manifest_path.parent),
                            "status": "STAGED",
                        }
                    ],
                    "status_counts": {"STAGED": 1},
                },
            )

            report = status_tool.build_status_report(stage_report_path)

        self.assertEqual({"RUNNING": 1}, report["status_counts"])
        self.assertEqual(0, report["ready_count"])
        self.assertEqual(1, report["running_count"])
        self.assertEqual(0, report["completed_count"])
        self.assertEqual(0.5, report["progress_summary"]["days_progress_ratio_mean"])
        self.assertEqual(0.4, report["progress_summary"]["trades_progress_ratio_mean"])
        self.assertEqual(0.5, report["progress_summary"]["sharpe_progress_ratio_mean"])
        self.assertEqual(0.8, report["progress_summary"]["pf_progress_ratio_mean"])


if __name__ == "__main__":
    unittest.main()
