import json
import tempfile
import unittest
from pathlib import Path

from tools import legend_mt5_paper_forward_apply_results as apply_tool


def _write_json(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


class TestLegendMt5PaperForwardApplyResults(unittest.TestCase):
    def test_apply_results_updates_matching_manifest_and_reports_missing_runs(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            stage_report_path = root / "legend_paper_forward_stage.json"
            results_path = root / "executor_results.json"
            apply_report_path = root / "legend_paper_forward_apply_results.json"
            manifest_a = root / "runs" / "legend-pf-r1-legend-macd-above-zero-cross" / "run_manifest.json"
            manifest_b = root / "runs" / "legend-pf-r2-historical-s-bred940-trend-core" / "run_manifest.json"

            _write_json(
                manifest_a,
                {
                    "paper_run_id": "legend-pf-r1-legend-macd-above-zero-cross",
                    "strategy_name": "legend-macd-above-zero-cross",
                    "execution_mode": "PAPER",
                    "paper_comment_prefix": "legend_paper_forward_legend_macd_above_zero_cross",
                    "target_forward_days": 30,
                    "target_forward_trades": 300,
                    "target_forward_sharpe": 0.70,
                    "target_forward_pf": 1.50,
                    "observed_forward_days": 0,
                    "observed_forward_trades": 0,
                    "observed_forward_sharpe": None,
                    "observed_forward_pf": None,
                    "status": "STAGED",
                    "status_reason": "awaiting_paper_forward_executor",
                },
            )
            _write_json(
                manifest_b,
                {
                    "paper_run_id": "legend-pf-r2-historical-s-bred940-trend-core",
                    "strategy_name": "historical-s-bred940-trend-core",
                    "execution_mode": "PAPER",
                    "paper_comment_prefix": "legend_paper_forward_historical_s_bred940_trend_core",
                    "target_forward_days": 30,
                    "target_forward_trades": 300,
                    "target_forward_sharpe": 0.70,
                    "target_forward_pf": 1.50,
                    "observed_forward_days": 0,
                    "observed_forward_trades": 0,
                    "observed_forward_sharpe": None,
                    "observed_forward_pf": None,
                    "status": "STAGED",
                    "status_reason": "awaiting_paper_forward_executor",
                },
            )
            _write_json(
                stage_report_path,
                {
                    "schema_version": 1,
                    "generated_at_utc": "2026-03-08T06:25:50Z",
                    "runner_input_source": "/tmp/runner.json",
                    "items": [
                        {
                            "paper_run_id": "legend-pf-r1-legend-macd-above-zero-cross",
                            "strategy_name": "legend-macd-above-zero-cross",
                            "shortlist_rank": 1,
                            "manifest_path": str(manifest_a),
                            "planned_run_root": str(manifest_a.parent),
                            "status": "STAGED",
                        },
                        {
                            "paper_run_id": "legend-pf-r2-historical-s-bred940-trend-core",
                            "strategy_name": "historical-s-bred940-trend-core",
                            "shortlist_rank": 2,
                            "manifest_path": str(manifest_b),
                            "planned_run_root": str(manifest_b.parent),
                            "status": "STAGED",
                        },
                    ],
                    "status_counts": {"STAGED": 2},
                },
            )
            _write_json(
                results_path,
                {
                    "schema_version": 1,
                    "generated_at_utc": "2026-03-08T06:30:00Z",
                    "items": [
                        {
                            "paper_run_id": "legend-pf-r1-legend-macd-above-zero-cross",
                            "observed_forward_days": 12,
                            "observed_forward_trades": 150,
                            "observed_forward_sharpe": 0.81,
                            "observed_forward_pf": 1.62,
                            "status": "RUNNING",
                            "status_reason": "paper_forward_midway_update",
                        },
                        {
                            "paper_run_id": "legend-pf-r9-missing",
                            "observed_forward_days": 8,
                            "observed_forward_trades": 90,
                            "observed_forward_sharpe": 0.40,
                            "observed_forward_pf": 1.10,
                            "status": "RUNNING",
                            "status_reason": "should_be_missing",
                        },
                    ],
                },
            )

            report = apply_tool.build_apply_report(stage_report_path, results_path)
            apply_tool.write_apply_report(report, apply_report_path)

            updated_manifest_a = json.loads(manifest_a.read_text(encoding="utf-8"))
            untouched_manifest_b = json.loads(manifest_b.read_text(encoding="utf-8"))

        self.assertEqual(1, report["updated_count"])
        self.assertEqual(1, report["missing_count"])
        self.assertEqual({"RUNNING": 1, "STAGED": 1}, report["status_counts"])
        self.assertEqual("RUNNING", updated_manifest_a["status"])
        self.assertEqual("paper_forward_midway_update", updated_manifest_a["status_reason"])
        self.assertEqual(150, updated_manifest_a["observed_forward_trades"])
        self.assertEqual("STAGED", untouched_manifest_b["status"])
        self.assertEqual(
            ["updated", "unchanged", "missing"],
            [item["apply_status"] for item in report["items"]],
        )


if __name__ == "__main__":
    unittest.main()
