import importlib.util
import json
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace
from unittest.mock import patch


MODULE_PATH = Path(__file__).with_name("polymarket_weather_calibration_update.py")


def load_module():
    spec = importlib.util.spec_from_file_location("polymarket_weather_calibration_update", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TestPolymarketWeatherCalibrationUpdate(unittest.TestCase):
    def test_normalize_forecast_models(self) -> None:
        mod = load_module()
        defaults = mod.normalize_forecast_models([])
        self.assertGreaterEqual(len(defaults), 1)
        self.assertEqual(["a", "b", "c"], mod.normalize_forecast_models(["a,b", "b", " c "]))

    def test_parse_json_from_stdout_fallback(self) -> None:
        mod = load_module()
        payload = mod.parse_json_from_stdout("noise\n" + json.dumps({"accuracy": {"brier": 0.1}}))
        self.assertEqual(0.1, float(payload["accuracy"]["brier"]))

    def test_build_backtest_cmd(self) -> None:
        mod = load_module()
        cmd = mod.build_backtest_cmd(
            python_bin="python3",
            backtest_script=Path("tools/polymarket_weather_backtest.py"),
            start_date=mod.date(2026, 1, 1),
            end_date=mod.date(2026, 1, 31),
            forecast_models=["gfs", "ecmwf"],
            write_calibration="/tmp/cal.json",
            calibration_file="/tmp/cal.json",
            write_report="/tmp/report.json",
        )
        text = " ".join(cmd)
        self.assertIn("--skip-trading", text)
        self.assertIn("--write-calibration /tmp/cal.json", text)
        self.assertIn("--calibration-file /tmp/cal.json", text)
        self.assertIn("--write-report /tmp/report.json", text)
        self.assertIn("--forecast-model gfs", text)
        self.assertIn("--forecast-model ecmwf", text)

    def test_main_writes_summary_and_returns_ok(self) -> None:
        mod = load_module()
        with TemporaryDirectory() as tmpdir:
            cal_path = Path(tmpdir) / "calibration.json"
            report_path = Path(tmpdir) / "summary.json"

            # three runs: train, eval-uncal, eval-cal
            train_stdout = json.dumps({"accuracy": {"brier": 0.10, "logloss": 0.33, "resolved_markets": 100}})
            base_stdout = json.dumps({"accuracy": {"brier": 0.12, "logloss": 0.36, "resolved_markets": 40}})
            calib_stdout = json.dumps({"accuracy": {"brier": 0.11, "logloss": 0.34, "resolved_markets": 40}})
            fake_runs = [
                SimpleNamespace(returncode=0, stdout=train_stdout, stderr=""),
                SimpleNamespace(returncode=0, stdout=base_stdout, stderr=""),
                SimpleNamespace(returncode=0, stdout=calib_stdout, stderr=""),
            ]

            with patch.object(mod.subprocess, "run", side_effect=fake_runs):
                argv = [
                    "polymarket_weather_calibration_update.py",
                    "--calibration-file",
                    str(cal_path),
                    "--write-report",
                    str(report_path),
                    "--end-date",
                    "2026-02-14",
                    "--train-days",
                    "60",
                    "--eval-days",
                    "30",
                ]
                with patch.object(mod.sys, "argv", argv):
                    rc = mod.main()

            self.assertEqual(0, int(rc))
            self.assertTrue(report_path.exists())
            summary = json.loads(report_path.read_text(encoding="utf-8"))
            self.assertTrue(bool(summary["status"]["ok"]))
            self.assertEqual(-0.01, float(summary["metrics"]["delta_calibrated_minus_uncalibrated"]["brier"]))


if __name__ == "__main__":
    unittest.main()
