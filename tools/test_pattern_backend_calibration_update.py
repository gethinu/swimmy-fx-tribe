import importlib.util
import json
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch


MODULE_PATH = Path(__file__).with_name("pattern_backend_calibration_update.py")


def load_module():
    spec = importlib.util.spec_from_file_location("pattern_backend_calibration_update", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TestPatternBackendCalibrationUpdate(unittest.TestCase):
    def test_normalize_multi_values(self) -> None:
        mod = load_module()
        vals = mod.normalize_multi_values(["eurusd, gbpusd", "USDJPY", "eurusd"], defaults=["XAUUSD"])
        self.assertEqual(["EURUSD", "GBPUSD", "USDJPY"], vals)
        self.assertEqual(["XAUUSD"], mod.normalize_multi_values([], defaults=["XAUUSD"]))

    def test_build_weight_grid(self) -> None:
        mod = load_module()
        grid = mod.build_weight_grid(0.2)
        self.assertEqual(0.0, grid[0])
        self.assertEqual(1.0, grid[-1])
        self.assertTrue(all(0.0 <= x <= 1.0 for x in grid))
        self.assertEqual(sorted(set(grid)), grid)

    def test_choose_best_weight(self) -> None:
        mod = load_module()
        rows = [
            ("UP", {"p_up": 0.8, "p_down": 0.1, "p_flat": 0.1}, {"p_up": 0.2, "p_down": 0.7, "p_flat": 0.1}),
            ("DOWN", {"p_up": 0.7, "p_down": 0.2, "p_flat": 0.1}, {"p_up": 0.1, "p_down": 0.8, "p_flat": 0.1}),
        ]
        best = mod.choose_best_weight(rows, [0.0, 0.5, 1.0])
        self.assertEqual(0.5, float(best["best_weight"]))
        self.assertGreaterEqual(float(best["best_accuracy"]), 0.5)

    def test_extract_symbol_timeframe_weights(self) -> None:
        mod = load_module()
        rows = [
            {"symbol": "usdJpy", "timeframe": "h1", "best_vector_weight": 0.25},
            {"symbol": "EURUSD", "timeframe": "M15", "best_vector_weight": "0.4"},
            {"symbol": "BAD", "timeframe": "", "best_vector_weight": 0.3},
            {"symbol": "GBPUSD", "timeframe": "H4", "best_vector_weight": 1.2},
        ]
        out = mod.extract_symbol_timeframe_weights(rows)
        self.assertEqual({"EURUSD:M15": 0.4, "USDJPY:H1": 0.25}, out)

    def test_main_writes_report_and_weight(self) -> None:
        mod = load_module()
        with TemporaryDirectory() as tmpdir:
            base = Path(tmpdir)
            report_path = base / "report.json"
            weight_path = base / "ensemble_weight.json"

            fake_summary = {
                "schema_version": 1,
                "generated_at": "2026-02-17T00:00:00+00:00",
                "status": {"ok": True, "combos_evaluated": 1},
                "global": {"best_vector_weight": 0.25},
                "combo_results": [
                    {"symbol": "USDJPY", "timeframe": "H1", "best_vector_weight": 0.1},
                    {"symbol": "EURUSD", "timeframe": "M15", "best_vector_weight": 0.4},
                ],
            }

            with patch.object(mod, "run_calibration", return_value=fake_summary), patch("builtins.print"):
                argv = [
                    "pattern_backend_calibration_update.py",
                    "--write-report",
                    str(report_path),
                    "--write-weight",
                    str(weight_path),
                ]
                with patch.object(mod.sys, "argv", argv):
                    rc = mod.main()

            self.assertEqual(0, int(rc))
            self.assertTrue(report_path.exists())
            self.assertTrue(weight_path.exists())
            report = json.loads(report_path.read_text(encoding="utf-8"))
            weight = json.loads(weight_path.read_text(encoding="utf-8"))
            self.assertEqual(1, int(report["schema_version"]))
            self.assertEqual(0.25, float(weight["vector_weight"]))
            self.assertEqual(0.75, float(weight["clip_weight"]))
            self.assertIn("symbol_timeframe_weights", weight)
            self.assertEqual(0.1, float(weight["symbol_timeframe_weights"]["USDJPY:H1"]))
            self.assertEqual(0.4, float(weight["symbol_timeframe_weights"]["EURUSD:M15"]))


if __name__ == "__main__":
    unittest.main()
