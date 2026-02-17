import importlib.util
import json
import sys
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace
from unittest.mock import patch


MODULE_PATH = Path(__file__).with_name("pattern_backend_calibration_status.py")


def load_module():
    spec = importlib.util.spec_from_file_location("pattern_backend_calibration_status", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class PatternBackendCalibrationStatusTests(unittest.TestCase):
    def test_parse_iso8601_supports_z_suffix(self) -> None:
        mod = load_module()
        dt = mod.parse_iso8601("2026-02-17T06:00:00Z")
        self.assertEqual(timezone.utc, dt.tzinfo)
        self.assertEqual(2026, dt.year)
        self.assertEqual(2, dt.month)

    def test_evaluate_status_ok_when_report_fresh_and_timer_enabled(self) -> None:
        mod = load_module()
        with TemporaryDirectory() as tmpdir:
            report_path = Path(tmpdir) / "report.json"
            now = datetime.now(timezone.utc)
            payload = {
                "generated_at": now.isoformat(),
                "status": {"ok": True, "combos_evaluated": 3},
                "global": {"best_vector_weight": 0.25},
            }
            report_path.write_text(json.dumps(payload), encoding="utf-8")

            with patch.object(mod, "check_timer_enabled", return_value=("enabled", "enabled")):
                st = mod.evaluate_status(report_path, max_age_seconds=3600, timer_unit="x.timer", check_timer=True)

            self.assertTrue(bool(st["ok"]))
            self.assertEqual("enabled", st["timer"]["state"])
            self.assertEqual(0.25, float(st["report"]["best_vector_weight"]))

    def test_evaluate_status_fails_on_stale_or_disabled(self) -> None:
        mod = load_module()
        with TemporaryDirectory() as tmpdir:
            report_path = Path(tmpdir) / "report.json"
            stale = datetime.now(timezone.utc) - timedelta(days=5)
            payload = {
                "generated_at": stale.isoformat(),
                "status": {"ok": True, "combos_evaluated": 1},
                "global": {"best_vector_weight": 0.1},
            }
            report_path.write_text(json.dumps(payload), encoding="utf-8")

            with patch.object(mod, "check_timer_enabled", return_value=("disabled", "disabled")):
                st = mod.evaluate_status(report_path, max_age_seconds=3600, timer_unit="x.timer", check_timer=True)

            self.assertFalse(bool(st["ok"]))
            text = " | ".join(st["issues"])
            self.assertIn("report stale", text)
            self.assertIn("timer disabled", text)

    def test_main_returns_nonzero_with_fail_on_problem(self) -> None:
        mod = load_module()
        with TemporaryDirectory() as tmpdir:
            report_path = Path(tmpdir) / "missing.json"
            with patch.object(
                mod,
                "check_timer_enabled",
                return_value=("disabled", "disabled"),
            ):
                argv = [
                    "pattern_backend_calibration_status.py",
                    "--report",
                    str(report_path),
                    "--fail-on-problem",
                ]
                with patch.object(mod.sys, "argv", argv), patch("builtins.print"):
                    rc = mod.main()
            self.assertNotEqual(0, int(rc))


if __name__ == "__main__":
    unittest.main()

