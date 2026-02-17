import importlib.util
import json
import sqlite3
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch


MODULE_PATH = Path(__file__).with_name("check_order_timeframe_consistency.py")


def load_module():
    spec = importlib.util.spec_from_file_location("check_order_timeframe_consistency", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class CheckOrderTimeframeConsistencyTests(unittest.TestCase):
    def test_parse_tf_minutes_supports_label_and_numeric(self) -> None:
        mod = load_module()
        self.assertEqual(10080, mod.parse_tf_minutes("W1"))
        self.assertEqual(240, mod.parse_tf_minutes("240"))
        self.assertEqual(60, mod.parse_tf_minutes("H1"))
        self.assertEqual(43200, mod.parse_tf_minutes("MN"))
        self.assertEqual(43200, mod.parse_tf_minutes("MN1"))
        self.assertIsNone(mod.parse_tf_minutes("M0"))
        self.assertIsNone(mod.parse_tf_minutes("UNKNOWN"))

    def test_detect_timeframe_issues_flags_mismatch_and_missing_strategy(self) -> None:
        mod = load_module()
        events = [
            {
                "line": 10,
                "timestamp": "2026-02-17T17:04:10",
                "strategy": "Aggressive-Reversal",
                "timeframe": "M1",
                "symbol": "GBPUSD",
                "direction": "SELL",
            },
            {
                "line": 11,
                "timestamp": "2026-02-17T17:04:20",
                "strategy": "UT-NOT-IN-DB",
                "timeframe": "H1",
                "symbol": "EURUSD",
                "direction": "BUY",
            },
        ]
        expected = {"Aggressive-Reversal": 10080}
        issues = mod.detect_timeframe_issues(events, expected)
        self.assertEqual(2, len(issues))

        mismatch = issues[0]
        self.assertEqual("timeframe_mismatch", mismatch["type"])
        self.assertEqual("Aggressive-Reversal", mismatch["strategy"])
        self.assertEqual(1, mismatch["actual_minutes"])
        self.assertEqual(10080, mismatch["expected_minutes"])

        missing = issues[1]
        self.assertEqual("strategy_not_found", missing["type"])
        self.assertEqual("UT-NOT-IN-DB", missing["strategy"])

    def test_main_returns_nonzero_with_fail_on_issues(self) -> None:
        mod = load_module()
        with TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            db_path = tmp / "swimmy.db"
            log_path = tmp / "swimmy.json.log"

            conn = sqlite3.connect(db_path)
            cur = conn.cursor()
            cur.execute("CREATE TABLE strategies (name TEXT PRIMARY KEY, timeframe INTEGER)")
            cur.execute("INSERT INTO strategies(name, timeframe) VALUES (?, ?)", ("Aggressive-Reversal", 10080))
            conn.commit()
            conn.close()

            rows = [
                {
                    "schema_version": 1,
                    "timestamp": "2026-02-17T17:05:00",
                    "event_type": "execution.order_submitted",
                    "data": {
                        "strategy": "Aggressive-Reversal",
                        "timeframe": "M1",
                        "symbol": "GBPUSD",
                        "direction": "SELL",
                    },
                }
            ]
            with log_path.open("w", encoding="utf-8") as f:
                for row in rows:
                    f.write(json.dumps(row, ensure_ascii=False) + "\n")

            argv = [
                "check_order_timeframe_consistency.py",
                "--db",
                str(db_path),
                "--log",
                str(log_path),
                "--fail-on-issues",
            ]
            with patch.object(mod.sys, "argv", argv), patch("builtins.print"):
                rc = mod.main()
            self.assertNotEqual(0, int(rc))

    def test_main_lookback_minutes_filters_old_events(self) -> None:
        mod = load_module()
        with TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            db_path = tmp / "swimmy.db"
            log_path = tmp / "swimmy.json.log"

            conn = sqlite3.connect(db_path)
            cur = conn.cursor()
            cur.execute("CREATE TABLE strategies (name TEXT PRIMARY KEY, timeframe INTEGER)")
            cur.execute("INSERT INTO strategies(name, timeframe) VALUES (?, ?)", ("Aggressive-Reversal", 10080))
            conn.commit()
            conn.close()

            rows = [
                {
                    "schema_version": 1,
                    "timestamp": "2000-01-01T00:00:00",
                    "event_type": "execution.order_submitted",
                    "data": {
                        "strategy": "Aggressive-Reversal",
                        "timeframe": "M1",
                        "symbol": "GBPUSD",
                        "direction": "SELL",
                    },
                }
            ]
            with log_path.open("w", encoding="utf-8") as f:
                for row in rows:
                    f.write(json.dumps(row, ensure_ascii=False) + "\n")

            argv = [
                "check_order_timeframe_consistency.py",
                "--db",
                str(db_path),
                "--log",
                str(log_path),
                "--lookback-minutes",
                "1",
                "--fail-on-issues",
            ]
            with patch.object(mod.sys, "argv", argv), patch("builtins.print") as print_mock:
                rc = mod.main()
            self.assertEqual(0, int(rc))
            printed = " ".join(
                " ".join(str(part) for part in call.args) for call in print_mock.call_args_list
            )
            self.assertIn("orders=0", printed)


if __name__ == "__main__":
    unittest.main()
