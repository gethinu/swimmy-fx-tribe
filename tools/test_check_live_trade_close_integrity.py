import importlib.util
import sqlite3
import sys
import time
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch


MODULE_PATH = Path(__file__).with_name("check_live_trade_close_integrity.py")


def load_module():
    spec = importlib.util.spec_from_file_location("check_live_trade_close_integrity", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def invoke_main(mod, args: list[str]) -> tuple[int, str]:
    argv = ["check_live_trade_close_integrity.py", *args]
    with patch.object(mod.sys, "argv", argv), patch("builtins.print") as print_mock:
        rc = int(mod.main())
    output = "\n".join(" ".join(str(part) for part in call.args) for call in print_mock.call_args_list)
    return rc, output


class CheckLiveTradeCloseIntegrityTests(unittest.TestCase):
    def test_main_fails_when_db_missing(self) -> None:
        mod = load_module()
        with TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            db_path = tmp / "missing.db"
            log_path = tmp / "swimmy.log"
            log_path.write_text("", encoding="utf-8")

            rc, output = invoke_main(
                mod,
                [
                    "--db",
                    str(db_path),
                    "--log",
                    str(log_path),
                    "--lookback-minutes",
                    "60",
                ],
            )
            self.assertEqual(1, rc)
            self.assertIn("[FAIL] DB not found", output)

    def test_main_fails_when_trade_logs_table_missing(self) -> None:
        mod = load_module()
        with TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            db_path = tmp / "swimmy.db"
            log_path = tmp / "swimmy.log"
            log_path.write_text("", encoding="utf-8")
            sqlite3.connect(db_path).close()

            rc, output = invoke_main(
                mod,
                [
                    "--db",
                    str(db_path),
                    "--log",
                    str(log_path),
                    "--lookback-minutes",
                    "60",
                ],
            )
            self.assertEqual(1, rc)
            self.assertIn("[FAIL] trade_logs query failed", output)

    def test_after_id_can_exclude_historical_bad_rows(self) -> None:
        mod = load_module()
        with TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            db_path = tmp / "swimmy.db"
            log_path = tmp / "swimmy.log"
            log_path.write_text("", encoding="utf-8")

            with sqlite3.connect(db_path) as con:
                cur = con.cursor()
                cur.execute(
                    """
                    CREATE TABLE trade_logs (
                      id INTEGER PRIMARY KEY AUTOINCREMENT,
                      timestamp INTEGER,
                      strategy_name TEXT,
                      symbol TEXT,
                      direction TEXT,
                      category TEXT,
                      pnl REAL
                    )
                    """
                )
                now_ut = int(time.time()) + int(mod.CL_UNIVERSAL_OFFSET)
                cur.execute(
                    """
                    INSERT INTO trade_logs(timestamp, strategy_name, symbol, direction, category, pnl)
                    VALUES (?, ?, ?, ?, ?, ?)
                    """,
                    (now_ut, "unknown", "USDJPY", "UNKNOWN", "NIL", -10.0),
                )
                con.commit()

            rc_bad, output_bad = invoke_main(
                mod,
                [
                    "--db",
                    str(db_path),
                    "--log",
                    str(log_path),
                    "--lookback-minutes",
                    "60",
                ],
            )
            self.assertEqual(1, rc_bad)
            self.assertIn("rows=1 bad=1", output_bad)

            rc_filtered, output_filtered = invoke_main(
                mod,
                [
                    "--db",
                    str(db_path),
                    "--log",
                    str(log_path),
                    "--lookback-minutes",
                    "60",
                    "--after-id",
                    "1",
                ],
            )
            self.assertEqual(0, rc_filtered)
            self.assertIn("after_id=1 rows=0 bad=0", output_filtered)


if __name__ == "__main__":
    unittest.main()
