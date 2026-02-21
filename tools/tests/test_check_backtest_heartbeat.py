import os
import tempfile
import unittest
from pathlib import Path

from tools import check_backtest_heartbeat as hb


class TestCheckBacktestHeartbeat(unittest.TestCase):
    def _write(self, path: Path, text: str) -> None:
        path.write_text(text, encoding="utf-8")

    def test_parse_heartbeat_line(self) -> None:
        line = (
            "[BACKTEST-SVC] \u2764\ufe0f HEARTBEAT inflight=0/6 recv=10 submit=10 done=10 sent=10 "
            "rx_age=55s tx_age=54s"
        )
        row = hb.parse_heartbeat_line(line)
        self.assertIsNotNone(row)
        assert row is not None
        self.assertEqual(55, row["rx_age_seconds"])
        self.assertEqual(54, row["tx_age_seconds"])
        self.assertEqual("0/6", row["inflight"])

    def test_check_status_ok(self) -> None:
        with tempfile.TemporaryDirectory() as tmp_dir:
            log_path = Path(tmp_dir) / "backtest.log"
            self._write(
                log_path,
                "[BACKTEST-SVC] \u2764\ufe0f HEARTBEAT inflight=0/6 recv=1 submit=1 done=1 sent=1 rx_age=30s tx_age=20s\n",
            )
            out = hb.check_status(
                log_path=log_path,
                max_log_age_seconds=300,
                max_rx_age_seconds=120,
                max_tx_age_seconds=120,
            )
            self.assertTrue(out["ok"])
            self.assertEqual(30, out["rx_age_seconds"])
            self.assertEqual(20, out["tx_age_seconds"])

    def test_check_status_fails_when_log_too_old(self) -> None:
        with tempfile.TemporaryDirectory() as tmp_dir:
            log_path = Path(tmp_dir) / "backtest.log"
            self._write(
                log_path,
                "[BACKTEST-SVC] \u2764\ufe0f HEARTBEAT inflight=0/6 recv=1 submit=1 done=1 sent=1 rx_age=30s tx_age=20s\n",
            )
            old_ts = log_path.stat().st_mtime - 7200
            os.utime(log_path, (old_ts, old_ts))
            out = hb.check_status(
                log_path=log_path,
                max_log_age_seconds=300,
                max_rx_age_seconds=120,
                max_tx_age_seconds=120,
            )
            self.assertFalse(out["ok"])
            self.assertTrue(any("log too old" in x for x in out["issues"]))

    def test_check_status_fails_when_rx_age_exceeds_threshold(self) -> None:
        with tempfile.TemporaryDirectory() as tmp_dir:
            log_path = Path(tmp_dir) / "backtest.log"
            self._write(
                log_path,
                "[BACKTEST-SVC] \u2764\ufe0f HEARTBEAT inflight=0/6 recv=1 submit=1 done=1 sent=1 rx_age=301s tx_age=20s\n",
            )
            out = hb.check_status(
                log_path=log_path,
                max_log_age_seconds=300,
                max_rx_age_seconds=300,
                max_tx_age_seconds=120,
            )
            self.assertFalse(out["ok"])
            self.assertTrue(any("rx_age too high" in x for x in out["issues"]))

    def test_check_status_fails_without_heartbeat_line(self) -> None:
        with tempfile.TemporaryDirectory() as tmp_dir:
            log_path = Path(tmp_dir) / "backtest.log"
            self._write(log_path, "hello\nworld\n")
            out = hb.check_status(
                log_path=log_path,
                max_log_age_seconds=300,
                max_rx_age_seconds=120,
                max_tx_age_seconds=120,
            )
            self.assertFalse(out["ok"])
            self.assertTrue(any("heartbeat line not found" in x for x in out["issues"]))


if __name__ == "__main__":
    unittest.main()
