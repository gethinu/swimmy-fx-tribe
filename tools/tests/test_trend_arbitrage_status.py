import json
import tempfile
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path

from tools import trend_arbitrage_status as s


class TestTrendArbitrageStatus(unittest.TestCase):
    def _write(self, path: Path, payload) -> None:
        path.write_text(json.dumps(payload, ensure_ascii=False), encoding="utf-8")

    def test_check_status_ok(self) -> None:
        with tempfile.TemporaryDirectory() as tmp_dir:
            path = Path(tmp_dir) / "latest_run.json"
            now = datetime.now(timezone.utc)
            self._write(
                path,
                {
                    "ran_at": now.isoformat(),
                    "selected_count": 1,
                    "candidate_count": 10,
                },
            )
            st = s.check_status(latest_run_path=path, max_age_seconds=3600)
            self.assertTrue(st["ok"])
            self.assertEqual(1, st["selected_count"])

    def test_check_status_old(self) -> None:
        with tempfile.TemporaryDirectory() as tmp_dir:
            path = Path(tmp_dir) / "latest_run.json"
            old = datetime.now(timezone.utc) - timedelta(hours=10)
            self._write(path, {"ran_at": old.isoformat()})
            st = s.check_status(latest_run_path=path, max_age_seconds=3600)
            self.assertFalse(st["ok"])
            self.assertTrue(any("run too old" in x for x in st["issues"]))

    def test_check_status_missing_file(self) -> None:
        with tempfile.TemporaryDirectory() as tmp_dir:
            path = Path(tmp_dir) / "missing.json"
            st = s.check_status(latest_run_path=path, max_age_seconds=3600)
            self.assertFalse(st["ok"])
            self.assertTrue(any("not found" in x for x in st["issues"]))


if __name__ == "__main__":
    unittest.main()

