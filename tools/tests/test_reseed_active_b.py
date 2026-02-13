import sqlite3
import tempfile
import unittest
import os
from pathlib import Path
from unittest import mock

from tools.ops import reseed_active_b as mod


class TestReseedActiveB(unittest.TestCase):
    def setUp(self) -> None:
        self.tmpdir_obj = tempfile.TemporaryDirectory()
        self.tmpdir = Path(self.tmpdir_obj.name)
        self.library_root = self.tmpdir / "library"
        (self.library_root / "GRAVEYARD").mkdir(parents=True, exist_ok=True)
        (self.library_root / "RETIRED").mkdir(parents=True, exist_ok=True)
        (self.library_root / "B").mkdir(parents=True, exist_ok=True)

        self.conn = sqlite3.connect(":memory:")
        self.conn.execute(
            """
            CREATE TABLE strategies (
                name TEXT PRIMARY KEY,
                rank TEXT,
                timeframe INTEGER,
                direction TEXT,
                symbol TEXT,
                sharpe REAL,
                profit_factor REAL,
                win_rate REAL,
                max_dd REAL,
                updated_at INTEGER
            )
            """
        )

    def tearDown(self) -> None:
        self.conn.close()
        self.tmpdir_obj.cleanup()

    def _insert(self, rows):
        self.conn.executemany(
            """
            INSERT INTO strategies
                (name, rank, timeframe, direction, symbol, sharpe, profit_factor, win_rate, max_dd, updated_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            rows,
        )
        self.conn.commit()

    def _write_rank_file(self, rank_dir: str, name: str, rank: str) -> Path:
        path = self.library_root / rank_dir / f"{name}.lisp"
        path.write_text(
            f"(:NAME \"{name}\" :RANK :{rank} :STATUS :INACTIVE)\n",
            encoding="utf-8",
        )
        return path

    def test_select_reseed_candidates_limits_per_category(self) -> None:
        self._insert(
            [
                ("A1", ":GRAVEYARD", 60, "BOTH", "USDJPY", 1.2, 1.30, 0.40, 0.10, 1),
                ("A2", ":GRAVEYARD", 60, "BOTH", "USDJPY", 0.8, 1.20, 0.38, 0.12, 1),
                ("B1", ":RETIRED", 300, "BUY", "EURUSD", 0.7, 1.10, 0.36, 0.08, 1),
                ("X0", ":GRAVEYARD", 60, "BOTH", "USDJPY", 0.1, 1.00, 0.30, 0.30, 1),
            ]
        )

        selected = mod.select_reseed_candidates(self.conn, per_category=1)
        names = [c.name for c in selected]
        self.assertEqual(2, len(names))
        self.assertIn("A1", names)
        self.assertIn("B1", names)
        self.assertNotIn("A2", names)

    def test_apply_reseed_moves_file_and_updates_db(self) -> None:
        self._insert(
            [
                ("G1", ":GRAVEYARD", 60, "BOTH", "USDJPY", 1.0, 1.20, 0.40, 0.10, 1),
            ]
        )
        self._write_rank_file("GRAVEYARD", "G1", "GRAVEYARD")

        selected = mod.select_reseed_candidates(self.conn, per_category=5)
        summary = mod.apply_reseed(
            self.conn,
            self.library_root,
            selected,
            dry_run=False,
            now_ut=12345,
        )

        self.assertEqual(1, summary.updated)
        src = self.library_root / "GRAVEYARD" / "G1.lisp"
        dst = self.library_root / "B" / "G1.lisp"
        self.assertFalse(src.exists())
        self.assertTrue(dst.exists())
        body = dst.read_text(encoding="utf-8")
        self.assertIn(":RANK :B", body)
        self.assertIn(":STATUS :ACTIVE", body)

        row = self.conn.execute(
            "SELECT rank, updated_at FROM strategies WHERE name='G1'"
        ).fetchone()
        self.assertEqual((":B", 12345), row)

    def test_apply_reseed_dry_run_is_non_destructive(self) -> None:
        self._insert(
            [
                ("R1", ":RETIRED", 300, "SELL", "GBPUSD", 1.1, 1.30, 0.42, 0.10, 7),
            ]
        )
        self._write_rank_file("RETIRED", "R1", "RETIRED")

        selected = mod.select_reseed_candidates(self.conn, per_category=5)
        summary = mod.apply_reseed(
            self.conn,
            self.library_root,
            selected,
            dry_run=True,
            now_ut=9999,
        )

        self.assertEqual(1, summary.would_update)
        self.assertEqual(0, summary.updated)
        src = self.library_root / "RETIRED" / "R1.lisp"
        dst = self.library_root / "B" / "R1.lisp"
        self.assertTrue(src.exists())
        self.assertFalse(dst.exists())

        row = self.conn.execute(
            "SELECT rank, updated_at FROM strategies WHERE name='R1'"
        ).fetchone()
        self.assertEqual((":RETIRED", 7), row)

    def test_apply_reseed_tolerates_rewrite_permission_error(self) -> None:
        self._insert(
            [
                ("E1", ":GRAVEYARD", 60, "BOTH", "USDJPY", 1.0, 1.20, 0.40, 0.10, 1),
            ]
        )
        self._write_rank_file("GRAVEYARD", "E1", "GRAVEYARD")
        selected = mod.select_reseed_candidates(self.conn, per_category=5)

        with mock.patch.object(
            mod,
            "rewrite_strategy_file",
            side_effect=PermissionError("readonly"),
        ):
            summary = mod.apply_reseed(
                self.conn,
                self.library_root,
                selected,
                dry_run=False,
                now_ut=42,
            )

        self.assertEqual(1, summary.updated)
        self.assertEqual(1, summary.rewrite_errors)
        self.assertTrue((self.library_root / "B" / "E1.lisp").exists())
        row = self.conn.execute(
            "SELECT rank, updated_at FROM strategies WHERE name='E1'"
        ).fetchone()
        self.assertEqual((":B", 42), row)

    def test_apply_reseed_resolves_b_archive_conflict(self) -> None:
        self._insert(
            [
                ("C1", ":GRAVEYARD", 60, "BOTH", "USDJPY", 1.0, 1.20, 0.40, 0.10, 1),
            ]
        )
        self._write_rank_file("GRAVEYARD", "C1", "GRAVEYARD")
        self._write_rank_file("B", "C1", "B")

        selected = mod.select_reseed_candidates(self.conn, per_category=5)
        summary = mod.apply_reseed(
            self.conn,
            self.library_root,
            selected,
            dry_run=False,
            now_ut=77,
        )

        self.assertEqual(1, summary.conflicts)
        self.assertEqual(1, summary.conflicts_resolved)
        self.assertEqual(1, summary.updated)
        row = self.conn.execute(
            "SELECT rank, updated_at FROM strategies WHERE name='C1'"
        ).fetchone()
        self.assertEqual((":B", 77), row)

    def test_rewrite_strategy_file_replaces_readonly_file(self) -> None:
        path = self._write_rank_file("B", "RO1", "GRAVEYARD")
        os.chmod(path, 0o444)
        try:
            mod.rewrite_strategy_file(path)
        finally:
            os.chmod(path, 0o644)

        body = path.read_text(encoding="utf-8")
        self.assertIn(":RANK :B", body)
        self.assertIn(":STATUS :ACTIVE", body)


if __name__ == "__main__":
    unittest.main()
