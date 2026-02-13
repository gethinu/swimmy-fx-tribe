import sqlite3
import unittest

from tools.ops import reconcile_archive_db as mod


class TestReconcileArchiveDb(unittest.TestCase):
    def setUp(self) -> None:
        self.conn = sqlite3.connect(":memory:")
        self.conn.execute(
            """
            CREATE TABLE strategies (
                name TEXT PRIMARY KEY,
                rank TEXT,
                data_sexp TEXT,
                updated_at INTEGER
            )
            """
        )
        self.conn.executemany(
            "INSERT INTO strategies (name, rank, data_sexp, updated_at) VALUES (?, ?, ?, ?)",
            [
                ("A", ":GRAVEYARD", "(A)", 1),
                ("B", ":RETIRED", "(B)", 1),
                ("C", ":GRAVEYARD", "(C)", 1),
                ("D", ":B", "(D)", 1),
            ],
        )
        self.conn.commit()

    def tearDown(self) -> None:
        self.conn.close()

    def test_compute_db_only_archive_names(self) -> None:
        db_only = mod.compute_db_only_archive_names(self.conn, {"A"})
        self.assertEqual(["B", "C"], db_only)

    def test_prune_db_only_archive_names_dry_run(self) -> None:
        deleted, backed_up = mod.prune_db_only_archive_names(
            self.conn,
            ["B", "C"],
            dry_run=True,
        )
        self.assertEqual(2, deleted)
        self.assertEqual(2, backed_up)
        rows = self.conn.execute("SELECT name FROM strategies ORDER BY name").fetchall()
        self.assertEqual([("A",), ("B",), ("C",), ("D",)], rows)

    def test_prune_db_only_archive_names_with_backup(self) -> None:
        deleted, backed_up = mod.prune_db_only_archive_names(
            self.conn,
            ["B", "C"],
            dry_run=False,
            backup_table="archive_reconcile_backup",
        )
        self.assertEqual(2, deleted)
        self.assertEqual(2, backed_up)
        rows = self.conn.execute("SELECT name FROM strategies ORDER BY name").fetchall()
        self.assertEqual([("A",), ("D",)], rows)
        backups = self.conn.execute(
            "SELECT name, rank FROM archive_reconcile_backup ORDER BY name"
        ).fetchall()
        self.assertEqual([("B", ":RETIRED"), ("C", ":GRAVEYARD")], backups)


if __name__ == "__main__":
    unittest.main()
