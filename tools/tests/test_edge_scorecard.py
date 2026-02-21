import json
import sqlite3
import tempfile
import unittest
from pathlib import Path

from tools import edge_scorecard as es


def _create_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(
        """
        CREATE TABLE deployment_gate_status (
          strategy_name TEXT,
          decision TEXT,
          updated_at INTEGER
        );
        CREATE TABLE trade_logs (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          timestamp INTEGER,
          pnl REAL
        );
        CREATE TABLE strategies (
          name TEXT,
          rank TEXT,
          trades INTEGER,
          sharpe REAL,
          profit_factor REAL,
          win_rate REAL,
          max_dd REAL,
          oos_sharpe REAL,
          cpcv_pass_rate REAL,
          cpcv_median_maxdd REAL
        );
        """
    )
    conn.commit()


class TestEdgeScorecard(unittest.TestCase):
    def test_build_edge_scorecard_calculates_kpis(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            db_path = base / "swimmy.db"
            rank_report_path = base / "rank_conformance_latest.json"

            conn = sqlite3.connect(str(db_path))
            _create_schema(conn)
            now_ts = 1_700_000_000

            conn.executemany(
                "INSERT INTO deployment_gate_status(strategy_name, decision, updated_at) VALUES(?,?,?)",
                [
                    ("s1", "LIVE_READY", now_ts - 3600),
                    ("s2", "LIVE_READY", now_ts - 7200),
                    ("s3", "BLOCKED_OOS", now_ts - 3600),
                ],
            )
            conn.executemany(
                "INSERT INTO trade_logs(timestamp, pnl) VALUES(?,?)",
                [
                    (now_ts - 3600, 10.0),
                    (now_ts - 7200, -4.0),
                    (now_ts - 10 * 86400, 7.0),
                ],
            )
            conn.executemany(
                "INSERT INTO strategies(name, rank, trades, sharpe, profit_factor, win_rate, max_dd, oos_sharpe, cpcv_pass_rate, cpcv_median_maxdd) VALUES(?,?,?,?,?,?,?,?,?,?)",
                [
                    ("A-PASS", "A", 80, 0.7, 1.5, 0.5, 0.1, 0.5, 0.8, 0.1),
                    ("A-FAIL", "A", 20, 0.7, 1.5, 0.5, 0.1, 0.5, 0.8, 0.1),
                    ("S-PASS", "S", 140, 0.9, 1.45, 0.50, 0.09, 0.6, 0.8, 0.10),
                    ("S-FAIL", "S", 60, 0.9, 1.45, 0.50, 0.09, 0.6, 0.8, 0.10),
                    ("B-PASS", "B", 40, 0.2, 1.1, 0.4, 0.2, 0.1, 0.1, 0.3),
                ],
            )
            conn.commit()
            conn.close()

            rank_report_path.write_text(
                json.dumps(
                    {
                        "violations": {"total": 2, "floor": {"A_lt_50": 1}, "conformance": {"A_fail_to_B": 1}},
                        "transitions": {"promotion_count": 1, "demotion_count": 3, "changed_count": 4},
                    }
                ),
                encoding="utf-8",
            )

            scorecard = es.build_edge_scorecard(
                db_path=db_path,
                rank_report_path=rank_report_path,
                now_ts=now_ts,
                short_days=7,
                long_days=30,
            )

            self.assertEqual(2, scorecard["kpi_live_edge_guard"]["live_ready_count"])
            self.assertEqual(3, scorecard["kpi_live_edge_guard"]["total_count"])
            self.assertEqual(2, scorecard["kpi_rank_conformance"]["violations_total"])
            self.assertEqual(1, scorecard["kpi_breeder_parent_quality"]["A"]["pass_count"])
            self.assertEqual(2, scorecard["kpi_breeder_parent_quality"]["S"]["total_count"])
            self.assertEqual(2, scorecard["kpi_live_pnl_health"]["window_7d"]["trade_count"])
            self.assertEqual("degraded", scorecard["overall_status"])

    def test_run_edge_scorecard_writes_latest_and_history(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            db_path = base / "swimmy.db"
            out_path = base / "edge_scorecard_latest.json"
            history_dir = base / "edge_scorecard"
            rank_report_path = base / "rank_conformance_latest.json"

            conn = sqlite3.connect(str(db_path))
            _create_schema(conn)
            conn.commit()
            conn.close()

            rank_report_path.write_text(
                json.dumps({"violations": {"total": 0}, "transitions": {"promotion_count": 0, "demotion_count": 0}}),
                encoding="utf-8",
            )

            report = es.run_edge_scorecard(
                db_path=db_path,
                out_path=out_path,
                history_dir=history_dir,
                rank_report_path=rank_report_path,
                now_ts=1_700_000_000,
            )
            self.assertTrue(out_path.exists())
            self.assertTrue(any(history_dir.glob("edge_scorecard_*.json")))
            self.assertIn("overall_status", report)


if __name__ == "__main__":
    unittest.main()
