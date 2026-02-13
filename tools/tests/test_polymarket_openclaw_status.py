import unittest
import tempfile
from pathlib import Path
from unittest import mock

from tools import polymarket_openclaw_status as status


class TestPolymarketOpenclawStatus(unittest.TestCase):
    def test_parse_iso_datetime_accepts_z_suffix(self) -> None:
        dt = status.parse_iso_datetime("2026-02-13T06:58:04Z")
        self.assertIsNotNone(dt)
        self.assertIsNotNone(dt.tzinfo)

    def test_build_health_marks_stale(self) -> None:
        health = status.build_health(
            payload={"updated_at": "2026-02-13T00:00:00+00:00"},
            max_age_seconds=60,
            window_summary={},
            min_runs_in_window=0,
            min_sent_in_window=0,
            min_entries_in_window=0,
            now_iso="2026-02-13T01:10:00+00:00",
        )
        self.assertEqual("stale", health["status"])
        self.assertGreater(float(health["age_seconds"]), 60.0)

    def test_build_health_marks_error_when_execution_failed(self) -> None:
        health = status.build_health(
            payload={
                "updated_at": "2026-02-13T01:00:00+00:00",
                "live_execution_enabled": True,
                "execution_failed": 2,
            },
            max_age_seconds=0,
            window_summary={},
            min_runs_in_window=0,
            min_sent_in_window=0,
            min_entries_in_window=0,
            now_iso="2026-02-13T01:01:00+00:00",
        )
        self.assertEqual("error", health["status"])
        self.assertIn("failed orders", health["reason"])

    def test_build_health_marks_warn_when_runs_below_threshold(self) -> None:
        health = status.build_health(
            payload={"updated_at": "2026-02-13T01:00:00+00:00"},
            max_age_seconds=0,
            window_summary={"window_minutes": 60, "runs": 0, "execution_sent": 0},
            min_runs_in_window=1,
            min_sent_in_window=0,
            min_entries_in_window=0,
            now_iso="2026-02-13T01:01:00+00:00",
        )
        self.assertEqual("warn", health["status"])
        self.assertIn("runs below threshold", health["reason"])

    def test_build_health_marks_warn_when_sent_below_threshold(self) -> None:
        health = status.build_health(
            payload={"updated_at": "2026-02-13T01:00:00+00:00"},
            max_age_seconds=0,
            window_summary={"window_minutes": 60, "runs": 3, "execution_sent": 0},
            min_runs_in_window=0,
            min_sent_in_window=1,
            min_entries_in_window=0,
            now_iso="2026-02-13T01:01:00+00:00",
        )
        self.assertEqual("warn", health["status"])
        self.assertIn("sent below threshold", health["reason"])

    def test_build_health_marks_warn_when_entries_below_threshold(self) -> None:
        health = status.build_health(
            payload={"updated_at": "2026-02-13T01:00:00+00:00"},
            max_age_seconds=0,
            window_summary={"window_minutes": 60, "runs": 3, "entries": 0, "execution_sent": 0},
            min_runs_in_window=0,
            min_sent_in_window=0,
            min_entries_in_window=1,
            now_iso="2026-02-13T01:01:00+00:00",
        )
        self.assertEqual("warn", health["status"])
        self.assertIn("entries below threshold", health["reason"])

    def test_render_text_summary(self) -> None:
        text = status.render_text_summary(
            payload={
                "updated_at": "2026-02-13T06:58:04+00:00",
                "run_id": "r1",
                "entries": 2,
                "total_stake_usd": 4.0,
                "signal_count": 30,
                "agent_signal_count": 10,
                "agent_signal_ratio": 0.333333,
                "open_markets": 4,
                "blocked_open_markets": 1,
                "quality_filtered_markets": 2,
                "live_execution_enabled": True,
                "execution_sent": 2,
                "execution_failed": 0,
                "plan_file": "/tmp/plan.json",
                "report_file": "/tmp/report.json",
                "journal_file": "/tmp/journal.jsonl",
            },
            health={"status": "ok", "reason": "", "age_seconds": 5.0},
        )
        self.assertIn("Health: ok", text)
        self.assertIn("Run: r1", text)
        self.assertIn("Entries: 2", text)
        self.assertIn("Signals: total=30", text)
        self.assertIn("Markets: open=4 blocked=1 quality_filtered=2", text)

    def test_load_status_history(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            history_file = Path(td) / "status_history.jsonl"
            history_file.write_text(
                (
                    "{\"run_id\":\"r1\",\"entries\":1}\n"
                    "{\"run_id\":\"r2\",\"entries\":2}\n"
                    "not-json\n"
                    "{\"run_id\":\"r3\",\"entries\":3}\n"
                ),
                encoding="utf-8",
            )
            rows = status.load_status_history(history_file=history_file, last_runs=2)
        self.assertEqual(2, len(rows))
        self.assertEqual("r2", rows[0]["run_id"])
        self.assertEqual("r3", rows[1]["run_id"])

    def test_render_text_summary_includes_recent_runs(self) -> None:
        text = status.render_text_summary(
            payload={"run_id": "r1"},
            health={"status": "ok", "reason": "", "age_seconds": 1},
            recent_runs=[
                {"run_id": "r9", "entries": 2, "execution_sent": 1, "execution_failed": 0},
                {"run_id": "r10", "entries": 0, "execution_sent": 0, "execution_failed": 0},
            ],
        )
        self.assertIn("RecentRuns:", text)
        self.assertIn("r9", text)
        self.assertIn("r10", text)

    def test_summarize_recent_runs_with_window(self) -> None:
        summary = status.summarize_recent_runs(
            recent_runs=[
                {
                    "run_id": "r1",
                    "updated_at": "2026-02-13T07:00:00+00:00",
                    "entries": 1,
                    "execution_sent": 1,
                    "execution_failed": 0,
                    "total_stake_usd": 2.5,
                },
                {
                    "run_id": "r2",
                    "updated_at": "2026-02-13T07:20:00+00:00",
                    "entries": 0,
                    "execution_sent": 0,
                    "execution_failed": 1,
                    "total_stake_usd": 0.0,
                },
                {
                    "run_id": "r3",
                    "updated_at": "2026-02-13T07:30:00+00:00",
                    "entries": 2,
                    "execution_sent": 2,
                    "execution_failed": 0,
                    "total_stake_usd": 5.0,
                },
            ],
            window_minutes=20,
            now_iso="2026-02-13T07:35:00+00:00",
        )
        self.assertEqual(2, summary["runs"])
        self.assertEqual(2, summary["entries"])
        self.assertEqual(2, summary["execution_sent"])
        self.assertEqual(1, summary["execution_failed"])
        self.assertAlmostEqual(5.0, float(summary["total_stake_usd"]))

    def test_render_text_summary_includes_window_summary(self) -> None:
        text = status.render_text_summary(
            payload={"run_id": "r1"},
            health={"status": "ok", "reason": "", "age_seconds": 1},
            recent_runs=[],
            window_summary={
                "window_minutes": 30,
                "runs": 3,
                "entries": 2,
                "execution_sent": 2,
                "execution_failed": 1,
                "total_stake_usd": 6.0,
            },
        )
        self.assertIn("WindowSummary:", text)
        self.assertIn("window=30m", text)
        self.assertIn("runs=3", text)

    def test_env_int(self) -> None:
        with mock.patch.dict(status.os.environ, {}, clear=True):
            self.assertEqual(7, status._env_int("POLYCLAW_DUMMY", 7))
        with mock.patch.dict(status.os.environ, {"POLYCLAW_DUMMY": "12"}, clear=True):
            self.assertEqual(12, status._env_int("POLYCLAW_DUMMY", 7))
        with mock.patch.dict(status.os.environ, {"POLYCLAW_DUMMY": "bad"}, clear=True):
            self.assertEqual(7, status._env_int("POLYCLAW_DUMMY", 7))


if __name__ == "__main__":
    unittest.main()
