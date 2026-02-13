import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tools import polymarket_openclaw_cycle as cycle


class TestPolymarketOpenClawCycle(unittest.TestCase):
    def test_build_cycle_paths(self) -> None:
        paths = cycle.build_cycle_paths(Path("/tmp/out"), run_id="r1", run_date="2026-02-12")
        self.assertEqual(Path("/tmp/out/plan_r1.json"), paths["plan_file"])
        self.assertEqual(Path("/tmp/out/report_2026-02-12_r1.json"), paths["report_file"])
        self.assertEqual(Path("/tmp/out/journal.jsonl"), paths["journal_file"])

    def test_build_bot_command(self) -> None:
        cmd = cycle.build_bot_command(
            base_dir=Path("/repo"),
            config_file="cfg.json",
            signals_file="signals.jsonl",
            openclaw_cmd="",
            markets_file="m.json",
            settlements_file="s.json",
            allow_duplicate_open_markets=False,
            max_open_positions=7,
            max_daily_entries=11,
            max_daily_loss_streak=3,
            max_daily_realized_loss_usd=4.5,
            min_liquidity_usd=10000.0,
            min_volume_usd=5000.0,
            limit=123,
            run_id="r1",
            plan_file=Path("/tmp/plan.json"),
            journal_file=Path("/tmp/journal.jsonl"),
        )
        text = " ".join(cmd)
        self.assertIn("tools/polymarket_openclaw_bot.py", text)
        self.assertIn("--config-file cfg.json", text)
        self.assertIn("--signals-file signals.jsonl", text)
        self.assertIn("--markets-file m.json", text)
        self.assertIn("--settlements-file s.json", text)
        self.assertIn("--max-open-positions 7", text)
        self.assertIn("--max-daily-entries 11", text)
        self.assertIn("--max-daily-loss-streak 3", text)
        self.assertIn("--max-daily-realized-loss-usd 4.5", text)
        self.assertIn("--min-liquidity-usd 10000.0", text)
        self.assertIn("--min-volume-usd 5000.0", text)
        self.assertIn("--limit 123", text)
        self.assertIn("--run-id r1", text)

    def test_build_bot_command_with_allow_duplicate_open_markets(self) -> None:
        cmd = cycle.build_bot_command(
            base_dir=Path("/repo"),
            config_file="cfg.json",
            signals_file="signals.jsonl",
            openclaw_cmd="",
            markets_file="",
            settlements_file="",
            allow_duplicate_open_markets=True,
            max_open_positions=0,
            max_daily_entries=0,
            max_daily_loss_streak=0,
            max_daily_realized_loss_usd=0.0,
            min_liquidity_usd=0.0,
            min_volume_usd=0.0,
            limit=50,
            run_id="r2",
            plan_file=Path("/tmp/plan2.json"),
            journal_file=Path("/tmp/journal2.jsonl"),
        )
        text = " ".join(cmd)
        self.assertIn("--allow-duplicate-open-markets", text)

    def test_load_signal_summary_from_meta(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            signals_file.write_text('{"market_id":"m1","p_yes":0.6}\n', encoding="utf-8")
            meta_file.write_text(
                (
                    '{"updated_at":"2026-02-12T15:00:00+00:00","signal_count":5,'
                    '"source_counts":{"openclaw_agent":2,"heuristic_fallback":3},'
                    '"agent_signal_count":2,"agent_signal_ratio":0.4}'
                ),
                encoding="utf-8",
            )
            summary = cycle.load_signal_summary(
                signals_meta_file=str(meta_file),
                signals_file=str(signals_file),
            )

        self.assertEqual(5, summary["signal_count"])
        self.assertEqual(2, summary["agent_signal_count"])
        self.assertAlmostEqual(0.4, summary["agent_signal_ratio"])
        self.assertEqual(2, summary["source_counts"]["openclaw_agent"])
        self.assertEqual(3, summary["source_counts"]["heuristic_fallback"])

    def test_load_signal_summary_uses_signals_suffix_fallback(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            signals_file.write_text('{"market_id":"m1","p_yes":0.6}\n', encoding="utf-8")
            meta_file.write_text(
                '{"signal_count":4,"source_counts":{"openclaw_agent":1,"heuristic_fallback":3}}',
                encoding="utf-8",
            )
            summary = cycle.load_signal_summary(
                signals_meta_file="",
                signals_file=str(signals_file),
            )

        self.assertEqual(4, summary["signal_count"])
        self.assertEqual(1, summary["agent_signal_count"])
        self.assertAlmostEqual(0.25, summary["agent_signal_ratio"])

    def test_build_report_command_with_settlements(self) -> None:
        cmd = cycle.build_report_command(
            base_dir=Path("/repo"),
            journal_file=Path("/tmp/journal.jsonl"),
            run_date="2026-02-12",
            report_file=Path("/tmp/report.json"),
            settlements_file="s.json",
            fee_bps_per_side=20.0,
            slippage_bps_per_side=30.0,
        )
        text = " ".join(cmd)
        self.assertIn("tools/polymarket_openclaw_report.py", text)
        self.assertIn("--settlements-file s.json", text)
        self.assertIn("--fee-bps-per-side 20.0", text)
        self.assertIn("--slippage-bps-per-side 30.0", text)

    def test_build_execute_command(self) -> None:
        cmd = cycle.build_execute_command(
            base_dir=Path("/repo"),
            plan_file=Path("/tmp/plan.json"),
            run_id="r1",
            run_date="2026-02-12",
            order_type="fok",
            max_orders=3,
            min_expected_value_usd=0.2,
            min_stake_usd=1.5,
            fail_on_error=True,
            write_report=Path("/tmp/execution.json"),
        )
        text = " ".join(cmd)
        self.assertIn("tools/polymarket_openclaw_execute.py", text)
        self.assertIn("--plan-file /tmp/plan.json", text)
        self.assertIn("--run-id r1", text)
        self.assertIn("--run-date 2026-02-12", text)
        self.assertIn("--order-type FOK", text)
        self.assertIn("--max-orders 3", text)
        self.assertIn("--min-expected-value-usd 0.2", text)
        self.assertIn("--min-stake-usd 1.5", text)
        self.assertIn("--write-report /tmp/execution.json", text)
        self.assertIn("--fail-on-error", text)

    def test_build_fetch_settlements_command(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            existing = Path(td) / "existing.json"
            existing.write_text("{}", encoding="utf-8")
            cmd = cycle.build_fetch_settlements_command(
                base_dir=Path("/repo"),
                journal_file=Path("/tmp/journal.jsonl"),
                date_filter="2026-02-12",
                existing_settlements=existing,
                out_settlements=Path("/tmp/out.json"),
                min_price_for_win=0.98,
                min_gap=0.05,
                sleep_ms=0,
            )
        text = " ".join(cmd)
        self.assertIn("tools/polymarket_fetch_settlements.py", text)
        self.assertIn("--journal-file /tmp/journal.jsonl", text)
        self.assertIn("--date 2026-02-12", text)
        self.assertIn(f"--existing-settlements {existing}", text)
        self.assertIn("--write-settlements /tmp/out.json", text)

    def test_build_fetch_settlements_command_omits_invalid_existing_path(self) -> None:
        cmd = cycle.build_fetch_settlements_command(
            base_dir=Path("/repo"),
            journal_file=Path("/tmp/journal.jsonl"),
            date_filter="2026-02-12",
            existing_settlements=Path(""),
            out_settlements=Path("/tmp/out.json"),
            min_price_for_win=0.98,
            min_gap=0.05,
            sleep_ms=0,
        )
        text = " ".join(cmd)
        self.assertNotIn("--existing-settlements", text)

    def test_build_autotune_command(self) -> None:
        cmd = cycle.build_autotune_command(
            base_dir=Path("/repo"),
            journal_file=Path("/tmp/journal.jsonl"),
            settlements_file=Path("/tmp/settlements.json"),
            base_config_file="cfg.json",
            date_filter="2026-02-12",
            out_report=Path("/tmp/tune_report.json"),
            out_candidate_config=Path("/tmp/tuned.json"),
            min_trades=5,
            fee_bps_per_side=20.0,
            slippage_bps_per_side=30.0,
        )
        text = " ".join(cmd)
        self.assertIn("tools/polymarket_openclaw_autotune.py", text)
        self.assertIn("--journal-file /tmp/journal.jsonl", text)
        self.assertIn("--settlements-file /tmp/settlements.json", text)
        self.assertIn("--base-config-file cfg.json", text)
        self.assertIn("--write-candidate-config /tmp/tuned.json", text)

    def test_should_run_posttrade_steps(self) -> None:
        self.assertTrue(cycle.should_run_posttrade_steps({"entries": 1}))
        self.assertFalse(cycle.should_run_posttrade_steps({"entries": 0}))
        self.assertFalse(cycle.should_run_posttrade_steps({}))

    def test_should_apply_autotune_candidate(self) -> None:
        self.assertTrue(
            cycle.should_apply_autotune_candidate(
                best={"trades": 12, "realized_pnl_usd": 2.5},
                min_trades=10,
                min_realized_pnl_usd=1.0,
            )
        )
        self.assertFalse(
            cycle.should_apply_autotune_candidate(
                best={"trades": 8, "realized_pnl_usd": 2.5},
                min_trades=10,
                min_realized_pnl_usd=1.0,
            )
        )
        self.assertFalse(
            cycle.should_apply_autotune_candidate(
                best={"trades": 12, "realized_pnl_usd": 0.2},
                min_trades=10,
                min_realized_pnl_usd=1.0,
            )
        )

    def test_apply_candidate_config_with_backup(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            target = Path(td) / "active_config.json"
            backup_dir = Path(td) / "backups"
            target.write_text("{\"min_edge\": 0.02}", encoding="utf-8")
            candidate = {"min_edge": 0.03, "kelly_scale": 0.4}

            paths = cycle.apply_candidate_config_with_backup(
                candidate_config=candidate,
                target_config_path=target,
                backup_dir=backup_dir,
                run_id="r1",
            )

            applied = target.read_text(encoding="utf-8")
            backup = Path(paths["backup_file"]).read_text(encoding="utf-8")

        self.assertIn("\"min_edge\": 0.03", applied)
        self.assertIn("\"min_edge\": 0.02", backup)
        self.assertTrue(Path(paths["backup_file"]).name.startswith("active_config_r1_"))

    def test_build_latest_status_snapshot(self) -> None:
        snapshot = cycle.build_latest_status_snapshot(
            result={
                "run_id": "r1",
                "run_date": "2026-02-13",
                "plan_summary": {
                    "entries": 2,
                    "total_stake_usd": 3.5,
                    "open_markets": 5,
                    "blocked_open_markets": 2,
                    "quality_filtered_markets": 1,
                },
                "signal_summary": {"signal_count": 20, "agent_signal_count": 4, "agent_signal_ratio": 0.2},
                "live_execution_enabled": True,
                "execution": {"ok": True, "skipped": False, "attempted": 2, "sent": 2, "failed": 0},
                "report_summary": {"total_expected_value_usd": 0.3, "expected_return_on_stake": 0.0857},
                "paths": {
                    "plan_file": "/tmp/plan.json",
                    "report_file": "/tmp/report.json",
                    "journal_file": "/tmp/journal.jsonl",
                },
            },
            updated_at="2026-02-13T00:00:00+00:00",
        )
        self.assertEqual("r1", snapshot["run_id"])
        self.assertEqual(2, snapshot["entries"])
        self.assertAlmostEqual(3.5, snapshot["total_stake_usd"])
        self.assertTrue(snapshot["live_execution_enabled"])
        self.assertTrue(snapshot["execution_ok"])
        self.assertEqual(2, snapshot["execution_sent"])
        self.assertEqual(20, snapshot["signal_count"])
        self.assertEqual(5, snapshot["open_markets"])
        self.assertEqual(2, snapshot["blocked_open_markets"])
        self.assertEqual(1, snapshot["quality_filtered_markets"])
        self.assertEqual("/tmp/report.json", snapshot["report_file"])

    def test_build_latest_status_snapshot_when_live_disabled(self) -> None:
        snapshot = cycle.build_latest_status_snapshot(
            result={
                "run_id": "r3",
                "run_date": "2026-02-13",
                "plan_summary": {"entries": 0, "total_stake_usd": 0.0},
                "signal_summary": {"signal_count": 5, "agent_signal_count": 1, "agent_signal_ratio": 0.2},
                "live_execution_enabled": False,
                "execution": {},
                "report_summary": {},
                "paths": {},
            },
            updated_at="2026-02-13T00:00:00+00:00",
        )
        self.assertFalse(snapshot["live_execution_enabled"])
        self.assertTrue(snapshot["execution_ok"])
        self.assertTrue(snapshot["execution_skipped"])

    def test_write_latest_status_snapshot(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            status_file = Path(td) / "latest_status.json"
            cycle.write_latest_status_snapshot(
                status_file=status_file,
                snapshot={"run_id": "r2", "entries": 1},
            )
            payload = status_file.read_text(encoding="utf-8")
        self.assertIn("\"run_id\": \"r2\"", payload)
        self.assertIn("\"entries\": 1", payload)

    def test_append_status_history_caps_and_replaces_same_run(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            history_file = Path(td) / "status_history.jsonl"
            cycle.append_status_history(
                history_file=history_file,
                snapshot={"run_id": "r1", "entries": 1},
                max_entries=2,
            )
            cycle.append_status_history(
                history_file=history_file,
                snapshot={"run_id": "r2", "entries": 2},
                max_entries=2,
            )
            cycle.append_status_history(
                history_file=history_file,
                snapshot={"run_id": "r1", "entries": 3},
                max_entries=2,
            )
            lines = [line for line in history_file.read_text(encoding="utf-8").splitlines() if line.strip()]
        self.assertEqual(2, len(lines))
        self.assertIn("\"run_id\": \"r2\"", lines[0])
        self.assertIn("\"run_id\": \"r1\"", lines[1])
        self.assertIn("\"entries\": 3", lines[1])

    def test_main_writes_latest_status_even_when_execute_fails(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            out_dir = Path(td) / "out"
            out_dir.mkdir(parents=True, exist_ok=True)

            argv = [
                "polymarket_openclaw_cycle.py",
                "--config-file",
                "cfg.json",
                "--signals-file",
                "signals.jsonl",
                "--output-dir",
                str(out_dir),
                "--live-execution",
                "--live-fail-on-error",
            ]

            def fake_run_json(cmd):
                joined = " ".join(cmd)
                if "tools/polymarket_openclaw_bot.py" in joined:
                    return {"summary": {"entries": 1, "total_stake_usd": 1.0}}
                if "tools/polymarket_openclaw_execute.py" in joined:
                    raise subprocess.CalledProcessError(1, cmd, output="", stderr="boom")
                if "tools/polymarket_openclaw_report.py" in joined:
                    return {"total_expected_value_usd": 0.0, "expected_return_on_stake": 0.0}
                return {}

            with mock.patch.object(sys, "argv", argv):
                with mock.patch.object(cycle, "_run_json_command", side_effect=fake_run_json):
                    try:
                        cycle.main()
                    except BaseException:
                        pass

            status_file = out_dir / "latest_status.json"
            self.assertTrue(status_file.exists())
            payload = json.loads(status_file.read_text(encoding="utf-8"))
            self.assertTrue(payload.get("live_execution_enabled", False))
            self.assertFalse(payload.get("execution_ok", True))


if __name__ == "__main__":
    unittest.main()
