import tempfile
import unittest
from pathlib import Path

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
            limit=50,
            run_id="r2",
            plan_file=Path("/tmp/plan2.json"),
            journal_file=Path("/tmp/journal2.jsonl"),
        )
        text = " ".join(cmd)
        self.assertIn("--allow-duplicate-open-markets", text)

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


if __name__ == "__main__":
    unittest.main()
