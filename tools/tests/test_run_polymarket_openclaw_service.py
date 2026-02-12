import unittest
import tempfile
from datetime import datetime, timezone, timedelta
from pathlib import Path

from tools import run_polymarket_openclaw_service as svc


class TestRunPolymarketOpenclawService(unittest.TestCase):
    def test_build_cycle_args_with_signals_file(self) -> None:
        env = {
            "POLYCLAW_CONFIG_FILE": "/cfg.json",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_OUTPUT_DIR": "/out",
        }
        argv = svc.build_cycle_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("tools/polymarket_openclaw_cycle.py", text)
        self.assertIn("--config-file /cfg.json", text)
        self.assertIn("--signals-file /signals.jsonl", text)
        self.assertIn("--output-dir /out", text)

    def test_build_cycle_args_with_openclaw_cmd(self) -> None:
        env = {
            "POLYCLAW_CONFIG_FILE": "/cfg.json",
            "POLYCLAW_OPENCLAW_CMD": "openclaw signals --format jsonl",
        }
        argv = svc.build_cycle_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--openclaw-cmd openclaw signals --format jsonl", text)

    def test_build_cycle_args_with_autotune_apply_flags(self) -> None:
        env = {
            "POLYCLAW_CONFIG_FILE": "/cfg.json",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_AUTOTUNE_APPLY_BEST": "1",
            "POLYCLAW_AUTOTUNE_APPLY_MIN_TRADES": "25",
            "POLYCLAW_AUTOTUNE_APPLY_MIN_REALIZED_PNL_USD": "2.5",
            "POLYCLAW_AUTOTUNE_APPLY_TARGET_CONFIG": "/cfg_live.json",
        }
        argv = svc.build_cycle_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--autotune-apply-best", text)
        self.assertIn("--autotune-apply-min-trades 25", text)
        self.assertIn("--autotune-apply-min-realized-pnl-usd 2.5", text)
        self.assertIn("--autotune-apply-target-config /cfg_live.json", text)

    def test_build_cycle_args_with_allow_duplicate_open_markets(self) -> None:
        env = {
            "POLYCLAW_CONFIG_FILE": "/cfg.json",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_ALLOW_DUPLICATE_OPEN_MARKETS": "1",
        }
        argv = svc.build_cycle_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--allow-duplicate-open-markets", text)

    def test_build_cycle_args_with_max_open_positions(self) -> None:
        env = {
            "POLYCLAW_CONFIG_FILE": "/cfg.json",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_MAX_OPEN_POSITIONS": "9",
        }
        argv = svc.build_cycle_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--max-open-positions 9", text)

    def test_build_cycle_args_with_max_daily_entries(self) -> None:
        env = {
            "POLYCLAW_CONFIG_FILE": "/cfg.json",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_MAX_DAILY_ENTRIES": "6",
        }
        argv = svc.build_cycle_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--max-daily-entries 6", text)

    def test_build_cycle_args_with_max_daily_loss_streak(self) -> None:
        env = {
            "POLYCLAW_CONFIG_FILE": "/cfg.json",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_MAX_DAILY_LOSS_STREAK": "4",
        }
        argv = svc.build_cycle_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--max-daily-loss-streak 4", text)

    def test_build_cycle_args_with_max_daily_realized_loss_usd(self) -> None:
        env = {
            "POLYCLAW_CONFIG_FILE": "/cfg.json",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_MAX_DAILY_REALIZED_LOSS_USD": "7.5",
        }
        argv = svc.build_cycle_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--max-daily-realized-loss-usd 7.5", text)

    def test_build_cycle_args_requires_signal_source(self) -> None:
        env = {"POLYCLAW_CONFIG_FILE": "/cfg.json"}
        with self.assertRaises(ValueError):
            svc.build_cycle_args(env=env, base_dir=Path("/repo"))

    def test_build_signal_sync_args(self) -> None:
        env = {
            "POLYCLAW_OPENCLAW_CMD": "openclaw signals --format jsonl",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_SIGNAL_SYNC_MIN_SIGNALS": "3",
            "POLYCLAW_SIGNAL_SYNC_TIMEOUT_SECONDS": "15",
        }
        argv = svc.build_signal_sync_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("tools/openclaw_signal_sync.py", text)
        self.assertIn("--openclaw-cmd openclaw signals --format jsonl", text)
        self.assertIn("--signals-file /signals.jsonl", text)
        self.assertIn("--min-signals 3", text)
        self.assertIn("--timeout-seconds 15", text)

    def test_build_signal_sync_args_uses_heuristic_fallback(self) -> None:
        env = {
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_LIMIT": "120",
            "POLYCLAW_HEURISTIC_QUESTION_KEYWORDS": "nba,nfl",
        }
        argv = svc.build_signal_sync_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("tools/openclaw_signal_sync.py", text)
        self.assertIn("--openclaw-cmd", text)
        self.assertIn("tools/openclaw_signal_heuristic.py", text)
        self.assertIn("--limit 120", text)
        self.assertIn("--question-keyword nba", text)
        self.assertIn("--question-keyword nfl", text)

    def test_build_signal_sync_args_no_fallback_raises(self) -> None:
        env = {
            "POLYCLAW_USE_HEURISTIC_IF_NO_OPENCLAW_CMD": "0",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
        }
        with self.assertRaises(ValueError):
            svc.build_signal_sync_args(env=env, base_dir=Path("/repo"))

    def test_evaluate_signal_health_ok_from_meta(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            signals_file.write_text('{"market_id":"m1","p_yes":0.6}\n', encoding="utf-8")
            meta_file.write_text(
                (
                    '{"updated_at":"%s","signal_count":1}\n'
                    % datetime.now(timezone.utc).isoformat()
                ),
                encoding="utf-8",
            )
            env = {
                "POLYCLAW_SIGNALS_FILE": str(signals_file),
                "POLYCLAW_SIGNALS_META_FILE": str(meta_file),
                "POLYCLAW_REQUIRE_FRESH_SIGNALS": "1",
                "POLYCLAW_MAX_SIGNAL_AGE_SECONDS": "600",
                "POLYCLAW_MIN_SIGNAL_COUNT": "1",
            }

            health = svc.evaluate_signal_health(env=env, base_dir=Path("/repo"))

        self.assertTrue(health["ok"])
        self.assertEqual(1, health["signal_count"])

    def test_evaluate_signal_health_stale(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            signals_file.write_text('{"market_id":"m1","p_yes":0.6}\n', encoding="utf-8")
            stale_time = (datetime.now(timezone.utc) - timedelta(hours=3)).isoformat()
            meta_file.write_text('{"updated_at":"%s","signal_count":1}\n' % stale_time, encoding="utf-8")
            env = {
                "POLYCLAW_SIGNALS_FILE": str(signals_file),
                "POLYCLAW_SIGNALS_META_FILE": str(meta_file),
                "POLYCLAW_REQUIRE_FRESH_SIGNALS": "1",
                "POLYCLAW_MAX_SIGNAL_AGE_SECONDS": "60",
                "POLYCLAW_MIN_SIGNAL_COUNT": "1",
            }

            health = svc.evaluate_signal_health(env=env, base_dir=Path("/repo"))

        self.assertFalse(health["ok"])
        self.assertEqual("stale_signals", health["reason"])

    def test_evaluate_signal_health_low_count(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            signals_file.write_text("", encoding="utf-8")
            meta_file.write_text(
                '{"updated_at":"%s","signal_count":0}\n' % datetime.now(timezone.utc).isoformat(),
                encoding="utf-8",
            )
            env = {
                "POLYCLAW_SIGNALS_FILE": str(signals_file),
                "POLYCLAW_SIGNALS_META_FILE": str(meta_file),
                "POLYCLAW_REQUIRE_FRESH_SIGNALS": "1",
                "POLYCLAW_MAX_SIGNAL_AGE_SECONDS": "600",
                "POLYCLAW_MIN_SIGNAL_COUNT": "1",
            }

            health = svc.evaluate_signal_health(env=env, base_dir=Path("/repo"))

        self.assertFalse(health["ok"])
        self.assertEqual("low_signal_count", health["reason"])


if __name__ == "__main__":
    unittest.main()
