import unittest
import tempfile
from datetime import datetime, timezone, timedelta
from pathlib import Path
from unittest import mock

from tools import run_polymarket_openclaw_service as svc


class TestRunPolymarketOpenclawService(unittest.TestCase):
    def test_build_cycle_args_with_signals_file(self) -> None:
        env = {
            "POLYCLAW_CONFIG_FILE": "/cfg.json",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_SIGNALS_META_FILE": "/signals.meta.json",
            "POLYCLAW_OUTPUT_DIR": "/out",
        }
        argv = svc.build_cycle_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("tools/polymarket_openclaw_cycle.py", text)
        self.assertIn("--config-file /cfg.json", text)
        self.assertIn("--signals-file /signals.jsonl", text)
        self.assertIn("--signals-meta-file /signals.meta.json", text)
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

    def test_build_cycle_args_with_live_execution_flags(self) -> None:
        env = {
            "POLYCLAW_CONFIG_FILE": "/cfg.json",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_LIVE_EXECUTION": "1",
            "POLYCLAW_LIVE_ORDER_TYPE": "fok",
            "POLYCLAW_LIVE_MAX_ORDERS_PER_RUN": "2",
            "POLYCLAW_LIVE_MIN_EXPECTED_VALUE_USD": "0.25",
            "POLYCLAW_LIVE_MIN_STAKE_USD": "1.5",
            "POLYCLAW_LIVE_FAIL_ON_ERROR": "1",
        }
        argv = svc.build_cycle_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--live-execution", text)
        self.assertIn("--live-order-type FOK", text)
        self.assertIn("--live-max-orders 2", text)
        self.assertIn("--live-min-expected-value-usd 0.25", text)
        self.assertIn("--live-min-stake-usd 1.5", text)
        self.assertIn("--live-fail-on-error", text)

    def test_validate_live_runtime_configuration_disabled(self) -> None:
        status = svc.validate_live_runtime_configuration(env={})
        self.assertFalse(status["enabled"])
        self.assertTrue(status["ok"])
        self.assertEqual("disabled", status["reason"])

    def test_validate_live_runtime_configuration_enabled_requires_private_key(self) -> None:
        env = {
            "POLYCLAW_LIVE_EXECUTION": "1",
        }
        status = svc.validate_live_runtime_configuration(env=env)
        self.assertTrue(status["enabled"])
        self.assertFalse(status["ok"])
        self.assertEqual("missing_private_key", status["reason"])

    def test_validate_live_runtime_configuration_enabled_dry_run(self) -> None:
        env = {
            "POLYCLAW_LIVE_EXECUTION": "1",
            "POLYCLAW_LIVE_DRY_RUN": "1",
        }
        status = svc.validate_live_runtime_configuration(env=env)
        self.assertTrue(status["enabled"])
        self.assertTrue(status["dry_run"])
        self.assertTrue(status["ok"])
        self.assertEqual("dry_run", status["reason"])

    def test_validate_live_runtime_configuration_enabled_with_private_key(self) -> None:
        env = {
            "POLYCLAW_LIVE_EXECUTION": "1",
            "POLYCLAW_LIVE_PRIVATE_KEY": "0xabc123",
        }
        status = svc.validate_live_runtime_configuration(env=env)
        self.assertTrue(status["enabled"])
        self.assertFalse(status["dry_run"])
        self.assertTrue(status["ok"])
        self.assertEqual("ready", status["reason"])

    def test_validate_live_runtime_configuration_enabled_with_private_key_file(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            key_file = Path(td) / "key.txt"
            key_file.write_text("0xabc123", encoding="utf-8")
            env = {
                "POLYCLAW_LIVE_EXECUTION": "1",
                "POLYCLAW_LIVE_PRIVATE_KEY_FILE": str(key_file),
            }
            status = svc.validate_live_runtime_configuration(env=env)
        self.assertTrue(status["enabled"])
        self.assertFalse(status["dry_run"])
        self.assertTrue(status["ok"])
        self.assertEqual("ready", status["reason"])

    def test_build_cycle_args_with_min_liquidity_and_volume(self) -> None:
        env = {
            "POLYCLAW_CONFIG_FILE": "/cfg.json",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_MIN_LIQUIDITY_USD": "10000",
            "POLYCLAW_MIN_VOLUME_USD": "5000",
        }
        argv = svc.build_cycle_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--min-liquidity-usd 10000.0", text)
        self.assertIn("--min-volume-usd 5000.0", text)

    def test_build_cycle_args_requires_signal_source(self) -> None:
        env = {"POLYCLAW_CONFIG_FILE": "/cfg.json"}
        with self.assertRaises(ValueError):
            svc.build_cycle_args(env=env, base_dir=Path("/repo"))

    def test_build_signal_sync_args(self) -> None:
        env = {
            "POLYCLAW_OPENCLAW_CMD": "openclaw signals --format jsonl",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_LAST_GOOD_SIGNALS_FILE": "/signals.last_good.jsonl",
            "POLYCLAW_LAST_GOOD_SIGNALS_META_FILE": "/signals.last_good.meta.json",
            "POLYCLAW_SIGNAL_SYNC_MIN_SIGNALS": "3",
            "POLYCLAW_SIGNAL_SYNC_MIN_AGENT_SIGNALS": "2",
            "POLYCLAW_SIGNAL_SYNC_MIN_AGENT_RATIO": "0.4",
            "POLYCLAW_SIGNAL_SYNC_TIMEOUT_SECONDS": "15",
        }
        argv = svc.build_signal_sync_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("tools/openclaw_signal_sync.py", text)
        self.assertIn("--openclaw-cmd openclaw signals --format jsonl", text)
        self.assertIn("--signals-file /signals.jsonl", text)
        self.assertIn("--last-good-signals-file /signals.last_good.jsonl", text)
        self.assertIn("--last-good-meta-file /signals.last_good.meta.json", text)
        self.assertIn("--min-signals 3", text)
        self.assertIn("--min-agent-signals 2", text)
        self.assertIn("--min-agent-ratio 0.4", text)
        self.assertIn("--timeout-seconds 15", text)

    def test_build_signal_sync_args_auto_expands_timeout_for_bridge(self) -> None:
        env = {
            "POLYCLAW_OPENCLAW_CMD": (
                "python3 /repo/tools/openclaw_agent_signal_bridge.py "
                "--timeout-seconds 12 --agent-retries 2 --agent-retry-sleep-ms 500"
            ),
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_SIGNAL_SYNC_TIMEOUT_SECONDS": "30",
        }
        argv = svc.build_signal_sync_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--timeout-seconds 78", text)

    def test_build_signal_sync_args_keeps_larger_timeout(self) -> None:
        env = {
            "POLYCLAW_OPENCLAW_CMD": (
                "python3 /repo/tools/openclaw_agent_signal_bridge.py "
                "--timeout-seconds 12 --agent-retries 2 --agent-retry-sleep-ms 500"
            ),
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_SIGNAL_SYNC_TIMEOUT_SECONDS": "120",
        }
        argv = svc.build_signal_sync_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--timeout-seconds 120", text)

    def test_build_signal_sync_args_auto_expands_timeout_for_bridge_with_cap_fallbacks(self) -> None:
        env = {
            "POLYCLAW_OPENCLAW_CMD": (
                "python3 /repo/tools/openclaw_agent_signal_bridge.py "
                "--timeout-seconds 12 --agent-retries 2 --agent-fallback-retries 0 "
                "--agent-retry-sleep-ms 500 --agent-market-cap-fallbacks 12,8,5"
            ),
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
            "POLYCLAW_SIGNAL_SYNC_TIMEOUT_SECONDS": "30",
        }
        argv = svc.build_signal_sync_args(env=env, base_dir=Path("/repo"))
        text = " ".join(argv)
        self.assertIn("--timeout-seconds 78", text)

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

    def test_run_signal_sync_disabled(self) -> None:
        result = svc.run_signal_sync(env={}, base_dir=Path("/repo"))
        self.assertTrue(result["ok"])
        self.assertFalse(result["ran"])

    def test_run_signal_sync_success(self) -> None:
        env = {
            "POLYCLAW_SYNC_SIGNALS_BEFORE_RUN": "1",
            "POLYCLAW_OPENCLAW_CMD": "openclaw signals --format jsonl",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
        }
        proc = mock.Mock(returncode=0, stdout='{"ok": true, "signal_count": 10}', stderr="")
        with mock.patch.object(svc.subprocess, "run", return_value=proc):
            result = svc.run_signal_sync(env=env, base_dir=Path("/repo"))

        self.assertTrue(result["ran"])
        self.assertTrue(result["ok"])
        self.assertFalse(result["soft_failed"])
        self.assertEqual(0, result["exit_code"])
        self.assertEqual(10, result["result"]["signal_count"])

    def test_run_signal_sync_soft_fail(self) -> None:
        env = {
            "POLYCLAW_SYNC_SIGNALS_BEFORE_RUN": "1",
            "POLYCLAW_SYNC_SOFT_FAIL": "1",
            "POLYCLAW_OPENCLAW_CMD": "openclaw signals --format jsonl",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
        }
        proc = mock.Mock(returncode=1, stdout='{"ok": false, "error": "boom"}', stderr="boom")
        with mock.patch.object(svc.subprocess, "run", return_value=proc):
            result = svc.run_signal_sync(env=env, base_dir=Path("/repo"))

        self.assertTrue(result["ran"])
        self.assertFalse(result["ok"])
        self.assertTrue(result["soft_failed"])
        self.assertEqual(1, result["exit_code"])

    def test_run_signal_sync_hard_fail_raises(self) -> None:
        env = {
            "POLYCLAW_SYNC_SIGNALS_BEFORE_RUN": "1",
            "POLYCLAW_SYNC_SOFT_FAIL": "0",
            "POLYCLAW_OPENCLAW_CMD": "openclaw signals --format jsonl",
            "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
        }
        proc = mock.Mock(returncode=1, stdout='{"ok": false, "error": "boom"}', stderr="boom")
        with mock.patch.object(svc.subprocess, "run", return_value=proc):
            with self.assertRaises(svc.subprocess.CalledProcessError):
                svc.run_signal_sync(env=env, base_dir=Path("/repo"))

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

    def test_evaluate_signal_health_low_agent_signal_count(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            signals_file.write_text(
                "\n".join(
                    [
                        '{"market_id":"m1","p_yes":0.6}',
                        '{"market_id":"m2","p_yes":0.4}',
                        '{"market_id":"m3","p_yes":0.5}',
                        '{"market_id":"m4","p_yes":0.7}',
                    ]
                )
                + "\n",
                encoding="utf-8",
            )
            meta_file.write_text(
                (
                    '{"updated_at":"%s","signal_count":4,"source_counts":{"openclaw_agent":1,"heuristic_fallback":3}}\n'
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
                "POLYCLAW_MIN_AGENT_SIGNAL_COUNT": "2",
            }
            health = svc.evaluate_signal_health(env=env, base_dir=Path("/repo"))

        self.assertFalse(health["ok"])
        self.assertEqual("low_agent_signal_count", health["reason"])
        self.assertEqual(1, health["agent_signal_count"])

    def test_evaluate_signal_health_low_agent_signal_ratio(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            signals_file.write_text(
                "\n".join(
                    [
                        '{"market_id":"m1","p_yes":0.6}',
                        '{"market_id":"m2","p_yes":0.4}',
                        '{"market_id":"m3","p_yes":0.5}',
                        '{"market_id":"m4","p_yes":0.7}',
                    ]
                )
                + "\n",
                encoding="utf-8",
            )
            meta_file.write_text(
                (
                    '{"updated_at":"%s","signal_count":4,"source_counts":{"openclaw_agent":1,"heuristic_fallback":3}}\n'
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
                "POLYCLAW_MIN_AGENT_SIGNAL_RATIO": "0.5",
            }
            health = svc.evaluate_signal_health(env=env, base_dir=Path("/repo"))

        self.assertFalse(health["ok"])
        self.assertEqual("low_agent_signal_ratio", health["reason"])
        self.assertAlmostEqual(0.25, health["agent_signal_ratio"])

    def test_evaluate_signal_health_agent_signal_ratio_ok(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            meta_file = Path(td) / "signals.meta.json"
            signals_file.write_text(
                "\n".join(
                    [
                        '{"market_id":"m1","p_yes":0.6}',
                        '{"market_id":"m2","p_yes":0.4}',
                        '{"market_id":"m3","p_yes":0.5}',
                        '{"market_id":"m4","p_yes":0.7}',
                    ]
                )
                + "\n",
                encoding="utf-8",
            )
            meta_file.write_text(
                (
                    '{"updated_at":"%s","signal_count":4,"source_counts":{"openclaw_agent":2,"heuristic_fallback":2}}\n'
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
                "POLYCLAW_MIN_AGENT_SIGNAL_RATIO": "0.5",
            }
            health = svc.evaluate_signal_health(env=env, base_dir=Path("/repo"))

        self.assertTrue(health["ok"])
        self.assertAlmostEqual(0.5, health["agent_signal_ratio"])

    def test_evaluate_signal_health_reads_source_counts_from_signals_when_meta_missing(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            signals_file.write_text(
                "\n".join(
                    [
                        '{"market_id":"m1","p_yes":0.6,"source":"openclaw_agent"}',
                        '{"market_id":"m2","p_yes":0.4,"source":"heuristic_fallback"}',
                        '{"market_id":"m3","p_yes":0.5,"source":"openclaw_agent"}',
                        '{"market_id":"m4","p_yes":0.7,"source":"heuristic_fallback"}',
                    ]
                )
                + "\n",
                encoding="utf-8",
            )
            env = {
                "POLYCLAW_SIGNALS_FILE": str(signals_file),
                "POLYCLAW_REQUIRE_FRESH_SIGNALS": "1",
                "POLYCLAW_MAX_SIGNAL_AGE_SECONDS": "600",
                "POLYCLAW_MIN_SIGNAL_COUNT": "1",
                "POLYCLAW_MIN_AGENT_SIGNAL_RATIO": "0.5",
            }
            health = svc.evaluate_signal_health(env=env, base_dir=Path("/repo"))

        self.assertTrue(health["ok"])
        self.assertEqual(2, health["agent_signal_count"])
        self.assertAlmostEqual(0.5, health["agent_signal_ratio"])

    def test_evaluate_signal_health_fails_from_signals_when_meta_missing(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            signals_file = Path(td) / "signals.jsonl"
            signals_file.write_text(
                "\n".join(
                    [
                        '{"market_id":"m1","p_yes":0.6,"source":"heuristic_fallback"}',
                        '{"market_id":"m2","p_yes":0.4,"source":"heuristic_fallback"}',
                        '{"market_id":"m3","p_yes":0.5,"source":"heuristic_fallback"}',
                        '{"market_id":"m4","p_yes":0.7,"source":"heuristic_fallback"}',
                    ]
                )
                + "\n",
                encoding="utf-8",
            )
            env = {
                "POLYCLAW_SIGNALS_FILE": str(signals_file),
                "POLYCLAW_REQUIRE_FRESH_SIGNALS": "1",
                "POLYCLAW_MAX_SIGNAL_AGE_SECONDS": "600",
                "POLYCLAW_MIN_SIGNAL_COUNT": "1",
                "POLYCLAW_MIN_AGENT_SIGNAL_RATIO": "0.25",
            }
            health = svc.evaluate_signal_health(env=env, base_dir=Path("/repo"))

        self.assertFalse(health["ok"])
        self.assertEqual("low_agent_signal_ratio", health["reason"])
        self.assertEqual(0, health["agent_signal_count"])

    def test_select_effective_signal_source_uses_last_good(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            primary_signals = Path(td) / "signals.jsonl"
            primary_meta = Path(td) / "signals.meta.json"
            last_good_signals = Path(td) / "signals.last_good.jsonl"
            last_good_meta = Path(td) / "signals.last_good.meta.json"

            primary_signals.write_text('{"market_id":"m1","p_yes":0.6,"source":"heuristic_fallback"}\n', encoding="utf-8")
            primary_meta.write_text(
                (
                    '{"updated_at":"%s","signal_count":1,"source_counts":{"heuristic_fallback":1}}'
                    % datetime.now(timezone.utc).isoformat()
                ),
                encoding="utf-8",
            )
            last_good_signals.write_text('{"market_id":"m1","p_yes":0.6,"source":"openclaw_agent"}\n', encoding="utf-8")
            last_good_meta.write_text(
                (
                    '{"updated_at":"%s","signal_count":1,"source_counts":{"openclaw_agent":1}}'
                    % datetime.now(timezone.utc).isoformat()
                ),
                encoding="utf-8",
            )

            env = {
                "POLYCLAW_SIGNALS_FILE": str(primary_signals),
                "POLYCLAW_SIGNALS_META_FILE": str(primary_meta),
                "POLYCLAW_LAST_GOOD_SIGNALS_FILE": str(last_good_signals),
                "POLYCLAW_LAST_GOOD_SIGNALS_META_FILE": str(last_good_meta),
                "POLYCLAW_USE_LAST_GOOD_SIGNALS_ON_BAD": "1",
                "POLYCLAW_REQUIRE_FRESH_SIGNALS": "1",
                "POLYCLAW_MAX_SIGNAL_AGE_SECONDS": "600",
                "POLYCLAW_MIN_SIGNAL_COUNT": "1",
                "POLYCLAW_MIN_AGENT_SIGNAL_COUNT": "1",
            }
            selected = svc.select_effective_signal_source(env=env, base_dir=Path("/repo"))

        self.assertTrue(selected["used_last_good_signals"])
        self.assertTrue(selected["signal_health"]["ok"])
        self.assertEqual(str(last_good_signals), selected["selected_env"]["POLYCLAW_SIGNALS_FILE"])

    def test_select_effective_signal_source_without_last_good_keeps_primary(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            primary_signals = Path(td) / "signals.jsonl"
            primary_meta = Path(td) / "signals.meta.json"
            primary_signals.write_text('{"market_id":"m1","p_yes":0.6,"source":"heuristic_fallback"}\n', encoding="utf-8")
            primary_meta.write_text(
                (
                    '{"updated_at":"%s","signal_count":1,"source_counts":{"heuristic_fallback":1}}'
                    % datetime.now(timezone.utc).isoformat()
                ),
                encoding="utf-8",
            )
            env = {
                "POLYCLAW_SIGNALS_FILE": str(primary_signals),
                "POLYCLAW_SIGNALS_META_FILE": str(primary_meta),
                "POLYCLAW_USE_LAST_GOOD_SIGNALS_ON_BAD": "1",
                "POLYCLAW_REQUIRE_FRESH_SIGNALS": "1",
                "POLYCLAW_MAX_SIGNAL_AGE_SECONDS": "600",
                "POLYCLAW_MIN_SIGNAL_COUNT": "1",
                "POLYCLAW_MIN_AGENT_SIGNAL_COUNT": "1",
            }
            selected = svc.select_effective_signal_source(env=env, base_dir=Path("/repo"))

        self.assertFalse(selected["used_last_good_signals"])
        self.assertFalse(selected["signal_health"]["ok"])

    def test_format_signal_source_for_log_hides_full_env(self) -> None:
        payload = {
            "used_last_good_signals": False,
            "selected_env": {
                "POLYCLAW_SIGNALS_FILE": "/signals.jsonl",
                "POLYCLAW_SIGNALS_META_FILE": "/signals.meta.json",
                "SWIMMY_DISCORD_BOT_TOKEN": "secret",
            },
            "signal_health": {"ok": False, "reason": "low_agent_signal_count"},
            "last_good_signal_health": {"ok": True},
        }
        formatted = svc.format_signal_source_for_log(payload)
        self.assertEqual("/signals.jsonl", formatted["selected_signals_file"])
        self.assertEqual("/signals.meta.json", formatted["selected_signals_meta_file"])
        self.assertNotIn("selected_env", formatted)


if __name__ == "__main__":
    unittest.main()
