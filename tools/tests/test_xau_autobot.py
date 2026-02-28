import unittest
import tempfile
import os
import json
from types import SimpleNamespace
from unittest import mock
from pathlib import Path
from datetime import datetime, timezone

import tools.xau_autobot as xau_autobot_module
from tools.xau_autobot import (
    BotConfig,
    LiveUnderperformanceGuard,
    Mt5Gateway,
    append_runtime_journal,
    atr_last,
    atr_pct_series,
    build_sl_tp,
    can_open_trade,
    detect_regime,
    decide_signal,
    decide_signal_with_mode,
    decide_reversion_signal,
    ema_last,
    evaluate_once,
    extract_live_underperforming_signal,
    is_session_allowed,
    next_session_open_utc,
    resolve_config_path,
    runtime_trial_run_id_from_env,
    session_window_jst_label,
    should_send_session_block_notification,
    should_send_wait_summary_notification,
    runtime_journal_path_from_env,
    validate_trade_comment,
    validate_runtime_timeframe_contract,
    wait_reason_key,
    volatility_filter_pass,
    update_runtime_metrics,
    hydrate_m45_commander_from_profile,
    _tf_to_mt5,
)


class TestXauAutoBotMath(unittest.TestCase):
    def test_ema_last_calculation(self):
        values = [1.0, 2.0, 3.0, 4.0, 5.0]
        self.assertAlmostEqual(ema_last(values, period=3), 4.0625, places=6)

    def test_atr_last_calculation(self):
        highs = [10.0, 11.0, 12.0]
        lows = [9.0, 10.0, 11.0]
        closes = [9.5, 10.5, 11.5]
        self.assertAlmostEqual(atr_last(highs, lows, closes, period=3), 4.0 / 3.0, places=6)

    def test_decide_signal_buy_on_uptrend_pullback(self):
        side = decide_signal(
            last_close=1980.0,
            ema_fast=1982.0,
            ema_slow=1978.0,
            atr_value=2.0,
            pullback_atr=0.5,
        )
        self.assertEqual(side, "BUY")

    def test_decide_signal_sell_on_downtrend_pullback(self):
        side = decide_signal(
            last_close=2000.0,
            ema_fast=1998.0,
            ema_slow=2002.0,
            atr_value=2.0,
            pullback_atr=0.5,
        )
        self.assertEqual(side, "SELL")

    def test_decide_signal_hold_when_not_pulled_back(self):
        side = decide_signal(
            last_close=1982.5,
            ema_fast=1982.0,
            ema_slow=1978.0,
            atr_value=2.0,
            pullback_atr=0.5,
        )
        self.assertEqual(side, "HOLD")

    def test_detect_regime_trend_and_range(self):
        self.assertEqual(
            detect_regime(
                ema_fast=2002.0,
                ema_slow=1998.0,
                atr_value=2.0,
                trend_threshold=1.5,
            ),
            "trend",
        )
        self.assertEqual(
            detect_regime(
                ema_fast=2000.2,
                ema_slow=1999.8,
                atr_value=2.0,
                trend_threshold=1.5,
            ),
            "range",
        )

    def test_decide_reversion_signal(self):
        self.assertEqual(
            decide_reversion_signal(
                last_close=1996.0,
                ema_anchor=2000.0,
                atr_value=2.0,
                reversion_atr=1.5,
            ),
            "BUY",
        )
        self.assertEqual(
            decide_reversion_signal(
                last_close=2004.0,
                ema_anchor=2000.0,
                atr_value=2.0,
                reversion_atr=1.5,
            ),
            "SELL",
        )
        self.assertEqual(
            decide_reversion_signal(
                last_close=2001.0,
                ema_anchor=2000.0,
                atr_value=2.0,
                reversion_atr=1.5,
            ),
            "HOLD",
        )

    def test_decide_signal_with_mode_hybrid_switches_by_regime(self):
        trend = decide_signal_with_mode(
            strategy_mode="hybrid",
            last_close=1979.0,
            ema_fast=1982.0,
            ema_slow=1978.0,
            atr_value=2.0,
            pullback_atr=1.0,
            reversion_atr=1.0,
            trend_threshold=1.0,
        )
        self.assertEqual(trend["regime"], "trend")
        self.assertEqual(trend["source"], "trend")
        self.assertEqual(trend["signal"], "BUY")

        ranging = decide_signal_with_mode(
            strategy_mode="hybrid",
            last_close=1997.5,
            ema_fast=2000.2,
            ema_slow=1999.8,
            atr_value=2.0,
            pullback_atr=1.0,
            reversion_atr=1.0,
            trend_threshold=1.0,
        )
        self.assertEqual(ranging["regime"], "range")
        self.assertEqual(ranging["source"], "reversion")
        self.assertEqual(ranging["signal"], "BUY")

    def test_build_sl_tp_buy_and_sell(self):
        buy_sl, buy_tp = build_sl_tp(
            side="BUY",
            entry_price=2000.0,
            atr_value=2.0,
            sl_atr=1.5,
            tp_atr=2.0,
        )
        self.assertAlmostEqual(buy_sl, 1997.0, places=6)
        self.assertAlmostEqual(buy_tp, 2004.0, places=6)

        sell_sl, sell_tp = build_sl_tp(
            side="SELL",
            entry_price=2000.0,
            atr_value=2.0,
            sl_atr=1.5,
            tp_atr=2.0,
        )
        self.assertAlmostEqual(sell_sl, 2003.0, places=6)
        self.assertAlmostEqual(sell_tp, 1996.0, places=6)

    def test_can_open_trade_respects_limits(self):
        self.assertTrue(
            can_open_trade(spread_points=15.0, max_spread_points=20.0, open_positions=0, max_positions=1)
        )
        self.assertFalse(
            can_open_trade(spread_points=25.0, max_spread_points=20.0, open_positions=0, max_positions=1)
        )
        self.assertFalse(
            can_open_trade(spread_points=10.0, max_spread_points=20.0, open_positions=1, max_positions=1)
        )

    def test_is_session_allowed_for_standard_day_window(self):
        self.assertTrue(is_session_allowed(hour_utc=7, session_start=7, session_end=19))
        self.assertTrue(is_session_allowed(hour_utc=19, session_start=7, session_end=19))
        self.assertFalse(is_session_allowed(hour_utc=6, session_start=7, session_end=19))
        self.assertFalse(is_session_allowed(hour_utc=21, session_start=7, session_end=19))

    def test_is_session_allowed_for_wraparound_window(self):
        self.assertTrue(is_session_allowed(hour_utc=23, session_start=22, session_end=3))
        self.assertTrue(is_session_allowed(hour_utc=2, session_start=22, session_end=3))
        self.assertFalse(is_session_allowed(hour_utc=12, session_start=22, session_end=3))

    def test_volatility_filter_passes_when_disabled(self):
        self.assertTrue(
            volatility_filter_pass(
                atr_pct_values=[0.001, 0.0011, 0.0009],
                min_ratio_to_median=0.0,
                max_ratio_to_median=999.0,
                window=3,
                min_samples=3,
            )
        )

    def test_volatility_filter_respects_median_band(self):
        values = [0.0010, 0.0011, 0.0012, 0.0013, 0.0014]
        self.assertTrue(
            volatility_filter_pass(
                atr_pct_values=values,
                min_ratio_to_median=0.8,
                max_ratio_to_median=1.4,
                window=5,
                min_samples=5,
            )
        )
        self.assertFalse(
            volatility_filter_pass(
                atr_pct_values=values[:-1] + [0.0025],
                min_ratio_to_median=0.8,
                max_ratio_to_median=1.4,
                window=5,
                min_samples=5,
            )
        )

    def test_atr_pct_series(self):
        atr_values = [2.0, 1.0, 0.0]
        close_values = [2000.0, 1000.0, 500.0]
        result = atr_pct_series(atr_values, close_values)
        self.assertAlmostEqual(result[0], 0.001, places=8)
        self.assertAlmostEqual(result[1], 0.001, places=8)
        self.assertAlmostEqual(result[2], 0.0, places=8)

    def test_session_window_jst_label(self):
        self.assertEqual(session_window_jst_label(7, 19), "16:00-04:00 JST")
        self.assertEqual(session_window_jst_label(0, 23), "09:00-08:00 JST")

    def test_next_session_open_utc(self):
        now = datetime(2026, 2, 18, 5, 0, 0, tzinfo=timezone.utc)
        next_open = next_session_open_utc(now, session_start=7)
        self.assertEqual(next_open.hour, 7)
        self.assertEqual(next_open.day, 18)

        now_late = datetime(2026, 2, 18, 22, 0, 0, tzinfo=timezone.utc)
        next_open_late = next_session_open_utc(now_late, session_start=7)
        self.assertEqual(next_open_late.hour, 7)
        self.assertEqual(next_open_late.day, 19)

    def test_should_send_session_block_notification(self):
        payload = {"action": "BLOCKED", "reason": "session"}
        self.assertTrue(
            should_send_session_block_notification(payload, last_sent_unix=0, now_unix=1000, cooldown_sec=3600)
        )
        self.assertFalse(
            should_send_session_block_notification(payload, last_sent_unix=900, now_unix=1000, cooldown_sec=3600)
        )
        self.assertFalse(
            should_send_session_block_notification(
                {"action": "HOLD"}, last_sent_unix=0, now_unix=1000, cooldown_sec=3600
            )
        )

    def test_wait_reason_key(self):
        self.assertEqual(wait_reason_key({"action": "SKIP", "reason": "no_new_bar"}), "skip:no_new_bar")
        self.assertEqual(wait_reason_key({"action": "BLOCKED", "reason": "volatility"}), "blocked:volatility")
        self.assertEqual(wait_reason_key({"action": "HOLD"}), "hold:hold")
        self.assertEqual(wait_reason_key({"action": "ORDER"}), "")

    def test_should_send_wait_summary_notification(self):
        self.assertFalse(
            should_send_wait_summary_notification(
                wait_streak_total=0,
                last_sent_unix=0,
                now_unix=1000,
                cooldown_sec=1800,
            )
        )
        self.assertTrue(
            should_send_wait_summary_notification(
                wait_streak_total=5,
                last_sent_unix=0,
                now_unix=1000,
                cooldown_sec=1800,
            )
        )
        self.assertFalse(
            should_send_wait_summary_notification(
                wait_streak_total=5,
                last_sent_unix=900,
                now_unix=1000,
                cooldown_sec=1800,
            )
        )

    def test_update_runtime_metrics_counts_gap_rejects_and_signals(self):
        metrics = {
            "gate_check_count": 0,
            "gate_reject_gap_count": 0,
            "signal_counts": {"BUY": 0, "SELL": 0, "HOLD": 0},
        }
        update_runtime_metrics(
            metrics,
            {
                "action": "HOLD",
                "reason": "ema_gap_out_of_range",
                "gap_gate_checked": True,
            },
        )
        update_runtime_metrics(
            metrics,
            {
                "action": "ORDER",
                "side": "BUY",
                "gap_gate_checked": True,
            },
        )
        update_runtime_metrics(
            metrics,
            {
                "action": "BLOCKED",
                "reason": "session",
                "gap_gate_checked": False,
            },
        )
        self.assertEqual(metrics["gate_check_count"], 2)
        self.assertEqual(metrics["gate_reject_gap_count"], 1)
        self.assertEqual(metrics["signal_counts"]["HOLD"], 1)
        self.assertEqual(metrics["signal_counts"]["BUY"], 1)
        self.assertEqual(metrics["signal_counts"]["SELL"], 0)


class TestXauAutoBotConfig(unittest.TestCase):
    def test_config_defaults(self):
        cfg = BotConfig.from_dict({})
        self.assertEqual(cfg.symbol, "XAUUSD")
        self.assertEqual(cfg.timeframe, "M5")
        self.assertEqual(cfg.max_positions, 1)
        self.assertGreater(cfg.max_spread_points, 0.0)
        self.assertAlmostEqual(cfg.min_ema_gap_over_atr, 0.9, places=6)
        self.assertAlmostEqual(cfg.max_ema_gap_over_atr, 2.5, places=6)

    def test_config_supports_hybrid_mode_fields(self):
        cfg = BotConfig.from_dict(
            {
                "strategy_mode": "hybrid",
                "regime_trend_threshold": 1.1,
                "reversion_atr": 1.3,
                "reversion_sl_atr": 1.0,
                "reversion_tp_atr": 1.4,
            }
        )
        self.assertEqual(cfg.strategy_mode, "hybrid")
        self.assertAlmostEqual(cfg.regime_trend_threshold, 1.1, places=6)
        self.assertAlmostEqual(cfg.reversion_atr, 1.3, places=6)
        self.assertAlmostEqual(cfg.reversion_sl_atr, 1.0, places=6)
        self.assertAlmostEqual(cfg.reversion_tp_atr, 1.4, places=6)

    def test_config_rejects_invalid_strategy_mode(self):
        with self.assertRaises(ValueError):
            BotConfig.from_dict({"strategy_mode": "unsupported"})

    def test_config_rejects_m45_gate_on_non_m20_timeframe(self):
        with self.assertRaises(ValueError):
            BotConfig.from_dict(
                {
                    "timeframe": "M15",
                    "m45_bias_gate_policy": "block_opposite",
                }
            )

    def test_validate_trade_comment_accepts_31_chars(self):
        comment = "x" * 31
        self.assertEqual(validate_trade_comment(comment), comment)

    def test_validate_trade_comment_rejects_over_31_chars(self):
        with self.assertRaises(ValueError):
            validate_trade_comment("x" * 32)

    def test_bot_config_rejects_over_31_char_comment(self):
        with self.assertRaises(ValueError):
            BotConfig.from_dict({"comment": "x" * 32})

    def test_runtime_journal_path_from_env_default(self):
        with mock.patch.dict(os.environ, {}, clear=True):
            path = Path(runtime_journal_path_from_env())
        expected = (
            Path(xau_autobot_module.__file__).resolve().parent.parent
            / "data/reports/xau_autobot_runtime_journal_latest.jsonl"
        )
        self.assertEqual(path, expected)
        self.assertTrue(path.is_absolute())

    def test_runtime_journal_path_from_env_override(self):
        with mock.patch.dict(
            os.environ,
            {"XAU_AUTOBOT_RUNTIME_JOURNAL_PATH": "data/reports/custom_runtime_journal.jsonl"},
            clear=True,
        ):
            path = Path(runtime_journal_path_from_env())
        expected = Path(xau_autobot_module.__file__).resolve().parent.parent / "data/reports/custom_runtime_journal.jsonl"
        self.assertEqual(path, expected)
        self.assertTrue(path.is_absolute())

    def test_runtime_trial_run_id_prefers_env_value(self):
        with tempfile.TemporaryDirectory() as td:
            meta = Path(td) / "current_run.json"
            meta.write_text(
                json.dumps(
                    {
                        "run_id": "trial_v2_from_meta",
                        "trial_config": "tools/configs/a.json",
                    }
                ),
                encoding="utf-8",
            )
            with mock.patch.dict(
                os.environ,
                {
                    "XAU_AUTOBOT_TRIAL_RUN_ID": "trial_v2_from_env",
                    "XAU_AUTOBOT_TRIAL_RUN_META_PATH": str(meta),
                },
                clear=True,
            ):
                run_id = runtime_trial_run_id_from_env(config_path="tools/configs/a.json")
        self.assertEqual(run_id, "trial_v2_from_env")

    def test_runtime_trial_run_id_falls_back_to_meta_when_env_missing(self):
        with tempfile.TemporaryDirectory() as td:
            meta = Path(td) / "current_run.json"
            meta.write_text(
                json.dumps(
                    {
                        "run_id": "trial_v2_from_meta",
                        "trial_config": "tools/configs/xau_autobot.trial_v2_test.json",
                    }
                ),
                encoding="utf-8",
            )
            with mock.patch.dict(
                os.environ,
                {"XAU_AUTOBOT_TRIAL_RUN_META_PATH": str(meta)},
                clear=True,
            ):
                run_id = runtime_trial_run_id_from_env(
                    config_path="/repo/tools/configs/xau_autobot.trial_v2_test.json"
                )
        self.assertEqual(run_id, "trial_v2_from_meta")

    def test_runtime_trial_run_id_ignores_meta_when_config_mismatch(self):
        with tempfile.TemporaryDirectory() as td:
            meta = Path(td) / "current_run.json"
            meta.write_text(
                json.dumps(
                    {
                        "run_id": "trial_v2_from_meta",
                        "trial_config": "tools/configs/other_config.json",
                    }
                ),
                encoding="utf-8",
            )
            with mock.patch.dict(
                os.environ,
                {"XAU_AUTOBOT_TRIAL_RUN_META_PATH": str(meta)},
                clear=True,
            ):
                run_id = runtime_trial_run_id_from_env(
                    config_path="/repo/tools/configs/xau_autobot.trial_v2_test.json"
                )
        self.assertEqual(run_id, "")

    def test_runtime_trial_run_id_scans_variant_meta_files_when_default_mismatch(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            reports = root / "data" / "reports"
            reports.mkdir(parents=True, exist_ok=True)
            (reports / "xau_autobot_trial_v2_current_run.json").write_text(
                json.dumps(
                    {
                        "run_id": "trial_v2_primary",
                        "trial_config": "tools/configs/xau_autobot.primary.json",
                    }
                ),
                encoding="utf-8",
            )
            (reports / "xau_autobot_trial_v2_current_run_r2.json").write_text(
                json.dumps(
                    {
                        "run_id": "trial_v2_r2_match",
                        "trial_config": "tools/configs/xau_autobot.target.json",
                    }
                ),
                encoding="utf-8",
            )
            with mock.patch.object(xau_autobot_module, "REPO_ROOT", root):
                with mock.patch.dict(os.environ, {}, clear=True):
                    run_id = runtime_trial_run_id_from_env(
                        config_path="/repo/tools/configs/xau_autobot.target.json"
                    )
        self.assertEqual(run_id, "trial_v2_r2_match")

    def test_runtime_trial_run_id_returns_empty_when_no_variant_meta_matches(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            reports = root / "data" / "reports"
            reports.mkdir(parents=True, exist_ok=True)
            for suffix in ("", "_r1", "_r2", "_r3"):
                (reports / f"xau_autobot_trial_v2_current_run{suffix}.json").write_text(
                    json.dumps(
                        {
                            "run_id": f"trial_v2{suffix or '_primary'}",
                            "trial_config": "tools/configs/xau_autobot.other.json",
                        }
                    ),
                    encoding="utf-8",
                )
            with mock.patch.object(xau_autobot_module, "REPO_ROOT", root):
                with mock.patch.dict(os.environ, {}, clear=True):
                    run_id = runtime_trial_run_id_from_env(
                        config_path="/repo/tools/configs/xau_autobot.target.json"
                    )
        self.assertEqual(run_id, "")

    def test_append_runtime_journal_appends_json_line(self):
        with tempfile.TemporaryDirectory() as td:
            out = Path(td) / "runtime.jsonl"
            append_runtime_journal(out, {"action": "HOLD", "timestamp_utc": "2026-02-26T00:00:00+00:00"})
            append_runtime_journal(out, {"action": "ORDER", "timestamp_utc": "2026-02-26T00:05:00+00:00"})
            lines = out.read_text(encoding="utf-8").strip().splitlines()
            self.assertEqual(len(lines), 2)
            self.assertEqual(json.loads(lines[0])["action"], "HOLD")
            self.assertEqual(json.loads(lines[1])["action"], "ORDER")

    def test_hydrate_m45_commander_from_profile_loads_profile_values(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            config_path = root / "e10.json"
            profile_path = root / "m45.json"
            config_path.write_text("{}", encoding="utf-8")
            profile_path.write_text(
                json.dumps(
                    {
                        "timeframe": "M45",
                        "bars": 500,
                        "fast_ema": 11,
                        "slow_ema": 33,
                        "atr_period": 8,
                        "regime_trend_threshold": 2.8,
                    }
                ),
                encoding="utf-8",
            )
            cfg = BotConfig.from_dict(
                {
                    "timeframe": "M20",
                    "m45_bias_gate_policy": "block_opposite",
                    "m45_commander_config_path": "m45.json",
                    "m45_commander_source_bars": 600,
                }
            )
            hydrated = hydrate_m45_commander_from_profile(cfg, config_path=str(config_path))
            self.assertEqual(hydrated.m45_commander_fast_ema, 11)
            self.assertEqual(hydrated.m45_commander_slow_ema, 33)
            self.assertEqual(hydrated.m45_commander_atr_period, 8)
            self.assertAlmostEqual(hydrated.m45_commander_regime_trend_threshold, 2.8, places=6)
            self.assertEqual(hydrated.m45_commander_source_bars, 1500)

    def test_resolve_config_path_prefers_explicit(self):
        with tempfile.TemporaryDirectory() as td:
            p = Path(td) / "explicit.json"
            p.write_text("{}", encoding="utf-8")
            resolved = resolve_config_path(str(p), default_candidates=[])
            self.assertEqual(resolved, str(p))

    def test_resolve_config_path_uses_first_existing_default(self):
        with tempfile.TemporaryDirectory() as td:
            p1 = Path(td) / "missing.json"
            p2 = Path(td) / "active.json"
            p2.write_text("{}", encoding="utf-8")
            resolved = resolve_config_path("", default_candidates=[str(p1), str(p2)])
            self.assertEqual(resolved, str(p2))

    def test_resolve_config_path_returns_empty_when_no_candidates_exist(self):
        with tempfile.TemporaryDirectory() as td:
            p1 = Path(td) / "missing-a.json"
            p2 = Path(td) / "missing-b.json"
            resolved = resolve_config_path("", default_candidates=[str(p1), str(p2)])
            self.assertEqual(resolved, "")

    def test_extract_live_underperforming_signal(self):
        report = {
            "promotion_blocked": False,
            "live_gap": {
                "sample_quality": "ok",
                "underperforming": True,
                "underperforming_reasons": ["live_pf_below_1", "pf_gap_large"],
            },
        }
        under, reasons = extract_live_underperforming_signal(report)
        self.assertTrue(under)
        self.assertEqual(reasons, ["live_pf_below_1", "pf_gap_large"])

    def test_live_underperformance_guard_triggers_after_consecutive_alerts(self):
        with tempfile.TemporaryDirectory() as td:
            report_path = Path(td) / "promotion.json"
            report_path.write_text(
                '{"generated_at":"2026-02-22T11:00:00+00:00","promotion_blocked":true,'
                '"live_gap":{"sample_quality":"ok","underperforming":true,'
                '"underperforming_reasons":["live_pf_below_1"]}}',
                encoding="utf-8",
            )
            guard = LiveUnderperformanceGuard(
                enabled=True,
                report_path=report_path,
                min_streak=2,
                max_report_age_hours=72.0,
            )
            self.assertIsNone(guard.check(now_utc=datetime(2026, 2, 22, 11, 5, tzinfo=timezone.utc)))
            blocked = guard.check(now_utc=datetime(2026, 2, 22, 11, 10, tzinfo=timezone.utc))
            self.assertIsNotNone(blocked)
            self.assertEqual(blocked["action"], "BLOCKED")
            self.assertEqual(blocked["reason"], "live_underperforming_guard")
            self.assertEqual(blocked["guard_streak"], 2)

    def test_live_underperformance_guard_resets_streak_on_recovery(self):
        with tempfile.TemporaryDirectory() as td:
            report_path = Path(td) / "promotion.json"
            guard = LiveUnderperformanceGuard(
                enabled=True,
                report_path=report_path,
                min_streak=2,
                max_report_age_hours=72.0,
            )
            report_path.write_text(
                '{"promotion_blocked":true,"live_gap":{"sample_quality":"ok","underperforming":true}}',
                encoding="utf-8",
            )
            self.assertIsNone(guard.check(now_utc=datetime(2026, 2, 22, 11, 0, tzinfo=timezone.utc)))
            report_path.write_text(
                '{"promotion_blocked":false,"live_gap":{"sample_quality":"ok","underperforming":false}}',
                encoding="utf-8",
            )
            self.assertIsNone(guard.check(now_utc=datetime(2026, 2, 22, 11, 5, tzinfo=timezone.utc)))
            report_path.write_text(
                '{"promotion_blocked":true,"live_gap":{"sample_quality":"ok","underperforming":true}}',
                encoding="utf-8",
            )
            self.assertIsNone(guard.check(now_utc=datetime(2026, 2, 22, 11, 10, tzinfo=timezone.utc)))
            blocked = guard.check(now_utc=datetime(2026, 2, 22, 11, 15, tzinfo=timezone.utc))
            self.assertIsNotNone(blocked)
            self.assertEqual(blocked["guard_streak"], 2)

    def test_live_underperformance_guard_skips_stale_report(self):
        with tempfile.TemporaryDirectory() as td:
            report_path = Path(td) / "promotion.json"
            report_path.write_text(
                '{"promotion_blocked":true,"live_gap":{"sample_quality":"ok","underperforming":true}}',
                encoding="utf-8",
            )
            stale_ts = datetime(2026, 2, 20, 0, 0, tzinfo=timezone.utc).timestamp()
            os.utime(report_path, (stale_ts, stale_ts))
            guard = LiveUnderperformanceGuard(
                enabled=True,
                report_path=report_path,
                min_streak=1,
                max_report_age_hours=12.0,
            )
            blocked = guard.check(now_utc=datetime(2026, 2, 22, 11, 0, tzinfo=timezone.utc))
            self.assertIsNone(blocked)

    def test_live_underperformance_guard_default_is_disabled(self):
        with mock.patch.dict(
            os.environ,
            {"XAU_AUTOBOT_FAIL_ON_LIVE_UNDERPERFORMING": "1"},
            clear=True,
        ):
            guard = LiveUnderperformanceGuard.from_env()
            self.assertFalse(guard.enabled)

    def test_live_underperformance_guard_can_be_enabled_explicitly(self):
        with mock.patch.dict(
            os.environ,
            {
                "XAU_AUTOBOT_LIVE_GUARD_ENABLED": "1",
                "XAU_AUTOBOT_LIVE_GUARD_MIN_STREAK": "3",
                "XAU_AUTOBOT_LIVE_GUARD_MAX_REPORT_AGE_HOURS": "12",
            },
            clear=True,
        ):
            guard = LiveUnderperformanceGuard.from_env()
            self.assertTrue(guard.enabled)
            self.assertEqual(guard.min_streak, 3)
            self.assertEqual(guard.max_report_age_hours, 12.0)


class TestXauAutoBotTimeframeMapping(unittest.TestCase):
    def _fake_mt5(self, *, with_m20: bool = True):
        payload = {
            "TIMEFRAME_M1": 1,
            "TIMEFRAME_M5": 5,
            "TIMEFRAME_M15": 15,
            "TIMEFRAME_M30": 30,
            "TIMEFRAME_H1": 60,
            "TIMEFRAME_H4": 240,
        }
        if with_m20:
            payload["TIMEFRAME_M20"] = 20
        return SimpleNamespace(**payload)

    def test_tf_to_mt5_supports_m20(self):
        fake_mt5 = self._fake_mt5(with_m20=True)
        with mock.patch.object(xau_autobot_module, "mt5", fake_mt5):
            self.assertEqual(_tf_to_mt5("M20"), 20)
            self.assertEqual(_tf_to_mt5("m20"), 20)

    def test_tf_to_mt5_rejects_unknown_label(self):
        fake_mt5 = self._fake_mt5(with_m20=True)
        with mock.patch.object(xau_autobot_module, "mt5", fake_mt5):
            with self.assertRaises(ValueError):
                _tf_to_mt5("M45")

    def test_tf_to_mt5_fail_closed_when_constant_is_missing(self):
        fake_mt5 = self._fake_mt5(with_m20=False)
        with mock.patch.object(xau_autobot_module, "mt5", fake_mt5):
            with self.assertRaises(ValueError):
                _tf_to_mt5("M20")

    def test_validate_runtime_timeframe_contract_rejects_research_tf_in_live_mode(self):
        with self.assertRaises(ValueError):
            validate_runtime_timeframe_contract("H2", mode="live")

    def test_validate_runtime_timeframe_contract_rejects_research_tf_in_research_mode(self):
        with self.assertRaises(ValueError):
            validate_runtime_timeframe_contract("H5", mode="research")


class _FakeGateway:
    def __init__(
        self,
        *,
        rates,
        open_positions,
        has_opposite,
        close_results,
        open_positions_after_close,
        fetch_error=None,
        rates_by_tf=None,
    ):
        self._rates = rates
        self._open_positions = open_positions
        self._has_opposite = has_opposite
        self._close_results = close_results
        self._open_positions_after_close = open_positions_after_close
        self._fetch_error = fetch_error
        self._rates_by_tf = {str(k).upper(): v for k, v in (rates_by_tf or {}).items()}
        self.orders = []
        self.close_calls = 0

    def fetch_rates(self):
        if self._fetch_error is not None:
            raise self._fetch_error
        return self._rates

    def fetch_rates_for_timeframe(self, timeframe, bars):
        _ = bars
        if self._fetch_error is not None:
            raise self._fetch_error
        tf_key = str(timeframe).upper()
        if tf_key in self._rates_by_tf:
            return self._rates_by_tf[tf_key]
        return self._rates

    def get_tick_context(self):
        return (2000.2, 2000.0, 0.1)

    def open_positions(self):
        return self._open_positions

    def has_opposite_position(self, signal):
        return self._has_opposite

    def close_opposite_positions(self, signal):
        self.close_calls += 1
        self._open_positions = self._open_positions_after_close
        return list(self._close_results)

    def send_market_order(self, side, sl, tp):
        self.orders.append({"side": side, "sl": sl, "tp": tp})
        return {"retcode": 0, "request": {"side": side, "sl": sl, "tp": tp}}


class TestXauAutoBotLiveBehavior(unittest.TestCase):
    def _base_config(self) -> BotConfig:
        return BotConfig.from_dict(
            {
                "symbol": "XAUUSD",
                "fast_ema": 1,
                "slow_ema": 3,
                "atr_period": 2,
                "pullback_atr": 0.0,
                "session_start_hour_utc": 0,
                "session_end_hour_utc": 23,
                "min_atr_ratio_to_median": 0.0,
                "max_atr_ratio_to_median": 999.0,
                "min_ema_gap_over_atr": 0.0,
                "max_ema_gap_over_atr": 999.0,
                "max_spread_points": 80.0,
                "max_positions": 1,
            }
        )

    def _rates(self):
        return {
            "time": [1700000000, 1700000300, 1700000600, 1700000900, 1700001200],
            "high": [10.2, 9.2, 8.2, 7.2, 6.2],
            "low": [9.8, 8.8, 7.8, 6.8, 5.8],
            "close": [10.0, 9.0, 8.0, 7.0, 6.0],
        }

    def _m15_commander_rates_uptrend(self):
        times = [1700000000 + i * 900 for i in range(18)]
        close = [100.0 + i * 0.4 for i in range(18)]
        high = [c + 0.2 for c in close]
        low = [c - 0.2 for c in close]
        return {"time": times, "high": high, "low": low, "close": close}

    def test_evaluate_once_closes_opposite_position_before_new_entry(self):
        config = self._base_config()
        gateway = _FakeGateway(
            rates=self._rates(),
            open_positions=1,
            has_opposite=True,
            close_results=[{"retcode": 10009, "deal": 1}],
            open_positions_after_close=0,
        )

        payload, _ = evaluate_once(config, gateway, last_bar_time=None)

        self.assertEqual(payload["action"], "CLOSE")
        self.assertEqual(payload["signal"], "SELL")
        self.assertEqual(gateway.close_calls, 1)
        self.assertEqual(len(gateway.orders), 0)

    def test_evaluate_once_blocks_when_opposite_position_remains_open(self):
        config = self._base_config()
        gateway = _FakeGateway(
            rates=self._rates(),
            open_positions=1,
            has_opposite=True,
            close_results=[{"retcode": -1, "error": "close failed"}],
            open_positions_after_close=1,
        )

        payload, _ = evaluate_once(config, gateway, last_bar_time=None)

        self.assertEqual(payload["action"], "BLOCKED")
        self.assertEqual(payload["reason"], "opposite_close_failed")
        self.assertEqual(gateway.close_calls, 1)
        self.assertEqual(len(gateway.orders), 0)

    def test_evaluate_once_skips_when_mt5_has_not_enough_bars(self):
        config = self._base_config()
        gateway = _FakeGateway(
            rates=self._rates(),
            open_positions=0,
            has_opposite=False,
            close_results=[],
            open_positions_after_close=0,
            fetch_error=RuntimeError("not enough bars from MT5"),
        )

        payload, bar_time = evaluate_once(config, gateway, last_bar_time=None)

        self.assertEqual(payload["action"], "SKIP")
        self.assertEqual(payload["reason"], "not_enough_bars")
        self.assertEqual(bar_time, 0)

    def test_evaluate_once_holds_when_ema_gap_is_outside_range(self):
        config = BotConfig.from_dict(
            {
                "symbol": "XAUUSD",
                "fast_ema": 1,
                "slow_ema": 3,
                "atr_period": 2,
                "pullback_atr": 0.0,
                "session_start_hour_utc": 0,
                "session_end_hour_utc": 23,
                "min_atr_ratio_to_median": 0.0,
                "max_atr_ratio_to_median": 999.0,
                "min_ema_gap_over_atr": 0.9,
                "max_ema_gap_over_atr": 2.5,
                "max_spread_points": 80.0,
                "max_positions": 1,
            }
        )
        gateway = _FakeGateway(
            rates=self._rates(),
            open_positions=0,
            has_opposite=False,
            close_results=[],
            open_positions_after_close=0,
        )

        payload, _ = evaluate_once(config, gateway, last_bar_time=None)

        self.assertEqual(payload["action"], "HOLD")
        self.assertEqual(payload["reason"], "ema_gap_out_of_range")
        self.assertTrue(payload["gap_gate_checked"])
        self.assertAlmostEqual(payload["ema_gap_over_atr"], 0.78125, places=5)
        self.assertEqual(len(gateway.orders), 0)

    def test_evaluate_once_blocks_signal_when_m45_gate_rejects_opposite_direction(self):
        config = BotConfig.from_dict(
            {
                "symbol": "XAUUSD",
                "timeframe": "M20",
                "fast_ema": 1,
                "slow_ema": 3,
                "atr_period": 2,
                "pullback_atr": 0.0,
                "session_start_hour_utc": 0,
                "session_end_hour_utc": 23,
                "min_atr_ratio_to_median": 0.0,
                "max_atr_ratio_to_median": 999.0,
                "min_ema_gap_over_atr": 0.0,
                "max_ema_gap_over_atr": 999.0,
                "m45_bias_gate_policy": "block_opposite",
                "m45_neutral_policy": "allow_all",
                "m45_commander_source_timeframe": "M15",
                "m45_commander_resample_factor": 3,
                "m45_commander_fast_ema": 1,
                "m45_commander_slow_ema": 3,
                "m45_commander_atr_period": 2,
                "m45_commander_regime_trend_threshold": 0.5,
            }
        )
        gateway = _FakeGateway(
            rates=self._rates(),
            open_positions=0,
            has_opposite=False,
            close_results=[],
            open_positions_after_close=0,
            rates_by_tf={"M15": self._m15_commander_rates_uptrend()},
        )

        payload, _ = evaluate_once(config, gateway, last_bar_time=None)

        self.assertEqual(payload["action"], "HOLD")
        self.assertEqual(payload["reason"], "m45_bias_gate_blocked")
        self.assertEqual(payload.get("m45_bias"), "LONG")
        self.assertEqual(payload.get("m45_gate_policy"), "block_opposite")
        self.assertEqual(len(gateway.orders), 0)

    def test_evaluate_once_applies_trend_override_in_trend_state(self):
        rates = {
            "time": [1700000000, 1700001200, 1700002400, 1700003600, 1700004800],
            "high": [10.15, 10.15, 10.15, 10.15, 10.2],
            "low": [9.85, 9.85, 9.85, 9.85, 9.9],
            "close": [10.0, 10.0, 10.0, 10.0, 10.1],
        }
        config = BotConfig.from_dict(
            {
                "symbol": "XAUUSD",
                "timeframe": "M20",
                "fast_ema": 1,
                "slow_ema": 3,
                "atr_period": 2,
                "pullback_atr": 0.25,
                "session_start_hour_utc": 0,
                "session_end_hour_utc": 23,
                "min_atr_ratio_to_median": 0.0,
                "max_atr_ratio_to_median": 999.0,
                "min_ema_gap_over_atr": 0.12,
                "max_ema_gap_over_atr": 1.8,
                "m45_bias_gate_policy": "none",
                "trend_override_enabled": True,
                "trend_override_on": 0.5,
                "trend_override_off": 0.4,
                "trend_override_initial_state": "non_trend",
                "trend_override_min_ema_gap_over_atr": 0.18,
                "trend_override_pullback_atr": 0.23,
                "m45_commander_source_timeframe": "M15",
                "m45_commander_resample_factor": 3,
                "m45_commander_fast_ema": 1,
                "m45_commander_slow_ema": 3,
                "m45_commander_atr_period": 2,
                "m45_commander_regime_trend_threshold": 0.5,
            }
        )
        gateway = _FakeGateway(
            rates=rates,
            open_positions=0,
            has_opposite=False,
            close_results=[],
            open_positions_after_close=0,
            rates_by_tf={"M15": self._m15_commander_rates_uptrend()},
        )

        payload, _ = evaluate_once(config, gateway, last_bar_time=None)

        self.assertEqual(payload["action"], "HOLD")
        self.assertEqual(payload["reason"], "ema_gap_out_of_range")
        self.assertAlmostEqual(payload.get("min_ema_gap_over_atr", 0.0), 0.18, places=6)
        self.assertEqual(payload.get("trend_override_state"), "trend")
        self.assertEqual(len(gateway.orders), 0)


class _FakeOrderResult:
    def __init__(self, **kwargs):
        self._payload = dict(kwargs)

    def _asdict(self):
        return dict(self._payload)


class _FakeMt5OrderApi:
    TRADE_ACTION_DEAL = 1
    ORDER_TYPE_BUY = 0
    ORDER_TYPE_SELL = 1
    ORDER_TIME_GTC = 0
    ORDER_FILLING_FOK = 0
    ORDER_FILLING_IOC = 1
    ORDER_FILLING_RETURN = 2
    SYMBOL_FILLING_FOK = 1
    SYMBOL_FILLING_IOC = 2
    POSITION_TYPE_BUY = 0
    POSITION_TYPE_SELL = 1
    TRADE_RETCODE_DONE = 10009
    TRADE_RETCODE_INVALID_FILL = 10030

    def __init__(self):
        self._positions = []
        self._order_send_results = []
        self.order_requests = []
        self.unsupported_filling_modes = set()
        self.symbol_filling_mode = self.SYMBOL_FILLING_FOK | self.SYMBOL_FILLING_IOC
        self.initialize_calls = 0
        self.shutdown_calls = 0
        self.symbol_select_calls = []
        self.initialize_success = True
        self.symbol_select_success = True
        self._last_error = (0, "ok")

    def symbol_info_tick(self, _symbol):
        return mock.Mock(ask=100.2, bid=100.0)

    def symbol_info(self, _symbol):
        return mock.Mock(point=0.1, filling_mode=self.symbol_filling_mode)

    def positions_get(self, symbol=None):
        _ = symbol
        return list(self._positions)

    def order_send(self, request):
        self.order_requests.append(dict(request))
        fill_mode = int(request.get("type_filling", -1))
        if fill_mode in self.unsupported_filling_modes:
            self._last_error = (0, "ok")
            return _FakeOrderResult(retcode=10030, comment="Unsupported filling mode")
        if self._order_send_results:
            out = self._order_send_results.pop(0)
            payload = out._asdict()
            if int(payload.get("retcode", 0)) == 5:
                self._last_error = (1, "Success")
            else:
                self._last_error = (0, "ok")
            return out
        self._last_error = (0, "ok")
        return _FakeOrderResult(retcode=self.TRADE_RETCODE_DONE, comment="Request executed")

    def last_error(self):
        return self._last_error

    def initialize(self):
        self.initialize_calls += 1
        if self.initialize_success:
            self._last_error = (0, "ok")
            return True
        self._last_error = (1000, "initialize failed")
        return False

    def shutdown(self):
        self.shutdown_calls += 1

    def symbol_select(self, symbol, enable):
        self.symbol_select_calls.append((symbol, bool(enable)))
        if self.symbol_select_success:
            self._last_error = (0, "ok")
            return True
        self._last_error = (1001, "symbol_select failed")
        return False


class TestMt5GatewayFillingFallback(unittest.TestCase):
    def _base_config(self):
        return BotConfig.from_dict(
            {
                "symbol": "USDJPY",
                "comment": "uncorr_fx_usdjpy_h1",
                "dry_run": False,
                "once": True,
            }
        )

    def test_send_market_order_retries_when_ioc_is_rejected(self):
        fake_mt5 = _FakeMt5OrderApi()
        fake_mt5.symbol_filling_mode = fake_mt5.SYMBOL_FILLING_IOC
        fake_mt5.unsupported_filling_modes = {fake_mt5.ORDER_FILLING_IOC}
        cfg = self._base_config()
        gateway = Mt5Gateway(cfg)

        with mock.patch("tools.xau_autobot.mt5", fake_mt5):
            result = gateway.send_market_order("BUY", sl=99.0, tp=101.0)

        self.assertEqual(result["retcode"], 10009)
        self.assertEqual(result["request"]["type_filling"], fake_mt5.ORDER_FILLING_FOK)
        self.assertEqual([r["type_filling"] for r in fake_mt5.order_requests], [1, 0])

    def test_send_market_order_uses_symbol_supported_mode_first(self):
        fake_mt5 = _FakeMt5OrderApi()
        fake_mt5.symbol_filling_mode = fake_mt5.SYMBOL_FILLING_FOK
        fake_mt5.unsupported_filling_modes = {fake_mt5.ORDER_FILLING_IOC}
        cfg = self._base_config()
        gateway = Mt5Gateway(cfg)

        with mock.patch("tools.xau_autobot.mt5", fake_mt5):
            result = gateway.send_market_order("BUY", sl=99.0, tp=101.0)

        self.assertEqual(result["retcode"], 10009)
        self.assertEqual([r["type_filling"] for r in fake_mt5.order_requests], [fake_mt5.ORDER_FILLING_FOK])

    def test_send_market_order_caches_successful_filling_mode(self):
        fake_mt5 = _FakeMt5OrderApi()
        fake_mt5.symbol_filling_mode = 0
        fake_mt5.unsupported_filling_modes = {fake_mt5.ORDER_FILLING_FOK}
        cfg = self._base_config()
        gateway = Mt5Gateway(cfg)

        with mock.patch("tools.xau_autobot.mt5", fake_mt5):
            first = gateway.send_market_order("BUY", sl=99.0, tp=101.0)
            second = gateway.send_market_order("BUY", sl=99.0, tp=101.0)

        self.assertEqual(first["retcode"], 10009)
        self.assertEqual(second["retcode"], 10009)
        self.assertEqual(
            [r["type_filling"] for r in fake_mt5.order_requests],
            [fake_mt5.ORDER_FILLING_FOK, fake_mt5.ORDER_FILLING_IOC, fake_mt5.ORDER_FILLING_IOC],
        )

    def test_close_opposite_positions_retries_when_ioc_is_rejected(self):
        fake_mt5 = _FakeMt5OrderApi()
        fake_mt5._positions = [mock.Mock(type=0, magic=560061, volume=0.01, ticket=1234)]
        fake_mt5.symbol_filling_mode = fake_mt5.SYMBOL_FILLING_IOC
        fake_mt5.unsupported_filling_modes = {fake_mt5.ORDER_FILLING_IOC}
        cfg = BotConfig.from_dict(
            {
                "symbol": "USDJPY",
                "magic": 560061,
                "comment": "uncorr_fx_usdjpy_h1",
                "dry_run": False,
                "once": True,
            }
        )
        gateway = Mt5Gateway(cfg)

        with mock.patch("tools.xau_autobot.mt5", fake_mt5):
            results = gateway.close_opposite_positions("SELL")

        self.assertEqual(len(results), 1)
        self.assertEqual(results[0]["retcode"], 10009)
        self.assertEqual(results[0]["request"]["type_filling"], fake_mt5.ORDER_FILLING_FOK)
        self.assertEqual([r["type_filling"] for r in fake_mt5.order_requests], [1, 0])

    def test_send_market_order_retries_once_after_disk_error(self):
        fake_mt5 = _FakeMt5OrderApi()
        fake_mt5._order_send_results = [
            _FakeOrderResult(retcode=5, comment="Disk error"),
            _FakeOrderResult(retcode=10009, comment="Request executed"),
        ]
        cfg = self._base_config()
        gateway = Mt5Gateway(cfg)

        with mock.patch("tools.xau_autobot.mt5", fake_mt5):
            result = gateway.send_market_order("BUY", sl=99.0, tp=101.0)

        self.assertEqual(result["retcode"], 10009)
        self.assertTrue(result.get("reconnect_retried"))
        self.assertTrue(result.get("reconnect_success"))
        self.assertEqual(fake_mt5.shutdown_calls, 1)
        self.assertEqual(fake_mt5.initialize_calls, 1)
        self.assertEqual(fake_mt5.symbol_select_calls, [("USDJPY", True)])
        self.assertEqual(len(fake_mt5.order_requests), 2)

    def test_send_market_order_keeps_failure_when_reconnect_fails(self):
        fake_mt5 = _FakeMt5OrderApi()
        fake_mt5.initialize_success = False
        fake_mt5._order_send_results = [
            _FakeOrderResult(retcode=5, comment="Disk error"),
        ]
        cfg = self._base_config()
        gateway = Mt5Gateway(cfg)

        with mock.patch("tools.xau_autobot.mt5", fake_mt5):
            result = gateway.send_market_order("BUY", sl=99.0, tp=101.0)

        self.assertEqual(result["retcode"], 5)
        self.assertTrue(result.get("reconnect_retried"))
        self.assertFalse(result.get("reconnect_success"))
        self.assertEqual(fake_mt5.shutdown_calls, 1)
        self.assertEqual(fake_mt5.initialize_calls, 1)
        self.assertEqual(fake_mt5.symbol_select_calls, [])
        self.assertEqual(len(fake_mt5.order_requests), 1)


if __name__ == "__main__":
    unittest.main()
