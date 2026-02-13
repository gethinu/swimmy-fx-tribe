import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from tools import polymarket_openclaw_bot as bot


class TestOpenClawSignals(unittest.TestCase):
    def test_load_openclaw_signals_jsonl(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / "signals.jsonl"
            lines = [
                {"market_id": "m1", "p_yes": 0.63, "confidence": 0.9},
                {"market_id": "m2", "prob_yes": 0.41, "confidence": 0.7},
            ]
            path.write_text("\n".join(json.dumps(item) for item in lines), encoding="utf-8")

            signals = bot.load_openclaw_signals(path)

        self.assertEqual(2, len(signals))
        self.assertAlmostEqual(0.63, signals["m1"].p_yes)
        self.assertAlmostEqual(0.41, signals["m2"].p_yes)

    def test_load_openclaw_signals_skips_non_json_lines(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / "signals.jsonl"
            path.write_text(
                "\n".join(
                    [
                        "OpenClaw 2026.2.9",
                        json.dumps({"market_id": "m1", "p_yes": 0.63, "confidence": 0.9}),
                        "warning: config invalid",
                        json.dumps({"market_id": "m2", "prob_yes": 0.41, "confidence": 0.7}),
                    ]
                )
                + "\n",
                encoding="utf-8",
            )

            signals = bot.load_openclaw_signals(path)

        self.assertEqual(2, len(signals))
        self.assertAlmostEqual(0.63, signals["m1"].p_yes)
        self.assertAlmostEqual(0.41, signals["m2"].p_yes)

    def test_load_openclaw_signals_from_command_parses_array_with_noise(self) -> None:
        cp = mock.Mock()
        cp.stdout = "\n".join(
            [
                "OpenClaw startup",
                json.dumps(
                    [
                        {"market_id": "m1", "p_yes": 0.63, "confidence": 0.9},
                        {"market_id": "m2", "prob_yes": 0.41, "confidence": 0.7},
                    ]
                ),
                "done",
            ]
        )
        cp.returncode = 0

        with mock.patch.object(bot.subprocess, "run", return_value=cp):
            signals = bot.load_openclaw_signals_from_command("openclaw signals --format jsonl")

        self.assertEqual(2, len(signals))
        self.assertAlmostEqual(0.63, signals["m1"].p_yes)
        self.assertAlmostEqual(0.41, signals["m2"].p_yes)


class TestPolymarketParsing(unittest.TestCase):
    def test_parse_market_with_tokens(self) -> None:
        raw_market = {
            "id": "2001",
            "question": "Will example happen?",
            "tokens": [
                {"token_id": "yes-1", "outcome": "Yes", "price": "0.55"},
                {"token_id": "no-1", "outcome": "No", "price": "0.47"},
            ],
        }

        snap = bot.parse_gamma_market(raw_market)

        self.assertIsNotNone(snap)
        assert snap is not None
        self.assertEqual("2001", snap.market_id)
        self.assertAlmostEqual(0.55, snap.yes_price)
        self.assertAlmostEqual(0.47, snap.no_price)
        self.assertAlmostEqual(0.0, snap.liquidity_usd)
        self.assertAlmostEqual(0.0, snap.volume_usd)

    def test_parse_market_extracts_liquidity_and_volume(self) -> None:
        raw_market = {
            "id": "2003",
            "question": "Will sample happen?",
            "liquidity": "12345.6",
            "volume": "9876.5",
            "tokens": [
                {"token_id": "yes-3", "outcome": "Yes", "price": "0.51"},
                {"token_id": "no-3", "outcome": "No", "price": "0.50"},
            ],
        }

        snap = bot.parse_gamma_market(raw_market)

        self.assertIsNotNone(snap)
        assert snap is not None
        self.assertAlmostEqual(12345.6, snap.liquidity_usd)
        self.assertAlmostEqual(9876.5, snap.volume_usd)

    def test_parse_market_with_outcomes_array_strings(self) -> None:
        raw_market = {
            "id": "2002",
            "question": "Will sample happen?",
            "outcomes": "[\"Yes\", \"No\"]",
            "outcomePrices": "[\"0.44\", \"0.58\"]",
            "clobTokenIds": "[\"yes-2\", \"no-2\"]",
        }

        snap = bot.parse_gamma_market(raw_market)

        self.assertIsNotNone(snap)
        assert snap is not None
        self.assertEqual("2002", snap.market_id)
        self.assertAlmostEqual(0.44, snap.yes_price)
        self.assertAlmostEqual(0.58, snap.no_price)
        self.assertEqual("yes-2", snap.yes_token_id)
        self.assertEqual("no-2", snap.no_token_id)

    def test_filter_markets_by_keywords_case_insensitive(self) -> None:
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Will Lakers win the game?",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.52,
                no_price=0.49,
            ),
            bot.MarketSnapshot(
                market_id="m2",
                question="Will BTC close above 100k?",
                yes_token_id="y2",
                no_token_id="n2",
                yes_price=0.50,
                no_price=0.51,
            ),
        ]

        filtered = bot.filter_markets_by_keywords(markets, ["lakers", "nba"])

        self.assertEqual(1, len(filtered))
        self.assertEqual("m1", filtered[0].market_id)

    def test_hydrate_markets_for_signals_uses_existing_overlap(self) -> None:
        signals = {"m1": bot.OpenClawSignal(market_id="m1", p_yes=0.6, confidence=0.8)}
        raw_markets = [
            {
                "id": "m1",
                "question": "Will Lakers win?",
                "tokens": [
                    {"token_id": "y1", "outcome": "Yes", "price": "0.55"},
                    {"token_id": "n1", "outcome": "No", "price": "0.45"},
                ],
            }
        ]
        with mock.patch.object(bot, "fetch_gamma_markets_for_signal_ids") as mocked:
            hydrated = bot.hydrate_markets_for_signals(
                raw_markets=raw_markets,
                signals=signals,
                gamma_url="https://gamma-api.polymarket.com/markets",
                limit=100,
            )
        self.assertEqual(1, len(hydrated))
        self.assertEqual("m1", hydrated[0].market_id)
        mocked.assert_not_called()

    def test_hydrate_markets_for_signals_fallback_by_market_id(self) -> None:
        signals = {"m2": bot.OpenClawSignal(market_id="m2", p_yes=0.6, confidence=0.8)}
        raw_markets = [
            {
                "id": "m1",
                "question": "Will Lakers win?",
                "tokens": [
                    {"token_id": "y1", "outcome": "Yes", "price": "0.55"},
                    {"token_id": "n1", "outcome": "No", "price": "0.45"},
                ],
            }
        ]
        fallback_raw = [
            {
                "id": "m2",
                "question": "Will Yankees win?",
                "tokens": [
                    {"token_id": "y2", "outcome": "Yes", "price": "0.52"},
                    {"token_id": "n2", "outcome": "No", "price": "0.48"},
                ],
            }
        ]
        with mock.patch.object(bot, "fetch_gamma_markets_for_signal_ids", return_value=fallback_raw) as mocked:
            hydrated = bot.hydrate_markets_for_signals(
                raw_markets=raw_markets,
                signals=signals,
                gamma_url="https://gamma-api.polymarket.com/markets",
                limit=100,
            )
        self.assertEqual(1, len(hydrated))
        self.assertEqual("m2", hydrated[0].market_id)
        mocked.assert_called_once()


class TestTradePlanning(unittest.TestCase):
    def test_build_trade_plan_enforces_risk_budget(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=0.5,
            max_trade_risk_pct=0.25,
            max_positions=3,
            min_edge=0.02,
            kelly_scale=0.5,
        )
        signals = {
            "m1": bot.OpenClawSignal(market_id="m1", p_yes=0.65, confidence=0.9),
            "m2": bot.OpenClawSignal(market_id="m2", p_yes=0.61, confidence=0.8),
            "m3": bot.OpenClawSignal(market_id="m3", p_yes=0.56, confidence=0.8),
        }
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q1",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.55,
                no_price=0.46,
            ),
            bot.MarketSnapshot(
                market_id="m2",
                question="Q2",
                yes_token_id="y2",
                no_token_id="n2",
                yes_price=0.52,
                no_price=0.49,
            ),
            bot.MarketSnapshot(
                market_id="m3",
                question="Q3",
                yes_token_id="y3",
                no_token_id="n3",
                yes_price=0.51,
                no_price=0.50,
            ),
        ]

        plan = bot.build_trade_plan(signals=signals, markets=markets, config=config)

        self.assertGreaterEqual(len(plan.entries), 1)
        self.assertLessEqual(plan.total_stake_usd, 5.0)
        for entry in plan.entries:
            self.assertLessEqual(entry.stake_usd, 2.5)
            self.assertGreater(entry.edge, 0.0)
            self.assertGreater(entry.expected_value_usd, 0.0)

    def test_negative_edge_is_dropped(self) -> None:
        config = bot.BotConfig(bankroll_usd=1000.0, min_edge=0.01)
        signals = {"m1": bot.OpenClawSignal(market_id="m1", p_yes=0.42, confidence=0.8)}
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q",
                yes_token_id="y",
                no_token_id="n",
                yes_price=0.58,
                no_price=0.62,
            )
        ]

        plan = bot.build_trade_plan(signals=signals, markets=markets, config=config)

        self.assertEqual([], plan.entries)
        self.assertAlmostEqual(0.0, plan.total_stake_usd)

    def test_costs_can_reject_positive_gross_edge(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            min_edge=0.01,
            fee_bps_per_side=150.0,
            slippage_bps_per_side=150.0,
        )
        signals = {"m1": bot.OpenClawSignal(market_id="m1", p_yes=0.52, confidence=0.9)}
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q",
                yes_token_id="y",
                no_token_id="n",
                yes_price=0.50,
                no_price=0.49,
            )
        ]

        plan = bot.build_trade_plan(signals=signals, markets=markets, config=config)

        self.assertEqual([], plan.entries)

    def test_contrarian_fade_prefers_underdog_when_edge_positive(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            min_edge=0.01,
            enable_contrarian_fade=True,
            contrarian_favorite_threshold=0.60,
        )
        signals = {"m1": bot.OpenClawSignal(market_id="m1", p_yes=0.66, confidence=0.9)}
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Will favorite team win?",
                yes_token_id="y",
                no_token_id="n",
                yes_price=0.62,
                no_price=0.32,
            )
        ]

        plan = bot.build_trade_plan(signals=signals, markets=markets, config=config)

        self.assertEqual(1, len(plan.entries))
        self.assertEqual("NO", plan.entries[0].side)

    def test_contrarian_fade_handles_float_edge_boundary(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            min_edge=0.02,
            enable_contrarian_fade=True,
            contrarian_favorite_threshold=0.62,
        )
        signals = {"m1": bot.OpenClawSignal(market_id="m1", p_yes=0.66, confidence=0.9)}
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Will favorite team win?",
                yes_token_id="y",
                no_token_id="n",
                yes_price=0.62,
                no_price=0.32,
            )
        ]

        plan = bot.build_trade_plan(signals=signals, markets=markets, config=config)

        self.assertEqual(1, len(plan.entries))
        self.assertEqual("NO", plan.entries[0].side)

    def test_build_trade_plan_skips_blocked_open_markets(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=1.0,
            max_trade_risk_pct=1.0,
            max_positions=5,
            min_edge=0.01,
            kelly_scale=0.5,
        )
        signals = {
            "m1": bot.OpenClawSignal(market_id="m1", p_yes=0.65, confidence=0.9),
            "m2": bot.OpenClawSignal(market_id="m2", p_yes=0.66, confidence=0.9),
        }
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q1",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.55,
                no_price=0.46,
            ),
            bot.MarketSnapshot(
                market_id="m2",
                question="Q2",
                yes_token_id="y2",
                no_token_id="n2",
                yes_price=0.56,
                no_price=0.45,
            ),
        ]

        plan = bot.build_trade_plan(
            signals=signals,
            markets=markets,
            config=config,
            blocked_market_ids={"m1"},
        )

        self.assertEqual(1, len(plan.entries))
        self.assertEqual("m2", plan.entries[0].market_id)

    def test_build_trade_plan_respects_max_open_positions_cap(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=1.0,
            max_trade_risk_pct=1.0,
            max_positions=5,
            max_open_positions=2,
            min_edge=0.01,
            kelly_scale=0.5,
        )
        signals = {
            "m1": bot.OpenClawSignal(market_id="m1", p_yes=0.65, confidence=0.9),
        }
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q1",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.55,
                no_price=0.46,
            ),
        ]

        plan = bot.build_trade_plan(
            signals=signals,
            markets=markets,
            config=config,
            open_position_count=2,
        )

        self.assertEqual(0, len(plan.entries))

    def test_build_trade_plan_limits_new_entries_by_open_slots(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=1.0,
            max_trade_risk_pct=1.0,
            max_positions=5,
            max_open_positions=2,
            min_edge=0.01,
            kelly_scale=0.5,
        )
        signals = {
            "m1": bot.OpenClawSignal(market_id="m1", p_yes=0.65, confidence=0.9),
            "m2": bot.OpenClawSignal(market_id="m2", p_yes=0.64, confidence=0.9),
        }
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q1",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.55,
                no_price=0.46,
            ),
            bot.MarketSnapshot(
                market_id="m2",
                question="Q2",
                yes_token_id="y2",
                no_token_id="n2",
                yes_price=0.55,
                no_price=0.46,
            ),
        ]

        plan = bot.build_trade_plan(
            signals=signals,
            markets=markets,
            config=config,
            open_position_count=1,
        )

        self.assertEqual(1, len(plan.entries))

    def test_build_trade_plan_respects_max_daily_entries_cap(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=1.0,
            max_trade_risk_pct=1.0,
            max_positions=5,
            max_daily_entries=2,
            min_edge=0.01,
            kelly_scale=0.5,
        )
        signals = {
            "m1": bot.OpenClawSignal(market_id="m1", p_yes=0.65, confidence=0.9),
        }
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q1",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.55,
                no_price=0.46,
            ),
        ]

        plan = bot.build_trade_plan(
            signals=signals,
            markets=markets,
            config=config,
            daily_entry_count=2,
        )

        self.assertEqual(0, len(plan.entries))

    def test_build_trade_plan_respects_max_daily_loss_streak_cap(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=1.0,
            max_trade_risk_pct=1.0,
            max_positions=5,
            max_daily_loss_streak=2,
            min_edge=0.01,
            kelly_scale=0.5,
        )
        signals = {
            "m1": bot.OpenClawSignal(market_id="m1", p_yes=0.65, confidence=0.9),
        }
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q1",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.55,
                no_price=0.46,
            ),
        ]

        plan = bot.build_trade_plan(
            signals=signals,
            markets=markets,
            config=config,
            daily_loss_streak=2,
        )

        self.assertEqual(0, len(plan.entries))

    def test_build_trade_plan_respects_max_daily_realized_loss_cap(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=1.0,
            max_trade_risk_pct=1.0,
            max_positions=5,
            max_daily_realized_loss_usd=3.0,
            min_edge=0.01,
            kelly_scale=0.5,
        )
        signals = {
            "m1": bot.OpenClawSignal(market_id="m1", p_yes=0.65, confidence=0.9),
        }
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q1",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.55,
                no_price=0.46,
            ),
        ]

        plan = bot.build_trade_plan(
            signals=signals,
            markets=markets,
            config=config,
            daily_realized_pnl_usd=-3.1,
        )

        self.assertEqual(0, len(plan.entries))

    def test_build_trade_plan_skips_market_outside_price_sum_range(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=1.0,
            max_trade_risk_pct=1.0,
            max_positions=5,
            min_price_sum=0.95,
            max_price_sum=1.05,
            min_edge=0.01,
            kelly_scale=0.5,
        )
        signals = {
            "m1": bot.OpenClawSignal(market_id="m1", p_yes=0.72, confidence=0.9),
        }
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q1",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.72,
                no_price=0.40,
            ),
        ]

        plan = bot.build_trade_plan(signals=signals, markets=markets, config=config)

        self.assertEqual(0, len(plan.entries))
        self.assertEqual(1, plan.quality_filtered_markets)

    def test_build_trade_plan_skips_market_outside_side_price_range(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=1.0,
            max_trade_risk_pct=1.0,
            max_positions=5,
            min_market_price=0.05,
            max_market_price=0.95,
            min_edge=0.01,
            kelly_scale=0.5,
        )
        signals = {
            "m1": bot.OpenClawSignal(market_id="m1", p_yes=0.98, confidence=0.9),
        }
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q1",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.98,
                no_price=0.02,
            ),
        ]

        plan = bot.build_trade_plan(signals=signals, markets=markets, config=config)

        self.assertEqual(0, len(plan.entries))
        self.assertEqual(1, plan.quality_filtered_markets)

    def test_build_trade_plan_skips_market_below_min_liquidity(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=1.0,
            max_trade_risk_pct=1.0,
            max_positions=5,
            min_liquidity_usd=1000.0,
            min_edge=0.01,
            kelly_scale=0.5,
        )
        signals = {
            "m1": bot.OpenClawSignal(market_id="m1", p_yes=0.60, confidence=0.9),
        }
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q1",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.54,
                no_price=0.47,
                liquidity_usd=200.0,
            ),
        ]

        plan = bot.build_trade_plan(signals=signals, markets=markets, config=config)

        self.assertEqual(0, len(plan.entries))
        self.assertEqual(1, plan.quality_filtered_markets)

    def test_build_trade_plan_skips_market_below_min_volume(self) -> None:
        config = bot.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=1.0,
            max_trade_risk_pct=1.0,
            max_positions=5,
            min_volume_usd=1500.0,
            min_edge=0.01,
            kelly_scale=0.5,
        )
        signals = {
            "m1": bot.OpenClawSignal(market_id="m1", p_yes=0.60, confidence=0.9),
        }
        markets = [
            bot.MarketSnapshot(
                market_id="m1",
                question="Q1",
                yes_token_id="y1",
                no_token_id="n1",
                yes_price=0.54,
                no_price=0.47,
                liquidity_usd=5000.0,
                volume_usd=400.0,
            ),
        ]

        plan = bot.build_trade_plan(signals=signals, markets=markets, config=config)

        self.assertEqual(0, len(plan.entries))
        self.assertEqual(1, plan.quality_filtered_markets)


class TestRuntimeOptionMerge(unittest.TestCase):
    def test_merge_runtime_options_uses_config_when_cli_default(self) -> None:
        defaults = {"max_positions": 4, "enable_contrarian_fade": False, "question_keyword": []}
        cli = {"max_positions": 4, "enable_contrarian_fade": False, "question_keyword": []}
        config = {"max_positions": 2, "enable_contrarian_fade": True, "question_keywords": ["nba"]}

        merged = bot.merge_runtime_options(cli=cli, defaults=defaults, config=config)

        self.assertEqual(2, merged["max_positions"])
        self.assertTrue(merged["enable_contrarian_fade"])
        self.assertEqual(["nba"], merged["question_keyword"])

    def test_merge_runtime_options_prefers_cli_when_overridden(self) -> None:
        defaults = {"max_positions": 4, "question_keyword": []}
        cli = {"max_positions": 3, "question_keyword": ["nfl"]}
        config = {"max_positions": 2, "question_keywords": ["nba"]}

        merged = bot.merge_runtime_options(cli=cli, defaults=defaults, config=config)

        self.assertEqual(3, merged["max_positions"])
        self.assertEqual(["nfl"], merged["question_keyword"])


class TestJournalAppend(unittest.TestCase):
    def test_append_plan_to_journal_writes_summary_and_entries(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / "journal.jsonl"
            output = {
                "generated_at": "2026-02-12T08:00:00+00:00",
                "summary": {"entries": 2, "total_stake_usd": 3.0},
                "entries": [
                    {
                        "market_id": "m1",
                        "question": "Q1",
                        "side": "YES",
                        "token_id": "y1",
                        "entry_price": 0.55,
                        "stake_usd": 1.5,
                        "expected_value_usd": 0.12,
                        "edge": 0.03,
                        "confidence": 0.8,
                    },
                    {
                        "market_id": "m2",
                        "question": "Q2",
                        "side": "NO",
                        "token_id": "n2",
                        "entry_price": 0.42,
                        "stake_usd": 1.5,
                        "expected_value_usd": 0.08,
                        "edge": 0.02,
                        "confidence": 0.7,
                    },
                ],
            }

            bot.append_plan_to_journal(output=output, journal_path=path, run_id="r-1")

            lines = [json.loads(line) for line in path.read_text(encoding="utf-8").splitlines() if line.strip()]

        self.assertEqual(3, len(lines))
        self.assertEqual("run_summary", lines[0]["type"])
        self.assertEqual("entry", lines[1]["type"])
        self.assertEqual("entry", lines[2]["type"])
        self.assertEqual("r-1", lines[1]["run_id"])
        self.assertAlmostEqual(1.5, lines[1]["stake_usd"])
        self.assertAlmostEqual(0.55, lines[1]["entry_price"])

    def test_load_unresolved_market_ids_excludes_settled(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            journal_path = Path(td) / "journal.jsonl"
            journal_rows = [
                {"type": "run_summary", "run_id": "r1"},
                {"type": "entry", "market_id": "m1", "side": "YES"},
                {"type": "entry", "market_id": "m2", "side": "NO"},
            ]
            journal_path.write_text("\n".join(json.dumps(row) for row in journal_rows) + "\n", encoding="utf-8")
            settlements_path = Path(td) / "settlements.json"
            settlements_path.write_text(json.dumps({"m2": "YES"}), encoding="utf-8")

            settled_ids = bot.load_settled_market_ids(settlements_path)
            unresolved = bot.load_unresolved_market_ids(
                journal_path=journal_path,
                settled_market_ids=settled_ids,
            )

        self.assertEqual({"m1"}, unresolved)

    def test_load_daily_entry_count_filters_by_date(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            journal_path = Path(td) / "journal.jsonl"
            journal_rows = [
                {"type": "entry", "date": "2026-02-12", "market_id": "m1"},
                {"type": "entry", "date": "2026-02-12", "market_id": "m2"},
                {"type": "entry", "date": "2026-02-11", "market_id": "m3"},
                {"type": "run_summary", "date": "2026-02-12"},
            ]
            journal_path.write_text("\n".join(json.dumps(row) for row in journal_rows) + "\n", encoding="utf-8")

            count_today = bot.load_daily_entry_count(journal_path=journal_path, target_date="2026-02-12")
            count_other = bot.load_daily_entry_count(journal_path=journal_path, target_date="2026-02-11")

        self.assertEqual(2, count_today)
        self.assertEqual(1, count_other)

    def test_load_daily_loss_streak_uses_resolved_outcomes(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            journal_path = Path(td) / "journal.jsonl"
            rows = [
                {"type": "entry", "date": "2026-02-12", "market_id": "m1", "side": "YES"},
                {"type": "entry", "date": "2026-02-12", "market_id": "m2", "side": "NO"},
                {"type": "entry", "date": "2026-02-12", "market_id": "m3", "side": "YES"},
                {"type": "entry", "date": "2026-02-11", "market_id": "m4", "side": "YES"},
            ]
            journal_path.write_text("\n".join(json.dumps(row) for row in rows) + "\n", encoding="utf-8")
            settlements = {
                "m1": "YES",   # win
                "m2": "YES",   # loss (side NO)
                "m3": "NO",    # loss (side YES)
                "m4": "NO",
            }

            streak = bot.load_daily_loss_streak(
                journal_path=journal_path,
                target_date="2026-02-12",
                settlements=settlements,
            )

        self.assertEqual(2, streak)

    def test_load_daily_realized_pnl_uses_settled_entries_only(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            journal_path = Path(td) / "journal.jsonl"
            rows = [
                {
                    "type": "entry",
                    "date": "2026-02-12",
                    "market_id": "m1",
                    "side": "YES",
                    "stake_usd": 2.0,
                    "entry_price": 0.5,
                },
                {
                    "type": "entry",
                    "date": "2026-02-12",
                    "market_id": "m2",
                    "side": "NO",
                    "stake_usd": 3.0,
                    "entry_price": 0.6,
                },
                {
                    "type": "entry",
                    "date": "2026-02-12",
                    "market_id": "m3",
                    "side": "YES",
                    "stake_usd": 1.0,
                    "entry_price": 0.4,
                },
            ]
            journal_path.write_text("\n".join(json.dumps(row) for row in rows) + "\n", encoding="utf-8")
            settlements = {
                "m1": "YES",  # win => +2.0 gross
                "m2": "YES",  # loss => -3.0 gross
            }

            pnl = bot.load_daily_realized_pnl_usd(
                journal_path=journal_path,
                target_date="2026-02-12",
                settlements=settlements,
                fee_bps_per_side=20.0,
                slippage_bps_per_side=30.0,
            )

        self.assertAlmostEqual(-1.05, pnl, places=6)


if __name__ == "__main__":
    unittest.main()
