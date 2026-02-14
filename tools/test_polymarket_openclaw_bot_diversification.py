import importlib.util
import sys
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("polymarket_openclaw_bot.py")


def load_module():
    spec = importlib.util.spec_from_file_location("polymarket_openclaw_bot", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TestPolymarketOpenClawBotDiversification(unittest.TestCase):
    def test_build_trade_plan_groups_weather_ranges(self) -> None:
        mod = load_module()

        # Two mutually-exclusive ranges for the same city+date should not be
        # treated as "diversified" entries.
        q1 = "Will the highest temperature in New York City be between 34-35°F on February 13?"
        q2 = "Will the highest temperature in New York City be between 36-37°F on February 13?"

        markets = [
            mod.MarketSnapshot(
                market_id="m1",
                question=q1,
                yes_token_id="m1:YES",
                no_token_id="m1:NO",
                yes_price=0.10,
                no_price=0.90,
                liquidity_usd=1000.0,
                volume_usd=1000.0,
            ),
            mod.MarketSnapshot(
                market_id="m2",
                question=q2,
                yes_token_id="m2:YES",
                no_token_id="m2:NO",
                yes_price=0.10,
                no_price=0.90,
                liquidity_usd=1000.0,
                volume_usd=1000.0,
            ),
        ]
        signals = {
            "m1": mod.OpenClawSignal(market_id="m1", p_yes=0.30, confidence=0.9),
            "m2": mod.OpenClawSignal(market_id="m2", p_yes=0.30, confidence=0.9),
        }
        config = mod.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=10.0,  # budget $100
            max_trade_risk_pct=1.0,   # cap $10 per trade
            max_positions=4,
            min_edge=0.01,
            min_confidence=0.0,
            min_stake_usd=1.0,
            kelly_scale=1.0,
            fee_bps_per_side=0.0,
            slippage_bps_per_side=0.0,
        )

        plan = mod.build_trade_plan(signals=signals, markets=markets, config=config)

        # Desired safety: pick at most one range for the same underlying question.
        self.assertEqual(len(plan.entries), 1)

    def test_build_trade_plan_groups_weather_exact_and_threshold_buckets(self) -> None:
        mod = load_module()

        q1 = "Will the highest temperature in London be 5°C on February 14?"
        q2 = "Will the highest temperature in London be 6°C on February 14?"
        q3 = "Will the highest temperature in London be 4°C or below on February 14?"

        markets = [
            mod.MarketSnapshot(
                market_id="m1",
                question=q1,
                yes_token_id="m1:YES",
                no_token_id="m1:NO",
                yes_price=0.10,
                no_price=0.90,
                liquidity_usd=1000.0,
                volume_usd=1000.0,
            ),
            mod.MarketSnapshot(
                market_id="m2",
                question=q2,
                yes_token_id="m2:YES",
                no_token_id="m2:NO",
                yes_price=0.10,
                no_price=0.90,
                liquidity_usd=1000.0,
                volume_usd=1000.0,
            ),
            mod.MarketSnapshot(
                market_id="m3",
                question=q3,
                yes_token_id="m3:YES",
                no_token_id="m3:NO",
                yes_price=0.10,
                no_price=0.90,
                liquidity_usd=1000.0,
                volume_usd=1000.0,
            ),
        ]
        signals = {
            "m1": mod.OpenClawSignal(market_id="m1", p_yes=0.30, confidence=0.9),
            "m2": mod.OpenClawSignal(market_id="m2", p_yes=0.30, confidence=0.9),
            "m3": mod.OpenClawSignal(market_id="m3", p_yes=0.30, confidence=0.9),
        }
        config = mod.BotConfig(
            bankroll_usd=1000.0,
            max_daily_loss_pct=10.0,  # budget $100
            max_trade_risk_pct=1.0,   # cap $10 per trade
            max_positions=4,
            min_edge=0.01,
            min_confidence=0.0,
            min_stake_usd=1.0,
            kelly_scale=1.0,
            fee_bps_per_side=0.0,
            slippage_bps_per_side=0.0,
        )

        plan = mod.build_trade_plan(signals=signals, markets=markets, config=config)

        # Desired safety: don't enter multiple mutually-exclusive buckets for the same event.
        self.assertEqual(len(plan.entries), 1)


if __name__ == "__main__":
    unittest.main()
