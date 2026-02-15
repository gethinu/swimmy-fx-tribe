import importlib.util
import json
import sys
import unittest
from datetime import date, datetime, timezone
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch


MODULE_PATH = Path(__file__).with_name("polymarket_weather_backtest.py")


def load_module():
    spec = importlib.util.spec_from_file_location("polymarket_weather_backtest", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TestPolymarketWeatherBacktest(unittest.TestCase):
    def test_parse_clob_token_ids_accepts_list_and_json_string(self) -> None:
        mod = load_module()
        self.assertEqual(["1", "2"], mod.parse_clob_token_ids({"clobTokenIds": ["1", "2"]}))
        self.assertEqual(["1", "2"], mod.parse_clob_token_ids({"clobTokenIds": "[\"1\",\"2\"]"}))
        self.assertEqual([], mod.parse_clob_token_ids({"clobTokenIds": ""}))

    def test_select_price_at_timestamp_prefers_last_before_then_first_after(self) -> None:
        mod = load_module()
        hist = [
            {"t": 10, "p": 0.10},
            {"t": 20, "p": 0.20},
            {"t": 30, "p": 0.30},
        ]
        self.assertEqual(0.20, mod.select_price_at_timestamp(history=hist, ts=25))
        self.assertEqual(0.10, mod.select_price_at_timestamp(history=hist, ts=10))
        self.assertEqual(0.10, mod.select_price_at_timestamp(history=hist, ts=1))
        self.assertEqual(0.30, mod.select_price_at_timestamp(history=hist, ts=999))
        self.assertIsNone(mod.select_price_at_timestamp(history=[], ts=10))

    def test_simulate_trading_backtest_uses_snapshot_prices_when_available(self) -> None:
        mod = load_module()

        now_dir = "2026-02-15"
        snapshot_dir = "20260215T000000Z"
        with TemporaryDirectory() as tmpdir:
            base = Path(tmpdir) / "snapshots" / "polymarket_weather" / now_dir / snapshot_dir
            base.mkdir(parents=True, exist_ok=True)
            (base / "markets.jsonl").write_text(
                json.dumps({"market_id": "m1", "accepting_orders": True}) + "\n",
                encoding="utf-8",
            )
            (base / "clob_books.jsonl").write_text(
                json.dumps({"token_id": "t_yes", "asks": [{"price": "0.4", "size": "100"}], "bids": []})
                + "\n"
                + json.dumps({"token_id": "t_no", "asks": [{"price": "0.6", "size": "100"}], "bids": []})
                + "\n",
                encoding="utf-8",
            )

            rows = [
                mod.MarketForecast(
                    event_id="e1",
                    city="Chicago",
                    market_id="m1",
                    question="Will the highest temperature in Chicago be 37°F or below on February 16?",
                    yes_token_id="t_yes",
                    no_token_id="t_no",
                    event_end_date=date(2026, 2, 16),
                    entry_time_utc=datetime(2026, 2, 15, 0, 10, tzinfo=timezone.utc),
                    p_yes=0.8,
                    confidence=1.0,
                    winner="YES",
                )
            ]
            bot_config = mod.BotConfig(
                bankroll_usd=1000.0,
                max_daily_loss_pct=10.0,
                max_trade_risk_pct=10.0,
                max_positions=1,
                min_edge=0.02,
                min_stake_usd=1.0,
                fee_bps_per_side=0.0,
                slippage_bps_per_side=0.0,
            )
            with patch.object(mod, "fetch_clob_price_history", side_effect=AssertionError("history should not be called")):
                out = mod.simulate_trading_backtest(
                    rows=rows,
                    bot_config=bot_config,
                    price_history_interval="1m",
                    price_history_fidelity=60,
                    fee_bps_per_side=0.0,
                    slippage_bps_per_side=0.0,
                    snapshots_dir=str(Path(tmpdir) / "snapshots" / "polymarket_weather"),
                    snapshot_max_age_minutes=60,
                )
            trades = out.get("trades") or []
            self.assertEqual(1, len(trades))
            self.assertEqual(0.4, float(trades[0].get("entry_price")))
            self.assertEqual("snapshot", trades[0].get("pricing_source"))


if __name__ == "__main__":
    unittest.main()
