import inspect
import importlib.util
import io
import json
import sys
import unittest
from contextlib import redirect_stdout
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

    def test_fetch_clob_price_history_timeout_returns_empty(self) -> None:
        mod = load_module()

        with patch.object(mod, "urlopen", side_effect=TimeoutError("boom")):
            self.assertEqual(
                [],
                mod.fetch_clob_price_history(token_id="t_yes", interval="1m", fidelity=60, host="https://clob.polymarket.com"),
            )

    def test_build_market_forecasts_incorporates_model_disagreement_sigma(self) -> None:
        mod = load_module()

        events = [
            {
                "id": "e1",
                "startDate": "2026-02-14T00:00:00Z",
                "endDate": "2026-02-16T00:00:00Z",
                "markets": [
                    {
                        "id": "m1",
                        "question": "Will the highest temperature in Chicago be between 54-56°F on February 16?",
                        "clobTokenIds": "[\"t_yes\",\"t_no\"]",
                        "winner": "YES",
                    }
                ],
            }
        ]

        target_day = date(2026, 2, 16)
        mu_map = {("Chicago", target_day): 55.0}
        sigma_model_map = {("Chicago", target_day): 5.0}

        base = mod.build_market_forecasts(
            events=events,
            forecast_temp_max_f_by_city_date=mu_map,
            entry_offset_hours=0,
        )
        with_disagreement = mod.build_market_forecasts(
            events=events,
            forecast_temp_max_f_by_city_date=mu_map,
            sigma_model_f_by_city_date=sigma_model_map,
            entry_offset_hours=0,
        )

        self.assertEqual(1, len(base))
        self.assertEqual(1, len(with_disagreement))
        self.assertLess(with_disagreement[0].p_yes, base[0].p_yes)
        self.assertLess(with_disagreement[0].confidence, base[0].confidence)

    def test_build_market_forecasts_applies_probability_calibration(self) -> None:
        mod = load_module()

        sig = inspect.signature(mod.build_market_forecasts)
        self.assertIn("calibration", sig.parameters)

        events = [
            {
                "id": "e1",
                "startDate": "2026-02-14T00:00:00Z",
                "endDate": "2026-02-16T00:00:00Z",
                "markets": [
                    {
                        "id": "m1",
                        "question": "Will the highest temperature in Chicago be between 54-56°F on February 16?",
                        "clobTokenIds": "[\"t_yes\",\"t_no\"]",
                        "winner": "YES",
                    }
                ],
            }
        ]

        target_day = date(2026, 2, 16)
        mu_map = {("Chicago", target_day): 55.0}

        calibration = {"method": "isotonic", "points": [{"x": 0.0, "y": 0.5}, {"x": 1.0, "y": 0.5}]}
        out = mod.build_market_forecasts(
            events=events,
            forecast_temp_max_f_by_city_date=mu_map,
            entry_offset_hours=0,
            calibration=calibration,
        )
        self.assertEqual(1, len(out))
        self.assertEqual(0.5, float(out[0].p_yes))

    def test_main_supports_calibration_file(self) -> None:
        mod = load_module()

        calibration = {"method": "isotonic", "points": [{"x": 0.0, "y": 0.5}, {"x": 1.0, "y": 0.5}]}
        events = [
            {
                "id": "e1",
                "startDate": "2026-02-14T00:00:00Z",
                "endDate": "2026-02-15T00:00:00Z",
                "markets": [
                    {
                        "id": "m1",
                        "question": "Will the highest temperature in Chicago be between 54-56°F on February 15?",
                        "clobTokenIds": "[\"t_yes\",\"t_no\"]",
                        "winner": "YES",
                    }
                ],
            }
        ]

        with TemporaryDirectory() as tmpdir:
            base = Path(tmpdir)
            cal_path = base / "calibration.json"
            cal_path.write_text(json.dumps(calibration), encoding="utf-8")
            bot_path = base / "bot.json"
            bot_path.write_text("{}", encoding="utf-8")

            # Patch Open-Meteo geocoder in the source module; main() imports it dynamically.
            import tools.weather_open_meteo_signal as wom

            geo = wom.GeocodeResult(name="Chicago", latitude=41.8781, longitude=-87.6298, timezone="America/Chicago")
            forecast_payload = {"daily": {"time": ["2026-02-15"], "temperature_2m_max": [55.0]}}

            out = io.StringIO()
            with patch.object(mod, "fetch_gamma_series", return_value=[{"id": "s1", "slug": "chicago-daily-weather"}]):
                with patch.object(mod, "fetch_gamma_events_for_series", return_value=events):
                    with patch.object(wom, "geocode_city_open_meteo", return_value=geo):
                        with patch.object(mod, "_request_json", return_value=forecast_payload):
                            with patch.object(
                                mod,
                                "simulate_trading_backtest",
                                return_value={
                                    "resolved_trades": 0,
                                    "total_stake_usd": 0.0,
                                    "expected_value_usd": 0.0,
                                    "realized_pnl_usd": 0.0,
                                    "win_rate": None,
                                    "return_on_stake": None,
                                    "trades": [],
                                },
                            ):
                                argv = [
                                    "polymarket_weather_backtest.py",
                                    "--start-date",
                                    "2026-02-15",
                                    "--end-date",
                                    "2026-02-15",
                                    "--closed",
                                    "--bot-config-file",
                                    str(bot_path),
                                    "--candidate-per-event",
                                    "1",
                                    "--max-events-per-series",
                                    "1",
                                    "--calibration-file",
                                    str(cal_path),
                                ]
                                with patch.object(mod.sys, "argv", argv):
                                    try:
                                        with redirect_stdout(out):
                                            mod.main()
                                    except SystemExit as exc:
                                        self.fail(f"main() should accept --calibration-file (SystemExit={exc.code})")

            report = json.loads(out.getvalue())
            self.assertEqual(1, int(report["accuracy"]["resolved_markets"]))
            self.assertEqual(0.25, float(report["accuracy"]["brier"]))
            self.assertEqual(0.693147, float(report["accuracy"]["logloss"]))

    def test_main_writes_calibration_and_can_skip_trading(self) -> None:
        mod = load_module()

        events = [
            {
                "id": "e1",
                "startDate": "2026-02-14T00:00:00Z",
                "endDate": "2026-02-15T00:00:00Z",
                "markets": [
                    {
                        "id": "m1",
                        "question": "Will the highest temperature in Chicago be between 54-56°F on February 15?",
                        "clobTokenIds": "[\"t_yes\",\"t_no\"]",
                        "winner": "YES",
                    }
                ],
            }
        ]

        with TemporaryDirectory() as tmpdir:
            base = Path(tmpdir)
            bot_path = base / "bot.json"
            bot_path.write_text("{}", encoding="utf-8")
            cal_out = base / "calibration_out.json"

            import tools.weather_open_meteo_signal as wom

            geo = wom.GeocodeResult(name="Chicago", latitude=41.8781, longitude=-87.6298, timezone="America/Chicago")
            forecast_payload = {"daily": {"time": ["2026-02-15"], "temperature_2m_max": [55.0]}}

            out = io.StringIO()
            with patch.object(mod, "fetch_gamma_series", return_value=[{"id": "s1", "slug": "chicago-daily-weather"}]):
                with patch.object(mod, "fetch_gamma_events_for_series", return_value=events):
                    with patch.object(wom, "geocode_city_open_meteo", return_value=geo):
                        with patch.object(mod, "_request_json", return_value=forecast_payload):
                            with patch.object(mod, "simulate_trading_backtest", side_effect=AssertionError("trading should be skipped")):
                                argv = [
                                    "polymarket_weather_backtest.py",
                                    "--start-date",
                                    "2026-02-15",
                                    "--end-date",
                                    "2026-02-15",
                                    "--closed",
                                    "--bot-config-file",
                                    str(bot_path),
                                    "--candidate-per-event",
                                    "1",
                                    "--max-events-per-series",
                                    "1",
                                    "--skip-trading",
                                    "--write-calibration",
                                    str(cal_out),
                                ]
                                with patch.object(mod.sys, "argv", argv):
                                    try:
                                        with redirect_stdout(out):
                                            mod.main()
                                    except SystemExit as exc:
                                        self.fail(
                                            f"main() should accept --skip-trading/--write-calibration (SystemExit={exc.code})"
                                        )

            self.assertTrue(cal_out.exists())
            payload = json.loads(cal_out.read_text(encoding="utf-8"))
            self.assertEqual("isotonic", str(payload.get("method")))
            points = payload.get("points")
            self.assertIsInstance(points, list)
            self.assertGreaterEqual(len(points), 2)
            self.assertEqual(0.0, float(points[0].get("x")))
            self.assertEqual(1.0, float(points[-1].get("x")))


if __name__ == "__main__":
    unittest.main()
