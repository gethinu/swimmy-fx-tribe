import importlib.util
import sys
import unittest
from datetime import date, datetime, timezone
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("weather_open_meteo_signal.py")


def load_module():
    spec = importlib.util.spec_from_file_location("weather_open_meteo_signal", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TestWeatherOpenMeteoSignal(unittest.TestCase):
    def test_build_weather_signals_from_markets_uses_cache(self) -> None:
        mod = load_module()
        markets = [
            {
                "id": "m1",
                "question": "Will the highest temperature in New York City be between 34-35°F on February 13?",
            },
            {
                "id": "m2",
                "question": "Will the highest temperature in New York City be between 48-49°F on February 13?",
            },
            {"id": "x", "question": "Will the Boston Celtics win the 2026 NBA Finals?"},
        ]

        calls = {"geocode": 0, "forecast": 0}

        def geocode_city(city: str):
            calls["geocode"] += 1
            return mod.GeocodeResult(
                name=city,
                latitude=40.7128,
                longitude=-74.0060,
                timezone="America/New_York",
            )

        def fetch_forecast(*, latitude: float, longitude: float, timezone_name: str):
            calls["forecast"] += 1
            return [
                mod.ForecastDay(
                    day=date(2026, 2, 13),
                    temperature_max_f=35.0,
                    lead_days=1,
                )
            ]

        signals = mod.build_weather_signals_from_markets(
            markets=markets,
            now_utc=datetime(2026, 2, 12, 12, 0, tzinfo=timezone.utc),
            geocode_city=geocode_city,
            fetch_forecast=fetch_forecast,
        )

        # Two weather markets, one non-weather market filtered out.
        self.assertEqual(2, len(signals))
        self.assertEqual(1, calls["geocode"])
        self.assertEqual(1, calls["forecast"])
        self.assertTrue(all(row.get("source") == "weather_open_meteo" for row in signals))

        near = next(row for row in signals if row["market_id"] == "m1")
        far = next(row for row in signals if row["market_id"] == "m2")
        self.assertGreater(near["p_yes"], far["p_yes"])

    def test_build_weather_signals_skips_out_of_horizon(self) -> None:
        mod = load_module()
        markets = [
            {
                "id": "m1",
                "question": "Will the highest temperature in Tokyo be between 34-35°F on February 13?",
            }
        ]

        def geocode_city(city: str):
            return mod.GeocodeResult(
                name=city,
                latitude=35.6895,
                longitude=139.6917,
                timezone="Asia/Tokyo",
            )

        def fetch_forecast(*, latitude: float, longitude: float, timezone_name: str):
            # No matching date in forecast payload -> skip
            return [
                mod.ForecastDay(
                    day=date(2026, 2, 14),
                    temperature_max_f=40.0,
                    lead_days=1,
                )
            ]

        signals = mod.build_weather_signals_from_markets(
            markets=markets,
            now_utc=datetime(2026, 2, 12, 12, 0, tzinfo=timezone.utc),
            geocode_city=geocode_city,
            fetch_forecast=fetch_forecast,
        )
        self.assertEqual([], signals)


if __name__ == "__main__":
    unittest.main()
