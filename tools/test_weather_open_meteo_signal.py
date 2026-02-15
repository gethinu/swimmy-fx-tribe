import inspect
import importlib.util
import io
import math
import json
import sys
import unittest
from contextlib import redirect_stdout
from datetime import date, datetime, timezone
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch


MODULE_PATH = Path(__file__).with_name("weather_open_meteo_signal.py")


def load_module():
    spec = importlib.util.spec_from_file_location("weather_open_meteo_signal", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TestWeatherOpenMeteoSignal(unittest.TestCase):
    def test_geocode_and_forecast_network_errors_do_not_crash(self) -> None:
        mod = load_module()

        with patch.object(mod, "urlopen", side_effect=TimeoutError("boom")):
            self.assertIsNone(mod.geocode_city_open_meteo(city="London"))
            self.assertEqual(
                [],
                mod.fetch_forecast_open_meteo(
                    latitude=51.5072,
                    longitude=-0.1276,
                    timezone_name="Europe/London",
                    forecast_days=3,
                ),
            )

        with patch.object(mod, "urlopen", side_effect=TimeoutError("boom")):
            self.assertEqual([], mod.fetch_gamma_markets(gamma_url="https://example.invalid/markets", limit=1))
            self.assertEqual(
                [],
                mod.fetch_gamma_events_page(
                    events_url="https://example.invalid/events",
                    limit=1,
                    offset=0,
                    active=True,
                    closed=False,
                ),
            )
            self.assertEqual(
                [],
                mod.fetch_gamma_series_page(
                    series_url="https://example.invalid/series",
                    limit=1,
                    offset=0,
                    recurrence="daily",
                    active=True,
                    closed=False,
                ),
            )

    def test_extract_weather_event_ids(self) -> None:
        mod = load_module()
        events = [
            {"id": 1, "title": "Highest temperature in NYC on February 14?"},
            {"id": 2, "title": "NBA: Will the Knicks win tonight?"},
            {"id": "", "title": "Highest temperature in London on February 15?"},
        ]
        self.assertEqual(["1"], mod.extract_weather_event_ids(events))

    def test_extract_weather_series_ids(self) -> None:
        mod = load_module()
        series = [
            {"id": "10006", "slug": "london-daily-weather", "title": "London Daily Weather"},
            {"id": "42", "slug": "nba-playoffs", "title": "NBA"},
            {"id": "", "slug": "nyc-daily-weather", "title": "NYC Daily Weather"},
        ]
        self.assertEqual(["10006"], mod.extract_weather_series_ids(series))

    def test_collect_weather_event_ids_paginates(self) -> None:
        mod = load_module()
        pages = {
            0: [
                {"id": 1, "title": "Highest temperature in NYC on February 14?"},
                {"id": 2, "title": "NBA: Will the Knicks win tonight?"},
            ],
            2: [
                {"id": 3, "title": "Highest temperature in London on February 15?"},
            ],
            4: [],
        }

        def fetch_events_page(*, limit: int, offset: int):
            assert limit == 2
            return pages.get(offset, [])

        ids = mod.collect_weather_event_ids(
            fetch_events_page=fetch_events_page,
            page_size=2,
            max_pages=10,
            max_events=10,
        )
        self.assertEqual(["1", "3"], ids)

    def test_collect_weather_series_ids_paginates(self) -> None:
        mod = load_module()
        pages = {
            0: [
                {"id": "10006", "slug": "london-daily-weather"},
                {"id": "42", "slug": "nba-playoffs"},
            ],
            2: [
                {"id": "10005", "slug": "nyc-daily-weather"},
            ],
            4: [],
        }

        def fetch_series_page(*, limit: int, offset: int):
            assert limit == 2
            return pages.get(offset, [])

        ids = mod.collect_weather_series_ids(
            fetch_series_page=fetch_series_page,
            page_size=2,
            max_pages=10,
            max_series=10,
        )
        self.assertEqual(["10006", "10005"], ids)

    def test_extract_markets_from_events(self) -> None:
        mod = load_module()
        events = [
            {"id": "e1", "markets": [{"id": "m1"}, {"id": "m2"}]},
            {"id": "e2", "markets": []},
            {"id": "e3", "markets": [{"id": "m3"}]},
        ]
        markets = mod.extract_markets_from_events(events)
        self.assertEqual(["m1", "m2", "m3"], [m.get("id") for m in markets])

    def test_collect_weather_markets_from_series_ids_caps_and_dedupes(self) -> None:
        mod = load_module()
        series_ids = ["a", "b"]

        def fetch_events_for_series(*, series_id: str, limit: int, offset: int):
            assert limit == 2
            assert offset == 0
            if series_id == "a":
                return [{"markets": [{"id": "m1"}, {"id": "m2"}]}]
            return [{"markets": [{"id": "m2"}, {"id": "m3"}]}]

        markets = mod.collect_weather_markets_from_series_ids(
            series_ids=series_ids,
            fetch_events_for_series=fetch_events_for_series,
            events_limit=2,
            max_markets=2,
        )
        self.assertEqual(["m1", "m2"], [m.get("id") for m in markets])

    def test_fetch_weather_markets_from_series_orchestrates(self) -> None:
        mod = load_module()

        def fetch_series_page(*, limit: int, offset: int):
            assert limit == 2
            if offset == 0:
                return [{"id": "10006", "slug": "london-daily-weather"}]
            return []

        def fetch_events_for_series(*, series_id: str, limit: int, offset: int):
            assert series_id == "10006"
            assert limit == 3
            assert offset == 0
            return [{"markets": [{"id": "m1"}, {"id": "m2"}]}]

        markets = mod.fetch_weather_markets_from_series(
            fetch_series_page=fetch_series_page,
            fetch_events_for_series=fetch_events_for_series,
            series_page_size=2,
            series_max_pages=10,
            series_max_count=10,
            events_limit=3,
            max_markets=10,
        )
        self.assertEqual(["m1", "m2"], [m.get("id") for m in markets])

    def test_fetch_weather_markets_from_gamma_prefers_series(self) -> None:
        mod = load_module()

        def fake_series_page(*, series_url: str, limit: int, offset: int, recurrence: str, active: bool, closed: bool):
            assert recurrence == "daily"
            assert active is True
            assert closed is False
            if offset == 0:
                return [{"id": "10006", "slug": "london-daily-weather"}]
            return []

        def fake_events_page(*, events_url: str, limit: int, offset: int, active: bool, closed: bool, series_id: str | None = None):
            assert active is True
            assert closed is False
            assert series_id == "10006"
            assert offset == 0
            return [{"markets": [{"id": "m1"}, {"id": "m2"}]}]

        with patch.object(mod, "fetch_gamma_series_page", side_effect=fake_series_page):
            with patch.object(mod, "fetch_gamma_events_page", side_effect=fake_events_page):
                markets = mod.fetch_weather_markets_from_gamma(
                    series_url="https://gamma-api.polymarket.com/series",
                    events_url="https://gamma-api.polymarket.com/events",
                    series_page_size=2,
                    series_max_pages=10,
                    series_max_count=10,
                    events_limit=3,
                    max_markets=10,
                )

        self.assertEqual(["m1", "m2"], [m.get("id") for m in markets])

    def test_main_smoke_no_markets(self) -> None:
        mod = load_module()
        with patch.object(mod, "fetch_gamma_series_page", return_value=[]):
            with patch.object(mod, "fetch_gamma_events_page", return_value=[]):
                with patch.object(mod, "fetch_gamma_markets", return_value=[]):
                    with patch.object(mod.sys, "argv", ["weather_open_meteo_signal.py"]):
                        mod.main()

    def test_main_supports_calibration_file(self) -> None:
        mod = load_module()
        calibration = {"method": "isotonic", "points": [{"x": 0.0, "y": 0.5}, {"x": 1.0, "y": 0.5}]}
        markets = [
            {
                "id": "m1",
                "question": "Will the highest temperature in New York City be between 54-56°F on February 13?",
            }
        ]
        geo = mod.GeocodeResult(
            name="New York City",
            latitude=40.7128,
            longitude=-74.0060,
            timezone="America/New_York",
        )
        forecast = [mod.ForecastDay(day=date(2026, 2, 13), temperature_max_f=55.0, lead_days=1)]

        with TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "calibration.json"
            path.write_text(json.dumps(calibration), encoding="utf-8")
            out = io.StringIO()
            with patch.object(mod, "fetch_weather_markets_from_gamma", return_value=markets):
                with patch.object(mod, "geocode_city_open_meteo", return_value=geo):
                    with patch.object(mod, "fetch_forecast_open_meteo", return_value=forecast):
                        with patch.object(mod.sys, "argv", ["weather_open_meteo_signal.py", "--calibration-file", str(path)]):
                            try:
                                with redirect_stdout(out):
                                    mod.main()
                            except SystemExit as exc:
                                self.fail(f"main() should accept --calibration-file (SystemExit={exc.code})")
            lines = [line for line in out.getvalue().splitlines() if line.strip()]
            self.assertEqual(1, len(lines))
            row = json.loads(lines[0])
            self.assertEqual(0.5, float(row.get("p_yes")))

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

    def test_build_weather_signals_from_markets_supports_multi_model_ensemble(self) -> None:
        mod = load_module()
        markets = [
            {
                "id": "m1",
                "question": "Will the highest temperature in New York City be between 54-56°F on February 13?",
            }
        ]

        calls = {"geocode": 0, "forecast_models": []}

        def geocode_city(city: str):
            calls["geocode"] += 1
            return mod.GeocodeResult(
                name=city,
                latitude=40.7128,
                longitude=-74.0060,
                timezone="America/New_York",
            )

        def fetch_forecast(*, latitude: float, longitude: float, timezone_name: str, model: str = ""):
            _ = (latitude, longitude, timezone_name)
            calls["forecast_models"].append(model)
            temp_by_model = {"model_a": 50.0, "model_b": 60.0}
            return [
                mod.ForecastDay(
                    day=date(2026, 2, 13),
                    temperature_max_f=float(temp_by_model.get(model, 55.0)),
                    lead_days=1,
                )
            ]

        base_sigma = float(mod.sigma_f_for_lead_days(1))
        base_conf = float(mod.confidence_for_sigma(base_sigma))
        signals = mod.build_weather_signals_from_markets(
            markets=markets,
            now_utc=datetime(2026, 2, 12, 12, 0, tzinfo=timezone.utc),
            geocode_city=geocode_city,
            fetch_forecast=fetch_forecast,
            forecast_models=["model_a", "model_b"],
        )

        self.assertEqual(1, len(signals))
        self.assertEqual(1, calls["geocode"])
        self.assertEqual(["model_a", "model_b"], sorted(calls["forecast_models"]))

        row = signals[0]
        self.assertEqual("m1", row.get("market_id"))
        # Mean of 50F and 60F
        self.assertEqual(55.0, float(row.get("forecast_temperature_max_f")))

        sigma_total = math.sqrt((base_sigma**2) + (5.0**2))
        sigma_used = float(row.get("sigma_f"))
        self.assertEqual(round(sigma_total, 3), sigma_used)
        self.assertLess(float(row.get("confidence")), base_conf)

        parsed = mod.parse_temperature_bucket_question(markets[0]["question"])
        assert parsed is not None
        expected_p = mod.normal_bucket_probability(mu=55.0, sigma=sigma_total, low=parsed.low, high=parsed.high)
        self.assertEqual(round(float(expected_p), 6), float(row.get("p_yes")))

    def test_build_weather_signals_from_markets_applies_probability_calibration(self) -> None:
        mod = load_module()

        sig = inspect.signature(mod.build_weather_signals_from_markets)
        self.assertIn("calibration", sig.parameters)

        markets = [
            {
                "id": "m1",
                "question": "Will the highest temperature in New York City be between 54-56°F on February 13?",
            }
        ]

        def geocode_city(city: str):
            return mod.GeocodeResult(
                name=city,
                latitude=40.7128,
                longitude=-74.0060,
                timezone="America/New_York",
            )

        def fetch_forecast(*, latitude: float, longitude: float, timezone_name: str, model: str = ""):
            _ = (latitude, longitude, timezone_name, model)
            return [
                mod.ForecastDay(
                    day=date(2026, 2, 13),
                    temperature_max_f=55.0,
                    lead_days=1,
                )
            ]

        parsed = mod.parse_temperature_bucket_question(markets[0]["question"])
        assert parsed is not None
        sigma = float(mod.sigma_f_for_lead_days(1))
        p_raw = float(mod.normal_bucket_probability(mu=55.0, sigma=sigma, low=parsed.low, high=parsed.high))
        self.assertNotAlmostEqual(0.5, p_raw, places=3)

        calibration = {"method": "isotonic", "points": [{"x": 0.0, "y": 0.5}, {"x": 1.0, "y": 0.5}]}
        rows = mod.build_weather_signals_from_markets(
            markets=markets,
            now_utc=datetime(2026, 2, 12, 12, 0, tzinfo=timezone.utc),
            geocode_city=geocode_city,
            fetch_forecast=fetch_forecast,
            calibration=calibration,
        )
        self.assertEqual(1, len(rows))
        self.assertEqual(0.5, float(rows[0].get("p_yes")))

    def test_build_weather_signals_supports_celsius_exact_bucket(self) -> None:
        mod = load_module()
        markets = [
            {
                "id": "m1",
                "question": "Will the highest temperature in London be 5°C on February 14?",
            }
        ]

        def geocode_city(city: str):
            return mod.GeocodeResult(
                name=city,
                latitude=51.5072,
                longitude=-0.1276,
                timezone="Europe/London",
            )

        def fetch_forecast(*, latitude: float, longitude: float, timezone_name: str):
            return [
                mod.ForecastDay(
                    day=date(2026, 2, 14),
                    temperature_max_f=41.0,  # 5C
                    lead_days=0,
                )
            ]

        signals = mod.build_weather_signals_from_markets(
            markets=markets,
            now_utc=datetime(2026, 2, 13, 12, 0, tzinfo=timezone.utc),
            geocode_city=geocode_city,
            fetch_forecast=fetch_forecast,
        )
        self.assertEqual(1, len(signals))
        self.assertEqual("m1", signals[0]["market_id"])
        self.assertEqual("weather_open_meteo", signals[0]["source"])
        self.assertGreater(signals[0]["p_yes"], 0.2)

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
