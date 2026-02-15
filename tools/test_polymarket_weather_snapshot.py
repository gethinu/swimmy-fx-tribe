import importlib.util
import json
import sys
import unittest
from datetime import datetime, timezone
from io import BytesIO
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch
from urllib.error import HTTPError


MODULE_PATH = Path(__file__).with_name("polymarket_weather_snapshot.py")


def load_module():
    spec = importlib.util.spec_from_file_location("polymarket_weather_snapshot", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TestPolymarketWeatherSnapshot(unittest.TestCase):
    def test_collect_snapshot_writes_expected_files_and_trims_books(self) -> None:
        mod = load_module()

        series_url = "https://gamma-api.polymarket.com/series"
        events_url = "https://gamma-api.polymarket.com/events"
        clob_host = "https://clob.polymarket.com"

        series_payload = [
            {"id": "s1", "slug": "chicago-daily-weather", "title": "Chicago Daily Weather", "active": True, "closed": False},
            {"id": "s2", "slug": "nba-playoffs", "title": "NBA", "active": True, "closed": False},
        ]
        event_payload = [
            {
                "id": "e1",
                "title": "Highest temperature in Chicago on February 16?",
                "slug": "highest-temperature-in-chicago-on-february-16",
                "seriesSlug": "chicago-daily-weather",
                "active": True,
                "closed": False,
                "startDate": "2026-02-14T11:07:52.956845Z",
                "endDate": "2026-02-16T12:00:00Z",
                "markets": [
                    {
                        "id": "m1",
                        "conditionId": "0xabc",
                        "question": "Will the highest temperature in Chicago be 37°F or below on February 16?",
                        "active": True,
                        "closed": False,
                        "archived": False,
                        "outcomes": "[\"Yes\",\"No\"]",
                        "outcomePrices": "[\"0.45\",\"0.55\"]",
                        "clobTokenIds": "[\"t_yes\",\"t_no\"]",
                    }
                ],
            }
        ]

        def fake_request_json(url: str):
            if url.startswith(series_url):
                return series_payload
            if url.startswith(events_url):
                return event_payload
            if url.startswith(clob_host.rstrip("/") + "/book?"):
                # Always return a >depth orderbook so we can assert trimming.
                return {
                    "market": "t_yes",
                    "asset_id": "t_yes",
                    "timestamp": "123",
                    "bids": [
                        {"price": "0.10", "size": "1"},
                        {"price": "0.09", "size": "2"},
                    ],
                    "asks": [
                        {"price": "0.11", "size": "1"},
                        {"price": "0.12", "size": "2"},
                    ],
                    "last_trade_price": "0.10",
                    "min_order_size": "1",
                    "tick_size": "0.01",
                    "neg_risk": False,
                }
            raise AssertionError(f"unexpected url: {url}")

        class DummyCompleted:
            def __init__(self, stdout: str):
                self.stdout = stdout
                self.stderr = ""
                self.returncode = 0

        def fake_subprocess_run(*args, **kwargs):
            stdout = json.dumps(
                {
                    "market_id": "m1",
                    "p_yes": 0.8,
                    "confidence": 0.7,
                    "question": "Will the highest temperature in Chicago be 37°F or below on February 16?",
                    "source": "weather_open_meteo",
                    "forecast_temperature_max_f": 50.0,
                    "sigma_f": 2.0,
                    "lead_days": 2,
                }
            )
            return DummyCompleted(stdout=stdout + "\n")

        now = datetime(2026, 2, 15, 12, 0, 0, tzinfo=timezone.utc)
        with TemporaryDirectory() as tmpdir:
            base = Path(tmpdir)
            with patch.object(mod, "_request_json", side_effect=fake_request_json), patch.object(
                mod.subprocess, "run", side_effect=fake_subprocess_run
            ):
                out_dir = mod.collect_snapshot(
                    output_dir=base,
                    now=now,
                    series_url=series_url,
                    events_url=events_url,
                    clob_host=clob_host,
                    signal_command="echo ignored",
                    orderbook_mode="all",
                    orderbook_depth=1,
                )

            self.assertTrue((out_dir / "manifest.json").exists())
            self.assertTrue((out_dir / "gamma_series.json").exists())
            self.assertTrue((out_dir / "gamma_events.json").exists())
            self.assertTrue((out_dir / "markets.jsonl").exists())
            self.assertTrue((out_dir / "signals.jsonl").exists())
            self.assertTrue((out_dir / "clob_books.jsonl").exists())

            series = json.loads((out_dir / "gamma_series.json").read_text(encoding="utf-8"))
            self.assertEqual(["chicago-daily-weather"], [row.get("slug") for row in series])

            signals_lines = (out_dir / "signals.jsonl").read_text(encoding="utf-8").strip().splitlines()
            self.assertEqual(1, len(signals_lines))
            self.assertEqual("m1", json.loads(signals_lines[0]).get("market_id"))

            books_lines = (out_dir / "clob_books.jsonl").read_text(encoding="utf-8").strip().splitlines()
            # YES/NO token ids => 2 books
            self.assertEqual(2, len(books_lines))
            for line in books_lines:
                payload = json.loads(line)
                self.assertLessEqual(len(payload.get("bids", [])), 1)
                self.assertLessEqual(len(payload.get("asks", [])), 1)

    def test_collect_snapshot_records_book_failure_and_continues(self) -> None:
        mod = load_module()

        series_url = "https://gamma-api.polymarket.com/series"
        events_url = "https://gamma-api.polymarket.com/events"
        clob_host = "https://clob.polymarket.com"

        series_payload = [{"id": "s1", "slug": "chicago-daily-weather", "title": "Chicago Daily Weather", "active": True, "closed": False}]
        event_payload = [
            {
                "id": "e1",
                "title": "Highest temperature in Chicago on February 16?",
                "slug": "highest-temperature-in-chicago-on-february-16",
                "seriesSlug": "chicago-daily-weather",
                "active": True,
                "closed": False,
                "startDate": "2026-02-14T11:07:52.956845Z",
                "endDate": "2026-02-16T12:00:00Z",
                "markets": [
                    {
                        "id": "m1",
                        "conditionId": "0xabc",
                        "question": "Will the highest temperature in Chicago be 37°F or below on February 16?",
                        "active": True,
                        "closed": False,
                        "archived": False,
                        "outcomes": "[\"Yes\",\"No\"]",
                        "outcomePrices": "[\"0.45\",\"0.55\"]",
                        "clobTokenIds": "[\"t_yes\",\"t_no\"]",
                    }
                ],
            }
        ]

        def fake_request_json(url: str):
            if url.startswith(series_url):
                return series_payload
            if url.startswith(events_url):
                return event_payload
            if url.startswith(clob_host.rstrip("/") + "/book?") and "token_id=t_yes" in url:
                return {"bids": [{"price": "0.10", "size": "1"}], "asks": [{"price": "0.11", "size": "1"}]}
            if url.startswith(clob_host.rstrip("/") + "/book?") and "token_id=t_no" in url:
                raise HTTPError(url, 404, "Not Found", hdrs=None, fp=BytesIO(b"{\"error\":\"no book\"}"))
            raise AssertionError(f"unexpected url: {url}")

        class DummyCompleted:
            def __init__(self, stdout: str):
                self.stdout = stdout
                self.stderr = ""
                self.returncode = 0

        def fake_subprocess_run(*args, **kwargs):
            stdout = json.dumps({"market_id": "m1", "p_yes": 0.8, "confidence": 0.7})
            return DummyCompleted(stdout=stdout + "\n")

        now = datetime(2026, 2, 15, 12, 0, 0, tzinfo=timezone.utc)
        with TemporaryDirectory() as tmpdir:
            base = Path(tmpdir)
            with patch.object(mod, "_request_json", side_effect=fake_request_json), patch.object(
                mod.subprocess, "run", side_effect=fake_subprocess_run
            ):
                out_dir = mod.collect_snapshot(
                    output_dir=base,
                    now=now,
                    series_url=series_url,
                    events_url=events_url,
                    clob_host=clob_host,
                    signal_command="echo ignored",
                    orderbook_mode="all",
                    orderbook_depth=10,
                )

            errors_path = out_dir / "errors.jsonl"
            self.assertTrue(errors_path.exists())
            errors = [json.loads(line) for line in errors_path.read_text(encoding="utf-8").splitlines() if line.strip()]
            self.assertTrue(any(row.get("token_id") == "t_no" and row.get("http_status") == 404 for row in errors))

            books_lines = (out_dir / "clob_books.jsonl").read_text(encoding="utf-8").strip().splitlines()
            # We still wrote the successful book.
            self.assertTrue(any(json.loads(line).get("token_id") == "t_yes" for line in books_lines))


if __name__ == "__main__":
    unittest.main()
