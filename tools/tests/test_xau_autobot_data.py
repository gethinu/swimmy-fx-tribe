from datetime import datetime, timedelta, timezone
import unittest
from unittest import mock

from tools.xau_autobot_data import (
    _extract_ohlc_from_chart,
    build_chunk_windows,
    load_ohlc,
    parse_period_to_timedelta,
    requires_chunked_intraday,
)


class TestXauAutoBotData(unittest.TestCase):
    def test_parse_period_to_timedelta(self):
        self.assertEqual(parse_period_to_timedelta("90d"), timedelta(days=90))
        self.assertEqual(parse_period_to_timedelta("2wk"), timedelta(days=14))
        self.assertEqual(parse_period_to_timedelta("1mo"), timedelta(days=30))
        self.assertIsNone(parse_period_to_timedelta("max"))

    def test_requires_chunked_intraday(self):
        self.assertTrue(requires_chunked_intraday(interval="5m", period="90d"))
        self.assertFalse(requires_chunked_intraday(interval="5m", period="60d"))
        self.assertFalse(requires_chunked_intraday(interval="1d", period="365d"))

    def test_build_chunk_windows(self):
        start = datetime(2026, 1, 1, tzinfo=timezone.utc)
        end = datetime(2026, 4, 1, tzinfo=timezone.utc)
        windows = build_chunk_windows(start=start, end=end, chunk_days=59)
        self.assertGreaterEqual(len(windows), 2)
        self.assertEqual(windows[0][0], start)
        self.assertEqual(windows[-1][1], end)
        for lo, hi in windows:
            self.assertLess(lo, hi)

    def test_extract_ohlc_from_chart_filters_missing_rows(self):
        payload = {
            "chart": {
                "result": [
                    {
                        "timestamp": [1735689600, 1735689660],
                        "indicators": {
                            "quote": [
                                {
                                    "open": [2600.1, None],
                                    "high": [2601.1, 2602.2],
                                    "low": [2599.5, 2600.0],
                                    "close": [2600.8, 2601.3],
                                }
                            ]
                        },
                    }
                ],
                "error": None,
            }
        }
        times, opens, highs, lows, closes = _extract_ohlc_from_chart(payload)
        self.assertEqual(len(times), 1)
        self.assertEqual(times[0], datetime.fromtimestamp(1735689600, tz=timezone.utc))
        self.assertEqual(opens, [2600.1])
        self.assertEqual(highs, [2601.1])
        self.assertEqual(lows, [2599.5])
        self.assertEqual(closes, [2600.8])

    def test_load_ohlc_prefers_chart_api_when_available(self):
        expected = (
            [datetime(2026, 1, 1, tzinfo=timezone.utc)],
            [2600.0],
            [2602.0],
            [2598.0],
            [2601.0],
        )
        with mock.patch("tools.xau_autobot_data._load_ohlc_chart_api", return_value=expected) as chart_mock:
            result = load_ohlc(ticker="GC=F", period="30d", interval="5m")
        chart_mock.assert_called_once()
        self.assertEqual(result, expected)

    def test_load_ohlc_falls_back_to_yfinance_when_chart_api_fails(self):
        expected = (
            [datetime(2026, 1, 2, tzinfo=timezone.utc)],
            [2605.0],
            [2608.0],
            [2603.0],
            [2606.0],
        )
        with (
            mock.patch("tools.xau_autobot_data._load_ohlc_chart_api", side_effect=RuntimeError("chart api unavailable")),
            mock.patch("tools.xau_autobot_data._load_ohlc_yfinance", return_value=expected) as yf_mock,
        ):
            result = load_ohlc(
                ticker="GC=F",
                period="30d",
                interval="5m",
                yf_module=object(),
            )
        yf_mock.assert_called_once()
        self.assertEqual(result, expected)


if __name__ == "__main__":
    unittest.main()
