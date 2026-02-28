import os
import tempfile
from datetime import datetime, timedelta, timezone
from pathlib import Path
import unittest
from unittest import mock

from tools.xau_autobot_data import (
    _extract_ohlc_from_chart,
    build_chunk_windows,
    load_ohlc,
    normalize_interval_alias,
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

    def test_load_ohlc_normalizes_m60_alias(self):
        expected = (
            [datetime(2026, 1, 3, tzinfo=timezone.utc)],
            [2610.0],
            [2612.0],
            [2608.0],
            [2611.0],
        )
        with mock.patch("tools.xau_autobot_data._load_ohlc_chart_api", return_value=expected) as chart_mock:
            result = load_ohlc(ticker="GC=F", period="60d", interval="m60")
        chart_mock.assert_called_once_with(
            ticker="GC=F",
            period="60d",
            interval="60m",
            http_opener=None,
        )
        self.assertEqual(result, expected)

    def test_load_ohlc_normalizes_h4_alias_to_4h(self):
        expected = (
            [datetime(2026, 1, 4, tzinfo=timezone.utc)],
            [2612.0],
            [2616.0],
            [2610.0],
            [2614.0],
        )
        with mock.patch("tools.xau_autobot_data._load_ohlc_chart_api", return_value=expected) as chart_mock:
            result = load_ohlc(ticker="GC=F", period="180d", interval="h4")
        chart_mock.assert_called_once_with(
            ticker="GC=F",
            period="180d",
            interval="4h",
            http_opener=None,
        )
        self.assertEqual(result, expected)

    def test_load_ohlc_resamples_h5_from_60m(self):
        times = [
            datetime(2026, 1, 1, 0, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 1, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 2, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 3, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 4, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 5, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 6, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 7, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 8, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 9, tzinfo=timezone.utc),
        ]
        source = (
            times,
            [1.0, 2.0, 3.0, 4.0, 5.0, 10.0, 11.0, 12.0, 13.0, 14.0],
            [2.0, 3.0, 4.0, 5.0, 6.0, 11.0, 12.0, 13.0, 14.0, 15.0],
            [0.5, 1.5, 2.5, 3.5, 4.5, 9.5, 10.5, 11.5, 12.5, 13.5],
            [1.5, 2.5, 3.5, 4.5, 5.5, 10.5, 11.5, 12.5, 13.5, 14.5],
        )
        with mock.patch("tools.xau_autobot_data._load_ohlc_chart_api", return_value=source) as chart_mock:
            times_out, opens, highs, lows, closes = load_ohlc(ticker="GC=F", period="60d", interval="h5")
        chart_mock.assert_called_once_with(
            ticker="GC=F",
            period="60d",
            interval="60m",
            http_opener=None,
        )
        self.assertEqual(len(times_out), 2)
        self.assertEqual(times_out[0], times[4])
        self.assertEqual(times_out[1], times[9])
        self.assertEqual(opens, [1.0, 10.0])
        self.assertEqual(highs, [6.0, 15.0])
        self.assertEqual(lows, [0.5, 9.5])
        self.assertEqual(closes, [5.5, 14.5])

    def test_load_ohlc_resamples_h5_with_utc_anchor_contract(self):
        times = [
            datetime(2026, 1, 1, 1, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 2, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 3, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 4, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 5, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 6, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 7, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 8, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 9, tzinfo=timezone.utc),
            datetime(2026, 1, 1, 10, tzinfo=timezone.utc),
        ]
        source = (
            times,
            [1.0, 2.0, 3.0, 4.0, 5.0, 10.0, 11.0, 12.0, 13.0, 14.0],
            [2.0, 3.0, 4.0, 5.0, 6.0, 11.0, 12.0, 13.0, 14.0, 15.0],
            [0.5, 1.5, 2.5, 3.5, 4.5, 9.5, 10.5, 11.5, 12.5, 13.5],
            [1.5, 2.5, 3.5, 4.5, 5.5, 10.5, 11.5, 12.5, 13.5, 14.5],
        )
        with mock.patch("tools.xau_autobot_data._load_ohlc_chart_api", return_value=source):
            times_out, opens, highs, lows, closes = load_ohlc(ticker="GC=F", period="60d", interval="h5")
        self.assertEqual(len(times_out), 1)
        self.assertEqual(times_out[0], times[8])
        self.assertEqual(opens, [5.0])
        self.assertEqual(highs, [14.0])
        self.assertEqual(lows, [4.5])
        self.assertEqual(closes, [13.5])

    def test_load_ohlc_rejects_unsupported_resample_anchor(self):
        expected = (
            [datetime(2026, 1, 1, tzinfo=timezone.utc)],
            [2600.0],
            [2602.0],
            [2598.0],
            [2601.0],
        )
        with (
            mock.patch.dict(os.environ, {"XAU_AUTOBOT_RESAMPLE_ANCHOR": "LOCAL_00:00"}, clear=False),
            mock.patch("tools.xau_autobot_data._load_ohlc_chart_api", return_value=expected),
        ):
            with self.assertRaises(ValueError):
                load_ohlc(ticker="GC=F", period="30d", interval="m45")

    def test_normalize_interval_alias_supports_mid_timeframes(self):
        self.assertEqual(normalize_interval_alias("m20"), ("5m", 4))
        self.assertEqual(normalize_interval_alias("m45"), ("15m", 3))
        self.assertEqual(normalize_interval_alias("h2"), ("60m", 2))
        self.assertEqual(normalize_interval_alias("h3"), ("60m", 3))

    def test_load_ohlc_mt5_csv_resamples_and_applies_period_window(self):
        with tempfile.TemporaryDirectory() as td:
            csv_path = Path(td) / "XAUUSD_M15.csv"
            base = datetime(2026, 1, 1, 0, 0, tzinfo=timezone.utc)
            lines = ["timestamp,open,high,low,close,volume"]
            # 3 days of M15 bars (288 rows)
            for i in range(3 * 24 * 4):
                ts = int((base + timedelta(minutes=15 * i)).timestamp())
                op = 2600.0 + (0.1 * i)
                hi = op + 0.3
                lo = op - 0.3
                cl = op + 0.1
                lines.append(f"{ts},{op:.5f},{hi:.5f},{lo:.5f},{cl:.5f},100")
            csv_path.write_text("\n".join(lines) + "\n", encoding="utf-8")

            times, opens, highs, lows, closes = load_ohlc(
                ticker="XAUUSD",
                period="2d",
                interval="m45",
                data_source="mt5_csv",
                source_csv_path=str(csv_path),
            )

            # 2 days window from 15m source => approximately 64 bars of M45.
            self.assertGreaterEqual(len(times), 63)
            self.assertLessEqual(len(times), 65)
            self.assertEqual(len(opens), len(times))
            self.assertEqual(len(highs), len(times))
            self.assertEqual(len(lows), len(times))
            self.assertEqual(len(closes), len(times))
            self.assertEqual(times[-1], base + timedelta(days=3) - timedelta(minutes=15))

    def test_load_ohlc_mt5_csv_requires_source_path(self):
        with self.assertRaises(RuntimeError):
            load_ohlc(
                ticker="XAUUSD",
                period="60d",
                interval="m45",
                data_source="mt5_csv",
                source_csv_path="",
            )


if __name__ == "__main__":
    unittest.main()
