from datetime import datetime, timedelta, timezone
import unittest

from tools.xau_autobot_data import (
    build_chunk_windows,
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


if __name__ == "__main__":
    unittest.main()
