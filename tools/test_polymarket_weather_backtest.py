import importlib.util
import sys
import unittest
from pathlib import Path


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


if __name__ == "__main__":
    unittest.main()

