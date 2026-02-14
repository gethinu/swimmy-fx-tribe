import importlib.util
import sys
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("weather_range_model.py")


def load_module():
    spec = importlib.util.spec_from_file_location("weather_range_model", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TestWeatherRangeModel(unittest.TestCase):
    def test_parse_temperature_range_question(self) -> None:
        mod = load_module()
        q = "Will the highest temperature in New York City be between 34-35°F on February 13?"
        parsed = mod.parse_temperature_range_question(q)
        self.assertIsNotNone(parsed)
        assert parsed is not None
        self.assertEqual(parsed.city, "New York City")
        self.assertEqual(parsed.month, 2)
        self.assertEqual(parsed.day, 13)
        self.assertEqual(parsed.low_f, 34)
        self.assertEqual(parsed.high_f, 35)

    def test_parse_rejects_non_weather_question(self) -> None:
        mod = load_module()
        q = "Will the Boston Celtics win the 2026 NBA Finals?"
        self.assertIsNone(mod.parse_temperature_range_question(q))

    def test_range_probability_is_reasonable(self) -> None:
        mod = load_module()
        # With a very tight sigma, probability should be near 1 for a range containing the mean.
        p = mod.normal_range_probability(mu=35.0, sigma=0.01, low_f=34, high_f=35)
        self.assertGreater(p, 0.999)


if __name__ == "__main__":
    unittest.main()

