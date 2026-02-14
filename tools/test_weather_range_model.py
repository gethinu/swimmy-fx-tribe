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
    def test_parse_temperature_bucket_question_between_f(self) -> None:
        mod = load_module()
        q = "Will the highest temperature in New York City be between 34-35°F on February 13?"
        parsed = mod.parse_temperature_bucket_question(q)
        self.assertIsNotNone(parsed)
        assert parsed is not None
        self.assertEqual(parsed.city, "New York City")
        self.assertEqual(parsed.month, 2)
        self.assertEqual(parsed.day, 13)
        self.assertEqual(parsed.unit, "F")
        self.assertEqual(parsed.low, 34)
        self.assertEqual(parsed.high, 35)

    def test_parse_temperature_bucket_question_exact_c(self) -> None:
        mod = load_module()
        q = "Will the highest temperature in London be 5°C on February 14?"
        parsed = mod.parse_temperature_bucket_question(q)
        self.assertIsNotNone(parsed)
        assert parsed is not None
        self.assertEqual(parsed.city, "London")
        self.assertEqual(parsed.month, 2)
        self.assertEqual(parsed.day, 14)
        self.assertEqual(parsed.unit, "C")
        self.assertEqual(parsed.low, 5)
        self.assertEqual(parsed.high, 5)

    def test_parse_temperature_bucket_question_or_below_c(self) -> None:
        mod = load_module()
        q = "Will the highest temperature in London be 2°C or below on February 14?"
        parsed = mod.parse_temperature_bucket_question(q)
        self.assertIsNotNone(parsed)
        assert parsed is not None
        self.assertEqual(parsed.city, "London")
        self.assertEqual(parsed.month, 2)
        self.assertEqual(parsed.day, 14)
        self.assertEqual(parsed.unit, "C")
        self.assertIsNone(parsed.low)
        self.assertEqual(parsed.high, 2)

    def test_parse_temperature_bucket_question_or_higher_f(self) -> None:
        mod = load_module()
        q = "Will the highest temperature in New York City be 50°F or higher on February 14?"
        parsed = mod.parse_temperature_bucket_question(q)
        self.assertIsNotNone(parsed)
        assert parsed is not None
        self.assertEqual(parsed.city, "New York City")
        self.assertEqual(parsed.month, 2)
        self.assertEqual(parsed.day, 14)
        self.assertEqual(parsed.unit, "F")
        self.assertEqual(parsed.low, 50)
        self.assertIsNone(parsed.high)

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

    def test_bucket_probability_handles_exact_and_unbounded(self) -> None:
        mod = load_module()
        p_exact = mod.normal_bucket_probability(mu=5.0, sigma=0.01, low=5, high=5)
        self.assertGreater(p_exact, 0.999)
        p_below = mod.normal_bucket_probability(mu=-10.0, sigma=0.01, low=None, high=-10)
        self.assertGreater(p_below, 0.999)
        p_above = mod.normal_bucket_probability(mu=50.0, sigma=0.01, low=50, high=None)
        self.assertGreater(p_above, 0.999)


if __name__ == "__main__":
    unittest.main()
