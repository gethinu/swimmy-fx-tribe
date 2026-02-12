import unittest

from tools.xau_autobot_windows_probe import (
    compute_spread_points,
    evaluate_spread,
    percentile,
    summarize_spreads,
)


class TestXauAutoBotWindowsProbe(unittest.TestCase):
    def test_compute_spread_points(self):
        self.assertAlmostEqual(compute_spread_points(ask=2000.2, bid=2000.0, point=0.01), 20.0, places=8)

    def test_percentile(self):
        values = [1.0, 2.0, 3.0, 4.0, 5.0]
        self.assertAlmostEqual(percentile(values, 50.0), 3.0, places=8)
        self.assertAlmostEqual(percentile(values, 80.0), 4.2, places=8)

    def test_summarize_spreads(self):
        values = [10.0, 20.0, 30.0, 40.0, 50.0]
        s = summarize_spreads(values)
        self.assertEqual(s["count"], 5)
        self.assertAlmostEqual(s["median"], 30.0, places=8)
        self.assertAlmostEqual(s["p95"], 48.0, places=8)
        self.assertAlmostEqual(s["max"], 50.0, places=8)

    def test_evaluate_spread(self):
        result = evaluate_spread(
            spread_points=80.0,
            price=5000.0,
            point=0.01,
            break_even_roundtrip_pct=0.08,
            commission_roundtrip_pct=0.02,
            slippage_roundtrip_pct=0.01,
            safety_margin=0.8,
        )
        self.assertEqual(result["verdict"], "GO")
        self.assertTrue(result["safety_pass"])


if __name__ == "__main__":
    unittest.main()
