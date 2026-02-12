import unittest

from tools.xau_autobot_readiness import (
    estimate_break_even_cost,
    readiness_verdict,
    total_return_from_gross,
)


class TestXauAutoBotReadiness(unittest.TestCase):
    def test_total_return_from_gross(self):
        gross = [0.01, -0.005, 0.02]
        total = total_return_from_gross(gross, cost_per_side=0.0)
        expected = (1.01 * 0.995 * 1.02) - 1.0
        self.assertAlmostEqual(total, expected, places=10)

    def test_estimate_break_even_cost(self):
        # (1 + 0.02 - 2c)^2 - 1 = 0 => c = 0.01
        gross = [0.02, 0.02]
        fn = lambda c: total_return_from_gross(gross, c)
        breakeven = estimate_break_even_cost(fn, lo=0.0, hi=0.02, iterations=40)
        self.assertAlmostEqual(breakeven, 0.01, places=4)

    def test_readiness_verdict(self):
        self.assertEqual(readiness_verdict(assumed_cost_side=0.0002, break_even_cost_side=0.0004), "GO")
        self.assertEqual(readiness_verdict(assumed_cost_side=0.00035, break_even_cost_side=0.0004), "CAUTION")
        self.assertEqual(readiness_verdict(assumed_cost_side=0.0005, break_even_cost_side=0.0004), "NO_GO")


if __name__ == "__main__":
    unittest.main()
