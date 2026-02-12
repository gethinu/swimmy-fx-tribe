import unittest

from tools.xau_autobot_cost_guard import (
    build_spread_scenarios,
    effective_roundtrip_cost,
    required_max_spread_points,
    required_max_spread_points_for_ratio,
    roundtrip_cost_verdict,
)


class TestXauAutoBotCostGuard(unittest.TestCase):
    def test_effective_roundtrip_cost(self):
        # spread_points=20, point=0.01, price=2000 => spread_pct = 0.01%
        cost = effective_roundtrip_cost(
            spread_points=20.0,
            price=2000.0,
            point=0.01,
            commission_roundtrip_pct=0.02,
            slippage_roundtrip_pct=0.01,
        )
        self.assertAlmostEqual(cost, 0.04, places=8)

    def test_required_max_spread_points(self):
        # budget = 0.08% * 0.8 - 0.02% = 0.044%
        # max points = 0.044% * 2000 / 0.01 = 88
        points = required_max_spread_points(
            break_even_roundtrip_pct=0.08,
            price=2000.0,
            point=0.01,
            commission_roundtrip_pct=0.02,
            slippage_roundtrip_pct=0.0,
            safety_margin=0.8,
        )
        self.assertAlmostEqual(points, 88.0, places=6)

    def test_roundtrip_cost_verdict(self):
        self.assertEqual(roundtrip_cost_verdict(effective_roundtrip_pct=0.03, break_even_roundtrip_pct=0.08), "GO")
        self.assertEqual(roundtrip_cost_verdict(effective_roundtrip_pct=0.06, break_even_roundtrip_pct=0.08), "CAUTION")
        self.assertEqual(roundtrip_cost_verdict(effective_roundtrip_pct=0.09, break_even_roundtrip_pct=0.08), "NO_GO")

    def test_required_max_spread_points_for_ratio(self):
        points = required_max_spread_points_for_ratio(
            break_even_roundtrip_pct=0.08,
            ratio_limit=0.7,
            price=2000.0,
            point=0.01,
            commission_roundtrip_pct=0.02,
            slippage_roundtrip_pct=0.01,
        )
        self.assertAlmostEqual(points, 52.0, places=6)

    def test_build_spread_scenarios(self):
        scenarios = build_spread_scenarios(
            spread_points_values=[50.0, 80.0, 150.0],
            price=2000.0,
            point=0.01,
            break_even_roundtrip_pct=0.08,
            commission_roundtrip_pct=0.02,
            slippage_roundtrip_pct=0.01,
            safety_margin=0.8,
        )
        self.assertEqual(scenarios[0]["verdict"], "GO")
        self.assertEqual(scenarios[1]["verdict"], "CAUTION")
        self.assertEqual(scenarios[2]["verdict"], "NO_GO")


if __name__ == "__main__":
    unittest.main()
