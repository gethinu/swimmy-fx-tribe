import unittest
from datetime import datetime, timezone

from tools.xau_autobot_cost_guard import (
    _fetch_latest_price,
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

    def test_fetch_latest_price_falls_back_intervals(self):
        calls = []

        def fake_load_ohlc(*, ticker: str, period: str, interval: str):
            calls.append((ticker, period, interval))
            if period == "1d":
                raise RuntimeError("no data yet")
            return (
                [datetime(2026, 2, 12, tzinfo=timezone.utc)],
                [0.0],
                [0.0],
                [0.0],
                [2860.25],
            )

        price = _fetch_latest_price("GC=F", load_ohlc_fn=fake_load_ohlc)
        self.assertEqual(price, 2860.25)
        self.assertEqual(
            calls,
            [("GC=F", "1d", "1m"), ("GC=F", "5d", "5m")],
        )

    def test_fetch_latest_price_raises_when_all_attempts_fail(self):
        def always_fail(*, ticker: str, period: str, interval: str):
            raise RuntimeError(f"missing {ticker} {period} {interval}")

        with self.assertRaisesRegex(RuntimeError, "could not fetch price for GC=F"):
            _fetch_latest_price("GC=F", load_ohlc_fn=always_fail)


if __name__ == "__main__":
    unittest.main()
