import unittest

from tools import polymarket_openclaw_autotune as tune


class TestPolymarketOpenclawAutotune(unittest.TestCase):
    def test_parse_grid(self) -> None:
        values = tune.parse_grid("0.01, 0.02,0.03")
        self.assertEqual([0.01, 0.02, 0.03], values)

    def test_simulate_params(self) -> None:
        records = [
            {
                "type": "entry",
                "market_id": "m1",
                "date": "2026-02-12",
                "side": "YES",
                "entry_price": 0.5,
                "stake_usd": 10.0,
                "edge": 0.03,
                "expected_value_usd": 1.0,
            },
            {
                "type": "entry",
                "market_id": "m2",
                "date": "2026-02-12",
                "side": "NO",
                "entry_price": 0.4,
                "stake_usd": 10.0,
                "edge": 0.01,
                "expected_value_usd": 0.2,
            },
        ]
        settlements = {"m1": "YES", "m2": "YES"}
        result = tune.simulate_params(
            records=records,
            settlements=settlements,
            min_edge=0.02,
            kelly_scale=0.4,
            base_kelly_scale=0.4,
            fee_bps_per_side=0.0,
            slippage_bps_per_side=0.0,
            min_trades=1,
        )
        self.assertEqual(1, result["trades"])
        self.assertAlmostEqual(10.0, result["stake_usd"])
        self.assertAlmostEqual(10.0, result["realized_pnl_usd"])

    def test_grid_search_returns_best_candidate(self) -> None:
        records = [
            {
                "type": "entry",
                "market_id": "m1",
                "date": "2026-02-12",
                "side": "YES",
                "entry_price": 0.5,
                "stake_usd": 10.0,
                "edge": 0.03,
                "expected_value_usd": 1.0,
            },
            {
                "type": "entry",
                "market_id": "m2",
                "date": "2026-02-12",
                "side": "NO",
                "entry_price": 0.4,
                "stake_usd": 10.0,
                "edge": 0.01,
                "expected_value_usd": 0.2,
            },
        ]
        settlements = {"m1": "YES", "m2": "YES"}
        best, leaderboard = tune.grid_search(
            records=records,
            settlements=settlements,
            edge_grid=[0.01, 0.02],
            kelly_grid=[0.2, 0.4],
            base_kelly_scale=0.4,
            fee_bps_per_side=0.0,
            slippage_bps_per_side=0.0,
            min_trades=1,
        )
        self.assertIsNotNone(best)
        assert best is not None
        self.assertEqual(0.02, best["min_edge"])
        self.assertEqual(0.4, best["kelly_scale"])
        self.assertGreaterEqual(len(leaderboard), 1)


if __name__ == "__main__":
    unittest.main()
