import unittest

from tools.legend_gate_compare import sma_series, rsi_series, model_gate_predict
from tools.legend_gate_compare import simulate_trades, compute_metrics, daily_returns_from_trades, sharpe_ratio


class TestIndicators(unittest.TestCase):
    def test_sma_basic(self):
        values = [1, 2, 3, 4, 5]
        sma = sma_series(values, 3)
        self.assertIsNone(sma[0])
        self.assertIsNone(sma[1])
        self.assertAlmostEqual(sma[2], 2.0)

    def test_rsi_basic(self):
        values = [1, 2, 3, 4, 5, 6, 7]
        rsi = rsi_series(values, 2)
        self.assertIsNotNone(rsi[-1])

    def test_model_gate_shape(self):
        closes = [1.0 + i * 0.01 for i in range(60)]
        preds = model_gate_predict(closes)
        self.assertEqual(len(preds), len(closes))

    def test_model_gate_has_buy_in_uptrend(self):
        closes = [1.0 + i * 0.01 for i in range(80)]
        preds = model_gate_predict(closes)
        self.assertIn("BUY", preds)


class TestSimulator(unittest.TestCase):
    def test_sl_tp_priority(self):
        # One bar after entry hits both SL and TP; SL should win.
        opens = [100.0, 100.0]
        highs = [100.0, 101.0]
        lows = [100.0, 99.0]
        closes = [100.0, 100.5]
        trades = simulate_trades(
            opens,
            highs,
            lows,
            closes,
            entries=[0],
            exits=[],
            sl=0.5,
            tp=0.5,
            slippage=0.0,
        )
        self.assertEqual(len(trades), 1)
        self.assertLess(trades[0][2], 0.0)


class TestMetrics(unittest.TestCase):
    def test_compute_metrics_basic(self):
        trade_returns = [0.1, -0.05, 0.05]
        daily_returns = [0.01, -0.005, 0.0]
        metrics = compute_metrics(trade_returns, daily_returns=daily_returns)
        self.assertEqual(metrics["trades"], 3)
        self.assertAlmostEqual(metrics["win"], 2 / 3)
        self.assertAlmostEqual(metrics["pf"], 3.0)
        self.assertAlmostEqual(metrics["sharpe"], sharpe_ratio(daily_returns), places=6)

    def test_daily_returns_from_trades_groups_by_day(self):
        timestamps = [0, 3600, 90000]  # day 0, day 0, day 1
        trades = [(0, 1, 0.1), (1, 2, -0.05)]
        daily = daily_returns_from_trades(timestamps, trades)
        self.assertEqual(len(daily), 2)
        self.assertAlmostEqual(daily[0], 0.1)
        self.assertAlmostEqual(daily[1], -0.05)

    def test_daily_returns_from_trades_compounds_within_day(self):
        timestamps = [0, 3600, 7200]  # all day 0
        trades = [(0, 1, 0.1), (1, 2, 0.1)]
        daily = daily_returns_from_trades(timestamps, trades)
        self.assertEqual(len(daily), 1)
        self.assertAlmostEqual(daily[0], 0.21)


if __name__ == "__main__":
    unittest.main()
