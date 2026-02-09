import unittest

from tools.legend_gate_compare import sma_series, rsi_series, model_gate_predict


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


if __name__ == "__main__":
    unittest.main()
