import unittest

from tools.xau_autobot import (
    BotConfig,
    atr_last,
    atr_pct_series,
    build_sl_tp,
    can_open_trade,
    decide_signal,
    ema_last,
    is_session_allowed,
    volatility_filter_pass,
)


class TestXauAutoBotMath(unittest.TestCase):
    def test_ema_last_calculation(self):
        values = [1.0, 2.0, 3.0, 4.0, 5.0]
        self.assertAlmostEqual(ema_last(values, period=3), 4.0625, places=6)

    def test_atr_last_calculation(self):
        highs = [10.0, 11.0, 12.0]
        lows = [9.0, 10.0, 11.0]
        closes = [9.5, 10.5, 11.5]
        self.assertAlmostEqual(atr_last(highs, lows, closes, period=3), 4.0 / 3.0, places=6)

    def test_decide_signal_buy_on_uptrend_pullback(self):
        side = decide_signal(
            last_close=1980.0,
            ema_fast=1982.0,
            ema_slow=1978.0,
            atr_value=2.0,
            pullback_atr=0.5,
        )
        self.assertEqual(side, "BUY")

    def test_decide_signal_sell_on_downtrend_pullback(self):
        side = decide_signal(
            last_close=2000.0,
            ema_fast=1998.0,
            ema_slow=2002.0,
            atr_value=2.0,
            pullback_atr=0.5,
        )
        self.assertEqual(side, "SELL")

    def test_decide_signal_hold_when_not_pulled_back(self):
        side = decide_signal(
            last_close=1982.5,
            ema_fast=1982.0,
            ema_slow=1978.0,
            atr_value=2.0,
            pullback_atr=0.5,
        )
        self.assertEqual(side, "HOLD")

    def test_build_sl_tp_buy_and_sell(self):
        buy_sl, buy_tp = build_sl_tp(
            side="BUY",
            entry_price=2000.0,
            atr_value=2.0,
            sl_atr=1.5,
            tp_atr=2.0,
        )
        self.assertAlmostEqual(buy_sl, 1997.0, places=6)
        self.assertAlmostEqual(buy_tp, 2004.0, places=6)

        sell_sl, sell_tp = build_sl_tp(
            side="SELL",
            entry_price=2000.0,
            atr_value=2.0,
            sl_atr=1.5,
            tp_atr=2.0,
        )
        self.assertAlmostEqual(sell_sl, 2003.0, places=6)
        self.assertAlmostEqual(sell_tp, 1996.0, places=6)

    def test_can_open_trade_respects_limits(self):
        self.assertTrue(
            can_open_trade(spread_points=15.0, max_spread_points=20.0, open_positions=0, max_positions=1)
        )
        self.assertFalse(
            can_open_trade(spread_points=25.0, max_spread_points=20.0, open_positions=0, max_positions=1)
        )
        self.assertFalse(
            can_open_trade(spread_points=10.0, max_spread_points=20.0, open_positions=1, max_positions=1)
        )

    def test_is_session_allowed_for_standard_day_window(self):
        self.assertTrue(is_session_allowed(hour_utc=7, session_start=7, session_end=19))
        self.assertTrue(is_session_allowed(hour_utc=19, session_start=7, session_end=19))
        self.assertFalse(is_session_allowed(hour_utc=6, session_start=7, session_end=19))
        self.assertFalse(is_session_allowed(hour_utc=21, session_start=7, session_end=19))

    def test_is_session_allowed_for_wraparound_window(self):
        self.assertTrue(is_session_allowed(hour_utc=23, session_start=22, session_end=3))
        self.assertTrue(is_session_allowed(hour_utc=2, session_start=22, session_end=3))
        self.assertFalse(is_session_allowed(hour_utc=12, session_start=22, session_end=3))

    def test_volatility_filter_passes_when_disabled(self):
        self.assertTrue(
            volatility_filter_pass(
                atr_pct_values=[0.001, 0.0011, 0.0009],
                min_ratio_to_median=0.0,
                max_ratio_to_median=999.0,
                window=3,
                min_samples=3,
            )
        )

    def test_volatility_filter_respects_median_band(self):
        values = [0.0010, 0.0011, 0.0012, 0.0013, 0.0014]
        self.assertTrue(
            volatility_filter_pass(
                atr_pct_values=values,
                min_ratio_to_median=0.8,
                max_ratio_to_median=1.4,
                window=5,
                min_samples=5,
            )
        )
        self.assertFalse(
            volatility_filter_pass(
                atr_pct_values=values[:-1] + [0.0025],
                min_ratio_to_median=0.8,
                max_ratio_to_median=1.4,
                window=5,
                min_samples=5,
            )
        )

    def test_atr_pct_series(self):
        atr_values = [2.0, 1.0, 0.0]
        close_values = [2000.0, 1000.0, 500.0]
        result = atr_pct_series(atr_values, close_values)
        self.assertAlmostEqual(result[0], 0.001, places=8)
        self.assertAlmostEqual(result[1], 0.001, places=8)
        self.assertAlmostEqual(result[2], 0.0, places=8)


class TestXauAutoBotConfig(unittest.TestCase):
    def test_config_defaults(self):
        cfg = BotConfig.from_dict({})
        self.assertEqual(cfg.symbol, "XAUUSD")
        self.assertEqual(cfg.timeframe, "M5")
        self.assertEqual(cfg.max_positions, 1)
        self.assertGreater(cfg.max_spread_points, 0.0)


if __name__ == "__main__":
    unittest.main()
