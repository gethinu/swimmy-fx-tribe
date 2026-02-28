import unittest

from tools.xau_autobot_regime_switch_eval import (
    mode_from_strength_with_hysteresis,
    m45_bias_from_state,
    window_pass_reasons,
)


class TestXauAutoBotRegimeSwitchEval(unittest.TestCase):
    def test_mode_from_strength_with_hysteresis(self):
        self.assertEqual(mode_from_strength_with_hysteresis(None, strength=2.40, high=2.35, low=2.05), "trend")
        self.assertEqual(mode_from_strength_with_hysteresis(None, strength=2.00, high=2.35, low=2.05), "reversion")
        self.assertEqual(
            mode_from_strength_with_hysteresis("trend", strength=2.20, high=2.35, low=2.05),
            "trend",
        )
        self.assertEqual(
            mode_from_strength_with_hysteresis("reversion", strength=2.20, high=2.35, low=2.05),
            "reversion",
        )
        self.assertEqual(
            mode_from_strength_with_hysteresis("trend", strength=2.00, high=2.35, low=2.05),
            "reversion",
        )

    def test_m45_bias_from_state(self):
        self.assertEqual(m45_bias_from_state(ema_fast=10.0, ema_slow=9.0, regime_strength=2.5, min_strength=2.4), 1)
        self.assertEqual(m45_bias_from_state(ema_fast=9.0, ema_slow=10.0, regime_strength=2.5, min_strength=2.4), -1)
        self.assertEqual(m45_bias_from_state(ema_fast=10.0, ema_slow=9.0, regime_strength=2.3, min_strength=2.4), 0)

    def test_window_pass_reasons(self):
        self.assertEqual(window_pass_reasons(closed=12.0, pf=1.10, max_dd=0.07, closed_floor=12, pf_floor=1.10, dd_ceiling=0.07), [])
        self.assertEqual(
            window_pass_reasons(closed=11.0, pf=1.20, max_dd=0.05, closed_floor=12, pf_floor=1.10, dd_ceiling=0.07),
            ["closed_lt_floor"],
        )
        self.assertEqual(
            window_pass_reasons(closed=20.0, pf=1.00, max_dd=0.08, closed_floor=12, pf_floor=1.10, dd_ceiling=0.07),
            ["pf_lt_floor", "dd_gt_ceiling"],
        )


if __name__ == "__main__":
    unittest.main()
