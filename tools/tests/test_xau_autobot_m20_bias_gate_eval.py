import unittest

from tools.xau_autobot_m20_bias_gate_eval import (
    apply_m45_bias_gate,
    classify_commander_mode,
    gate_active_with_flip_relax,
    is_flip_relax_active,
    is_flip_relax_opposite_entry,
    loss_streak_p95_from_returns,
    next_two_state_mode,
    percentile_nearest_rank,
    pf_active_from_window,
    select_gate_policy_for_state,
    should_start_bias_flip_cooldown,
    should_start_loss_cooldown,
    update_flip_relax_until,
)


class TestXauAutoBotM20BiasGateEval(unittest.TestCase):
    def test_classify_commander_mode(self):
        self.assertEqual(
            classify_commander_mode(has_state=False, regime_strength=0.0, trend_threshold=2.35, neutral_max_strength=2.05),
            "neutral",
        )
        self.assertEqual(
            classify_commander_mode(has_state=True, regime_strength=2.4, trend_threshold=2.35, neutral_max_strength=2.05),
            "trend",
        )
        self.assertEqual(
            classify_commander_mode(has_state=True, regime_strength=2.2, trend_threshold=2.35, neutral_max_strength=2.05),
            "reversion",
        )
        self.assertEqual(
            classify_commander_mode(has_state=True, regime_strength=2.0, trend_threshold=2.35, neutral_max_strength=2.05),
            "neutral",
        )

    def test_apply_m45_bias_gate_none(self):
        self.assertEqual(
            apply_m45_bias_gate(signal=1, bias=-1, gate_policy="none", neutral_policy="allow_all", gate_active=True),
            (1, False),
        )

    def test_apply_m45_bias_gate_block_opposite(self):
        self.assertEqual(
            apply_m45_bias_gate(
                signal=-1,
                bias=1,
                gate_policy="block_opposite",
                neutral_policy="allow_all",
                gate_active=True,
            ),
            (0, True),
        )
        self.assertEqual(
            apply_m45_bias_gate(signal=1, bias=1, gate_policy="block_opposite", neutral_policy="allow_all", gate_active=True),
            (1, False),
        )

    def test_apply_m45_bias_gate_neutral_policy(self):
        self.assertEqual(
            apply_m45_bias_gate(signal=1, bias=0, gate_policy="block_opposite", neutral_policy="allow_all", gate_active=True),
            (1, False),
        )
        self.assertEqual(
            apply_m45_bias_gate(signal=1, bias=0, gate_policy="block_opposite", neutral_policy="block_all", gate_active=True),
            (0, True),
        )

    def test_apply_m45_bias_gate_hard_lock(self):
        self.assertEqual(
            apply_m45_bias_gate(signal=1, bias=1, gate_policy="hard_lock", neutral_policy="allow_all", gate_active=True),
            (1, False),
        )
        self.assertEqual(
            apply_m45_bias_gate(signal=-1, bias=1, gate_policy="hard_lock", neutral_policy="allow_all", gate_active=True),
            (0, True),
        )

    def test_apply_m45_bias_gate_inactive_bypasses_restriction(self):
        self.assertEqual(
            apply_m45_bias_gate(
                signal=-1,
                bias=1,
                gate_policy="block_opposite",
                neutral_policy="allow_all",
                gate_active=False,
            ),
            (-1, False),
        )

    def test_should_start_bias_flip_cooldown(self):
        self.assertFalse(should_start_bias_flip_cooldown(prev_bias=None, current_bias=1))
        self.assertFalse(should_start_bias_flip_cooldown(prev_bias=1, current_bias=1))
        self.assertTrue(should_start_bias_flip_cooldown(prev_bias=1, current_bias=-1))
        self.assertTrue(should_start_bias_flip_cooldown(prev_bias=0, current_bias=1))

    def test_update_flip_relax_until(self):
        self.assertEqual(
            update_flip_relax_until(
                prev_bias=None,
                current_bias=1,
                current_index=10,
                flip_relax_m20_bars=3,
                relax_until_index=-1,
            ),
            -1,
        )
        self.assertEqual(
            update_flip_relax_until(
                prev_bias=1,
                current_bias=-1,
                current_index=10,
                flip_relax_m20_bars=3,
                relax_until_index=-1,
            ),
            13,
        )
        self.assertEqual(
            update_flip_relax_until(
                prev_bias=1,
                current_bias=-1,
                current_index=10,
                flip_relax_m20_bars=3,
                relax_until_index=15,
            ),
            15,
        )

    def test_is_flip_relax_active(self):
        self.assertTrue(is_flip_relax_active(current_index=10, relax_until_index=13))
        self.assertFalse(is_flip_relax_active(current_index=13, relax_until_index=13))

    def test_gate_active_with_flip_relax(self):
        self.assertTrue(
            gate_active_with_flip_relax(
                has_state=True,
                gate_policy="block_opposite",
                flip_relax_active=False,
            )
        )
        self.assertFalse(
            gate_active_with_flip_relax(
                has_state=True,
                gate_policy="block_opposite",
                flip_relax_active=True,
            )
        )
        self.assertFalse(
            gate_active_with_flip_relax(
                has_state=True,
                gate_policy="none",
                flip_relax_active=False,
            )
        )
        self.assertFalse(
            gate_active_with_flip_relax(
                has_state=False,
                gate_policy="block_opposite",
                flip_relax_active=False,
            )
        )

    def test_is_flip_relax_opposite_entry(self):
        self.assertTrue(is_flip_relax_opposite_entry(signal=-1, bias=1, in_flip_relax=True))
        self.assertFalse(is_flip_relax_opposite_entry(signal=1, bias=1, in_flip_relax=True))
        self.assertFalse(is_flip_relax_opposite_entry(signal=-1, bias=1, in_flip_relax=False))

    def test_should_start_loss_cooldown(self):
        self.assertTrue(should_start_loss_cooldown(exit_reason="sl", trade_return=-0.01))
        self.assertFalse(should_start_loss_cooldown(exit_reason="tp", trade_return=-0.01))
        self.assertFalse(should_start_loss_cooldown(exit_reason="reverse", trade_return=-0.01))
        self.assertFalse(should_start_loss_cooldown(exit_reason="sl", trade_return=0.01))

    def test_loss_streak_p95_from_returns(self):
        returns = [0.01, -0.01, -0.02, 0.02, -0.01, -0.01, -0.01, 0.03]
        self.assertEqual(loss_streak_p95_from_returns(returns), 3.0)

    def test_pf_active_from_window(self):
        self.assertIsNone(pf_active_from_window(closed=11.0, pf=1.2, closed_floor=12.0))
        self.assertEqual(pf_active_from_window(closed=12.0, pf=1.2, closed_floor=12.0), 1.2)

    def test_percentile_nearest_rank(self):
        values = [10.0, 20.0, 30.0, 40.0, 50.0]
        self.assertEqual(percentile_nearest_rank(values, 0.05), 10.0)
        self.assertEqual(percentile_nearest_rank(values, 0.5), 30.0)

    def test_next_two_state_mode_hysteresis(self):
        self.assertEqual(
            next_two_state_mode(prev_state="non_trend", regime_strength=2.34, trend_on=2.35, trend_off=2.25),
            "non_trend",
        )
        self.assertEqual(
            next_two_state_mode(prev_state="non_trend", regime_strength=2.35, trend_on=2.35, trend_off=2.25),
            "trend",
        )
        self.assertEqual(
            next_two_state_mode(prev_state="trend", regime_strength=2.30, trend_on=2.35, trend_off=2.25),
            "trend",
        )
        self.assertEqual(
            next_two_state_mode(prev_state="trend", regime_strength=2.25, trend_on=2.35, trend_off=2.25),
            "non_trend",
        )

    def test_select_gate_policy_for_state(self):
        self.assertEqual(select_gate_policy_for_state("trend"), "block_opposite")
        self.assertEqual(select_gate_policy_for_state("non_trend"), "none")


if __name__ == "__main__":
    unittest.main()
