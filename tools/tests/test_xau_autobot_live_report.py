import unittest
from types import SimpleNamespace

from tools.xau_autobot_live_report import (
    aggregate_closed_positions,
    build_filter_diagnostics,
    should_notify_threshold,
    summarize_closed_positions,
    update_notify_state,
)


class TestXauAutoBotLiveReport(unittest.TestCase):
    def _deal(
        self,
        *,
        symbol="XAUUSD",
        magic=560070,
        comment="xau_autobot_tuned_auto",
        deal_type=0,
        entry=0,
        position_id=1,
        time=1700000000,
        profit=0.0,
        swap=0.0,
        commission=0.0,
        fee=0.0,
    ):
        return SimpleNamespace(
            symbol=symbol,
            magic=magic,
            comment=comment,
            type=deal_type,
            entry=entry,
            position_id=position_id,
            time=time,
            profit=profit,
            swap=swap,
            commission=commission,
            fee=fee,
        )

    def test_aggregate_closed_positions_sums_entry_and_exit_costs(self):
        deals = [
            self._deal(position_id=101, entry=0, time=1, commission=-1.0),
            self._deal(position_id=101, entry=1, time=2, profit=10.0, commission=-1.0),
            self._deal(position_id=202, entry=0, time=3, commission=-1.0),
            self._deal(position_id=202, entry=1, time=4, profit=-5.0, commission=-1.0),
        ]

        closed = aggregate_closed_positions(
            deals=deals,
            symbol="XAUUSD",
            magic=560070,
            comment_prefix="xau_autobot_tuned_auto",
        )

        self.assertEqual(len(closed), 2)
        self.assertAlmostEqual(closed[0]["net_profit"], 8.0, places=6)
        self.assertAlmostEqual(closed[1]["net_profit"], -7.0, places=6)

    def test_aggregate_closed_positions_filters_symbol_magic_and_comment(self):
        deals = [
            self._deal(position_id=1, entry=0, time=1, commission=-1.0),
            self._deal(position_id=1, entry=1, time=2, profit=3.0, commission=-1.0),
            self._deal(position_id=2, symbol="USDJPY", entry=1, time=3, profit=99.0),
            self._deal(position_id=3, magic=1, entry=1, time=4, profit=99.0),
            self._deal(position_id=4, comment="other", entry=1, time=5, profit=99.0),
        ]

        closed = aggregate_closed_positions(
            deals=deals,
            symbol="XAUUSD",
            magic=560070,
            comment_prefix="xau_autobot_tuned_auto",
        )

        self.assertEqual(len(closed), 1)
        self.assertEqual(closed[0]["position_id"], 1)

    def test_aggregate_closed_positions_accepts_truncated_entry_comment_prefix(self):
        deals = [
            self._deal(
                position_id=900,
                entry=0,
                time=10,
                commission=-1.0,
                magic=560072,
                comment="xau_autobot_tria",
            ),
            self._deal(position_id=900, entry=1, time=20, profit=6.0, commission=-1.0, magic=560072, comment="[tp]"),
        ]

        closed = aggregate_closed_positions(
            deals=deals,
            symbol="XAUUSD",
            magic=560072,
            comment_prefix="xau_autobot_trial_v2_20260222",
        )

        self.assertEqual(len(closed), 1)
        self.assertEqual(closed[0]["position_id"], 900)
        self.assertAlmostEqual(closed[0]["net_profit"], 4.0, places=6)

    def test_aggregate_closed_positions_accepts_exit_comment_mismatch(self):
        deals = [
            self._deal(position_id=500, entry=0, time=10, commission=-1.0, comment="xau_autobot_tuned_auto"),
            self._deal(position_id=500, entry=1, time=20, profit=5.0, commission=-1.0, comment="[sl]"),
        ]

        closed = aggregate_closed_positions(
            deals=deals,
            symbol="XAUUSD",
            magic=560070,
            comment_prefix="xau_autobot_tuned_auto",
        )

        self.assertEqual(len(closed), 1)
        self.assertEqual(closed[0]["position_id"], 500)
        self.assertAlmostEqual(closed[0]["net_profit"], 3.0, places=6)

    def test_aggregate_closed_positions_accepts_exit_magic_mismatch(self):
        deals = [
            self._deal(position_id=700, entry=0, time=10, commission=-1.0, magic=560070, comment="xau_autobot"),
            self._deal(position_id=700, entry=1, time=20, profit=8.0, commission=-1.0, magic=0, comment=""),
        ]

        closed = aggregate_closed_positions(
            deals=deals,
            symbol="XAUUSD",
            magic=560070,
            comment_prefix="xau_autobot",
        )

        self.assertEqual(len(closed), 1)
        self.assertEqual(closed[0]["position_id"], 700)
        self.assertAlmostEqual(closed[0]["net_profit"], 6.0, places=6)

    def test_summarize_closed_positions_builds_kpis(self):
        closed = [
            {"position_id": 1, "close_time": 1, "net_profit": 10.0},
            {"position_id": 2, "close_time": 2, "net_profit": -4.0},
            {"position_id": 3, "close_time": 3, "net_profit": 3.0},
        ]
        summary = summarize_closed_positions(closed)

        self.assertEqual(summary["closed_positions"], 3)
        self.assertAlmostEqual(summary["net_profit"], 9.0, places=6)
        self.assertAlmostEqual(summary["gross_profit"], 13.0, places=6)
        self.assertAlmostEqual(summary["gross_loss"], -4.0, places=6)
        self.assertAlmostEqual(summary["win_rate"], 2.0 / 3.0, places=8)
        self.assertAlmostEqual(summary["profit_factor"], 13.0 / 4.0, places=8)
        self.assertAlmostEqual(summary["max_drawdown_abs"], 4.0, places=6)

    def test_build_filter_diagnostics_counts_stages(self):
        deals = [
            self._deal(symbol="XAUUSD", magic=560070, comment="xau_autobot_tuned_auto", deal_type=0),
            self._deal(symbol="XAUUSD", magic=560070, comment="[tp]", deal_type=1),
            self._deal(symbol="XAUUSD", magic=999, comment="xau_autobot_tuned_auto", deal_type=0),
            self._deal(symbol="EURUSD", magic=560070, comment="xau_autobot_tuned_auto", deal_type=0),
        ]

        diagnostics = build_filter_diagnostics(
            deals=deals,
            symbol="XAUUSD",
            magic=560070,
            comment_prefix="xau_autobot_tuned_auto",
        )

        self.assertEqual(diagnostics["tradable_deals"], 4.0)
        self.assertEqual(diagnostics["after_symbol_filter"], 3.0)
        self.assertEqual(diagnostics["after_magic_filter"], 2.0)
        self.assertEqual(diagnostics["after_comment_prefix_filter"], 1.0)

    def test_build_filter_diagnostics_accepts_truncated_comment_prefix(self):
        deals = [
            self._deal(symbol="XAUUSD", magic=560072, comment="xau_autobot_tria", deal_type=0),
            self._deal(symbol="XAUUSD", magic=560072, comment="[tp]", deal_type=1),
        ]

        diagnostics = build_filter_diagnostics(
            deals=deals,
            symbol="XAUUSD",
            magic=560072,
            comment_prefix="xau_autobot_trial_v2_20260222",
        )

        self.assertEqual(diagnostics["after_symbol_filter"], 2.0)
        self.assertEqual(diagnostics["after_magic_filter"], 2.0)
        self.assertEqual(diagnostics["after_comment_prefix_filter"], 1.0)

    def test_should_notify_threshold_when_first_reach(self):
        state = {}
        self.assertTrue(should_notify_threshold(closed_positions=30.0, threshold=30, state=state))

    def test_should_not_notify_threshold_when_already_notified(self):
        state = {"threshold_notified": {"30": {"closed_positions": 30.0}}}
        self.assertFalse(should_notify_threshold(closed_positions=32.0, threshold=30, state=state))

    def test_should_not_notify_threshold_when_below_threshold(self):
        state = {}
        self.assertFalse(should_notify_threshold(closed_positions=29.0, threshold=30, state=state))

    def test_update_notify_state_records_threshold(self):
        updated = update_notify_state(
            state={},
            threshold=30,
            closed_positions=31.0,
            now_utc="2026-02-13T00:00:00+00:00",
        )
        self.assertIn("threshold_notified", updated)
        self.assertIn("30", updated["threshold_notified"])


if __name__ == "__main__":
    unittest.main()
