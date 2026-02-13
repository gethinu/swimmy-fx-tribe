import unittest
from types import SimpleNamespace

from tools.xau_autobot_live_report import (
    aggregate_closed_positions,
    build_filter_diagnostics,
    summarize_closed_positions,
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


if __name__ == "__main__":
    unittest.main()
