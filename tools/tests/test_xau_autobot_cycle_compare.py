import unittest

from tools.xau_autobot_cycle_compare import (
    build_cycle_command,
    parse_json_lines,
    parse_periods,
    pick_cycle_summary,
)


class TestXauAutoBotCycleCompare(unittest.TestCase):
    def test_parse_periods(self):
        self.assertEqual(parse_periods("45d, 60d ,90d"), ["45d", "60d", "90d"])
        self.assertEqual(parse_periods(""), [])

    def test_parse_json_lines_and_pick_summary(self):
        stdout = "\n".join(
            [
                '{"timestamp":"2026-02-12T13:00:00+00:00","period":"45d","interval":"5m","backtest":{"pf":1.2},"readiness":{"verdict":"GO"},"cost_guard":{"verdict":"GO"}}',
                '{"notified":true}',
            ]
        )
        rows = parse_json_lines(stdout)
        self.assertEqual(len(rows), 2)
        summary = pick_cycle_summary(rows)
        self.assertEqual(summary["period"], "45d")
        self.assertEqual(summary["readiness"]["verdict"], "GO")

    def test_build_cycle_command(self):
        cmd = build_cycle_command(
            python_exe="python3",
            ticker="GC=F",
            period="60d",
            interval="5m",
            cost_per_side=0.0002,
            top_k=8,
            assumed_cost_side=0.0002,
            spread_points=80.0,
            spread_grid="20,40,60",
            point=0.01,
            commission_roundtrip_pct=0.02,
            slippage_roundtrip_pct=0.01,
            reports_dir="data/reports",
            write_config="tools/configs/xau_autobot.tuned_auto_gc_m5_60d.json",
            write_summary="data/reports/xau_autobot_cycle_summary_60d.json",
            webhook="",
            webhook_fallbacks="",
            market_hours_only=True,
        )
        self.assertEqual(cmd[0], "python3")
        self.assertIn("tools/xau_autobot_cycle.py", cmd)
        self.assertIn("--period", cmd)
        self.assertIn("60d", cmd)
        self.assertIn("--market-hours-only", cmd)
        self.assertIn("--write-summary", cmd)
        self.assertIn("data/reports/xau_autobot_cycle_summary_60d.json", cmd)


if __name__ == "__main__":
    unittest.main()
