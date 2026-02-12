from datetime import datetime, timezone
import unittest
from unittest import mock

from tools.xau_autobot_cycle import (
    build_discord_headers,
    build_cycle_summary,
    build_optimize_command,
    dispatch_discord_notification,
    is_xau_market_open,
    parse_json_lines,
    pick_backtest_summary,
    pick_optimize_best,
)


class TestXauAutoBotCycle(unittest.TestCase):
    def test_build_discord_headers_contains_user_agent(self):
        headers = build_discord_headers()
        self.assertIn("Content-Type", headers)
        self.assertIn("User-Agent", headers)
        self.assertTrue(headers["User-Agent"])

    def test_parse_json_lines_skips_blanks(self):
        payload = '{"a": 1}\n\n{"b": 2}\n'
        rows = parse_json_lines(payload)
        self.assertEqual(len(rows), 2)
        self.assertEqual(rows[0]["a"], 1)
        self.assertEqual(rows[1]["b"], 2)

    def test_pick_backtest_summary(self):
        rows = [
            {"preset": "base", "segment": "all", "pf": 0.8},
            {"preset": "tuned", "segment": "split_0.5_is", "pf": 1.0},
            {"preset": "tuned", "segment": "all", "pf": 1.23, "total_return": 0.06},
        ]
        out = pick_backtest_summary(rows, preset="tuned", segment="all")
        self.assertEqual(out["pf"], 1.23)
        self.assertAlmostEqual(out["total_return"], 0.06)

    def test_pick_optimize_best_rank1(self):
        rows = [
            {"rank": 2, "score": 1.2},
            {"rank": 1, "score": 1.6, "candidate": {"fast_ema": 28}},
            {"rank": 3, "score": 1.1},
        ]
        best = pick_optimize_best(rows)
        self.assertEqual(best["rank"], 1)
        self.assertEqual(best["candidate"]["fast_ema"], 28)

    def test_build_optimize_command(self):
        cmd = build_optimize_command(
            python_exe="./.venv/bin/python",
            ticker="GC=F",
            period="90d",
            interval="5m",
            cost_per_side=0.0002,
            top_k=8,
            write_config="tools/configs/xau_autobot.tuned_auto_gc_m5_90d.json",
        )
        self.assertEqual(cmd[0], "./.venv/bin/python")
        self.assertIn("tools/xau_autobot_optimize.py", cmd)
        self.assertIn("--period", cmd)
        self.assertIn("90d", cmd)
        self.assertIn("--write-config", cmd)

    def test_build_cycle_summary(self):
        summary = build_cycle_summary(
            timestamp="2026-02-12T19:00:00Z",
            period="90d",
            interval="5m",
            tuned_config_path="tools/configs/xau_autobot.tuned_auto_gc_m5_90d.json",
            backtest={"pf": 1.25, "total_return": 0.08},
            optimize_best={"rank": 1, "score": 1.7},
            readiness={"verdict": "GO", "break_even_roundtrip_cost": 0.0007},
            cost_guard={"verdict": "GO", "max_spread_points_go": 100.0},
            report_paths={"readiness": "data/reports/a.json", "cost_guard": "data/reports/b.json"},
        )
        self.assertEqual(summary["period"], "90d")
        self.assertEqual(summary["readiness"]["verdict"], "GO")
        self.assertEqual(summary["cost_guard"]["verdict"], "GO")
        self.assertEqual(summary["reports"]["cost_guard"], "data/reports/b.json")

    def test_dispatch_discord_notification_soft_fail(self):
        with mock.patch("tools.xau_autobot_cycle._post_discord", side_effect=RuntimeError("boom")):
            result = dispatch_discord_notification(["https://example.invalid/webhook"], {}, strict=False)
        self.assertFalse(result["notified"])
        self.assertIn("boom", result["error"])

    def test_dispatch_discord_notification_strict(self):
        with mock.patch("tools.xau_autobot_cycle._post_discord", side_effect=RuntimeError("boom")):
            with self.assertRaises(RuntimeError):
                dispatch_discord_notification(["https://example.invalid/webhook"], {}, strict=True)

    def test_dispatch_discord_notification_fallback_success(self):
        calls = []

        def _fake_post(url, _summary):
            calls.append(url)
            if "primary" in url:
                raise RuntimeError("403")

        with mock.patch("tools.xau_autobot_cycle._post_discord", side_effect=_fake_post):
            result = dispatch_discord_notification(
                ["https://primary.invalid/webhook", "https://fallback.invalid/webhook"],
                {},
                strict=False,
            )
        self.assertTrue(result["notified"])
        self.assertEqual(calls, ["https://primary.invalid/webhook", "https://fallback.invalid/webhook"])
        self.assertEqual(result["used_webhook"], "https://fallback.invalid/webhook")

    def test_is_xau_market_open(self):
        self.assertFalse(is_xau_market_open(datetime(2026, 2, 14, 12, 0, tzinfo=timezone.utc)))  # Sat
        self.assertFalse(is_xau_market_open(datetime(2026, 2, 15, 21, 0, tzinfo=timezone.utc)))  # Sun pre-open
        self.assertTrue(is_xau_market_open(datetime(2026, 2, 15, 22, 0, tzinfo=timezone.utc)))   # Sun open
        self.assertTrue(is_xau_market_open(datetime(2026, 2, 13, 21, 0, tzinfo=timezone.utc)))   # Fri pre-close
        self.assertFalse(is_xau_market_open(datetime(2026, 2, 13, 22, 0, tzinfo=timezone.utc)))  # Fri close


if __name__ == "__main__":
    unittest.main()
