import json
import tempfile
import unittest
from pathlib import Path

from tools import legend_mt5_evidence_bridge as bridge


def _write_json(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


class TestLegendMt5EvidenceBridge(unittest.TestCase):
    def test_build_bridge_report_keeps_only_canonical_shortlist_candidates(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            manifest = root / "inventory" / "inventory_tester_manifest.json"
            walk_root = root / "walkforward"
            _write_json(
                manifest,
                [
                    {
                        "job": {
                            "slug": "legend-macd-above-zero-cross",
                            "job_set": "legend",
                            "expert": "Legend_MACDAboveZeroCross.ex5",
                            "source_rel": "src/mt5/legend_batch2/Legend_MACDAboveZeroCross.mq5",
                            "symbol": "USDJPY",
                            "period": "H4",
                            "notes": "",
                        },
                        "run_id": "run_a",
                        "from_date": "2025.01.01",
                        "to_date": "2025.03.01",
                        "summary": {
                            "profit_factor": "2.98",
                            "sharpe_ratio": "3.19",
                            "total_trades": "10",
                            "total_net_profit": "12.93",
                        },
                    },
                    {
                        "job": {
                            "slug": "legend-perfect-order-sma",
                            "job_set": "legend",
                            "expert": "Legend_PerfectOrderSMA.ex5",
                            "source_rel": "src/mt5/legend_batch1/Legend_PerfectOrderSMA.mq5",
                            "symbol": "USDJPY",
                            "period": "M30",
                            "notes": "",
                        },
                        "run_id": "run_a",
                        "from_date": "2025.01.01",
                        "to_date": "2025.03.01",
                        "summary": {
                            "profit_factor": "0.96",
                            "sharpe_ratio": "-1.20",
                            "total_trades": "8",
                            "total_net_profit": "-5.34",
                        },
                    },
                ],
            )
            _write_json(
                walk_root / "legend-macd-above-zero-cross" / "walkforward_summary.json",
                {
                    "job": "legend-macd-above-zero-cross",
                    "retune_profile": "wide",
                    "folds_total": 3,
                    "folds_passing": 3,
                    "aggregate": {
                        "forward_profit_total": 116.34,
                        "forward_pf_mean": 2.9664,
                        "forward_sharpe_mean": 4.7661,
                        "forward_trades_total": 46,
                    },
                },
            )

            report = bridge.build_bridge_report(
                inventory_manifests=[manifest],
                walkforward_roots=[walk_root],
            )

        self.assertEqual(["legend-macd-above-zero-cross"], report["shortlist"])
        self.assertEqual(["legend-macd-above-zero-cross"], [entry["strategy_name"] for entry in report["strategies"]])

    def test_build_bridge_report_aggregates_multiple_inventory_manifests(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            manifest_a = root / "inventory_a" / "inventory_tester_manifest.json"
            manifest_b = root / "inventory_b" / "inventory_tester_manifest.json"
            walk_root = root / "walkforward_wide"

            _write_json(
                manifest_a,
                [
                    {
                        "job": {
                            "slug": "legend-macd-above-zero-cross",
                            "job_set": "legend",
                            "expert": "Legend_MACDAboveZeroCross.ex5",
                            "source_rel": "src/mt5/legend_batch2/Legend_MACDAboveZeroCross.mq5",
                            "symbol": "USDJPY",
                            "period": "H4",
                            "notes": "",
                        },
                        "run_id": "run_20260307_105536",
                        "from_date": "2024.01.01",
                        "to_date": "2024.06.28",
                        "summary": {
                            "profit_factor": "1.80",
                            "sharpe_ratio": "3.25",
                            "total_trades": "20",
                            "total_net_profit": "16.13",
                        },
                    }
                ],
            )
            _write_json(
                manifest_b,
                [
                    {
                        "job": {
                            "slug": "legend-macd-above-zero-cross",
                            "job_set": "legend",
                            "expert": "Legend_MACDAboveZeroCross.ex5",
                            "source_rel": "src/mt5/legend_batch2/Legend_MACDAboveZeroCross.mq5",
                            "symbol": "USDJPY",
                            "period": "H4",
                            "notes": "",
                        },
                        "run_id": "run_20260307_105948",
                        "from_date": "2025.06.24",
                        "to_date": "2025.12.20",
                        "summary": {
                            "profit_factor": "1.34",
                            "sharpe_ratio": "1.10",
                            "total_trades": "12",
                            "total_net_profit": "5.20",
                        },
                    }
                ],
            )
            _write_json(
                walk_root / "legend-macd-above-zero-cross" / "walkforward_summary.json",
                {
                    "job": "legend-macd-above-zero-cross",
                    "retune_profile": "wide",
                    "folds_total": 3,
                    "folds_passing": 3,
                    "aggregate": {
                        "forward_profit_total": 116.34,
                        "forward_pf_mean": 2.9664,
                        "forward_sharpe_mean": 4.7661,
                        "forward_trades_total": 46,
                    },
                },
            )

            report = bridge.build_bridge_report(
                inventory_manifests=[manifest_a, manifest_b],
                walkforward_roots=[walk_root],
            )

        strategy = report["strategies"][0]
        inventory = strategy["inventory_validation"]
        self.assertEqual(2, inventory["windows_total"])
        self.assertEqual(2, inventory["positive_windows"])
        self.assertEqual("run_20260307_105948", inventory["latest_run_id"])
        self.assertEqual("2025.06.24", inventory["latest_window"]["from_date"])
        self.assertEqual("2025.12.20", inventory["latest_window"]["to_date"])

    def test_build_bridge_report_prefers_canonical_profiles_and_shortlist_order(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            inventory_manifest = root / "inventory" / "inventory_tester_manifest.json"
            _write_json(
                inventory_manifest,
                [
                    {
                        "job": {
                            "slug": "legend-macd-above-zero-cross",
                            "job_set": "legend",
                            "expert": "Legend_MACDAboveZeroCross.ex5",
                            "source_rel": "src/mt5/legend_batch2/Legend_MACDAboveZeroCross.mq5",
                            "symbol": "USDJPY",
                            "period": "H4",
                            "notes": "",
                        },
                        "run_id": "run_inventory_1",
                        "from_date": "2024.12.26",
                        "to_date": "2025.06.23",
                        "summary": {
                            "profit_factor": "1.14",
                            "sharpe_ratio": "2.84",
                            "total_trades": "27",
                            "total_net_profit": "4.12",
                        },
                    },
                    {
                        "job": {
                            "slug": "historical-s-bred940-trend-core",
                            "job_set": "historical_s",
                            "expert": "HistS_Bred940TrendCore.ex5",
                            "source_rel": "src/mt5/historical_s_batch3/HistS_Bred940TrendCore.mq5",
                            "symbol": "USDJPY",
                            "period": "H1",
                            "notes": "",
                        },
                        "run_id": "run_inventory_1",
                        "from_date": "2024.12.26",
                        "to_date": "2025.06.23",
                        "summary": {
                            "profit_factor": "0.89",
                            "sharpe_ratio": "-0.62",
                            "total_trades": "66",
                            "total_net_profit": "-32.42",
                        },
                    },
                    {
                        "job": {
                            "slug": "legend-pullback-breakout",
                            "job_set": "legend",
                            "expert": "Legend_PullbackBreakout.ex5",
                            "source_rel": "src/mt5/legend_batch1/Legend_PullbackBreakout.mq5",
                            "symbol": "USDJPY",
                            "period": "H1",
                            "notes": "",
                        },
                        "run_id": "run_inventory_1",
                        "from_date": "2024.12.26",
                        "to_date": "2025.06.23",
                        "summary": {
                            "profit_factor": "1.28",
                            "sharpe_ratio": "4.43",
                            "total_trades": "240",
                            "total_net_profit": "63.54",
                        },
                    },
                ],
            )

            narrow_root = root / "walkforward_narrow"
            wide_root = root / "walkforward_wide"
            medium_root = root / "walkforward_medium"

            _write_json(
                narrow_root / "legend-macd-above-zero-cross" / "walkforward_summary.json",
                {
                    "job": "legend-macd-above-zero-cross",
                    "folds_total": 3,
                    "folds_passing": 3,
                    "aggregate": {
                        "forward_profit_total": 53.84,
                        "forward_pf_mean": 2.2692,
                        "forward_sharpe_mean": 4.0689,
                        "forward_trades_total": 47,
                    },
                },
            )
            _write_json(
                wide_root / "legend-macd-above-zero-cross" / "walkforward_summary.json",
                {
                    "job": "legend-macd-above-zero-cross",
                    "retune_profile": "wide",
                    "folds_total": 3,
                    "folds_passing": 3,
                    "aggregate": {
                        "forward_profit_total": 116.34,
                        "forward_pf_mean": 2.9664,
                        "forward_sharpe_mean": 4.7661,
                        "forward_trades_total": 46,
                    },
                },
            )
            _write_json(
                wide_root / "legend-pullback-breakout" / "walkforward_summary.json",
                {
                    "job": "legend-pullback-breakout",
                    "retune_profile": "wide",
                    "folds_total": 3,
                    "folds_passing": 3,
                    "aggregate": {
                        "forward_profit_total": 213.03,
                        "forward_pf_mean": 1.6812,
                        "forward_sharpe_mean": 6.8827,
                        "forward_trades_total": 389,
                    },
                },
            )
            _write_json(
                medium_root / "historical-s-bred940-trend-core" / "walkforward_summary.json",
                {
                    "job": "historical-s-bred940-trend-core",
                    "retune_profile": "medium",
                    "folds_total": 3,
                    "folds_passing": 3,
                    "aggregate": {
                        "forward_profit_total": 237.99,
                        "forward_pf_mean": 1.8208,
                        "forward_sharpe_mean": 2.4491,
                        "forward_trades_total": 141,
                    },
                },
            )

            report = bridge.build_bridge_report(
                inventory_manifests=[inventory_manifest],
                walkforward_roots=[narrow_root, wide_root, medium_root],
            )

        self.assertEqual(
            [
                "legend-macd-above-zero-cross",
                "historical-s-bred940-trend-core",
                "legend-pullback-breakout",
            ],
            report["shortlist"],
        )

        by_name = {entry["strategy_name"]: entry for entry in report["strategies"]}
        self.assertEqual(1, by_name["legend-macd-above-zero-cross"]["shortlist_rank"])
        self.assertEqual("primary_stability_first", by_name["legend-macd-above-zero-cross"]["shortlist_role"])
        self.assertEqual(
            "wide",
            by_name["legend-macd-above-zero-cross"]["canonical_walkforward"]["retune_profile"],
        )
        self.assertEqual(
            "medium",
            by_name["historical-s-bred940-trend-core"]["canonical_walkforward"]["retune_profile"],
        )
        self.assertEqual(
            "wide",
            by_name["legend-pullback-breakout"]["canonical_walkforward"]["retune_profile"],
        )
        self.assertTrue(by_name["legend-macd-above-zero-cross"]["contract_alignment"]["requires_paper_forward"])
        self.assertTrue(by_name["historical-s-bred940-trend-core"]["contract_alignment"]["requires_paper_forward"])
        self.assertTrue(by_name["legend-pullback-breakout"]["contract_alignment"]["requires_paper_forward"])

    def test_build_bridge_report_keeps_proxy_evidence_out_of_rank_and_deployment_gates(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            inventory_manifest = root / "inventory" / "inventory_tester_manifest.json"
            _write_json(
                inventory_manifest,
                [
                    {
                        "job": {
                            "slug": "legend-macd-above-zero-cross",
                            "job_set": "legend",
                            "expert": "Legend_MACDAboveZeroCross.ex5",
                            "source_rel": "src/mt5/legend_batch2/Legend_MACDAboveZeroCross.mq5",
                            "symbol": "USDJPY",
                            "period": "H4",
                            "notes": "",
                        },
                        "run_id": "run_inventory_1",
                        "from_date": "2024.12.26",
                        "to_date": "2025.06.23",
                        "summary": {
                            "profit_factor": "1.14",
                            "sharpe_ratio": "2.84",
                            "total_trades": "27",
                            "total_net_profit": "4.12",
                        },
                    }
                ],
            )
            walk_root = root / "walkforward_wide"
            _write_json(
                walk_root / "legend-macd-above-zero-cross" / "walkforward_summary.json",
                {
                    "job": "legend-macd-above-zero-cross",
                    "retune_profile": "wide",
                    "folds_total": 3,
                    "folds_passing": 3,
                    "aggregate": {
                        "forward_profit_total": 116.34,
                        "forward_pf_mean": 2.9664,
                        "forward_sharpe_mean": 4.7661,
                        "forward_trades_total": 46,
                    },
                },
            )

            report = bridge.build_bridge_report(
                inventory_manifests=[inventory_manifest],
                walkforward_roots=[walk_root],
            )

        strategy = report["strategies"][0]
        alignment = strategy["contract_alignment"]
        self.assertFalse(alignment["rank_evidence_satisfied"])
        self.assertFalse(alignment["deployment_gate_ready"])
        self.assertIn("mt5_inventory_not_canonical_rank_evidence", alignment["reason_codes"])
        self.assertIn("mt5_walkforward_not_canonical_paper_forward", alignment["reason_codes"])
        self.assertEqual("queue_paper_forward_probe", strategy["next_action"])


if __name__ == "__main__":
    unittest.main()
