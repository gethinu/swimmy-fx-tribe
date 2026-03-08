from pathlib import Path
import unittest


ROOT = Path("/home/swimmy/swimmy")


class TestLegendMq5Docs(unittest.TestCase):
    def test_batch1_doc_uses_japanese_natural_language_sections(self) -> None:
        doc = ROOT / "doc/knowledge/legend_mq5_batch1_20260307.md"
        text = doc.read_text(encoding="utf-8")

        self.assertIn("自然言語版", text)
        self.assertIn("市場アイデア", text)
        self.assertIn("Perfect-Order-SMA", text)
        self.assertIn("Simple-Momentum-Sync", text)
        self.assertIn("Pullback-Breakout", text)

    def test_batch2_doc_exists_with_japanese_natural_language_sections(self) -> None:
        doc = ROOT / "doc/knowledge/legend_mq5_batch2_20260307.md"
        self.assertTrue(doc.exists(), "batch-2 doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("自然言語版", text)
        self.assertIn("Trend-Pullback-Entry", text)
        self.assertIn("Sweet-Chariot-SMA-40", text)
        self.assertIn("MACD-Above-Zero-Cross", text)

    def test_batch2_mq5_files_exist(self) -> None:
        expected = {
            "Legend_TrendPullbackEntry.mq5": "Trend-Pullback-Entry",
            "Legend_SweetChariotSMA40.mq5": "Sweet-Chariot-SMA-40",
            "Legend_MACDAboveZeroCross.mq5": "MACD-Above-Zero-Cross",
        }

        for filename, marker in expected.items():
            path = ROOT / "src/mt5/legend_batch2" / filename
            self.assertTrue(path.exists(), f"{filename} must exist")
            text = path.read_text(encoding="utf-8")
            self.assertIn(marker, text)

    def test_batch3_doc_exists_with_japanese_natural_language_sections(self) -> None:
        doc = ROOT / "doc/knowledge/legend_mq5_batch3_20260307.md"
        self.assertTrue(doc.exists(), "batch-3 doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("自然言語版", text)
        self.assertIn("MACD-Signal-Cross", text)
        self.assertIn("Legend-London-Breakout-V1", text)
        self.assertIn("Legend-RSI-Reversion-V1", text)

    def test_batch3_mq5_files_exist(self) -> None:
        expected = {
            "Legend_MACDSignalCross.mq5": "MACD-Signal-Cross",
            "Legend_LondonBreakoutV1.mq5": "Legend-London-Breakout-V1",
            "Legend_RSIReversionV1.mq5": "Legend-RSI-Reversion-V1",
        }

        for filename, marker in expected.items():
            path = ROOT / "src/mt5/legend_batch3" / filename
            self.assertTrue(path.exists(), f"{filename} must exist")
            text = path.read_text(encoding="utf-8")
            self.assertIn(marker, text)

    def test_batch4_doc_exists_with_japanese_natural_language_sections(self) -> None:
        doc = ROOT / "doc/knowledge/legend_mq5_batch4_20260307.md"
        self.assertTrue(doc.exists(), "batch-4 doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("drop", text.lower())
        self.assertIn("自然言語版", text)
        self.assertIn("MACD-Zero-Cross-Long", text)
        self.assertIn("MACD-Expansion", text)
        self.assertIn("Crossover-Plus-MACD", text)

    def test_batch4_mq5_files_are_removed_from_repository(self) -> None:
        self.assertFalse((ROOT / "src/mt5/archive/legend_batch4/Legend_MACDZeroCrossLong.mq5").exists())
        self.assertFalse((ROOT / "src/mt5/archive/legend_batch4/Legend_MACDExpansion.mq5").exists())
        self.assertFalse((ROOT / "src/mt5/archive/legend_batch4/Legend_CrossoverPlusMACD.mq5").exists())

    def test_external_legend_source_audit_doc_exists(self) -> None:
        doc = ROOT / "doc/knowledge/legend_external_source_audit_20260307.md"
        self.assertTrue(doc.exists(), "external legend source audit doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("Legend-London-Breakout-V1", text)
        self.assertIn("Legend-RSI-Reversion-V1", text)
        self.assertIn("正本", text)
        self.assertIn("legacy reference only", text)

    def test_legend_mt5_compile_report_doc_exists(self) -> None:
        doc = ROOT / "doc/knowledge/legend_mt5_compile_report_20260307.md"
        self.assertTrue(doc.exists(), "legend mt5 compile report doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("22 / 22", text)
        self.assertIn("compile 成功", text)
        self.assertIn("scripts/compile_swimmybridge_mt5.sh", text)
        self.assertIn("Strategy Tester", text)

    def test_legend_mt5_tester_automation_doc_exists(self) -> None:
        doc = ROOT / "doc/knowledge/legend_mt5_tester_automation_20260307.md"
        self.assertTrue(doc.exists(), "legend mt5 tester automation doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("tools/mt5_inventory_tester.py", text)
        self.assertIn("portable", text)
        self.assertIn("legend-perfect-order-sma", text)
        self.assertNotIn("legend-macd-zero-cross-long", text)
        self.assertIn("timeframe=3600", text)

    def test_followup_validation_doc_closes_internal_freeze_tasks(self) -> None:
        doc = ROOT / "doc/knowledge/legend_mt5_followup_validation_20260307.md"
        self.assertTrue(doc.exists(), "followup validation doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("The MT5 freeze follow-up is complete for the current local environment.", text)
        self.assertIn(
            "medium fallback for `historical-s-bred940-trend-core` is treated as canonical closeout evidence",
            text,
        )
        self.assertIn("true broker-variance replay", text)
        self.assertIn("repo cleanup boundary has already been documented", text)

    def test_inventory_doc_reduces_remaining_blocker_to_broker_variance(self) -> None:
        doc = ROOT / "doc/knowledge/legend_s_rank_inventory_20260307.md"
        self.assertTrue(doc.exists(), "legend s-rank inventory doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("`medium` fallback を canonical とし", text)
        self.assertIn("現時点で残っている実務上の blocker は broker variance だけです", text)
        self.assertIn("cleanup boundary も未解決のままではありません", text)

    def test_cleanup_boundary_doc_records_current_operating_decisions(self) -> None:
        doc = ROOT / "doc/knowledge/legend_mt5_cleanup_boundary_20260307.md"
        self.assertTrue(doc.exists(), "cleanup boundary doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("現時点の運用決定", text)
        self.assertIn("audit trail として repo に残してよい", text)
        self.assertIn("separate 扱いを維持する", text)

    def test_strategies_legendary_only_keeps_external_legends_as_legacy_notes(self) -> None:
        path = ROOT / "src/lisp/strategies/strategies-legendary.lisp"
        text = path.read_text(encoding="utf-8")

        self.assertIn("legacy seed only", text)
        self.assertNotIn('(make-strategy :name "Legend-London-Breakout-V1"', text)
        self.assertNotIn('(make-strategy :name "Legend-RSI-Reversion-V1"', text)


if __name__ == "__main__":
    unittest.main()
