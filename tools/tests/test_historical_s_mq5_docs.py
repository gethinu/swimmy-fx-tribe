from pathlib import Path
import unittest


ROOT = Path("/home/swimmy/swimmy")


class TestHistoricalSMq5Docs(unittest.TestCase):
    def test_historical_batch1_doc_exists_with_japanese_sections(self) -> None:
        doc = ROOT / "doc/knowledge/historical_s_mq5_batch1_20260307.md"
        self.assertTrue(doc.exists(), "historical-s batch-1 doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("自然言語版", text)
        self.assertIn("Bred-Bred--222-Gen30-N3980040329-718", text)
        self.assertIn("Bred-Bred--723-Gen29-N3980038311-278", text)
        self.assertIn("RECRUIT-RND-1768781166-12", text)
        self.assertIn("実行正本", text)

    def test_historical_batch1_mq5_files_exist(self) -> None:
        expected = {
            "HistS_Bred222TrendCore.mq5": "Bred-Bred--222-Gen30-N3980040329-718",
            "HistS_Bred723TrendCore.mq5": "Bred-Bred--723-Gen29-N3980038311-278",
            "HistS_RecruitRndTrendCross.mq5": "RECRUIT-RND-1768781166-12",
        }

        for filename, marker in expected.items():
            path = ROOT / "src/mt5/historical_s_batch1" / filename
            self.assertTrue(path.exists(), f"{filename} must exist")
            text = path.read_text(encoding="utf-8")
            self.assertIn(marker, text)

    def test_historical_batch2_doc_exists_with_japanese_sections(self) -> None:
        doc = ROOT / "doc/knowledge/historical_s_mq5_batch2_20260307.md"
        self.assertTrue(doc.exists(), "historical-s batch-2 doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("自然言語版", text)
        self.assertIn("同一シグナルコア", text)
        self.assertIn("Bred-Bred--508-Gen32-N3980040829-808", text)
        self.assertIn("Bred-Bred--794-Gen32-N3980040593-767", text)
        self.assertIn("Bred-Bred--128-Gen28-N3980038170-239", text)

    def test_historical_batch2_mq5_files_exist(self) -> None:
        expected = {
            "HistS_Bred508TrendCore.mq5": "Bred-Bred--508-Gen32-N3980040829-808",
            "HistS_Bred794TrendCore.mq5": "Bred-Bred--794-Gen32-N3980040593-767",
            "HistS_Bred128TrendCore.mq5": "Bred-Bred--128-Gen28-N3980038170-239",
        }

        for filename, marker in expected.items():
            path = ROOT / "src/mt5/historical_s_batch2" / filename
            self.assertTrue(path.exists(), f"{filename} must exist")
            text = path.read_text(encoding="utf-8")
            self.assertIn(marker, text)

    def test_historical_batch3_doc_exists_with_completion_sections(self) -> None:
        doc = ROOT / "doc/knowledge/historical_s_mq5_batch3_20260307.md"
        self.assertTrue(doc.exists(), "historical-s batch-3 doc must exist")

        text = doc.read_text(encoding="utf-8")
        self.assertIn("自然言語版", text)
        self.assertIn("同一シグナルコア", text)
        self.assertIn("duplicate pair", text)
        self.assertIn("Bred-Bred--187-Gen23-N3980038264-261", text)
        self.assertIn("Bred-Bred--436-Gen32-N3980040463-744", text)
        self.assertIn("Bred-Bred--940-Gen31-N3980039835-605", text)
        self.assertIn("Bred-Bred--586-Gen29-N3980038495-317", text)
        self.assertIn("Bred-Bred--458-Gen32-N3980040289-704", text)
        self.assertIn("Bred-Bred--139-Gen11-N3979972567-6", text)
        self.assertIn("Bred-Bred--139-Gen11-N3979972610-6", text)
        self.assertIn("12 本すべて", text)

    def test_historical_batch3_mq5_files_exist(self) -> None:
        expected = {
            "HistS_Bred187TrendCore.mq5": "Bred-Bred--187-Gen23-N3980038264-261",
            "HistS_Bred436TrendCore.mq5": "Bred-Bred--436-Gen32-N3980040463-744",
            "HistS_Bred940TrendCore.mq5": "Bred-Bred--940-Gen31-N3980039835-605",
            "HistS_Bred586TrendCore.mq5": "Bred-Bred--586-Gen29-N3980038495-317",
            "HistS_Bred458TrendCore.mq5": "Bred-Bred--458-Gen32-N3980040289-704",
            "HistS_Bred139TrendCore_3979972567.mq5": "Bred-Bred--139-Gen11-N3979972567-6",
            "HistS_Bred139TrendCore_3979972610.mq5": "Bred-Bred--139-Gen11-N3979972610-6",
        }

        for filename, marker in expected.items():
            path = ROOT / "src/mt5/historical_s_batch3" / filename
            self.assertTrue(path.exists(), f"{filename} must exist")
            text = path.read_text(encoding="utf-8")
            self.assertIn(marker, text)


if __name__ == "__main__":
    unittest.main()
