import importlib
import os
import sys
import unittest


RSS_XML = """<?xml version="1.0" encoding="utf-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:rss="http://purl.org/rss/1.0/"
         xmlns:dc="http://purl.org/dc/elements/1.1/">
  <rss:channel rdf:about="http://lists.repec.org/mailman/listinfo/nep-fmk">
    <rss:title>Financial Markets</rss:title>
  </rss:channel>
  <rss:item rdf:about="https://d.repec.org/n?u=RePEc:arx:papers:2601.11958&amp;r=&amp;r=fmk">
    <rss:title>Autonomous Market Intelligence: Agentic AI Nowcasting Predicts Stock Returns</rss:title>
    <rss:link>https://d.repec.org/n?u=RePEc:arx:papers:2601.11958&amp;r=&amp;r=fmk</rss:link>
    <rss:description>Agentic AI nowcasting for stock returns.</rss:description>
    <dc:creator>Zefeng Chen</dc:creator>
    <dc:creator>Darcy Pu</dc:creator>
    <dc:date>2026-01</dc:date>
  </rss:item>
</rdf:RDF>
"""


class TestRepecRss(unittest.TestCase):
    def setUp(self):
        self._orig_env = os.environ.copy()
        self._scout_path = "/home/swimmy/swimmy/tools/arxiv-scout"
        if self._scout_path not in sys.path:
            sys.path.insert(0, self._scout_path)
        sys.modules.pop("arxiv_scout", None)

    def tearDown(self):
        os.environ.clear()
        os.environ.update(self._orig_env)
        sys.modules.pop("arxiv_scout", None)
        if sys.path and sys.path[0] == self._scout_path:
            sys.path.pop(0)

    def test_parse_repec_rss_extracts_items(self):
        arxiv_scout = importlib.import_module("arxiv_scout")
        items = arxiv_scout.parse_repec_rss(RSS_XML)
        self.assertEqual(len(items), 1)
        item = items[0]
        self.assertEqual(
            item["title"],
            "Autonomous Market Intelligence: Agentic AI Nowcasting Predicts Stock Returns",
        )
        self.assertEqual(
            item["link"],
            "https://d.repec.org/n?u=RePEc:arx:papers:2601.11958&r=&r=fmk",
        )
        self.assertIn("Agentic AI", item["description"])
        self.assertEqual(item["authors"], ["Zefeng Chen", "Darcy Pu"])
        self.assertEqual(item["published"], "2026-01")

    def test_normalize_repec_item_arxiv_id(self):
        arxiv_scout = importlib.import_module("arxiv_scout")
        item = {
            "title": "Test Paper",
            "link": "https://d.repec.org/n?u=RePEc:arx:papers:2601.11958&r=&r=fmk",
            "description": "FX forecasting",
            "authors": ["A"],
            "published": "2026-01",
        }
        paper = arxiv_scout.normalize_repec_item(item, ["q-fin.TR"])
        self.assertEqual(paper["id"], "2601.11958")
        self.assertEqual(paper["categories"], ["q-fin.TR"])

    def test_repec_keyword_match(self):
        arxiv_scout = importlib.import_module("arxiv_scout")
        self.assertTrue(
            arxiv_scout.repec_matches_keywords("FX market", ["fx", "forex"])
        )
        self.assertFalse(
            arxiv_scout.repec_matches_keywords("equity market", ["forex"])
        )

    def test_scout_papers_includes_repec(self):
        arxiv_scout = importlib.import_module("arxiv_scout")

        arxiv_scout.search_arxiv = lambda *args, **kwargs: None
        arxiv_scout.parse_arxiv_response = lambda *args, **kwargs: []
        arxiv_scout.time.sleep = lambda *args, **kwargs: None

        arxiv_scout.fetch_repec_papers = lambda: [
            {
                "id": "2601.11958",
                "title": "RePEc Paper",
                "summary": "FX",
                "authors": ["A"],
                "published": "2026-01",
                "link": "https://d.repec.org/n?u=RePEc:arx:papers:2601.11958",
                "categories": ["q-fin.TR"],
            }
        ]

        arxiv_scout.evaluate_with_gemini = lambda *args, **kwargs: {
            "score": 6,
            "summary_ja": "ok",
            "key_tech": "-",
            "how_to_use": "ok",
            "relevance": "MEDIUM",
        }

        saved = {}
        arxiv_scout.load_json = lambda *args, **kwargs: {"papers": []}

        def _save(path, data):
            saved[getattr(path, "name", str(path))] = data

        arxiv_scout.save_json = _save

        arxiv_scout.scout_papers()
        pending = saved.get("pending_papers.json", {}).get("papers", [])
        self.assertEqual(len(pending), 1)
        self.assertEqual(pending[0]["id"], "2601.11958")


if __name__ == "__main__":
    unittest.main()
