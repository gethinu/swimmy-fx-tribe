# RePEc RSS POC Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** NEP RSSからRePEc論文を最小限取り込み、既存arXiv Scoutの評価・通知フローに合流させる。

**Architecture:** RePEc RSSをパースして既存paperスキーマへ正規化し、キーワードで絞った上で `scout_papers()` に合流。arXiv IDが含まれる場合はID正規化で重複回避し、なければRePEc専用IDを生成する。

**Tech Stack:** Python 3, requests, xml.etree.ElementTree, unittest

> **Note:** `/home/swimmy/arxiv-scout` はGitリポジトリではないため、コード変更はGit管理外。テストは `/home/swimmy/swimmy/tests` に追加し、必要ならそこでコミットする。

---

### Task 1: RePEc RSSパース/正規化のテスト追加

**Files:**
- Create: `tests/test_arxiv_scout_repec_rss.py`

**Step 1: Write the failing test**

```python
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
  <rss:item rdf:about="https://d.repec.org/n?u=RePEc:arx:papers:2601.11958&r=&r=fmk">
    <rss:title>Autonomous Market Intelligence: Agentic AI Nowcasting Predicts Stock Returns</rss:title>
    <rss:link>https://d.repec.org/n?u=RePEc:arx:papers:2601.11958&r=&r=fmk</rss:link>
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
        self._scout_path = "/home/swimmy/arxiv-scout"
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


if __name__ == "__main__":
    unittest.main()
```

**Step 2: Run test to verify it fails**

Run: `python3 -m unittest tests.test_arxiv_scout_repec_rss`
Expected: FAIL with `AttributeError` (missing parse_repec_rss / normalize_repec_item / repec_matches_keywords)

---

### Task 2: RePEc RSSの実装（最小）

**Files:**
- Modify: `/home/swimmy/arxiv-scout/arxiv_scout.py`

**Step 1: Write minimal implementation**

```python
import hashlib

REPEC_RSS_FEEDS = [
    ("https://nep.repec.org/rss/nep-fmk.rss.xml", ["q-fin.TR"]),
    ("https://nep.repec.org/rss/nep-mon.rss.xml", ["q-fin.RM"]),
    ("https://nep.repec.org/rss/nep-rmg.rss.xml", ["q-fin.RM"]),
    ("https://nep.repec.org/rss/nep-big.rss.xml", ["cs.LG"]),
    ("https://nep.repec.org/rss/nep-cmp.rss.xml", ["cs.LG"]),
    ("https://nep.repec.org/rss/nep-ict.rss.xml", ["cs.LG"]),
]

REPEC_KEYWORDS_FX = [
    "fx",
    "forex",
    "foreign exchange",
    "currency",
    "exchange rate",
    "market microstructure",
    "trading",
]
REPEC_KEYWORDS_AI = [
    "ai",
    "artificial intelligence",
    "machine learning",
    "deep learning",
    "transformer",
    "llm",
    "reinforcement learning",
    "neural",
]


def fetch_repec_rss(feed_url: str):
    try:
        resp = requests.get(feed_url, timeout=20)
        resp.raise_for_status()
        return resp.text
    except Exception as e:
        logger.info(f" RePEc RSS Error: {e}")
        return None


def parse_repec_rss(xml_text: str):
    items = []
    if not xml_text:
        return items
    try:
        root = ET.fromstring(xml_text)
        ns = {
            "rss": "http://purl.org/rss/1.0/",
            "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            "dc": "http://purl.org/dc/elements/1.1/",
        }
        for item in root.findall("rss:item", ns):
            title = item.findtext("rss:title", default="", namespaces=ns)
            link = item.findtext("rss:link", default="", namespaces=ns)
            desc = item.findtext("rss:description", default="", namespaces=ns)
            creators = [
                c.text for c in item.findall("dc:creator", ns) if c.text
            ]
            date = item.findtext("dc:date", default="", namespaces=ns)
            items.append(
                {
                    "title": title.strip(),
                    "link": link.strip(),
                    "description": (desc or "").strip(),
                    "authors": creators[:5],
                    "published": (date or "").strip(),
                }
            )
    except Exception as e:
        logger.info(f" RePEc RSS Parse Error: {e}")
    return items


def repec_matches_keywords(text: str, keywords):
    if not text:
        return False
    lower = text.lower()
    return any(kw in lower for kw in keywords)


def _extract_arxiv_id_from_repec_link(link: str):
    marker = "RePEc:arx:papers:"
    if marker in link:
        return link.split(marker, 1)[1].split("&", 1)[0]
    return None


def normalize_repec_item(item: dict, default_categories):
    link = item.get("link", "")
    arxiv_id = _extract_arxiv_id_from_repec_link(link)
    if arxiv_id:
        paper_id = arxiv_id
    else:
        base = (item.get("title", "") + link).encode("utf-8")
        paper_id = "repec:" + hashlib.sha1(base).hexdigest()
    return {
        "id": paper_id,
        "title": item.get("title", ""),
        "summary": item.get("description", ""),
        "authors": item.get("authors", [])[:5],
        "published": item.get("published", ""),
        "link": link,
        "categories": default_categories,
    }


def fetch_repec_papers():
    papers = []
    for feed_url, default_categories in REPEC_RSS_FEEDS:
        xml_text = fetch_repec_rss(feed_url)
        if not xml_text:
            continue
        items = parse_repec_rss(xml_text)
        keywords = (
            REPEC_KEYWORDS_FX
            if "q-fin" in " ".join(default_categories)
            else REPEC_KEYWORDS_AI
        )
        for item in items:
            text = f"{item.get('title','')} {item.get('description','')}"
            if not repec_matches_keywords(text, keywords):
                continue
            papers.append(normalize_repec_item(item, default_categories))
    return papers
```

**Step 2: Run test to verify it passes**

Run: `python3 -m unittest tests.test_arxiv_scout_repec_rss`
Expected: PASS

**Step 3: Commit (tests only, if desired)**

```bash
git add tests/test_arxiv_scout_repec_rss.py
git commit -m "test: add RePEc RSS parsing tests"
```

---

### Task 3: scout_papers へRePEc合流のテスト追加

**Files:**
- Modify: `tests/test_arxiv_scout_external.py`
- Modify: `tests/test_arxiv_scout_repec_rss.py`

**Step 1: Write the failing test (integration)**

```python
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
```

**Step 2: Run test to verify it fails**

Run: `python3 -m unittest tests.test_arxiv_scout_repec_rss.TestRepecRss.test_scout_papers_includes_repec`
Expected: FAIL (fetch_repec_papers is not called in scout_papers)

**Step 3: Update existing external test to avoid network**

Add in `tests/test_arxiv_scout_external.py` after import:

```python
        if hasattr(arxiv_scout, "fetch_repec_papers"):
            arxiv_scout.fetch_repec_papers = lambda: []
```

**Step 4: Commit (tests only, if desired)**

```bash
git add tests/test_arxiv_scout_repec_rss.py tests/test_arxiv_scout_external.py
git commit -m "test: cover RePEc scout integration"
```

---

### Task 4: scout_papers へRePEc合流の実装

**Files:**
- Modify: `/home/swimmy/arxiv-scout/arxiv_scout.py`

**Step 1: Write minimal implementation**

```python
    # RePEc RSS (optional)
    repec_papers = fetch_repec_papers()
    for paper in repec_papers:
        paper_id = paper["id"]
        if paper_id in seen_ids:
            continue

        evaluation = evaluate_with_gemini(paper)
        if not evaluation:
            evaluation = fallback_evaluate(paper)

        paper_with_eval = {**paper, **evaluation}
        if evaluation.get("relevance") in ["HIGH", "MEDIUM"]:
            pending_papers.append(paper_with_eval)
            new_count += 1
        seen_ids.add(paper_id)
```

**Step 2: Run tests to verify they pass**

Run: `python3 -m unittest tests.test_arxiv_scout_repec_rss`
Expected: PASS

---

## Optional: README update

**Files:**
- Modify: `/home/swimmy/arxiv-scout/README.md`

Add a short note about RePEc RSS POC (NEP feeds, keyword filter).

