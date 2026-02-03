import importlib
import os
import sys
import unittest


class DummyResponse:
    status_code = 204
    text = ""


class DummyRequests:
    def __init__(self):
        self.payloads = []

    def post(self, url, json=None, timeout=None):
        self.payloads.append({"url": url, "content": (json or {}).get("content", "")})
        return DummyResponse()


class TestRepecSection(unittest.TestCase):
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

    def test_repec_section_is_sent_with_top5(self):
        os.environ["SWIMMY_ARXIV_REPORT_WEBHOOK"] = "https://example.com/webhook"
        arxiv_scout = importlib.import_module("arxiv_scout")

        dummy_requests = DummyRequests()
        arxiv_scout.requests = dummy_requests
        arxiv_scout.time.sleep = lambda *args, **kwargs: None

        papers = [
            {
                "id": "2601.11958",
                "title": "RePEc A",
                "summary_ja": "s",
                "key_tech": "-",
                "how_to_use": "u",
                "score": 7,
                "relevance": "MEDIUM",
                "link": "https://d.repec.org/n?u=RePEc:arx:papers:2601.11958&r=&r=fmk",
                "categories": ["q-fin.TR"],
            },
            {
                "id": "repec:abc",
                "title": "RePEc B",
                "summary_ja": "s",
                "key_tech": "-",
                "how_to_use": "u",
                "score": 6,
                "relevance": "MEDIUM",
                "link": "https://d.repec.org/n?u=RePEc:nbr:nberwo:34588&r=&r=mon",
                "categories": ["q-fin.RM"],
            },
            {
                "id": "2602.00001",
                "title": "Arxiv X",
                "summary_ja": "s",
                "key_tech": "-",
                "how_to_use": "u",
                "score": 9,
                "relevance": "HIGH",
                "link": "http://arxiv.org/abs/2602.00001",
                "categories": ["q-fin.TR"],
            },
        ]

        arxiv_scout.send_summary_to_discord(papers)

        contents = [p["content"] for p in dummy_requests.payloads]
        self.assertTrue(any("# 📚 RePEc Picks" in c for c in contents))


if __name__ == "__main__":
    unittest.main()
