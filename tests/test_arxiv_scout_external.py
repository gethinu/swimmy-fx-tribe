import importlib
import os
import sys
import unittest


class TestArxivScoutExternalSources(unittest.TestCase):
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

    def test_scout_does_not_use_external_sources(self):
        arxiv_scout = importlib.import_module("arxiv_scout")

        # Avoid network calls and sleep delays.
        arxiv_scout.search_arxiv = lambda *args, **kwargs: None
        arxiv_scout.parse_arxiv_response = lambda *args, **kwargs: []
        arxiv_scout.time.sleep = lambda *args, **kwargs: None

        # Avoid file IO in tests.
        arxiv_scout.load_json = lambda *args, **kwargs: {"papers": []}
        arxiv_scout.save_json = lambda *args, **kwargs: None

        if hasattr(arxiv_scout, "load_external_papers"):
            def _fail():
                raise AssertionError("external sources used")

            arxiv_scout.load_external_papers = _fail

        arxiv_scout.scout_papers()
