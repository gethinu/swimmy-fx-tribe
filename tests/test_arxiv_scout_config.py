import importlib
import os
import sys
import unittest


class TestArxivScoutConfig(unittest.TestCase):
    def setUp(self):
        self._orig_env = os.environ.copy()
        self._scout_path = "/home/swimmy/arxiv-scout"
        if self._scout_path not in sys.path:
            sys.path.insert(0, self._scout_path)
        os.environ.pop("SWIMMY_ARXIV_MAX_RESULTS", None)
        sys.modules.pop("arxiv_scout", None)

    def tearDown(self):
        os.environ.clear()
        os.environ.update(self._orig_env)
        sys.modules.pop("arxiv_scout", None)
        if sys.path and sys.path[0] == self._scout_path:
            sys.path.pop(0)

    def test_default_max_results_is_at_least_30(self):
        arxiv_scout = importlib.import_module("arxiv_scout")
        if not hasattr(arxiv_scout, "MAX_RESULTS_PER_QUERY"):
            self.fail("Expected MAX_RESULTS_PER_QUERY to be defined")
        self.assertGreaterEqual(arxiv_scout.MAX_RESULTS_PER_QUERY, 30)
