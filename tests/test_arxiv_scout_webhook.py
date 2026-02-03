import importlib
import os
import sys
import unittest


class TestArxivScoutWebhook(unittest.TestCase):
    def setUp(self):
        self._orig_env = os.environ.copy()
        self._module_name = "arxiv_scout"
        self._scout_path = "/home/swimmy/arxiv-scout"
        if self._scout_path not in sys.path:
            sys.path.insert(0, self._scout_path)
        sys.modules.pop(self._module_name, None)

    def tearDown(self):
        os.environ.clear()
        os.environ.update(self._orig_env)
        sys.modules.pop(self._module_name, None)
        if sys.path and sys.path[0] == self._scout_path:
            sys.path.pop(0)

    def test_resolve_discord_webhook_reads_env(self):
        arxiv_scout = importlib.import_module("arxiv_scout")
        os.environ["SWIMMY_ARXIV_REPORT_WEBHOOK"] = "https://example.com/webhook"

        if not hasattr(arxiv_scout, "resolve_discord_webhook"):
            self.fail("Expected resolve_discord_webhook to exist")

        self.assertEqual(
            arxiv_scout.resolve_discord_webhook(),
            "https://example.com/webhook",
        )

    def test_resolve_discord_webhook_strips_inline_comment(self):
        arxiv_scout = importlib.import_module("arxiv_scout")
        os.environ["SWIMMY_ARXIV_REPORT_WEBHOOK"] = (
            "https://example.com/webhook # REQUIRED for arXiv Scout"
        )

        self.assertEqual(
            arxiv_scout.resolve_discord_webhook(),
            "https://example.com/webhook",
        )
