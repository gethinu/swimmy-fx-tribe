import unittest

import notifier


class TestNotifierPreview(unittest.TestCase):
    def test_preview_from_embed_description(self):
        payload = {
            "embeds": [
                {
                    "title": "🚨 ALERT",
                    "description": "🧊 **Stagnant C-Rank Summary (Last 1h)**\nTotal: 3\nTop: `A`, `B`",
                }
            ]
        }
        preview = notifier.format_payload_preview(payload, max_len=120)
        self.assertIn("Stagnant C-Rank Summary", preview)
        self.assertNotIn("\n", preview)

    def test_preview_from_soft_kill(self):
        payload = {
            "embeds": [
                {
                    "title": "🚨 ALERT",
                    "description": "🛡️ **Strategy Soft-Killed (Cooldown)**\nName: X",
                }
            ]
        }
        preview = notifier.format_payload_preview(payload, max_len=120)
        self.assertIn("Strategy Soft-Killed", preview)

    def test_preview_from_content_fallback(self):
        payload = {
            "content": "Fallback content message",
            "embeds": [],
        }
        preview = notifier.format_payload_preview(payload, max_len=120)
        self.assertEqual("Fallback content message", preview)

    def test_preview_empty_when_missing(self):
        payload = {"embeds": []}
        preview = notifier.format_payload_preview(payload, max_len=120)
        self.assertEqual("", preview)

    def test_preview_truncates(self):
        payload = {
            "embeds": [
                {
                    "title": "🚨 ALERT",
                    "description": "X" * 200,
                }
            ]
        }
        preview = notifier.format_payload_preview(payload, max_len=40)
        self.assertEqual(40, len(preview))
        self.assertTrue(preview.endswith("…"))


if __name__ == "__main__":
    unittest.main()
