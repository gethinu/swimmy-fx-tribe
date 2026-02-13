import unittest

from tools import notifier


class TestNotifierRedaction(unittest.TestCase):
    def test_redact_discord_webhook_url_redacts_token(self) -> None:
        url = "https://discord.com/api/webhooks/123456789012345678/dummy"
        redacted = notifier.redact_discord_webhook_url(url)
        self.assertIn("123456789012345678", redacted)
        self.assertNotIn("dummy", redacted)
        self.assertIn("<redacted>", redacted)

    def test_redact_discord_webhook_url_accepts_discordapp_domain(self) -> None:
        url = "https://discordapp.com/api/webhooks/123/dummy"
        redacted = notifier.redact_discord_webhook_url(url)
        self.assertIn("/123/", redacted)
        self.assertNotIn("dummy", redacted)

    def test_redact_discord_webhook_url_unknown_url(self) -> None:
        self.assertEqual("<redacted-url>", notifier.redact_discord_webhook_url("https://example.com/x"))


if __name__ == "__main__":
    unittest.main()
