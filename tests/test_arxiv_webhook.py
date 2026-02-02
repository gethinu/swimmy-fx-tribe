import importlib
import os
import sys
import unittest


class TestArxivWebhookConfig(unittest.TestCase):
    def setUp(self):
        self._orig_env = os.environ.copy()
        self._install_discord_stub()
        self._install_aiohttp_stub()
        sys.modules.pop("discord_bot", None)

    def tearDown(self):
        os.environ.clear()
        os.environ.update(self._orig_env)
        sys.modules.pop("discord_bot", None)
        for name in ("discord", "discord.ext", "discord.ext.commands", "aiohttp"):
            sys.modules.pop(name, None)

    def _install_discord_stub(self):
        class DummyIntents:
            def __init__(self):
                self.message_content = False

            @classmethod
            def default(cls):
                return cls()

        class DummyBot:
            def __init__(self, *args, **kwargs):
                self.loop = None

            def event(self, coro):
                return coro

        class DummyWebhook:
            @classmethod
            def from_url(cls, *args, **kwargs):
                return cls()

        discord_stub = type(sys)("discord")
        discord_stub.Intents = DummyIntents
        discord_stub.Webhook = DummyWebhook
        discord_stub.NotFound = Exception
        discord_stub.Forbidden = Exception
        discord_stub.HTTPException = Exception

        ext_stub = type(sys)("discord.ext")
        commands_stub = type(sys)("discord.ext.commands")
        commands_stub.Bot = DummyBot
        ext_stub.commands = commands_stub
        discord_stub.ext = ext_stub

        sys.modules["discord"] = discord_stub
        sys.modules["discord.ext"] = ext_stub
        sys.modules["discord.ext.commands"] = commands_stub

    def _install_aiohttp_stub(self):
        class DummyClientSession:
            async def __aenter__(self):
                return self

            async def __aexit__(self, exc_type, exc, tb):
                return False

        aiohttp_stub = type(sys)("aiohttp")
        aiohttp_stub.ClientSession = DummyClientSession
        sys.modules["aiohttp"] = aiohttp_stub

    def test_arxiv_webhook_env_is_exposed(self):
        os.environ["SWIMMY_DISCORD_BOT_TOKEN"] = "test-token"
        os.environ["SWIMMY_ARXIV_REPORT_WEBHOOK_BOT"] = "https://example.com/webhook"
        os.environ.pop("SWIMMY_ARXIV_REPORT_WEBHOOK", None)

        try:
            discord_bot = importlib.import_module("discord_bot")
        except SystemExit as exc:
            self.fail(f"discord_bot should not exit during import: {exc}")
        except Exception as exc:
            self.fail(f"discord_bot import should not error: {exc}")

        if not hasattr(discord_bot, "ARXIV_REPORT_DAILY_WEBHOOK"):
            self.fail("Expected ARXIV_REPORT_DAILY_WEBHOOK to be defined on discord_bot")

        self.assertEqual(
            discord_bot.ARXIV_REPORT_DAILY_WEBHOOK,
            "https://example.com/webhook",
        )
