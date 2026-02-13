import re
import subprocess
import unittest
from pathlib import Path


class TestNoCommittedDiscordWebhooks(unittest.TestCase):
    def test_no_committed_real_discord_webhooks(self) -> None:
        # Defensive: webhook URLs are effectively secrets (token is in the URL).
        # This test fails without printing the secret content (only file paths).
        try:
            raw = subprocess.check_output(["git", "ls-files"], text=True)
        except Exception as exc:  # pragma: no cover
            raise unittest.SkipTest(f"git ls-files unavailable: {exc}") from exc

        # Match full Discord webhook URL with numeric ID and a non-placeholder token.
        pat = re.compile(r"https?://discord\.com/api/webhooks/(\d+)/([^\s\"'<>]+)")

        offenders = []
        for rel in [line.strip() for line in raw.splitlines() if line.strip()]:
            path = Path(rel)
            if not path.exists() or not path.is_file():
                continue
            try:
                text = path.read_text(encoding="utf-8", errors="ignore")
            except Exception:
                continue

            for m in pat.finditer(text):
                token = m.group(2)
                # Allow docs/tests to use explicit placeholders.
                if token.lower() in {"dummy", "..."}:
                    continue
                if "..." in token:
                    continue
                if token.lower().startswith(("your_", "example", "changeme", "<redacted")):
                    continue
                offenders.append(rel)
                break

        self.assertEqual([], offenders, f"Committed Discord webhook URLs found in: {offenders}")


if __name__ == "__main__":
    unittest.main()
