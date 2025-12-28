"""
Auto-Journal Configuration

Central configuration for screenshot capture, OCR, AI summarization, and storage.
All settings can be customized here without modifying other modules.
"""

from __future__ import annotations

import os
import logging
from pathlib import Path
from typing import Final
from dotenv import load_dotenv

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="[AutoJournal] %(levelname)s: %(message)s",
)
logger = logging.getLogger("auto_journal")

# Load .env from parent directory
load_dotenv(Path(__file__).parent.parent / ".env")

# ============================================================================
# Paths
# ============================================================================
BASE_DIR: Final[Path] = Path(__file__).parent
DATA_DIR: Final[Path] = BASE_DIR / "data"
SCREENSHOTS_DIR: Final[Path] = DATA_DIR / "screenshots"
LOGS_DIR: Final[Path] = DATA_DIR / "logs"
REPORTS_DIR: Final[Path] = DATA_DIR / "reports"

# Ensure directories exist at import time
for directory in (SCREENSHOTS_DIR, LOGS_DIR, REPORTS_DIR):
    directory.mkdir(parents=True, exist_ok=True)

# ============================================================================
# Screenshot Settings
# ============================================================================
CAPTURE_INTERVAL_SECONDS: Final[int] = 60  # 1 minute between captures
SCREENSHOT_RETENTION_DAYS: Final[int] = 7  # Auto-delete after 7 days
SCREENSHOT_QUALITY: Final[int] = 85  # JPEG quality (1-100)
SCREENSHOT_SCALE: Final[float] = 0.5  # Resize factor for storage

# ============================================================================
# Privacy Filter
# ============================================================================
# Window titles containing these keywords will be skipped
PRIVACY_FILTER_KEYWORDS: Final[tuple[str, ...]] = (
    "password",
    "secret",
    "credential",
    "login",
    "signin",
    "パスワード",
    "ログイン",
    "認証",
    "1password",
    "lastpass",
    "bitwarden",
    "keychain",
    "wallet",
)

# ============================================================================
# OCR Settings
# ============================================================================
OCR_LANGUAGE: Final[str] = "eng+jpn"  # Tesseract language codes
OCR_TIMEOUT_SECONDS: Final[int] = 30
OCR_MAX_TEXT_LENGTH: Final[int] = 2000  # Max chars to store per entry

# ============================================================================
# AI Settings (Gemini)
# ============================================================================
GEMINI_API_KEY: Final[str] = os.getenv("SWIMMY_GEMINI_API_KEY", "")
GEMINI_MODEL: Final[str] = "gemini-2.0-flash"
GEMINI_TEMPERATURE: Final[float] = 0.7
GEMINI_MAX_TOKENS: Final[int] = 2000
GEMINI_RETRY_COUNT: Final[int] = 3
GEMINI_RETRY_DELAY: Final[float] = 2.0  # seconds

# ============================================================================
# Discord Settings
# ============================================================================
DISCORD_DAILY_WEBHOOK: Final[str] = os.getenv("SWIMMY_DISCORD_JOURNAL", "")
DISCORD_MAX_MESSAGE_LENGTH: Final[int] = 1900  # Discord limit is 2000
