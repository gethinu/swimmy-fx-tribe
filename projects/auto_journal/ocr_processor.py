"""
OCR Processor Module

Extracts text from screenshots using Tesseract OCR.
Supports both command-line tesseract and pytesseract library.
Includes keyword extraction for activity categorization.
"""

from __future__ import annotations

import subprocess
import re
from pathlib import Path
from typing import TYPE_CHECKING

from PIL import Image

import config
from config import logger

if TYPE_CHECKING:
    pass


def extract_text_tesseract(image_path: Path | str) -> str:
    """
    Extract text from image using tesseract command line.

    Args:
        image_path: Path to the image file.

    Returns:
        Extracted text, or empty string on failure.
    """
    try:
        result = subprocess.run(
            ["tesseract", str(image_path), "stdout", "-l", config.OCR_LANGUAGE],
            capture_output=True,
            text=True,
            timeout=config.OCR_TIMEOUT_SECONDS,
        )
        if result.returncode == 0:
            return result.stdout.strip()
        else:
            logger.warning(f"OCR error: {result.stderr[:100]}")
            return ""
    except FileNotFoundError:
        logger.error(
            "tesseract not found. Install: sudo apt install tesseract-ocr tesseract-ocr-jpn"
        )
        return ""
    except subprocess.TimeoutExpired:
        logger.warning(f"OCR timeout after {config.OCR_TIMEOUT_SECONDS}s")
        return ""
    except OSError as e:
        logger.error(f"OCR system error: {e}")
        return ""


def extract_text_pytesseract(image: Image.Image) -> str:
    """
    Extract text from PIL Image using pytesseract library.

    Args:
        image: PIL Image to process.

    Returns:
        Extracted text, or empty string on failure.
    """
    try:
        import pytesseract

        text = pytesseract.image_to_string(image, lang=config.OCR_LANGUAGE)
        return text.strip()
    except ImportError:
        logger.debug("pytesseract not installed, falling back to CLI")
        return ""
    except Exception as e:
        logger.warning(f"pytesseract error: {e}")
        return ""


def extract_text(image_or_path: Path | str | Image.Image) -> str:
    """
    Main OCR function: extract text from image.

    Accepts either a file path or PIL Image.
    Tries pytesseract first for PIL Images, falls back to CLI.

    Args:
        image_or_path: Either a Path/str to image file, or PIL Image.

    Returns:
        Extracted text (may be empty if OCR fails or image has no text).
    """
    if isinstance(image_or_path, Image.Image):
        # Try pytesseract first for PIL images
        text = extract_text_pytesseract(image_or_path)
        if text:
            return text
        # Fallback: save temp file and use command line
        temp_path = Path("/tmp/auto_journal_ocr_temp.png")
        try:
            image_or_path.save(temp_path)
            text = extract_text_tesseract(temp_path)
        finally:
            temp_path.unlink(missing_ok=True)
        return text
    else:
        return extract_text_tesseract(Path(image_or_path))


# ============================================================================
# Keyword Extraction
# ============================================================================

# File extensions that indicate coding activity
_CODING_EXTENSIONS: tuple[str, ...] = (
    ".py",
    ".lisp",
    ".js",
    ".ts",
    ".jsx",
    ".tsx",
    ".md",
    ".json",
    ".html",
    ".css",
    ".scss",
    ".go",
    ".rs",
    ".c",
    ".cpp",
    ".h",
    ".java",
    ".rb",
    ".php",
    ".sh",
    ".yaml",
    ".yml",
    ".toml",
)

# Application keywords and their categories
_APP_CATEGORIES: dict[str, str] = {
    # Coding
    "vscode": "coding",
    "visual studio": "coding",
    "intellij": "coding",
    "pycharm": "coding",
    "vim": "coding",
    "neovim": "coding",
    "emacs": "coding",
    "sublime": "coding",
    # Terminal
    "terminal": "terminal",
    "iterm": "terminal",
    "konsole": "terminal",
    "bash": "terminal",
    "zsh": "terminal",
    # Browser
    "chrome": "browser",
    "firefox": "browser",
    "safari": "browser",
    "edge": "browser",
    "brave": "browser",
    # Communication
    "slack": "communication",
    "discord": "communication",
    "teams": "communication",
    "zoom": "communication",
    "meet": "communication",
    # Social
    "twitter": "social",
    "x.com": "social",
    "facebook": "social",
    "instagram": "social",
    # Video
    "youtube": "video",
    "netflix": "video",
    "twitch": "video",
    # Documents
    "notion": "documents",
    "obsidian": "documents",
    "word": "documents",
    "docs": "documents",
    "sheets": "documents",
    "excel": "documents",
}


def extract_keywords(text: str) -> list[str]:
    """
    Extract activity keywords/topics from OCR text.

    Analyzes text for:
    - File extensions (indicating coding in specific languages)
    - Application names (indicating activity type)

    Args:
        text: OCR text to analyze.

    Returns:
        List of extracted keyword strings.
    """
    if not text:
        return []

    keywords: set[str] = set()
    text_lower = text.lower()

    # Detect file extensions → coding:language
    for ext in _CODING_EXTENSIONS:
        if ext in text_lower:
            lang = ext[1:]  # Remove leading dot
            keywords.add(f"coding:{lang}")

    # Detect applications → category
    for app, category in _APP_CATEGORIES.items():
        if app in text_lower:
            keywords.add(category)

    # Detect URLs
    if re.search(r"https?://", text_lower):
        keywords.add("web")

    # Detect Git activity
    if any(
        kw in text_lower for kw in ("git ", "commit", "branch", "merge", "pull request")
    ):
        keywords.add("git")

    return sorted(keywords)


if __name__ == "__main__":
    import sys

    if len(sys.argv) > 1:
        text = extract_text(Path(sys.argv[1]))
        print(f"Extracted text ({len(text)} chars):")
        print(text[:500] + "..." if len(text) > 500 else text)
        print(f"\nKeywords: {extract_keywords(text)}")
    else:
        print("Usage: python -m auto_journal.ocr_processor <image_path>")
