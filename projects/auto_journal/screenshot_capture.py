"""
Screenshot Capture Module

Captures screenshots using multiple backends with automatic fallback:
1. scrot (preferred for Linux/WSL)
2. gnome-screenshot
3. PIL ImageGrab

Includes privacy filtering to skip sensitive windows.
"""

from __future__ import annotations

import subprocess
import datetime
from pathlib import Path
from typing import TYPE_CHECKING

from PIL import Image
import io

from . import config
from .config import logger

if TYPE_CHECKING:
    pass


def get_active_window_title() -> str:
    """
    Get the title of the currently active window using xdotool.

    Returns:
        Window title string, or "Unknown" if detection fails.
    """
    try:
        result = subprocess.run(
            ["xdotool", "getactivewindow", "getwindowname"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0:
            title = result.stdout.strip()
            return title if title else "Unknown"
    except (subprocess.TimeoutExpired, FileNotFoundError, PermissionError, OSError):
        pass
    return "Unknown"


def is_privacy_sensitive(window_title: str) -> bool:
    """
    Check if the window title contains privacy-sensitive keywords.

    Args:
        window_title: The window title to check.

    Returns:
        True if the window should be skipped for privacy.
    """
    if not window_title:
        return False
    title_lower = window_title.lower()
    return any(
        keyword.lower() in title_lower for keyword in config.PRIVACY_FILTER_KEYWORDS
    )


def _capture_with_scrot() -> Image.Image | None:
    """Capture screenshot using scrot (Linux)."""
    try:
        result = subprocess.run(
            ["scrot", "-z", "-"],
            capture_output=True,
            timeout=10,
        )
        if result.returncode == 0 and result.stdout:
            return Image.open(io.BytesIO(result.stdout))
    except (
        subprocess.TimeoutExpired,
        FileNotFoundError,
        PermissionError,
        OSError,
    ) as e:
        logger.debug(f"scrot capture failed: {e}")
    return None


def _capture_with_gnome_screenshot() -> Image.Image | None:
    """Capture screenshot using gnome-screenshot."""
    temp_file = Path("/tmp/auto_journal_temp.png")
    try:
        result = subprocess.run(
            ["gnome-screenshot", "-f", str(temp_file)],
            capture_output=True,
            timeout=10,
        )
        if result.returncode == 0 and temp_file.exists():
            image = Image.open(temp_file)
            image.load()  # Force load before deleting file
            temp_file.unlink(missing_ok=True)
            return image
    except (
        subprocess.TimeoutExpired,
        FileNotFoundError,
        PermissionError,
        OSError,
    ) as e:
        logger.debug(f"gnome-screenshot capture failed: {e}")
    finally:
        temp_file.unlink(missing_ok=True)
    return None


def _capture_with_imagegrab() -> Image.Image | None:
    """Capture screenshot using PIL ImageGrab (works on macOS, may work on Windows)."""
    try:
        from PIL import ImageGrab

        return ImageGrab.grab()
    except Exception as e:
        logger.debug(f"ImageGrab capture failed: {e}")
    return None


def capture_screenshot() -> tuple[Image.Image | None, str]:
    """
    Capture a screenshot using available backends.

    Tries multiple capture methods in order of preference.
    Respects privacy filtering to skip sensitive windows.

    Returns:
        Tuple of (PIL Image or None if skipped/failed, active window title)
    """
    active_window = get_active_window_title()

    # Privacy check
    if is_privacy_sensitive(active_window):
        logger.info(f"Skipping privacy-sensitive window: {active_window[:50]}...")
        return None, active_window

    # Try capture methods in order
    for capture_fn in (
        _capture_with_scrot,
        _capture_with_gnome_screenshot,
        _capture_with_imagegrab,
    ):
        image = capture_fn()
        if image is not None:
            return image, active_window

    logger.warning("All screenshot capture methods failed")
    return None, active_window


def save_screenshot(image: Image.Image) -> Path:
    """
    Save screenshot to disk with timestamp.

    Resizes image according to config.SCREENSHOT_SCALE for storage efficiency.

    Args:
        image: PIL Image to save.

    Returns:
        Path to the saved screenshot file.
    """
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"screenshot_{timestamp}.png"
    filepath = config.SCREENSHOTS_DIR / filename

    # Resize for storage efficiency
    scale = config.SCREENSHOT_SCALE
    new_size = (int(image.width * scale), int(image.height * scale))
    resized = image.resize(new_size, Image.Resampling.LANCZOS)
    resized.save(filepath, "PNG", optimize=True)

    return filepath


def capture_and_save() -> tuple[Path | None, str]:
    """
    Main function: capture screenshot and save to disk.

    Returns:
        Tuple of (filepath or None if skipped/failed, active window title)
    """
    image, active_window = capture_screenshot()

    if image is None:
        return None, active_window

    filepath = save_screenshot(image)
    logger.info(f"Saved: {filepath.name} ({active_window[:40]})")

    return filepath, active_window


if __name__ == "__main__":
    filepath, window = capture_and_save()
    if filepath:
        print(f"Screenshot saved: {filepath}")
    else:
        print(f"Screenshot skipped (window: {window})")
