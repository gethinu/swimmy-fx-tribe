"""
Journal Storage Module

Manages JSON log files for daily activities.
Provides entry creation, retrieval, statistics, and automatic cleanup.
"""

from __future__ import annotations

import json
import datetime
from dataclasses import dataclass, asdict, field
from pathlib import Path
from typing import Any

import config
from config import logger


@dataclass
class JournalEntry:
    """Represents a single journal entry with timestamp and activity data."""

    timestamp: str
    active_window: str
    ocr_text: str = ""
    tags: list[str] = field(default_factory=list)
    screenshot: str | None = None

    def to_dict(self) -> dict[str, Any]:
        """Convert entry to dictionary for JSON serialization."""
        return asdict(self)

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> JournalEntry:
        """Create entry from dictionary."""
        return cls(
            timestamp=data.get("timestamp", ""),
            active_window=data.get("active_window", "Unknown"),
            ocr_text=data.get("ocr_text", ""),
            tags=data.get("tags", []),
            screenshot=data.get("screenshot"),
        )


def get_log_path_for_date(date: datetime.date) -> Path:
    """Get the log file path for a specific date."""
    return config.LOGS_DIR / f"journal_{date.strftime('%Y-%m-%d')}.json"


def get_today_log_path() -> Path:
    """Get the log file path for today."""
    return get_log_path_for_date(datetime.date.today())


def load_entries(log_path: Path) -> list[dict[str, Any]]:
    """
    Load entries from a log file.

    Args:
        log_path: Path to the JSON log file.

    Returns:
        List of entry dictionaries.
    """
    if not log_path.exists():
        return []
    try:
        with open(log_path, "r", encoding="utf-8") as f:
            data = json.load(f)
            return data if isinstance(data, list) else []
    except (json.JSONDecodeError, IOError) as e:
        logger.warning(f"Failed to load {log_path.name}: {e}")
        return []


def save_entries(entries: list[dict[str, Any]], log_path: Path) -> None:
    """
    Save entries to a log file.

    Args:
        entries: List of entry dictionaries.
        log_path: Path to save to.
    """
    with open(log_path, "w", encoding="utf-8") as f:
        json.dump(entries, f, ensure_ascii=False, indent=2)


def load_today_log() -> list[dict[str, Any]]:
    """Load today's log entries."""
    return load_entries(get_today_log_path())


def save_log(entries: list[dict[str, Any]]) -> None:
    """Save log entries to today's file."""
    save_entries(entries, get_today_log_path())


def add_entry(
    active_window: str,
    ocr_text: str,
    tags: list[str] | None = None,
    screenshot_path: str | None = None,
) -> JournalEntry:
    """
    Add a new log entry for the current time.

    Args:
        active_window: Title of the active window.
        ocr_text: Extracted OCR text (will be truncated to OCR_MAX_TEXT_LENGTH).
        tags: List of activity tags.
        screenshot_path: Path to the saved screenshot.

    Returns:
        The created JournalEntry.
    """
    # Truncate OCR text to prevent oversized entries
    truncated_text = ocr_text[: config.OCR_MAX_TEXT_LENGTH] if ocr_text else ""

    entry = JournalEntry(
        timestamp=datetime.datetime.now().isoformat(),
        active_window=active_window,
        ocr_text=truncated_text,
        tags=tags or [],
        screenshot=str(screenshot_path) if screenshot_path else None,
    )

    entries = load_today_log()
    entries.append(entry.to_dict())
    save_log(entries)

    return entry


def get_entries_for_date(date: datetime.date) -> list[dict[str, Any]]:
    """Get all entries for a specific date."""
    return load_entries(get_log_path_for_date(date))


def get_entries_for_today() -> list[dict[str, Any]]:
    """Get all entries for today."""
    return load_today_log()


def cleanup_old_data() -> int:
    """
    Delete screenshots and logs older than retention period.

    Returns:
        Number of deleted files.
    """
    cutoff_date = datetime.date.today() - datetime.timedelta(
        days=config.SCREENSHOT_RETENTION_DAYS
    )
    deleted_count = 0

    # Clean up old logs
    for log_file in config.LOGS_DIR.glob("journal_*.json"):
        try:
            date_str = log_file.stem.replace("journal_", "")
            file_date = datetime.datetime.strptime(date_str, "%Y-%m-%d").date()
            if file_date < cutoff_date:
                log_file.unlink()
                deleted_count += 1
                logger.debug(f"Deleted old log: {log_file.name}")
        except ValueError:
            continue

    # Clean up old screenshots
    for screenshot in config.SCREENSHOTS_DIR.glob("screenshot_*.png"):
        try:
            # Extract date from filename: screenshot_YYYYMMDD_HHMMSS.png
            date_str = screenshot.stem.split("_")[1]
            file_date = datetime.datetime.strptime(date_str, "%Y%m%d").date()
            if file_date < cutoff_date:
                screenshot.unlink()
                deleted_count += 1
                logger.debug(f"Deleted old screenshot: {screenshot.name}")
        except (ValueError, IndexError):
            continue

    if deleted_count > 0:
        logger.info(f"Cleaned up {deleted_count} old files")

    return deleted_count


@dataclass
class DailyStats:
    """Statistics for a day's journal entries."""

    total_captures: int
    first_capture: str
    last_capture: str
    top_windows: dict[str, int]
    top_tags: dict[str, int]

    def to_dict(self) -> dict[str, Any]:
        return asdict(self)


def get_daily_summary_stats(entries: list[dict[str, Any]]) -> dict[str, Any]:
    """
    Generate statistics from log entries.

    Args:
        entries: List of journal entries.

    Returns:
        Dictionary with capture count, time range, top windows, and top tags.
    """
    if not entries:
        return DailyStats(
            total_captures=0,
            first_capture="",
            last_capture="",
            top_windows={},
            top_tags={},
        ).to_dict()

    windows: dict[str, int] = {}
    tags: dict[str, int] = {}

    for entry in entries:
        # Count windows (simplify title by taking first segment)
        window = entry.get("active_window", "Unknown")
        window_simple = window.split(" - ")[0][:30] if window else "Unknown"
        windows[window_simple] = windows.get(window_simple, 0) + 1

        # Count tags
        for tag in entry.get("tags", []):
            tags[tag] = tags.get(tag, 0) + 1

    # Sort by frequency and take top 10
    top_windows = dict(sorted(windows.items(), key=lambda x: -x[1])[:10])
    top_tags = dict(sorted(tags.items(), key=lambda x: -x[1])[:10])

    # Get time range
    timestamps = [e.get("timestamp", "") for e in entries if e.get("timestamp")]
    first_time = timestamps[0] if timestamps else ""
    last_time = timestamps[-1] if timestamps else ""

    return DailyStats(
        total_captures=len(entries),
        first_capture=first_time,
        last_capture=last_time,
        top_windows=top_windows,
        top_tags=top_tags,
    ).to_dict()


if __name__ == "__main__":
    entries = get_entries_for_today()
    stats = get_daily_summary_stats(entries)
    print(f"Today's entries: {stats['total_captures']}")
    print(f"Top windows: {stats['top_windows']}")
    print(f"Top tags: {stats['top_tags']}")
