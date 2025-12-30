#!/usr/bin/env python3
"""
Swimmy Auto-Journal - Main Entry Point

Screenshot → OCR → AI Daily Report Generation

Usage:
    python -m auto_journal.auto_journal capture     # Take one screenshot
    python -m auto_journal.auto_journal summarize   # Generate today's report
    python -m auto_journal.auto_journal daemon      # Run continuous capture
    python -m auto_journal.auto_journal status      # Show today's stats
    python -m auto_journal.auto_journal cleanup     # Clean old files
"""
from __future__ import annotations

import argparse
import time
import signal
import sys
from typing import NoReturn

import os
import sys
# Add parent directory to path for standalone execution
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import config
from config import logger
from screenshot_capture import capture_and_save
from ocr_processor import extract_text, extract_keywords
from journal_storage import (
    add_entry,
    get_entries_for_today,
    get_daily_summary_stats,
    cleanup_old_data,
)
from ai_summarizer import generate_and_save_report


# Global flag for daemon mode
_running = True


def _signal_handler(signum: int, frame: object) -> None:
    """Handle shutdown signal gracefully."""
    global _running
    logger.info("Shutting down...")
    _running = False


def cmd_capture() -> bool:
    """
    Take one screenshot and process it.

    Returns:
        True if capture was successful, False if skipped or failed.
    """
    logger.info("Capturing screenshot...")

    filepath, active_window = capture_and_save()

    if filepath is None:
        return False

    # OCR processing
    logger.info("Extracting text...")
    ocr_text = extract_text(filepath)
    tags = extract_keywords(ocr_text)

    # Store entry
    entry = add_entry(
        active_window=active_window,
        ocr_text=ocr_text,
        tags=tags,
        screenshot_path=str(filepath),
    )

    logger.info(
        f"Entry saved: window={active_window[:40]}, "
        f"ocr_chars={len(ocr_text)}, tags={tags}"
    )

    return True


def cmd_summarize(no_discord: bool = False) -> None:
    """Generate today's summary report."""
    logger.info("Generating daily report...")

    entries = get_entries_for_today()
    if not entries:
        logger.warning("No entries for today. Nothing to summarize.")
        return

    report, filepath = generate_and_save_report(post_discord=not no_discord)

    print("\n" + "=" * 60)
    print(report)
    print("=" * 60)
    print(f"\n[AutoJournal] Report saved: {filepath}")


def cmd_daemon() -> NoReturn | None:
    """Run continuous screenshot capture."""
    global _running

    signal.signal(signal.SIGINT, _signal_handler)
    signal.signal(signal.SIGTERM, _signal_handler)

    interval = config.CAPTURE_INTERVAL_SECONDS
    logger.info(f"Starting daemon mode (interval: {interval}s)")
    logger.info("Press Ctrl+C to stop")

    # Initial cleanup
    cleanup_old_data()

    capture_count = 0
    success_count = 0

    while _running:
        try:
            if cmd_capture():
                success_count += 1
            capture_count += 1

            # Progress report every 10 captures
            if capture_count % 10 == 0:
                logger.info(
                    f"Session progress: {success_count}/{capture_count} successful"
                )

            # Cleanup every hour (60 captures at 1-min interval)
            if capture_count % 60 == 0:
                cleanup_old_data()

            # Wait for next interval (check running flag each second)
            for _ in range(interval):
                if not _running:
                    break
                time.sleep(1)

        except Exception as e:
            logger.error(f"Capture error: {e}")
            time.sleep(10)  # Wait before retry

    logger.info(f"Daemon stopped. Total: {success_count}/{capture_count} successful")
    return None


def cmd_status() -> None:
    """Show today's statistics."""
    entries = get_entries_for_today()
    stats = get_daily_summary_stats(entries)

    print("\n" + "=" * 50)
    print("  📓 Auto-Journal Status")
    print("=" * 50)
    print(f"  Today's captures: {stats.get('total_captures', 0)}")

    first_capture = stats.get("first_capture", "")
    last_capture = stats.get("last_capture", "")
    if first_capture:
        print(f"  First capture: {first_capture[:19]}")
        print(f"  Last capture: {last_capture[:19] if last_capture else 'N/A'}")

    top_windows = stats.get("top_windows", {})
    if top_windows:
        print("\n  🖥️ Top Windows:")
        for window, count in list(top_windows.items())[:5]:
            print(f"    • {window}: {count}")

    top_tags = stats.get("top_tags", {})
    if top_tags:
        print("\n  🏷️ Top Tags:")
        for tag, count in list(top_tags.items())[:5]:
            print(f"    • {tag}: {count}")

    print("=" * 50 + "\n")


def cmd_cleanup() -> None:
    """Clean up old files."""
    deleted = cleanup_old_data()
    print(f"[AutoJournal] Deleted {deleted} old files")


def main() -> None:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Swimmy Auto-Journal System",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Commands:
  capture    Take one screenshot and process OCR
  summarize  Generate today's AI report
  daemon     Run continuous capture (every minute)
  status     Show today's statistics
  cleanup    Delete old files (>7 days)

Examples:
  ./run_journal.sh capture
  ./run_journal.sh summarize --no-discord
  ./run_journal.sh daemon
        """,
    )

    parser.add_argument(
        "command",
        choices=["capture", "summarize", "daemon", "status", "cleanup"],
        help="Command to execute",
    )
    parser.add_argument(
        "--no-discord",
        action="store_true",
        help="Don't post report to Discord (for summarize command)",
    )

    args = parser.parse_args()

    commands = {
        "capture": lambda: cmd_capture(),
        "summarize": lambda: cmd_summarize(no_discord=args.no_discord),
        "daemon": cmd_daemon,
        "status": cmd_status,
        "cleanup": cmd_cleanup,
    }

    commands[args.command]()


if __name__ == "__main__":
    main()
