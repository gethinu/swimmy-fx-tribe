# Auto-Journal Windows Native Capture
# This script runs directly on Windows to capture screenshots
# Requires: pip install Pillow pytesseract requests python-dotenv pywin32

import os
import sys
import json
import time
import signal
import datetime
import subprocess
from pathlib import Path
from typing import Any

# Try to import Windows-specific modules
try:
    from PIL import ImageGrab, Image
    import pytesseract
    import requests
    from dotenv import load_dotenv
except ImportError as e:
    print(f"Missing dependency: {e}")
    print("Install with: pip install Pillow pytesseract requests python-dotenv")
    sys.exit(1)

try:
    import win32gui

    HAS_WIN32 = True
except ImportError:
    HAS_WIN32 = False
    print("Note: pywin32 not installed, window titles won't be detected")
    print("Install with: pip install pywin32")

# Configuration
CAPTURE_INTERVAL = 60  # seconds
DATA_DIR = Path(__file__).parent / "auto_journal" / "data"
SCREENSHOTS_DIR = DATA_DIR / "screenshots"
LOGS_DIR = DATA_DIR / "logs"
REPORTS_DIR = DATA_DIR / "reports"

# Ensure directories exist
for d in (SCREENSHOTS_DIR, LOGS_DIR, REPORTS_DIR):
    d.mkdir(parents=True, exist_ok=True)

# Load environment
load_dotenv(Path(__file__).parent / ".env")
GEMINI_API_KEY = os.getenv("SWIMMY_GEMINI_API_KEY", "")
DISCORD_WEBHOOK = os.getenv("SWIMMY_DISCORD_JOURNAL", "")

# Privacy filter
PRIVACY_KEYWORDS = [
    "password",
    "ログイン",
    "1password",
    "lastpass",
    "bitwarden",
    "credential",
    "secret",
    "signin",
    "パスワード",
]

# Tesseract path (adjust if needed)
if os.name == "nt":
    tesseract_path = r"C:\Program Files\Tesseract-OCR\tesseract.exe"
    if os.path.exists(tesseract_path):
        pytesseract.pytesseract.tesseract_cmd = tesseract_path

running = True


def signal_handler(sig, frame):
    global running
    print("\n[AutoJournal] Shutting down...")
    running = False


signal.signal(signal.SIGINT, signal_handler)


def get_active_window_title() -> str:
    """Get the title of the currently active window (Windows)."""
    if not HAS_WIN32:
        return "Unknown"
    try:
        hwnd = win32gui.GetForegroundWindow()
        return win32gui.GetWindowText(hwnd) or "Unknown"
    except Exception:
        return "Unknown"


def is_privacy_sensitive(title: str) -> bool:
    """Check if window should be skipped for privacy."""
    title_lower = title.lower()
    return any(kw.lower() in title_lower for kw in PRIVACY_KEYWORDS)


def capture_screenshot() -> tuple[Image.Image | None, str]:
    """Capture screenshot using PIL ImageGrab (Windows native)."""
    title = get_active_window_title()

    if is_privacy_sensitive(title):
        print(f"[AutoJournal] Skipping privacy-sensitive: {title[:50]}")
        return None, title

    try:
        image = ImageGrab.grab()
        return image, title
    except Exception as e:
        print(f"[AutoJournal] Screenshot failed: {e}")
        return None, title


def save_screenshot(image: Image.Image) -> Path:
    """Save screenshot with timestamp."""
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    filepath = SCREENSHOTS_DIR / f"screenshot_{timestamp}.png"

    # Resize for storage
    new_size = (image.width // 2, image.height // 2)
    resized = image.resize(new_size, Image.Resampling.LANCZOS)
    resized.save(filepath, "PNG", optimize=True)

    return filepath


def extract_text(image: Image.Image) -> str:
    """Extract text using pytesseract."""
    try:
        text = pytesseract.image_to_string(image, lang="eng+jpn")
        return text.strip()
    except Exception as e:
        print(f"[AutoJournal] OCR failed: {e}")
        return ""


def get_today_log_path() -> Path:
    """Get log path for today."""
    today = datetime.date.today().strftime("%Y-%m-%d")
    return LOGS_DIR / f"journal_{today}.json"


def add_entry(title: str, ocr_text: str, screenshot: str) -> None:
    """Add entry to today's log."""
    log_path = get_today_log_path()

    entries = []
    if log_path.exists():
        try:
            entries = json.loads(log_path.read_text(encoding="utf-8"))
        except json.JSONDecodeError:
            entries = []

    entry = {
        "timestamp": datetime.datetime.now().isoformat(),
        "active_window": title,
        "ocr_text": ocr_text[:2000],
        "tags": [],
        "screenshot": str(screenshot),
    }
    entries.append(entry)

    log_path.write_text(
        json.dumps(entries, ensure_ascii=False, indent=2), encoding="utf-8"
    )


def capture_once() -> bool:
    """Perform one capture cycle."""
    print("[AutoJournal] Capturing...")

    image, title = capture_screenshot()
    if image is None:
        return False

    filepath = save_screenshot(image)
    print(f"[AutoJournal] Saved: {filepath.name}")

    ocr_text = extract_text(image)
    print(f"[AutoJournal] OCR: {len(ocr_text)} chars")

    add_entry(title, ocr_text, str(filepath))
    print(f"[AutoJournal] Entry added ({title[:40]})")

    return True


def daemon() -> None:
    """Run continuous capture."""
    print(f"[AutoJournal] Starting daemon (interval: {CAPTURE_INTERVAL}s)")
    print("[AutoJournal] Press Ctrl+C to stop")

    count = 0
    last_summary_date = None

    while running:
        try:
            if capture_once():
                count += 1

            if count % 10 == 0 and count > 0:
                print(f"[AutoJournal] Total captures: {count}")

            # Auto-summarize at 23:00
            now = datetime.datetime.now()
            today = now.date()
            if now.hour == 23 and now.minute < 2 and last_summary_date != today:
                print("[AutoJournal] Generating daily summary...")
                summarize()
                last_summary_date = today

            for _ in range(CAPTURE_INTERVAL):
                if not running:
                    break
                time.sleep(1)
        except Exception as e:
            print(f"[AutoJournal] Error: {e}")
            time.sleep(10)

    print(f"[AutoJournal] Stopped. Total: {count}")


def load_today_entries() -> list[dict]:
    """Load today's log entries."""
    log_path = get_today_log_path()
    if log_path.exists():
        try:
            return json.loads(log_path.read_text(encoding="utf-8"))
        except json.JSONDecodeError:
            return []
    return []


def get_daily_stats(entries: list[dict]) -> dict:
    """Generate statistics from entries."""
    if not entries:
        return {"total": 0, "windows": {}, "first": "", "last": ""}

    windows = {}
    for e in entries:
        w = e.get("active_window", "Unknown").split(" - ")[0][:30]
        windows[w] = windows.get(w, 0) + 1

    top_windows = dict(sorted(windows.items(), key=lambda x: -x[1])[:10])

    return {
        "total": len(entries),
        "windows": top_windows,
        "first": entries[0].get("timestamp", "")[:19],
        "last": entries[-1].get("timestamp", "")[:19],
    }


def call_gemini(prompt: str) -> str:
    """Call Gemini API."""
    if not GEMINI_API_KEY:
        return "Error: GEMINI_API_KEY not set"

    url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent"

    try:
        response = requests.post(
            f"{url}?key={GEMINI_API_KEY}",
            headers={"Content-Type": "application/json"},
            json={
                "contents": [{"parts": [{"text": prompt}]}],
                "generationConfig": {"temperature": 0.7, "maxOutputTokens": 2000},
            },
            timeout=60,
        )
        response.raise_for_status()
        data = response.json()
        return (
            data.get("candidates", [{}])[0]
            .get("content", {})
            .get("parts", [{}])[0]
            .get("text", "")
        )
    except Exception as e:
        return f"Error: {e}"


def generate_report(entries: list[dict], stats: dict) -> str:
    """Generate report using Gemini."""
    date_str = datetime.date.today().strftime("%Y年%m月%d日")

    window_summary = "\n".join(f"- {w}: {c}回" for w, c in stats["windows"].items())

    prompt = f"""あなたは作業日報を生成するアシスタントです。
{date_str}の作業日報を生成してください。

## 基本情報
- 総キャプチャ数: {stats['total']}
- 開始時刻: {stats['first']}
- 終了時刻: {stats['last']}

## アクティブウィンドウ（頻度順）
{window_summary if window_summary else '(なし)'}

## 出力フォーマット
# {date_str} 作業日報

## 📊 サマリー
- 総作業時間: (推定)
- メイン作業: (推定)
- 生産性: (高/中/低)

## 📝 タイムライン
- 時間帯ごとの活動

## 🎯 明日への提案
- 改善点

簡潔に日本語で出力してください。
"""
    return call_gemini(prompt)


def post_to_discord(report: str) -> bool:
    """Post report to Discord with folder link."""
    if not DISCORD_WEBHOOK:
        print("[AutoJournal] Discord webhook not configured")
        return False

    date_str = datetime.date.today().strftime("%Y-%m-%d")
    folder_path = (
        f"\\\\wsl$\\Ubuntu\\home\\swimmy\\swimmy\\auto_journal\\data\\screenshots"
    )

    # Truncate report if too long
    if len(report) > 1500:
        report = report[:1500] + "\n... (続きは日報ファイルを参照)"

    message = f"""**📓 Auto-Journal Daily Report**

{report}

📁 **Screenshots:** `{folder_path}`
📅 **Date:** {date_str}
"""

    try:
        response = requests.post(
            DISCORD_WEBHOOK,
            json={"content": message},
            timeout=10,
        )
        response.raise_for_status()
        print("[AutoJournal] Posted to Discord")
        return True
    except Exception as e:
        print(f"[AutoJournal] Discord failed: {e}")
        return False


def summarize() -> None:
    """Generate and post daily summary."""
    print("[AutoJournal] Generating daily summary...")

    entries = load_today_entries()
    if not entries:
        print("[AutoJournal] No entries for today")
        return

    stats = get_daily_stats(entries)
    print(f"[AutoJournal] Processing {stats['total']} entries...")

    report = generate_report(entries, stats)

    # Save report
    date_str = datetime.date.today().strftime("%Y-%m-%d")
    report_path = REPORTS_DIR / f"report_{date_str}.md"
    report_path.write_text(report, encoding="utf-8")
    print(f"[AutoJournal] Report saved: {report_path}")

    # Post to Discord
    post_to_discord(report)

    print("[AutoJournal] Summary complete!")
    print("\n" + "=" * 50)
    print(report)
    print("=" * 50)


def main():
    if len(sys.argv) < 2:
        print("Usage: python auto_journal_windows.py [capture|daemon|summarize]")
        return

    cmd = sys.argv[1]
    if cmd == "capture":
        capture_once()
    elif cmd == "daemon":
        daemon()
    elif cmd == "summarize":
        summarize()
    else:
        print(f"Unknown command: {cmd}")


if __name__ == "__main__":
    main()
