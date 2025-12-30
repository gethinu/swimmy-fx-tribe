"""
AI Summarizer Module

Uses Gemini API to generate daily reports from journal logs.
Includes retry logic and Discord webhook integration.
"""

from __future__ import annotations

import datetime
import time
from pathlib import Path
from typing import Any

import requests

import config
from config import logger
from journal_storage import (
    get_entries_for_today,
    get_entries_for_date,
    get_daily_summary_stats,
)


def create_daily_prompt(entries: list[dict[str, Any]], stats: dict[str, Any]) -> str:
    """
    Create a prompt for AI summarization.

    Args:
        entries: List of journal entries.
        stats: Daily statistics dictionary.

    Returns:
        Formatted prompt string for Gemini API.
    """
    date_str = datetime.date.today().strftime("%Y年%m月%d日")

    # Format window activity summary
    window_summary = "\n".join(
        f"- {window}: {count}回"
        for window, count in stats.get("top_windows", {}).items()
    )

    # Format tag summary
    tag_summary = ", ".join(stats.get("top_tags", {}).keys())

    # Collect sample OCR texts for context
    sample_texts: list[str] = []
    for entry in entries[:20]:  # Sample first 20 entries
        ocr = entry.get("ocr_text", "")[:300]
        window = entry.get("active_window", "")
        if ocr:
            sample_texts.append(f"[{window}]\n{ocr}")

    prompt = f"""あなたは作業日報を生成するアシスタントです。
以下のスクリーンショットデータから、{date_str}の作業日報を生成してください。

## 基本情報
- 総キャプチャ数: {stats.get('total_captures', 0)}
- 開始時刻: {stats.get('first_capture', 'N/A')[:19]}
- 終了時刻: {stats.get('last_capture', 'N/A')[:19]}
- 主なタグ: {tag_summary if tag_summary else '(なし)'}

## アクティブウィンドウ（頻度順）
{window_summary if window_summary else '(データなし)'}

## スクリーンショットから抽出したテキスト（サンプル）
{chr(10).join(sample_texts[:10]) if sample_texts else '(OCRデータなし)'}

## 出力フォーマット
以下の形式でMarkdown日報を生成してください：

# {date_str} 作業日報

## 📊 サマリー
- 総作業時間: (開始-終了時刻から推定)
- メイン作業: (ウィンドウとOCRから推定)
- 生産性: (高/中/低)

## 📝 タイムライン
- 時間帯ごとの主な活動を箇条書き

## 💡 本日の学び・気づき
- OCRテキストから観察されたトピック

## 🎯 明日への提案
- 改善点や次にやるべきこと

日本語で出力してください。データが少ない場合は「データ不足のため推定」と明記してください。
"""
    return prompt


def call_gemini_api(prompt: str) -> str:
    """
    Call Gemini API with retry logic.

    Args:
        prompt: The prompt to send to Gemini.

    Returns:
        Generated text response, or error message.
    """
    if not config.GEMINI_API_KEY:
        return "Error: GEMINI_API_KEY not set in environment"

    url = (
        f"https://generativelanguage.googleapis.com/v1beta/models/"
        f"{config.GEMINI_MODEL}:generateContent"
    )

    headers = {"Content-Type": "application/json"}

    payload = {
        "contents": [{"parts": [{"text": prompt}]}],
        "generationConfig": {
            "temperature": config.GEMINI_TEMPERATURE,
            "maxOutputTokens": config.GEMINI_MAX_TOKENS,
        },
    }

    last_error: Exception | None = None

    for attempt in range(config.GEMINI_RETRY_COUNT):
        try:
            response = requests.post(
                f"{url}?key={config.GEMINI_API_KEY}",
                headers=headers,
                json=payload,
                timeout=60,
            )
            response.raise_for_status()

            data = response.json()
            candidates = data.get("candidates", [])
            if candidates:
                content = candidates[0].get("content", {})
                parts = content.get("parts", [])
                if parts:
                    return parts[0].get("text", "")

            return "Error: Empty response from Gemini API"

        except requests.RequestException as e:
            last_error = e
            if attempt < config.GEMINI_RETRY_COUNT - 1:
                logger.warning(f"Gemini API attempt {attempt + 1} failed: {e}")
                time.sleep(config.GEMINI_RETRY_DELAY * (attempt + 1))

    return f"Error calling Gemini API after {config.GEMINI_RETRY_COUNT} attempts: {last_error}"


def generate_daily_report(date: datetime.date | None = None) -> str:
    """
    Generate daily report for specified date.

    Args:
        date: Date to generate report for (default: today).

    Returns:
        Markdown-formatted daily report.
    """
    if date is None:
        date = datetime.date.today()
        entries = get_entries_for_today()
    else:
        entries = get_entries_for_date(date)

    date_str = date.strftime("%Y年%m月%d日")

    if not entries:
        return f"# {date_str} 作業日報\n\n📭 記録がありません。"

    stats = get_daily_summary_stats(entries)
    prompt = create_daily_prompt(entries, stats)

    logger.info(f"Generating report with {len(entries)} entries...")
    report = call_gemini_api(prompt)

    return report


def save_report(report: str, date: datetime.date | None = None) -> Path:
    """
    Save report to file.

    Args:
        report: Report content to save.
        date: Date for the report (default: today).

    Returns:
        Path to the saved report file.
    """
    if date is None:
        date = datetime.date.today()

    filename = f"report_{date.strftime('%Y-%m-%d')}.md"
    filepath = config.REPORTS_DIR / filename

    with open(filepath, "w", encoding="utf-8") as f:
        f.write(report)

    logger.info(f"Report saved: {filepath}")
    return filepath


def post_to_discord(report: str) -> bool:
    """
    Post report to Discord via webhook.

    Args:
        report: Report content to post.

    Returns:
        True if posted successfully, False otherwise.
    """
    if not config.DISCORD_DAILY_WEBHOOK:
        logger.debug("Discord webhook not configured")
        return False

    # Truncate for Discord's message limit
    max_len = config.DISCORD_MAX_MESSAGE_LENGTH - 200  # Reserve space for folder path
    if len(report) > max_len:
        report = report[:max_len] + "\n\n... (詳細はファイルを参照)"

    date_str = datetime.date.today().strftime("%Y-%m-%d")
    folder_path = r"\\wsl$\Ubuntu\home\swimmy\swimmy\auto_journal\data\screenshots"

    payload = {
        "content": f"**📓 Auto-Journal Daily Report**\n\n{report}\n\n📁 **Screenshots:** `{folder_path}`\n📅 **Date:** {date_str}"
    }

    try:
        response = requests.post(
            config.DISCORD_DAILY_WEBHOOK,
            json=payload,
            timeout=10,
        )
        response.raise_for_status()
        logger.info("Report posted to Discord")
        return True
    except requests.RequestException as e:
        logger.error(f"Discord post failed: {e}")
        return False


def generate_and_save_report(
    date: datetime.date | None = None,
    post_discord: bool = True,
) -> tuple[str, Path]:
    """
    Main function: generate, save, and optionally post report.

    Args:
        date: Date for the report (default: today).
        post_discord: Whether to post to Discord webhook.

    Returns:
        Tuple of (report content, saved file path).
    """
    report = generate_daily_report(date)
    filepath = save_report(report, date)

    if post_discord:
        post_to_discord(report)

    return report, filepath


if __name__ == "__main__":
    import sys

    no_discord = "--no-discord" in sys.argv
    report, path = generate_and_save_report(post_discord=not no_discord)

    print("\n" + "=" * 60)
    print(report)
    print("=" * 60)
    print(f"\nSaved to: {path}")
