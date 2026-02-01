#!/usr/bin/env python3
"""
Swimmy Discord Bot - Bidirectional communication with Swimmy trading system
Version 1.0

Commands:
  swimmy status / swimmy 状況 - Get current status
  swimmy goal / swimmy 目標 - Get goal progress
  swimmy market / swimmy 相場 - Get market status
  swimmy help - Show help
"""

import discord
from discord.ext import commands
import aiohttp
import asyncio
import os
import json
from datetime import datetime
from pathlib import Path

# Bot configuration - token MUST be set via environment variable
TOKEN = os.environ.get("SWIMMY_DISCORD_BOT_TOKEN")
if not TOKEN:
    raise ValueError("SWIMMY_DISCORD_BOT_TOKEN environment variable is required")

def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = str(resolve_base_dir())
SWIMMY_QUERY_FILE = os.path.join(BASE_DIR, ".opus", "query.txt")
SWIMMY_RESPONSE_FILE = os.path.join(BASE_DIR, ".opus", "response.txt")
SWIMMY_STATUS_FILE = os.path.join(BASE_DIR, ".opus", "live_status.json")

# Singleton check
import fcntl
import sys

PID_FILE = "/tmp/swimmy_discord_bot.pid"
fp = open(PID_FILE, "w")
try:
    fcntl.lockf(fp, fcntl.LOCK_EX | fcntl.LOCK_NB)
    fp.write(str(os.getpid()))
    fp.flush()
except IOError:
    print("❌ Another instance is already running. Exiting.")
    sys.exit(0)

# Bot setup with intents
intents = discord.Intents.default()
intents.message_content = True
bot = commands.Bot(command_prefix="swimmy ", intents=intents)

# Status cache (updated by Swimmy)
status_cache = {
    "daily_pnl": 0,
    "accumulated_pnl": 0,
    "goal_progress": 0,
    "regime": "UNKNOWN",
    "volatility": "UNKNOWN",
    "leader": "UNKNOWN",
    "danger_level": 0,
    "last_updated": None,
}


def load_status():
    """Load status from Swimmy's status file"""
    global status_cache
    try:
        if os.path.exists(SWIMMY_STATUS_FILE):
            with open(SWIMMY_STATUS_FILE, "r") as f:
                status_cache = json.load(f)
    except Exception as e:
        print(f"Could not load status: {e}")


def format_status():
    """Format status for Discord"""
    load_status()
    return f"""🐟 **Swimmy Status**
━━━━━━━━━━━━━━━━━
📊 **Daily PnL**: ¥{status_cache.get('daily_pnl', 0):,.0f}
💰 **Total PnL**: ¥{status_cache.get('accumulated_pnl', 0):,.0f}
🎯 **Goal**: {status_cache.get('goal_progress', 0):.1f}%
📈 **Regime**: {status_cache.get('regime', 'UNKNOWN')}
⚡ **Volatility**: {status_cache.get('volatility', 'UNKNOWN')}
👑 **Leader**: {status_cache.get('leader', 'UNKNOWN')}
⚠️ **Danger**: {status_cache.get('danger_level', 0)}
━━━━━━━━━━━━━━━━━
🕐 Updated: {datetime.now().strftime('%H:%M:%S')}"""


def format_goal():
    """Format goal progress for Discord"""
    load_status()
    monthly_goal = status_cache.get("monthly_goal", 100000)
    accumulated = status_cache.get("accumulated_pnl", 0)
    progress = status_cache.get("goal_progress", 0)
    remaining = monthly_goal - accumulated

    return f"""🎯 **Goal Progress**
━━━━━━━━━━━━━━━━━
💎 **Monthly Target**: ¥{monthly_goal:,.0f}
� **Current**: ¥{accumulated:,.0f}
📈 **Progress**: {progress:.1f}%
📉 **Remaining**: ¥{remaining:,.0f}
━━━━━━━━━━━━━━━━━"""


def format_market():
    """Format market status for Discord"""
    load_status()
    return f"""📈 **Market Status**
━━━━━━━━━━━━━━━━━
🌊 **Regime**: {status_cache.get('regime', 'UNKNOWN')}
⚡ **Volatility**: {status_cache.get('volatility', 'UNKNOWN')}
👑 **Leader**: {status_cache.get('leader', 'UNKNOWN')}
🐟 **Ecosystem Health**: {status_cache.get('ecosystem_health', 0):.0f}%
━━━━━━━━━━━━━━━━━"""


def format_help():
    """Format help message"""
    return """🐟 **Swimmy Bot Commands**
━━━━━━━━━━━━━━━━━
`swimmy status` / `swimmy 状況` - 現在の状態
`swimmy goal` / `swimmy 目標` - 目標進捗
`swimmy market` / `swimmy 相場` - 市場状況
`swimmy tribe` / `swimmy 部族` - 4部族の意見
`swimmy progress` / `swimmy 進捗` - Warmup進捗
`swimmy all` / `swimmy 全部` - 全部まとめて表示
`swimmy help` - このヘルプ
━━━━━━━━━━━━━━━━━
📚 **arXiv Scout**
`arxiv add <番号>` - 論文をストックに追加
`arxiv list` - 今日のレポート
━━━━━━━━━━━━━━━━━
🤖 Powered by Opus AI Partnership"""


def format_tribes():
    """Format tribe status for Discord"""
    load_status()
    tribes = status_cache.get("tribes", {})
    consensus = status_cache.get("tribe_consensus", {})

    # Emoji mapping
    emojis = {"hunters": "🏹", "shamans": "🔮", "breakers": "⚔️", "raiders": "🗡️"}
    names = {
        "hunters": "Hunters (追跡者)",
        "shamans": "Shamans (呪術師)",
        "breakers": "Breakers (破壊者)",
        "raiders": "Raiders (盗賊)",
    }

    msg = """🏛️ **Four Great Clans Status**
━━━━━━━━━━━━━━━━━\n"""

    for clan in ["hunters", "shamans", "breakers", "raiders"]:
        info = tribes.get(clan, {})
        direction = info.get("direction", "HOLD").upper()
        confidence = info.get("confidence", 0)
        reason = info.get("reason", "-")
        emoji = emojis.get(clan, "🏛️")
        name = names.get(clan, clan)

        # Direction color emoji
        dir_emoji = (
            "🟢" if direction == "BUY" else ("🔴" if direction == "SELL" else "⚪")
        )

        msg += f"{emoji} **{name}**\n"
        msg += f"   {dir_emoji} {direction} ({confidence:.0f}%)\n"
        msg += f"   💡 {reason}\n\n"

    # Consensus
    cons_dir = consensus.get("direction", "HOLD").upper()
    cons_str = consensus.get("strength", 0)
    cons_emoji = "🟢" if cons_dir == "BUY" else ("🔴" if cons_dir == "SELL" else "⚪")

    msg += f"━━━━━━━━━━━━━━━━━\n"
    msg += f"🏛️ **Consensus**: {cons_emoji} {cons_dir} ({cons_str:.0f}%)"

    return msg


def format_progress():
    """Format warmup progress for Discord"""
    load_status()
    total = status_cache.get("total_trades", 0)
    warmup_pct = status_cache.get("warmup_progress", 0)
    warmup_done = status_cache.get("warmup_complete", False)

    # Progress bar
    filled = int(warmup_pct / 10)
    bar = "█" * filled + "░" * (10 - filled)

    if warmup_done:
        status = "✅ Warmup Complete! Full mode active."
    else:
        remaining = 50 - total
        status = f"⏳ {remaining} trades until full mode"

    return f"""📊 **Warmup Progress**
━━━━━━━━━━━━━━━━━
🔄 **Trades**: {total} / 50
📈 **Progress**: [{bar}] {warmup_pct}%
{status}
━━━━━━━━━━━━━━━━━
💡 First 50 trades = learning mode
   After 50 = full autonomous trading"""


# =============================================================================
# arXiv Scout連携機能
# =============================================================================

ARXIV_DATA_DIR = "/home/swimmy/arxiv-scout/data"
LAST_REPORT_FILE = f"{ARXIV_DATA_DIR}/last_report.json"
STOCK_PAPERS_FILE = f"{ARXIV_DATA_DIR}/stock_papers.json"
ARXIV_REPORT_WEBHOOK = (
    os.getenv("SWIMMY_ARXIV_REPORT_WEBHOOK", "").strip().strip('"').strip("'")
)


def mask_webhook(url: str) -> str:
    if not url:
        return "unset"
    tail = url[-6:] if len(url) > 6 else url
    return f"...{tail}"


async def send_daily_arxiv_report(webhook_url: str) -> None:
    """Send arXiv report to the configured webhook once per day at 08:00."""
    await bot.wait_until_ready()
    last_sent_date = None

    try:
        async with aiohttp.ClientSession() as session:
            webhook = discord.Webhook.from_url(webhook_url, session=session)
            while not bot.is_closed():
                now = datetime.now()
                if now.hour == 8 and now.minute == 0:
                    today = now.date().isoformat()
                    if last_sent_date != today:
                        report_msg = arxiv_show_list()
                        try:
                            await webhook.send(report_msg)
                            last_sent_date = today
                        except Exception as e:
                            print(f"❌ Failed to send arXiv report webhook: {e}")

                    await asyncio.sleep(60)
                else:
                    await asyncio.sleep(20)
    except Exception as e:
        print(f"❌ Invalid arXiv report webhook ({mask_webhook(webhook_url)}): {e}")


def arxiv_add_to_stock(number: int) -> str:
    """番号指定で論文をストックに追加"""
    try:
        # last_report.json読み込み
        if not os.path.exists(LAST_REPORT_FILE):
            return "❌ レポートがありません。まず `arxiv list` で確認してください。"

        with open(LAST_REPORT_FILE, "r", encoding="utf-8") as f:
            report = json.load(f)

        papers = report.get("papers", [])
        target = None
        for p in papers:
            if p.get("number") == number:
                target = p
                break

        if not target:
            return f"❌ 番号 {number} の論文が見つかりません。(1-{len(papers)}の範囲で指定)"

        # stock_papers.json読み込み・追加
        stock_data = {"papers": [], "last_updated": None}
        if os.path.exists(STOCK_PAPERS_FILE):
            with open(STOCK_PAPERS_FILE, "r", encoding="utf-8") as f:
                stock_data = json.load(f)

        stock_papers = stock_data.get("papers", [])
        stock_ids = {p.get("id") for p in stock_papers}

        if target["id"] in stock_ids:
            return f"⚠️ 既にストック済み: **{target['title'][:40]}...**"

        # 追加
        stock_papers.append(
            {
                "id": target["id"],
                "title": target["title"],
                "link": target["link"],
                "score": target.get("score"),
                "key_tech": target.get("key_tech"),
                "how_to_use": target.get("how_to_use"),
                "stocked_at": datetime.now().isoformat(),
                "added_via": "discord",
            }
        )

        stock_data["papers"] = stock_papers
        stock_data["last_updated"] = datetime.now().isoformat()

        with open(STOCK_PAPERS_FILE, "w", encoding="utf-8") as f:
            json.dump(stock_data, f, ensure_ascii=False, indent=2)

        return f"""✅ **ストックに追加しました！**
━━━━━━━━━━━━━━━━━
📄 **{target['title'][:50]}...**
🔧 技術: {target.get('key_tech', '-')}
📊 スコア: {target.get('score', '?')}/10
🔗 <{target['link']}>"""

    except Exception as e:
        return f"❌ エラー: {e}"


def arxiv_show_list() -> str:
    """今日のレポート一覧を表示"""
    try:
        if not os.path.exists(LAST_REPORT_FILE):
            return "❌ レポートがありません。"

        with open(LAST_REPORT_FILE, "r", encoding="utf-8") as f:
            report = json.load(f)

        papers = report.get("papers", [])
        if not papers:
            return "📭 レポートに論文がありません。"

        msg = f"📚 **arXiv Scout レポート** ({report.get('date', '?')})\n━━━━━━━━━━━━━━━━━\n"
        for p in papers[:10]:
            num = p.get("number", "?")
            score = p.get("score", "?")
            title = p.get("title", "")[:35]
            msg += f"**{num}.** [{score}/10] {title}...\n"

        msg += "━━━━━━━━━━━━━━━━━\n💡 `arxiv add <番号>` でストック追加"
        return msg

    except Exception as e:
        return f"❌ エラー: {e}"


def arxiv_show_stock() -> str:
    """ストック論文一覧を表示"""
    try:
        if not os.path.exists(STOCK_PAPERS_FILE):
            return "📭 ストックが空です。"

        with open(STOCK_PAPERS_FILE, "r", encoding="utf-8") as f:
            stock_data = json.load(f)

        papers = stock_data.get("papers", [])
        if not papers:
            return "📭 ストックが空です。"

        msg = f"📦 **ストック論文** ({len(papers)}件)\n━━━━━━━━━━━━━━━━━\n"
        for i, p in enumerate(papers[-5:], 1):  # 最新5件
            score = p.get("score", "?")
            title = p.get("title", "")[:30]
            msg += f"**{i}.** [{score}/10] {title}...\n"

        msg += "━━━━━━━━━━━━━━━━━"
        return msg

    except Exception as e:
        return f"❌ エラー: {e}"


@bot.event
async def on_ready():
    print(f"🐟 Swimmy Bot connected as {bot.user}")
    print(f"🌐 Serving {len(bot.guilds)} guild(s)")
    if ARXIV_REPORT_WEBHOOK:
        bot.loop.create_task(send_daily_arxiv_report(ARXIV_REPORT_WEBHOOK))
        print(
            "📚 arXiv daily report scheduler enabled "
            f"(webhook={mask_webhook(ARXIV_REPORT_WEBHOOK)})."
        )
    else:
        print("⚠️ arXiv daily report scheduler disabled (no webhook set).")


@bot.event
async def on_message(message):
    # Ignore own messages
    if message.author == bot.user:
        return

    content = message.content.lower().strip()

    # Check for swimmy commands
    if content.startswith("swimmy "):
        query = content[7:].strip()

        if query in ["status", "状況", "どう", "ステータス"]:
            await message.channel.send(format_status())

        elif query in ["goal", "目標", "いくら"]:
            await message.channel.send(format_goal())

        elif query in ["market", "相場", "マーケット"]:
            await message.channel.send(format_market())

        elif query in ["tribe", "部族", "tribes", "クラン", "clan"]:
            await message.channel.send(format_tribes())

        elif query in ["progress", "進捗", "warmup", "ウォームアップ", "トレード数"]:
            await message.channel.send(format_progress())

        elif query in ["all", "全部", "すべて", "まとめ"]:
            await message.channel.send(format_status())
            await message.channel.send(format_goal())
            await message.channel.send(format_market())
            await message.channel.send(format_tribes())
            await message.channel.send(format_progress())

        elif query in ["help", "ヘルプ", "?"]:
            await message.channel.send(format_help())

        else:
            # Unknown command
            await message.channel.send(
                f"❓ Unknown command: `{query}`\nType `swimmy help` for available commands."
            )

    # Also respond to just "swimmy" or mentions
    elif content == "swimmy" or bot.user.mentioned_in(message):
        await message.channel.send(format_status())

    # ===== arXiv Scout連携コマンド =====
    elif content.startswith("arxiv "):
        query = content[6:].strip()

        # arxiv add <番号> または arxiv <番号>番追加
        if query.startswith("add ") or "追加" in query:
            # 番号を抽出
            import re

            numbers = re.findall(r"\d+", query)
            if numbers:
                num = int(numbers[0])
                result = arxiv_add_to_stock(num)
                await message.channel.send(result)
            else:
                await message.channel.send(
                    "❓ 番号を指定してください。例: `arxiv add 1`"
                )

        elif query in ["list", "リスト", "一覧"]:
            result = arxiv_show_list()
            await message.channel.send(result)

        elif query in ["stock", "ストック"]:
            result = arxiv_show_stock()
            await message.channel.send(result)

        elif query in ["help", "ヘルプ"]:
            await message.channel.send(
                """📚 **arXiv Scout Commands**
━━━━━━━━━━━━━━━━━
`arxiv add <番号>` - 論文をストックに追加
`arxiv list` - 今日のレポート一覧
`arxiv stock` - ストック論文一覧
`arxiv help` - このヘルプ
━━━━━━━━━━━━━━━━━"""
            )

        else:
            # 数字だけの場合も追加として処理
            import re

            numbers = re.findall(r"^\d+$", query)
            if numbers:
                result = arxiv_add_to_stock(int(numbers[0]))
                await message.channel.send(result)
            else:
                await message.channel.send(
                    "❓ 不明なコマンド。`arxiv help` で使い方を確認"
                )

    # 日本語で「1番追加」「2番ストック」
    elif "番追加" in content or "番ストック" in content:
        import re

        numbers = re.findall(r"\d+", content)
        if numbers:
            result = arxiv_add_to_stock(int(numbers[0]))
            await message.channel.send(result)


def main():
    print("🐟 Starting Swimmy Discord Bot...")
    print(f"📁 Status file: {SWIMMY_STATUS_FILE}")

    try:
        bot.run(TOKEN)
    except discord.LoginFailure:
        print("❌ Invalid token! Please check SWIMMY_DISCORD_BOT_TOKEN")
    except Exception as e:
        print(f"❌ Error: {e}")


if __name__ == "__main__":
    main()
