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
import asyncio
import os
import json
from datetime import datetime

# Bot configuration - token MUST be set via environment variable
TOKEN = os.environ.get("SWIMMY_DISCORD_BOT_TOKEN")
if not TOKEN:
    raise ValueError("SWIMMY_DISCORD_BOT_TOKEN environment variable is required")
SWIMMY_QUERY_FILE = "/home/swimmy/swimmy/.opus/query.txt"
SWIMMY_RESPONSE_FILE = "/home/swimmy/swimmy/.opus/response.txt"
SWIMMY_STATUS_FILE = "/home/swimmy/swimmy/.opus/live_status.json"

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
`swimmy all` / `swimmy 全部` - 全部まとめて表示
`swimmy help` - このヘルプ
━━━━━━━━━━━━━━━━━
🤖 Powered by Opus AI Partnership"""


@bot.event
async def on_ready():
    print(f"🐟 Swimmy Bot connected as {bot.user}")
    print(f"🌐 Serving {len(bot.guilds)} guild(s)")


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

        elif query in ["all", "全部", "すべて", "まとめ"]:
            await message.channel.send(format_status())
            await message.channel.send(format_goal())
            await message.channel.send(format_market())

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
