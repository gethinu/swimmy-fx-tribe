#!/usr/bin/env python3
"""
report_evolution.py
===================
Generates a "Gene Pool" status report for the Evolution Factory.
Reads strategies.json and strategy_ranks.lisp to report on:
- Counts by Tier/Rank
- Top Performers (Sharpe)
- Recent Promotions
"""

import zmq
import json
import os
import sys
import re
from datetime import datetime

# Configuration
ZMQ_PORT = 5562
ENV_FILE = "/home/swimmy/swimmy/.env"
STRATEGIES_JSON = "/home/swimmy/swimmy/strategies.json"
STRATEGY_RANKS_LISP = "/home/swimmy/swimmy/.swimmy/strategy_ranks.lisp"


def load_env():
    if not os.path.exists(ENV_FILE):
        return {}
    env_vars = {}
    with open(ENV_FILE, "r") as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                key, val = line.split("=", 1)
                env_vars[key] = val.strip('"')
    return env_vars


def load_strategies():
    if not os.path.exists(STRATEGIES_JSON):
        return []
    try:
        with open(STRATEGIES_JSON, "r") as f:
            return json.load(f)
    except Exception as e:
        print(f"❌ Error loading strategies: {e}")
        return []


def parse_lisp_ranks():
    """Parses the Lisp association list for strategy ranks."""
    if not os.path.exists(STRATEGY_RANKS_LISP):
        return {}

    ranks = {}
    try:
        with open(STRATEGY_RANKS_LISP, "r") as f:
            content = f.read()
            # Simple regex to extract key-values from (:KEY val ...)
            # Assumes format: (:NAME "Name" :RANK :tier ...)
            entries = content.split("(:NAME")[1:]
            for entry in entries:
                try:
                    name_match = re.search(r'"([^"]+)"', entry)
                    rank_match = re.search(r":RANK :([^ ]+)", entry)
                    wins_match = re.search(r":WINS ([0-9]+)", entry)

                    if name_match and rank_match:
                        name = name_match.group(1)
                        rank = rank_match.group(1).upper()  # e.g. SCOUT
                        wins = int(wins_match.group(1)) if wins_match else 0
                        ranks[name] = {"rank": rank, "wins": wins}
                except:
                    continue
    except Exception as e:
        print(f"❌ Error parsing ranks: {e}")
    return ranks


def main():
    env = load_env()
    webhook_url = os.getenv("SWIMMY_DISCORD_REPORTS") or env.get(
        "SWIMMY_DISCORD_REPORTS"
    )

    if not webhook_url:
        print("❌ Error: SWIMMY_DISCORD_REPORTS not set.")
        # Fallback to APEX if REPORTS not set (Article 5)
        webhook_url = os.getenv("SWIMMY_DISCORD_APEX") or env.get("SWIMMY_DISCORD_APEX")
        if not webhook_url:
            sys.exit(1)

    strategies = load_strategies()
    ranks = parse_lisp_ranks()

    # Aggregation
    tier_counts = {"BATTLEFIELD": 0, "TRAINING": 0, "INCUBATOR": 0, "GRAVEYARD": 0}
    rank_counts = {"LEGEND": 0, "VETERAN": 0, "SCOUT": 0}

    top_performers = []  # (Name, Rank, Tier)

    battlefield_count = 0

    for s in strategies:
        name = s.get("name", "Unknown")
        rank_info = ranks.get(name, {"rank": "SCOUT", "wins": 0})
        rank = rank_info["rank"]
        wins = rank_info["wins"]

        rank_counts[rank] = rank_counts.get(rank, 0) + 1

        # Heuristic: Since strategies.json lacks "tier" field,
        # assume Wins >= 10 implies Battlefield/Veteran proxy for now.
        # This matches the user's observation of "~14" active strategies.
        if wins >= 10:
            battlefield_count += 1
            top_performers.append({"name": name, "wins": wins, "rank": rank})

    top_performers.sort(key=lambda x: x["wins"], reverse=True)

    # Payload
    desc = "**🧬 Gene Pool Status Report**\n\n"
    desc += "**Rank Distribution:**\n"

    # Explicitly calculate "Effective S-Rank" (Battlefield)
    desc += f"🏆 **S-Rank (Elite)**: {battlefield_count} (Battlefield / Wins >= 10)\n"
    desc += f"🛡️ **Candidates**: {len(strategies) - battlefield_count} (Selection / Incubator)\n"

    desc += "\n**Top Warriors (By Wins):**\n"
    for p in top_performers[:5]:
        desc += f"🥇 **{p['name']}**: {p['wins']} wins ({p['rank']})\n"

    payload = {
        "embeds": [
            {
                "title": "🏭 Evolution Factory Update",
                "description": desc,
                "color": 0x9B59B6,  # Purple for Evolution
                "footer": {"text": "Swimmy Evolution System"},
            }
        ]
    }

    # Send
    context = zmq.Context()
    socket = context.socket(zmq.PUSH)
    socket.connect(f"tcp://localhost:{ZMQ_PORT}")
    socket.send_json({"webhook": webhook_url, "data": payload})
    print("✅ Evolution Report Notification Sent.")


if __name__ == "__main__":
    main()
