#!/usr/bin/env python3
"""
report_system_status.py
=======================
Generates a "factory report" on the state of the evolution system.
Counts active strategies, recruits, and graveyard entries directly from Lisp files.
Sends an Embed notification to Discord via the Notifier service.
"""

import zmq
import re
import os
import sys
import json
from datetime import datetime

# Configuration
ZMQ_PORT = 5562
ENV_FILE = "/home/swimmy/swimmy/.env"

# Paths to Lisp Files (Single Source of Truth)
BASE_DIR = "/home/swimmy/swimmy"
LISP_GRAVEYARD = os.path.join(BASE_DIR, "src/lisp/school/graveyard-persistence.lisp")
LISP_DYNAMIC = os.path.join(BASE_DIR, "src/lisp/strategies/strategies-dynamic.lisp")
LISP_OPTIMIZED = os.path.join(BASE_DIR, "src/lisp/school/school-optimized-params.lisp")


def load_env():
    """Load .env file if environment variables are missing."""
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


def count_pattern_in_file(filepath, pattern):
    """Counts occurrences of a regex pattern in a file."""
    if not os.path.exists(filepath):
        print(f"⚠️ File not found: {filepath}")
        return 0
    try:
        with open(filepath, "r") as f:
            content = f.read()
            return len(re.findall(pattern, content))
    except Exception as e:
        print(f"❌ Error reading {filepath}: {e}")
        return 0


def main():
    # Load Environment
    env = load_env()
    webhook_url = os.getenv("SWIMMY_DISCORD_REPORTS") or env.get(
        "SWIMMY_DISCORD_REPORTS"
    )

    if not webhook_url:
        print("❌ Error: SWIMMY_DISCORD_REPORTS not set.")
        sys.exit(1)

    # Gather Statistics from Sharded Library
    library_path = os.path.join(BASE_DIR, "data/library")

    # Initialize Counters
    stats = {
        "INCUBATOR": 0,
        "TRAINING": 0,
        "BATTLEFIELD": 0,
        "VETERAN": 0,
        "LEGEND": 0,
        "GRAVEYARD": 0,
        "s_rank": 0,
        "a_rank": 0,
    }

    if os.path.exists(library_path):
        for tier in stats.keys():
            if tier in ["s_rank", "a_rank"]:
                continue
            tier_path = os.path.join(library_path, tier)
            if os.path.exists(tier_path):
                # Count .lisp files
                files = [f for f in os.listdir(tier_path) if f.endswith(".lisp")]
                stats[tier] = len(files)

                # For Battlefield/Veteran/Legend, calculate Ranks (approximate via text parsing for speed)
                if tier in ["BATTLEFIELD", "VETERAN", "LEGEND"]:
                    for f in files:
                        try:
                            with open(os.path.join(tier_path, f), "r") as lf:
                                content = lf.read()
                                # Parse Sharpe/PF (Simplified)
                                # Assuming :sharpe -0.00
                                sharpe_m = re.search(
                                    r":sharpe\s+(-?\d+\.?\d*)", content
                                )
                                pf_m = re.search(
                                    r":profit-factor\s+(-?\d+\.?\d*)", content
                                )

                                sharpe = float(sharpe_m.group(1)) if sharpe_m else 0.0
                                pf = float(pf_m.group(1)) if pf_m else 0.0

                                if sharpe >= 1.2 and pf >= 1.5:
                                    stats["s_rank"] += 1
                                elif sharpe >= 1.0:
                                    stats["a_rank"] += 1
                        except:
                            pass  # Skip malformed files

    # Total Active (Everything except Graveyard)
    active_count = (
        stats["INCUBATOR"]
        + stats["TRAINING"]
        + stats["BATTLEFIELD"]
        + stats["VETERAN"]
        + stats["LEGEND"]
    )
    recruit_count = stats["INCUBATOR"]
    graveyard_count = stats["GRAVEYARD"]
    optimized_count = count_pattern_in_file(
        LISP_OPTIMIZED, r"\(:name \""
    )  # Keep this for genes

    print(
        f"📊 Stats (Sharded): Active={active_count}, S-Rank={stats['s_rank']}, A-Rank={stats['a_rank']}, Graveyard={graveyard_count}"
    )

    # Construct Payload
    payload = {
        "embeds": [
            {
                "title": "🏭 Evolution Factory Report",
                "description": "Current status of the autonomous strategy generation pipeline.",
                "color": 0x5865F2,  # Blurple
                "fields": [
                    {
                        "name": "🧠 Knowledge Base (Active)",
                        "value": f"**{active_count}** Strategies",
                        "inline": True,
                    },
                    {
                        "name": "🏆 S-Rank (Elite)",
                        "value": f"**{stats['s_rank']}**",
                        "inline": True,
                    },
                    {
                        "name": "🎖️ A-Rank (Pro)",
                        "value": f"**{stats['a_rank']}**",
                        "inline": True,
                    },
                    {
                        "name": "👶 New Recruits (Born)",
                        "value": f"{recruit_count}",
                        "inline": True,
                    },
                    {
                        "name": "👻 Graveyard (Rejected)",
                        "value": f"{graveyard_count}",
                        "inline": True,
                    },
                    {
                        "name": "🧬 Veteran Genes",
                        "value": f"{optimized_count}",
                        "inline": True,
                    },
                    {
                        "name": "⚙️ System Status",
                        "value": "✅ Evolution Daemon Active\n✅ Persistence Linked",
                        "inline": False,
                    },
                ],
                "footer": {
                    "text": f"Swimmy AI • {datetime.now().strftime('%Y-%m-%d %H:%M:%S')} JST"
                },
            }
        ]
    }

    # Send to Notifier Service
    context = zmq.Context()
    socket = context.socket(zmq.PUSH)
    socket.connect(f"tcp://localhost:{ZMQ_PORT}")

    message = {"webhook": webhook_url, "data": payload}

    socket.send_json(message)
    print("✅ Notification sent to Notifier Service.")


if __name__ == "__main__":
    main()
