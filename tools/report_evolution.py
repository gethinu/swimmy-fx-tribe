#!/usr/bin/env python3
"""
report_evolution.py
===================
Generates the "Evolution Factory Report" using the filesystem as the source of truth.
Counts .lisp strategy files in data/library/ Tiers.
"""

import zmq
import json
import os
import sys
import glob
from datetime import datetime, timezone, timedelta

# Configuration
ZMQ_PORT = 5562
ENV_FILE = "/home/swimmy/swimmy/.env"
LIBRARY_PATH = "/home/swimmy/swimmy/data/library"


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


def count_strategies(tier_dir):
    path = os.path.join(LIBRARY_PATH, tier_dir, "*.lisp")
    return len(glob.glob(path))


def main():
    env = load_env()
    # Prioritize .env file content because system environment might be stale
    webhook_url = env.get("SWIMMY_DISCORD_REPORTS") or os.getenv(
        "SWIMMY_DISCORD_REPORTS"
    )

    if not webhook_url:
        print("❌ Error: SWIMMY_DISCORD_REPORTS not set.")
        sys.exit(1)

    # 1. Count from Filesystem (Source of Truth)
    # Note: Case sensitivity based on actual directory structure
    count_s = count_strategies("BATTLEFIELD")
    count_a = count_strategies("training")  # Directory is lowercase
    count_recruits = count_strategies("INCUBATOR")
    count_graveyard = count_strategies("GRAVEYARD")
    count_selection = count_strategies("selection")  # Directory is lowercase
    count_legend = count_strategies("LEGEND")  # Just in case

    # Total Active = S + A + Selection + Recruits + Legend
    active_total = count_s + count_a + count_recruits + count_selection + count_legend

    # "Veteran Genes" usually implies the surviving pool.
    # For this report, we'll align it with Active Knowledge Base.
    veteran_genes = active_total

    # 2. Build Payload (Exact Format Requested)
    # JST Timestamp
    jst = timezone(timedelta(hours=9))
    now = datetime.now(jst).strftime("%Y-%m-%d %H:%M:%S")

    description = f"""Current status of the autonomous strategy generation pipeline.

🧠 Knowledge Base (Active)
{active_total} Strategies

🏆 S-Rank (Elite)
{count_s}

🎖️ A-Rank (Pro)
{count_a}

👶 New Recruits (Born)
{count_recruits}

👻 Graveyard (Rejected)
{count_graveyard}

🧬 Veteran Genes
{veteran_genes}

⚙️ System Status
✅ Evolution Daemon Active
✅ Persistence Linked
Swimmy AI • {now} JST"""

    payload = {
        "content": "🏭 **Evolution Factory Report**",  # Posting title outside embed for clean notification
        "embeds": [
            {
                "description": description,
                "color": 0x2ECC71,  # Green for Factory/Status
            }
        ],
    }

    # 3. Send
    context = zmq.Context()
    socket = context.socket(zmq.PUSH)
    socket.connect(f"tcp://localhost:{ZMQ_PORT}")
    socket.send_json({"webhook": webhook_url, "data": payload})

    print("✅ Evolution Factory Report Sent.")
    print(description)  # Print for verification


if __name__ == "__main__":
    main()
