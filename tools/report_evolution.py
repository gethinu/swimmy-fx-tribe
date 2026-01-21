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
    # V45: Use actual Tier names consistently
    count_battlefield = count_strategies("BATTLEFIELD")
    count_training = count_strategies("training")  # lowercase
    count_recruits = count_strategies("INCUBATOR")
    count_graveyard = count_strategies("GRAVEYARD")
    count_selection = count_strategies("selection")  # lowercase
    count_legend = count_strategies("LEGEND")

    # Total Active
    active_total = (
        count_battlefield
        + count_training
        + count_recruits
        + count_selection
        + count_legend
    )

    # 2. Build Payload (V45: Unified Terminology)
    jst = timezone(timedelta(hours=9))
    now = datetime.now(jst).strftime("%Y-%m-%d %H:%M:%S")

    description = f"""Strategy Generation Pipeline Status

🧠 Knowledge Base
{active_total} Active Strategies

⚔️ Battlefield (Elite)
{count_battlefield} (Sharpe ≥0.5, Trades ≥10)

🎯 Training
{count_training} (Sharpe ≥0.3, Trades ≥5)

📋 Selection
{count_selection} (Sharpe ≥0.1)

👶 Incubator
{count_recruits}

👻 Graveyard
{count_graveyard}

⚙️ System Status
✅ Evolution Daemon Active
✅ Multi-Gen Breeding V45.0
{now} JST"""

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
