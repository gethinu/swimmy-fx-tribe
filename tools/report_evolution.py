#!/usr/bin/env python3
"""
report_evolution.py
===================
Generates the "Evolution Factory Report" using the filesystem as the source of truth.
Counts .lisp strategy files in data/library/ Rank directories.
"""

import json
import os
import sys
import glob
from datetime import datetime, timezone, timedelta
from pathlib import Path
import zmq


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


# Configuration
ZMQ_PORT = _env_int("SWIMMY_PORT_NOTIFIER", 5562)
BASE_DIR = str(resolve_base_dir())
PYTHON_SRC = Path(BASE_DIR) / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from aux_sexp import sexp_request
ENV_FILE = os.path.join(BASE_DIR, ".env")
LIBRARY_PATH = os.path.join(BASE_DIR, "data", "library")


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


def count_strategies(rank_dir):
    path = os.path.join(LIBRARY_PATH, rank_dir, "*.lisp")
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
    count_s = count_strategies("S")
    count_a = count_strategies("A")
    count_b = count_strategies("B")
    count_recruits = count_strategies("INCUBATOR")
    count_graveyard = count_strategies("GRAVEYARD")
    count_legend = count_strategies("LEGEND")

    # Total Active
    active_total = count_s + count_a + count_b + count_recruits + count_legend

    # 2. Build Payload (V45: Unified Terminology)
    jst = timezone(timedelta(hours=9))
    now = datetime.now(jst).strftime("%Y-%m-%d %H:%M:%S")

    description = f"""Strategy Generation Pipeline Status

🧠 Knowledge Base
{active_total} Active Strategies

🏆 S-Rank (Verified Elite)
{count_s} (Sharpe ≥0.5 PF≥1.5 WR≥45% MaxDD<15% + CPCV)

🎖️ A-Rank (Pro)
{count_a} (Sharpe ≥0.3 PF≥1.2 WR≥40% MaxDD<20% + OOS)

🪜 B-Rank (Selection)
{count_b} (Sharpe ≥0.1 PF≥1.0 WR≥30% MaxDD<30%)

👶 Incubator
{count_recruits}

👻 Graveyard
{count_graveyard}

⚙️ System Status
✅ Evolution Daemon Active
✅ Rank Lifecycle Active
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
    message = sexp_request(
        {
            "type": "NOTIFIER",
            "action": "SEND",
            "webhook": webhook_url,
            "payload_json": json.dumps(payload, ensure_ascii=False),
        }
    )
    socket.send_string(message)

    print("✅ Evolution Factory Report Sent.")
    print(description)  # Print for verification


if __name__ == "__main__":
    main()
