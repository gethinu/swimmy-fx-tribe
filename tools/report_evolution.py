#!/usr/bin/env python3
"""
report_evolution.py
===================
Generates the "Evolution Factory Report" using SQLite as the source of truth.
Counts ranks from data/memory/swimmy.db (or SWIMMY_DB_PATH override).
"""

import json
import os
import sys
import sqlite3
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
DB_PATH = os.getenv("SWIMMY_DB_PATH") or os.path.join(BASE_DIR, "data", "memory", "swimmy.db")


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


def normalize_rank(rank):
    if rank is None:
        return "NIL"
    s = str(rank).strip().upper()
    if s.startswith(":"):
        s = s[1:]
    return s


def get_db_rank_counts(db_path: Path | str = DB_PATH):
    conn = sqlite3.connect(str(db_path))
    try:
        cur = conn.cursor()
        cur.execute("SELECT rank, count(*) FROM strategies GROUP BY rank")
        rows = cur.fetchall()
    finally:
        conn.close()

    counts = {}
    total = 0
    for rank, count in rows:
        key = normalize_rank(rank)
        counts[key] = count
        total += count

    def count_rank(key):
        return counts.get(key, 0)

    s = count_rank("S")
    a = count_rank("A")
    b = count_rank("B")
    legend = count_rank("LEGEND")
    graveyard = count_rank("GRAVEYARD")
    retired = count_rank("RETIRED")
    incubator = count_rank("INCUBATOR")
    unranked = count_rank("NIL")
    active = total - graveyard - retired

    return {
        "total": total,
        "active": active,
        "s": s,
        "a": a,
        "b": b,
        "legend": legend,
        "graveyard": graveyard,
        "retired": retired,
        "incubator": incubator,
        "unranked": unranked,
    }


def main():
    env = load_env()
    # Prioritize .env file content because system environment might be stale
    webhook_url = env.get("SWIMMY_DISCORD_REPORTS") or os.getenv(
        "SWIMMY_DISCORD_REPORTS"
    )

    if not webhook_url:
        print("❌ Error: SWIMMY_DISCORD_REPORTS not set.")
        sys.exit(1)

    # 1. Count from SQLite (Source of Truth)
    counts = get_db_rank_counts(DB_PATH)
    count_s = counts["s"]
    count_a = counts["a"]
    count_b = counts["b"]
    count_recruits = counts["incubator"]
    count_graveyard = counts["graveyard"]
    count_legend = counts["legend"]
    active_total = counts["active"]

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
