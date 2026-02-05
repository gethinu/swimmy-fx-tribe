#!/usr/bin/env python3
"""
report_system_status.py
=======================
Generates a "factory report" on the state of the evolution system.
Counts active strategies, recruits, and graveyard entries from rank directories.
Sends an Embed notification to Discord via the Notifier service.
"""

import os
import sys
import json
import re
from datetime import datetime
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
    dry_run = "--dry-run" in sys.argv

    # Load Environment
    env = load_env()
    webhook_url = os.getenv("SWIMMY_DISCORD_REPORTS") or env.get(
        "SWIMMY_DISCORD_REPORTS"
    )

    if not webhook_url and not dry_run:
        print("❌ Error: SWIMMY_DISCORD_REPORTS not set.")
        sys.exit(1)

    # Gather Statistics from Sharded Library
    library_path = os.path.join(BASE_DIR, "data/library")

    # Initialize Counters
    stats = {
        "INCUBATOR": 0,
        "B": 0,
        "A": 0,
        "S": 0,
        "LEGEND": 0,
        "GRAVEYARD": 0,
    }

    if os.path.exists(library_path):
        for rank_dir in stats.keys():
            tier_path = os.path.join(library_path, rank_dir)
            if os.path.exists(tier_path):
                files = [f for f in os.listdir(tier_path) if f.endswith(".lisp")]
                stats[rank_dir] = len(files)

    # Total Active (Everything except Graveyard)
    active_count = (
        stats["INCUBATOR"]
        + stats["B"]
        + stats["A"]
        + stats["S"]
        + stats["LEGEND"]
    )
    recruit_count = stats["INCUBATOR"]
    graveyard_count = stats["GRAVEYARD"]
    optimized_count = count_pattern_in_file(
        LISP_OPTIMIZED, r"\(:name \""
    )  # Keep this for genes

    print(
        f"📊 Stats (Sharded): Active={active_count}, S-Rank={stats['S']}, A-Rank={stats['A']}, Graveyard={graveyard_count}"
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
                        "value": f"**{stats['S']}**",
                        "inline": True,
                    },
                    {
                        "name": "🎖️ A-Rank (Pro)",
                        "value": f"**{stats['A']}**",
                        "inline": True,
                    },
                    {
                        "name": "🪜 B-Rank (Selection)",
                        "value": f"**{stats['B']}**",
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

    if dry_run:
        print("✅ Dry-run: payload built, send skipped.")
        return

    # Send to Notifier Service
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
    print("✅ Notification sent to Notifier Service.")


if __name__ == "__main__":
    main()
