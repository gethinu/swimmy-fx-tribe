#!/usr/bin/env python3
"""
report_backtest_summary.py
==========================
Generates a performance report based on the latest backtest results.
Identifies S-Rank and SR-Rank candidates and sends a digest to Discord.
"""

import os
import sys
from pathlib import Path
import zmq
import json
from datetime import datetime, timezone, timedelta


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

BASE_DIR = resolve_base_dir()
PYTHON_SRC = BASE_DIR / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from sexp_utils import load_sexp_list
from aux_sexp import sexp_request

# Configuration
ZMQ_PORT = _env_int("SWIMMY_PORT_NOTIFIER", 5562)
ENV_FILE = str(BASE_DIR / ".env")
BACKTEST_CACHE = BASE_DIR / "data" / "backtest_cache.sexp"


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


def load_backtest_cache(path):
    path = Path(path)
    if not path.exists():
        return []
    try:
        return load_sexp_list(str(path))
    except Exception as e:
        print(f"❌ Error loading {path}: {e}")
        return []


def main():
    # Load Environment
    env = load_env()
    webhook_url = os.getenv("SWIMMY_DISCORD_REPORTS") or env.get(
        "SWIMMY_DISCORD_REPORTS"
    )

    if not webhook_url:
        print("❌ Error: SWIMMY_DISCORD_REPORTS not set.")
        sys.exit(1)

    # Load Backtest Data
    cache = load_backtest_cache(BACKTEST_CACHE)
    if not cache:
        print("⚠️ No backtest data found.")
        return

    # Process Strategies
    strategies = []

    def plist_to_dict(plist):
        if isinstance(plist, dict):
            return plist
        d = {}
        for i in range(0, len(plist), 2):
            if i + 1 < len(plist):
                # Keys are usually uppercase strings in Lisp JSON export
                d[plist[i]] = plist[i + 1]
        return d

    for entry in cache:
        name = entry.get("name", "Unknown")
        res_raw = entry.get("result", []) or []
        res = plist_to_dict(res_raw)

        # Keys might be "SHARPE" or "sharpe" depending on export
        # Try both or normalize
        sharpe = res.get("SHARPE") or res.get("sharpe") or 0.0
        pf = res.get("PROFIT-FACTOR") or res.get("profit_factor") or 0.0
        trades = res.get("TRADES") or res.get("trades") or 0
        wr = res.get("WIN-RATE") or res.get("win_rate") or 0.0

        strategies.append(
            {
                "name": name,
                "sharpe": float(sharpe),
                "pf": float(pf),
                "trades": int(trades),
                "win_rate": float(wr),
            }
        )

    # Sort by Sharpe
    strategies.sort(key=lambda x: x["sharpe"], reverse=True)

    # Rank Logic
    # S-Rank: Sharpe >= 1.2 AND PF >= 1.5
    # SR-Rank (Super Rare): Sharpe >= 2.0 (User requested check)

    s_rank = []
    sr_rank = []

    for s in strategies:
        if s["sharpe"] >= 2.0 and s["pf"] >= 1.5 and s["trades"] > 20:
            sr_rank.append(s)
        elif s["sharpe"] >= 1.2 and s["pf"] >= 1.5 and s["trades"] > 20:
            s_rank.append(s)

    # Construct Payload

    # Top 5 Table
    top_5_text = "```\n"
    top_5_text += f"{'Name':<25} | {'Shp':<4} | {'PF':<4} | {'Trds':<4}\n"
    top_5_text += "-" * 46 + "\n"
    for s in strategies[:5]:
        top_5_text += f"{s['name'][:25]:<25} | {s['sharpe']:<4.2f} | {s['pf']:<4.2f} | {s['trades']:<4}\n"
    top_5_text += "```"

    fields = [
        {
            "name": "🏆 Performance Leaderboard (Top 5)",
            "value": top_5_text,
            "inline": False,
        }
    ]

    # SR Rank Section
    if sr_rank:
        sr_text = "✨ **ULTRA RARE (SR) FOUND!** ✨\n"
        for s in sr_rank:
            sr_text += (
                f"💎 **{s['name']}**: Sharpe {s['sharpe']:.2f}, PF {s['pf']:.2f}\n"
            )
        fields.append(
            {"name": "🌌 SR Rank (Sharpe > 2.0)", "value": sr_text, "inline": False}
        )

    # S Rank Section
    if s_rank:
        s_text = ""
        for s in s_rank[:5]:  # Limit to 5 specific mentions
            s_text += f"🔸 **{s['name']}**: Sharpe {s['sharpe']:.2f}\n"
        if len(s_rank) > 5:
            s_text += f"...and {len(s_rank)-5} more."
        fields.append(
            {
                "name": "⭐️ S Rank Candidates (Sharpe > 1.2)",
                "value": s_text,
                "inline": False,
            }
        )
    else:
        # Fallback: Show "Best Available" if no S-Rank (Bootstrapping Phase)
        if not sr_rank and len(strategies) > 0:
            best_text = ""
            for s in strategies[:3]:
                best_text += f"🧱 **{s['name']}**: Sharpe {s['sharpe']:.2f}\n"

            fields.append(
                {
                    "name": "🚧 Provisional Top Candidates (Bootstrapping)",
                    "value": best_text
                    + "\n*System is in Alpha Injection / Acceleration Mode*",
                    "inline": False,
                }
            )
        elif not sr_rank:
            fields.append(
                {
                    "name": "⭐️ S Rank Candidates",
                    "value": "None found yet. Evolution continues...",
                    "inline": False,
                }
            )

    payload = {
        "embeds": [
            {
                "title": "🧪 Backtest Performance Report",
                "description": f"Analyzed {len(strategies)} strategies from the knowledge base.",
                "color": 0xE91E63,  # Pink/Red for Performance
                "fields": fields,
                "footer": {
                    "text": f"Swimmy AI • {datetime.now().strftime('%Y-%m-%d %H:%M:%S')} JST"
                },
            }
        ]
    }

    # Send to Notifier
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
    print(
        f"✅ Performance Report sent. (SR={len(sr_rank)}, S={len(s_rank)}, Total={len(strategies)})"
    )


if __name__ == "__main__":
    main()
