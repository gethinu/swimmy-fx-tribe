#!/usr/bin/env python3
"""
report_backtest_summary.py
==========================
Generates a performance report based on the latest backtest results.
Identifies S-Rank and SR-Rank candidates and sends a digest to Discord.
"""

import zmq
import json
import os
import sys
from datetime import datetime, timezone, timedelta

# Configuration
ZMQ_PORT = 5562
ENV_FILE = "/home/swimmy/swimmy/.env"
BACKTEST_CACHE = "/home/swimmy/swimmy/data/backtest_cache.json"


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


def load_json(path):
    if not os.path.exists(path):
        return []
    try:
        with open(path, "r") as f:
            return json.load(f)
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
    cache = load_json(BACKTEST_CACHE)
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

    message = {"webhook": webhook_url, "data": payload}

    socket.send_json(message)
    print(
        f"✅ Performance Report sent. (SR={len(sr_rank)}, S={len(s_rank)}, Total={len(strategies)})"
    )


if __name__ == "__main__":
    main()
