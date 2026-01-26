#!/usr/bin/env python3
"""
report_status.py
================
Generates a status report of the Swimmy system.
Reads from:
- data/backtest_cache.json (Strategy Performance)
- data/system_metrics.json (System Health)

Outputs:
- Total Strategies
- Top 10 Strategies by Sharpe
- S-Rank Candidates (Sharpe >= 1.2, PF >= 1.5)
- Count by Category/Clan
"""

import json
import os
import sys
from datetime import datetime, timezone, timedelta
from pathlib import Path


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent

# Paths
BASE_DIR = str(resolve_base_dir())
BACKTEST_CACHE = os.path.join(BASE_DIR, "data", "backtest_cache.json")
METRICS_FILE = os.path.join(BASE_DIR, "data", "system_metrics.json")


def load_json(path):
    if not os.path.exists(path):
        return [] if "cache" in path else {}
    try:
        with open(path, "r") as f:
            return json.load(f)
    except Exception as e:
        print(f"❌ Error loading {path}: {e}")
        return []


def format_timestamp(ts):
    if not ts:
        return "-"
    # JST = UTC+9
    dt = datetime.fromtimestamp(ts, timezone(timedelta(hours=9)))
    return dt.strftime("%Y-%m-%d %H:%M:%S")


def main():
    print("\n📊 SWIMMY SYSTEM STATUS REPORT 📊")
    print("===================================")

    # 1. System Metrics
    metrics = load_json(METRICS_FILE)
    if metrics:
        uptime = metrics.get("uptime_seconds", 0)
        u_str = str(timedelta(seconds=uptime))
        print(f"🖥️  System Uptime: {u_str}")
        print(f"🧠 Memory Usage: {metrics.get('heap_used_mb', 0):.2f} MB")
        print(f"📈 Total Strategies (Memory): {metrics.get('strategy_count', '?')}")
    else:
        print("⚠️  System metrics not available.")

    # 2. Strategy Performance
    cache = load_json(BACKTEST_CACHE)
    if not cache:
        print("\n⚠️  No backtest data found.")
        return

    # Parse Cache
    # Format: [{"name": "...", "timestamp": ..., "result": {"sharpe": 1.2, "profit_factor": 1.5, ...}}]
    strategies = []
    category_counts = {}

    for entry in cache:
        name = entry.get("name", "Unknown")
        res = entry.get("result", {}) or {}
        if isinstance(res, list):
            # Handle weird case where result is a list (likely empty or malformed)
            res = {}

        # Extract metrics
        sharpe = res.get("sharpe", 0.0) or 0.0  # Handle None
        pf = res.get("profit_factor", 0.0) or 0.0
        trades = res.get("trades", 0) or 0
        wr = res.get("win_rate", 0.0) or 0.0

        strategies.append(
            {
                "name": name,
                "sharpe": float(sharpe),
                "pf": float(pf),
                "trades": int(trades),
                "win_rate": float(wr),
                "timestamp": entry.get("timestamp", 0),
            }
        )

        # Infer Category from name (Weak heuristic but useful)
        cat = "Unknown"
        n_lower = name.lower()
        if "scalp" in n_lower:
            cat = "Scalp"
        elif "trend" in n_lower:
            cat = "Trend"
        elif "breakout" in n_lower:
            cat = "Breakout"
        elif "reversal" in n_lower:
            cat = "Reversal"

        category_counts[cat] = category_counts.get(cat, 0) + 1

    # Sorting
    strategies.sort(key=lambda x: x["sharpe"], reverse=True)

    # 3. Summary Stats
    print(f"\n📚 Knowledge Base: {len(strategies)} strategies tested")
    print("\n🏷️  Category Breakdown:")
    for cat, count in category_counts.items():
        print(f"  - {cat}: {count}")

    # 4. S-Rank Candidates
    s_rank = [s for s in strategies if s["sharpe"] >= 1.2 and s["pf"] >= 1.5]
    print(f"\n🏆 S-Rank Candidates ({len(s_rank)}):")
    if s_rank:
        print(f"{'Name':<40} | {'Sharpe':<6} | {'PF':<5} | {'Win%':<5} | {'Trades':<6}")
        print("-" * 80)
        for s in s_rank[:10]:  # Top 10 S-Rank
            print(
                f"{s['name'][:38]:<40} | {s['sharpe']:<6.2f} | {s['pf']:<5.2f} | {s['win_rate']:<5.1f} | {s['trades']:<6}"
            )
    else:
        print("  (None found yet)")

    # 5. Top 10 Overall
    print(f"\n🔝 Top 10 Overall by Sharpe:")
    print(f"{'Name':<40} | {'Sharpe':<6} | {'PF':<5} | {'Win%':<5} | {'Trades':<6}")
    print("-" * 80)
    for s in strategies[:10]:
        print(
            f"{s['name'][:38]:<40} | {s['sharpe']:<6.2f} | {s['pf']:<5.2f} | {s['win_rate']:<5.1f} | {s['trades']:<6}"
        )

    print("\n===================================")


if __name__ == "__main__":
    main()
