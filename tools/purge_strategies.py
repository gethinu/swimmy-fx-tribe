#!/usr/bin/env python3
import json
import os
import sys
import re

# Configuration
STRATEGIES_JSON = "strategies.json"
RANK_DB_PATH = "/home/swimmy/swimmy/.swimmy/strategy_ranks.lisp"
GRAVEYARD_FILE = "data/graveyard.json"


def load_json(path):
    if os.path.exists(path):
        try:
            with open(path, "r") as f:
                return json.load(f)
        except:
            return []
    return []


def save_json(path, data):
    with open(path, "w") as f:
        json.dump(data, f, indent=2)


def parse_lisp_db(path):
    """
    Parse the Lisp DB file to get ranks and metrics.
    Format: (:NAME "Name" :RANK :scout :TRADES 10 :WINS 5 :TOTAL-PNL -100.0 ...)
    """
    if not os.path.exists(path):
        return {}

    db = {}
    with open(path, "r") as f:
        content = f.read()

    # Regex to extract fields
    # (:NAME "(.+?)" :RANK (:\w+) :TRADES (\d+) :WINS (\d+) :TOTAL-PNL ([-\d\.]+)
    pattern = re.compile(
        r':NAME "(.+?)" :RANK (:\w+) :TRADES (\d+) :WINS (\d+) :TOTAL-PNL ([-\d\.]+)'
    )

    for match in pattern.finditer(content):
        name, rank, trades, wins, pnl = match.groups()
        db[name] = {
            "rank": rank,
            "trades": int(trades),
            "wins": int(wins),
            "pnl": float(pnl),
        }
    return db


def purge_strategies():
    print("💀 STARTING PURGE PROTOCOL (Up or Out)...")

    strats = load_json(STRATEGIES_JSON)
    ranks = parse_lisp_db(RANK_DB_PATH)
    graveyard = load_json(GRAVEYARD_FILE)

    survivors = []
    purged = []

    print(f"   Analyzing {len(strats)} strategies...")

    for s in strats:
        name = s["name"]
        stats = ranks.get(name)

        should_purge = False
        reason = ""

        if not stats:
            # If no stats yet (new recruit), keep it for 1 cycle equivalent
            # Or if it was just added.
            # Safe default: Keep until proven guilty?
            # Or if it's old (check timestamp in name?), purge.
            # For now, keep.
            survivors.append(s)
            continue

        rank = stats["rank"]
        pnl = stats["pnl"]

        # PURGE CRITERIA (Aggressive)
        if rank == ":scout":
            if pnl < 0:
                should_purge = True
                reason = f"Negative PnL ({pnl:.2f})"
            elif stats["trades"] > 5 and pnl < 100:  # Stagnant
                should_purge = True
                reason = "Stagnant (Low PnL)"

        if should_purge:
            print(f"   ❌ PURGING: {name} | Reason: {reason}")
            purged.append(s)
            # Add to graveyard if not exists (using simple check)
            # Actually, just append to graveyard list, we can dedupe later or just keep history
            graveyard.append(s)
        else:
            survivors.append(s)

    # Commit changes
    if purged:
        print(f"   🔥 Purged {len(purged)} strategies. Survivors: {len(survivors)}")

        # 1. Update Strategies
        save_json(STRATEGIES_JSON, survivors)

        # 2. Update Graveyard (Wisdom)
        save_json(GRAVEYARD_FILE, graveyard)
        print("   ⚰️  Graveyard updated with fresh corpses.")
    else:
        print("   ✨ No strategies matched purge criteria.")


if __name__ == "__main__":
    purge_strategies()
