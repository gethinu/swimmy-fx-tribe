#!/usr/bin/env python3
import json
import os
import sys
import random
import time
from backtest_service import BacktestService

# Configuration
# Configuration
MAX_ATTEMPTS = 50
MIN_SHARPE = 0.1
GRAVEYARD_FILE = "data/graveyard.json"
OPTIMIZED_FILE = "strategies_optimized.json"


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


def is_toxic(params, graveyard):
    """Check if params are too close to a known failure."""
    for corpse in graveyard:
        # Simple Euclidean distance check on normalized params could be better, but exact match for now
        # Actually let's just check if TF and SMA are identical
        if (
            corpse.get("timeframe") == params["timeframe"]
            and corpse.get("sma_short") == params["sma_short"]
            and corpse.get("sma_long") == params["sma_long"]
        ):
            return True
    return False


def mutate_best(best_strats, attempt):
    """Clone a winner and mutate it."""
    parent = random.choice(best_strats)

    # Mutate
    short = parent.get("sma_short", 10)
    long_p = parent.get("sma_long", 20)

    # Mutate +/- 10%
    if random.random() < 0.5:
        short = max(5, int(short * random.uniform(0.9, 1.1)))
    if random.random() < 0.5:
        long_p = max(short + 5, int(long_p * random.uniform(0.9, 1.1)))

    name = f"Recruit-Elite-{int(time.time())}-{attempt}"

    return {
        "name": name,
        "timeframe": parent.get("timeframe", 15),
        "sma_short": short,
        "sma_long": long_p,
        "sl": round(max(0.01, parent.get("sl", 0.05) * random.uniform(0.9, 1.1)), 3),
        "tp": round(max(0.01, parent.get("tp", 0.05) * random.uniform(0.9, 1.1)), 3),
        "volume": 0.01,
        "indicator_type": "sma",
        "filter_enabled": False,
    }


def generate_smart_strategy(attempt, best_strats, graveyard):
    # 80% Imitation (Wisdom), 20% Exploration (Random)
    if best_strats and random.random() < 0.8:
        candidate = mutate_best(best_strats, attempt)
    else:
        # Random but avoid Graveyard
        for _ in range(10):  # Try 10 times to find non-toxic
            candidate = generate_random_strategy(attempt)
            if not is_toxic(candidate, graveyard):
                break
    return candidate


def generate_random_strategy(attempt):
    tf = random.choice(TIMEFRAMES)
    name = f"Recruit-Rnd-{int(time.time())}-{attempt}"
    short = random.randint(5, 50)
    long_p = random.randint(50, 200)
    return {
        "name": name,
        "timeframe": tf,
        "sma_short": short,
        "sma_long": long_p,
        "sl": round(random.uniform(*SL_RANGES), 3),
        "tp": round(random.uniform(*TP_RANGES), 3),
        "volume": 0.01,
        "indicator_type": "sma",
        "filter_enabled": False,
    }


def get_tf_label(tf_mins):
    s = str(tf_mins).upper()
    if s in ["W1", "D1", "H4", "H1", "M30", "M15", "M5", "M1"]:
        return s
    if s == "60":
        return "H1"
    if s == "240":
        return "H4"
    if s == "1440":
        return "D1"
    if s == "10080":
        return "W1"
    return f"M{tf_mins}"


def run_exam():
    service = BacktestService(use_zmq=False)

    # Phase 5: Load Wisdom
    best_strats = load_json(OPTIMIZED_FILE)
    graveyard = load_json(GRAVEYARD_FILE)

    new_corpses = []

    try:
        if service.guardian_process:
            time.sleep(1)

        for i in range(MAX_ATTEMPTS):
            candidate = generate_smart_strategy(i, best_strats, graveyard)

            tf_raw = candidate["timeframe"]
            tf_label = get_tf_label(tf_raw)
            symbol = "USDJPY"
            candles_path = f"data/historical/{symbol}_{tf_label}.csv"

            if not os.path.exists(candles_path):
                continue

            payload = {
                "action": "BACKTEST",
                "strategy": candidate,
                "candles_file": candles_path,
                "strategy_name": candidate["name"],
                "symbol": symbol,
                "timeframe": tf_raw,
                "from_date": "2024-01-01",
                "to_date": "2025-01-01",
                "spread": 10,
                "balance": 10000,
            }

            res = service.run_backtest(payload)
            success = False

            if (
                res
                and res.get("type") == "BACKTEST_RESULT"
                and not res.get("result", {}).get("error")
            ):
                stats = res["result"]
                sharpe = stats.get("sharpe", 0.0)
                trades = stats.get("trades", 0)

                if sharpe >= MIN_SHARPE and trades > 1:
                    print(json.dumps(candidate))
                    success = True
                    # Append new wisdom (corpses) before exiting?
                    # Ideally yes, but maximizing speed.
                    # Let's save corpses found so far.
                    if new_corpses:
                        graveyard.extend(new_corpses)
                        save_json(GRAVEYARD_FILE, graveyard)
                    return 0

            # If failed, add to graveyard list
            if not success:
                new_corpses.append(candidate)

    finally:
        # Save accumulated failures
        if new_corpses:
            graveyard.extend(new_corpses)
            save_json(GRAVEYARD_FILE, graveyard)

        if service.guardian_process:
            service.guardian_process.terminate()

    return 1


if __name__ == "__main__":
    sys.exit(run_exam())
