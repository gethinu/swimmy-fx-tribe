#!/usr/bin/env python3
import json
import os
import sys
import random
import copy
import time
from backtest_service import BacktestService

# Configuration
STRATEGIES_JSON = "/home/swimmy/swimmy/strategies.json"
OPTIMIZED_JSON = "/home/swimmy/swimmy/strategies_optimized.json"
GENERATIONS = 5  # Phase 4: Increased from 1 to 5
POPULATION_SIZE = 20  # Phase 4: Increased from 10 to 20

# WFV: Train Period (Optimization)
TRAIN_FROM = "2016-01-01"
TRAIN_TO = "2023-12-31"

# Parameter Ranges
TIMEFRAMES = [5, 15, 30, 60, 240, 1440, 10080]  # Added W1
SMA_RANGES = (5, 200)
SL_RANGES = (0.01, 0.10)  # 10 pips to 100 pips approx
TP_RANGES = (0.01, 0.20)  # 10 pips to 200 pips


def load_strategies():
    if not os.path.exists(STRATEGIES_JSON):
        print("❌ strategies.json not found.")
        sys.exit(1)
    with open(STRATEGIES_JSON, "r") as f:
        return json.load(f)


def mutate_strategy(strat):
    """Create a mutant variant of the strategy"""
    mutant = copy.deepcopy(strat)

    # 1. Mutate Timeframe (20% chance)
    if random.random() < 0.2:
        mutant["timeframe"] = random.choice(TIMEFRAMES)

    # 2. Mutate SL/TP (50% chance)
    if random.random() < 0.5:
        # Small nudge
        sl = float(mutant.get("sl", 0.05))
        tp = float(mutant.get("tp", 0.05))

        mutant["sl"] = max(
            SL_RANGES[0], min(SL_RANGES[1], sl * random.uniform(0.8, 1.2))
        )
        mutant["tp"] = max(
            TP_RANGES[0], min(TP_RANGES[1], tp * random.uniform(0.8, 1.2))
        )

    # 3. Mutate SMA (50% chance)
    if random.random() < 0.5:
        short = int(mutant.get("sma_short", 10))
        long_p = int(mutant.get("sma_long", 20))

        if random.random() < 0.5:
            short = max(
                SMA_RANGES[0], min(SMA_RANGES[1], int(short * random.uniform(0.8, 1.2)))
            )
        else:
            long_p = max(
                SMA_RANGES[0],
                min(SMA_RANGES[1], int(long_p * random.uniform(0.8, 1.2))),
            )

        # Ensure hierarchy
        if short >= long_p:
            short = max(5, long_p - 5)

        mutant["sma_short"] = short
        mutant["sma_long"] = long_p

    return mutant


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


def run_evolution():
    print(f"🧬 STARTING EVOLUTIONARY OPTIMIZATION (Pop: {POPULATION_SIZE})")

    strategies = load_strategies()
    service = BacktestService(use_zmq=False)

    optimized_strategies = []

    try:
        total_strats = len(strategies)
        for i, parent in enumerate(strategies):
            name = parent["name"]
            print(f"\n[{i+1}/{total_strats}] Evolving {name}...", end="", flush=True)

            best_variant = parent
            best_fitness = -999.0

            # Create population: Parent + Mutants
            population = [parent] + [
                mutate_strategy(parent) for _ in range(POPULATION_SIZE)
            ]

            for idx, genome in enumerate(population):
                # Prepare Backtest
                tf_raw = genome.get("timeframe", 15)
                tf_label = get_tf_label(tf_raw)
                symbol = "USDJPY"  # Default for tuning
                candles_path = f"data/historical/{symbol}_{tf_label}.csv"

                # Check data exists
                if not os.path.exists(candles_path):
                    continue

                payload = {
                    "action": "BACKTEST",
                    "strategy": genome,
                    "candles_file": candles_path,
                    "strategy_name": name,
                    "symbol": symbol,
                    "timeframe": tf_raw,
                    "from_date": TRAIN_FROM,
                    "to_date": TRAIN_TO,
                    "spread": 10,
                    "balance": 1000000,
                }

                res = service.run_backtest(payload)
                if (
                    res
                    and res.get("type") == "BACKTEST_RESULT"
                    and not res.get("result", {}).get("error")
                ):
                    stats = res["result"]
                    sharpe = stats.get("sharpe", stats.get("sharpe_ratio", 0))
                    trades = stats.get("trades", stats.get("total_trades", 0))

                    # Fitness Function: Sharpe (penalize low trades)
                    if trades < 20:
                        fitness = -1.0
                    else:
                        fitness = sharpe

                    if fitness > best_fitness:
                        best_fitness = fitness
                        best_variant = genome
                        # print(f" * New Best: Fit={fitness:.2f} (Sh={sharpe:.2f} Tr={trades})")

            # Done with this strategy
            print(f" DONE. Best Fitness: {best_fitness:.2f}")
            optimized_strategies.append(best_variant)

    finally:
        if service.guardian_process:
            service.guardian_process.terminate()

    # Save Optimized Population
    print(f"\n💾 Saving Optimized Genomes to {OPTIMIZED_JSON}")
    with open(OPTIMIZED_JSON, "w") as f:
        json.dump(optimized_strategies, f, indent=2)

    print("✅ Evolution Cycle Complete.")


if __name__ == "__main__":
    run_evolution()
