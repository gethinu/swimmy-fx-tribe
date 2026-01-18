#!/usr/bin/env python3
import json
import os
import sys
import time
from datetime import datetime
from backtest_service import BacktestService

# Configuration
RANK_DB_PATH = "/home/swimmy/swimmy/.swimmy/strategy_ranks.lisp"
STRATEGIES_JSON = "/home/swimmy/swimmy/strategies.json"
START_YEAR = 2006
END_YEAR = 2026


def load_strategies():
    if not os.path.exists(STRATEGIES_JSON):
        os.system(
            "sbcl --noinform --load ~/quicklisp/setup.lisp --load swimmy.asd --eval '(ql:quickload :swimmy)' --load tools/dump_strategies.lisp --eval '(sb-ext:exit)'"
        )
    with open(STRATEGIES_JSON, "r") as f:
        return json.load(f)


def evaluate_rank(result):
    if not result:
        return ":scout"

    trades = result.get("total_trades", 0)
    sharpe = result.get("sharpe_ratio", 0.0)
    pnl = result.get("total_profit", 0.0)

    # S Rank (Legend)
    if trades >= 100 and sharpe >= 1.0 and pnl > 2000:
        return ":legend"

    # A Rank (Veteran)
    if trades >= 50 and sharpe >= 0.2 and pnl > 500:
        return ":veteran"

    # B Rank (Warrior)
    if trades >= 10 and sharpe >= 0.0:
        return ":warrior"

    return ":scout"


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


def run_qualification():
    print(f"🚀 STARTING QUALIFICATION BACKTEST ({START_YEAR}-{END_YEAR})")
    print(f"   Target: {RANK_DB_PATH}")

    strategies = load_strategies()
    print(f"   Loaded {len(strategies)} strategies.")

    ranked_data = []
    legends = 0
    veterans = 0
    warriors = 0
    scouts = 0

    service = BacktestService(use_zmq=False)

    try:
        for i, strat in enumerate(strategies):
            name = strat["name"]
            instruments = strat.get("instrument_list", [])
            symbol = instruments[0] if instruments else "USDJPY"

            tf_raw = strat.get("timeframe", 15)
            tf_label = get_tf_label(tf_raw)
            # Use Relative Path
            candles_path = f"data/historical/{symbol}_{tf_label}.csv"

            print(
                f"[{i+1}/{len(strategies)}] Testing {name} ({symbol} {tf_label})...",
                end="",
                flush=True,
            )

            payload = {
                "action": "BACKTEST",
                "strategy": strat,
                "candles_file": candles_path,
                "strategy_name": name,
                "symbol": symbol,
                "timeframe": tf_raw,
                "from_date": "2024-01-01",  # WFV Test Period (Out-of-Sample)
                "to_date": "2026-01-01",
                "spread": 10,
                "balance": 1000000,
            }

            start_ts = time.time()
            result = service.run_backtest(payload)
            duration = time.time() - start_ts

            result_data = result.get("result", {})
            error = result_data.get("error")

            # Check for success (No error key in result object)
            if result and result.get("type") == "BACKTEST_RESULT" and not error:
                stats = result_data
                rank = evaluate_rank(stats)

                # Normalize keys (Guardian uses snake_case: total_trades, sharpe_ratio)
                # My evaluate_rank uses them correctly.

                entry = {
                    "name": name,
                    "rank": rank,
                    "trades": stats.get("total_trades", 0),
                    "wins": stats.get(
                        "wins", 0
                    ),  # Update key: wins vs total_wins? Guardian returns 'wins'
                    "total_pnl": stats.get(
                        "pnl", 0.0
                    ),  # Update key: pnl vs total_profit? Guardian returns 'pnl'
                    "promotion_date": int(time.time()),
                    "last_trade": int(time.time()),
                }
                # Mapping fix: BacktestResult struct uses 'pnl', 'wins'.
                # My previous script used 'total_profit', 'total_wins'.

                ranked_data.append(entry)

                if rank == ":legend":
                    legends += 1
                elif rank == ":veteran":
                    veterans += 1
                elif rank == ":warrior":
                    warriors += 1
                else:
                    scouts += 1

                print(
                    f" DONE ({duration:.1f}s) -> {rank} (Tr:{entry['trades']} PnL:{entry['total_pnl']:.2f})"
                )
            else:
                print(f" FAILED: {error or 'unknown'}")
                entry = {
                    "name": name,
                    "rank": ":scout",
                    "trades": 0,
                    "wins": 0,
                    "total_pnl": 0,
                    "promotion_date": 0,
                    "last_trade": 0,
                }
                ranked_data.append(entry)
                scouts += 1

    finally:
        if service.guardian_process:
            service.guardian_process.terminate()

    print(f"\n💾 Generating Rank Database...")
    os.makedirs(os.path.dirname(RANK_DB_PATH), exist_ok=True)
    with open(RANK_DB_PATH, "w") as f:
        f.write("(\n")
        for item in ranked_data:
            f.write(
                f' (:NAME "{item["name"]}" :RANK {item["rank"]} :TRADES {item["trades"]} :WINS {item["wins"]} :TOTAL-PNL {item["total_pnl"]:.2f} :PROMOTION-DATE {item["promotion_date"]} :LAST-TRADE {item["last_trade"]})\n'
            )
        f.write(")\n")

    print(f"✅ Database Injected: {len(ranked_data)} records.")
    print(f"   🏆 S-Rank (Legends): {legends}")
    print(f"   🎖️ A-Rank (Veterans): {veterans}")
    print(f"   ⚔️ B-Rank (Warriors): {warriors}")
    print(f"   👶 C-Rank (Scouts):   {scouts}")


if __name__ == "__main__":
    run_qualification()
