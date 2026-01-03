#!/usr/bin/env python3
"""
Swimmy Backtest Result Formatter for Discord
V1.0 - Category-grouped summary view

Fixes the issue of posting 61 strategies individually.
Instead, posts a single summary grouped by category with top performers.
"""

import json
import os
from collections import defaultdict
from datetime import datetime

# Strategy categories based on name patterns
STRATEGY_CATEGORIES = {
    "MA-Cross": ["Cross", "SMA", "EMA", "Golden", "Death", "Silver"],
    "Trend": ["Trend", "TF-", "Puria", "Triple-Screen"],
    "MACD": ["MACD"],
    "RSI": ["RSI", "Elder"],
    "Bollinger": ["BB-", "Bollinger", "Squeeze"],
    "Stochastic": ["Stoch"],
    "Volatility": ["Vol", "ATR", "Breakout"],
    "Other": [],  # Catch-all
}


def categorize_strategy(name: str) -> str:
    """Categorize a strategy by its name."""
    for category, keywords in STRATEGY_CATEGORIES.items():
        for keyword in keywords:
            if keyword.lower() in name.lower():
                return category
    return "Other"


def format_backtest_summary(results: list) -> str:
    """
    Format backtest results as a category-grouped summary.

    Args:
        results: List of dicts with keys: name, sharpe, trades, win_rate

    Returns:
        Formatted string for Discord (under 2000 chars)
    """
    if not results:
        return "📊 バックテスト結果: データなし"

    # Group by category
    categories = defaultdict(list)
    for r in results:
        cat = categorize_strategy(r.get("name", "Unknown"))
        categories[cat].append(r)

    # Stats for each category
    summary_lines = ["📊 **バックテスト サマリー**"]
    summary_lines.append(f"━━━━━━━━━━━━━━━━━━━━")
    summary_lines.append(f"📅 {datetime.now().strftime('%Y-%m-%d %H:%M')}")
    summary_lines.append(f"🎯 総戦略数: {len(results)}")
    summary_lines.append("")

    # Overall stats
    all_sharpes = [r.get("sharpe", 0) for r in results]
    positive_sharpe = sum(1 for s in all_sharpes if s > 0)
    avg_sharpe = sum(all_sharpes) / len(all_sharpes) if all_sharpes else 0

    summary_lines.append(f"**📈 全体統計:**")
    summary_lines.append(f"  Sharpe > 0: {positive_sharpe}/{len(results)}")
    summary_lines.append(f"  平均Sharpe: {avg_sharpe:.2f}")
    summary_lines.append("")

    # Category breakdown
    summary_lines.append("**📦 カテゴリ別:**")
    for cat, strats in sorted(categories.items(), key=lambda x: -len(x[1])):
        if not strats:
            continue
        sharpes = [s.get("sharpe", 0) for s in strats]
        best = max(strats, key=lambda x: x.get("sharpe", 0))
        avg = sum(sharpes) / len(sharpes)
        positive = sum(1 for s in sharpes if s > 0)

        summary_lines.append(
            f"  **{cat}** ({len(strats)}): 👍{positive} | Avg={avg:.1f}"
        )
        summary_lines.append(
            f"    🏆 Best: {best.get('name', '?')} (SR={best.get('sharpe', 0):.2f})"
        )

    summary_lines.append("")

    # Top 5 performers
    top5 = sorted(results, key=lambda x: x.get("sharpe", 0), reverse=True)[:5]
    summary_lines.append("**🏆 Top 5:**")
    for i, s in enumerate(top5, 1):
        summary_lines.append(
            f"  {i}. {s.get('name', '?')}: SR={s.get('sharpe', 0):.2f}, WR={s.get('win_rate', 0):.0f}%"
        )

    # Bottom 5 (for debugging Sharpe=-3.75 issue)
    bottom5 = sorted(results, key=lambda x: x.get("sharpe", 0))[:5]
    summary_lines.append("")
    summary_lines.append("**⚠️ Bottom 5 (要調査):**")
    for s in bottom5:
        summary_lines.append(f"  ⚡ {s.get('name', '?')}: SR={s.get('sharpe', 0):.2f}")

    summary_lines.append("━━━━━━━━━━━━━━━━━━━━")

    return "\n".join(summary_lines)


def load_backtest_results(filepath: str) -> list:
    """Load backtest results from JSON file."""
    if os.path.exists(filepath):
        with open(filepath, "r") as f:
            return json.load(f)
    return []


def parse_discord_format(text: str) -> list:
    """Parse the old Discord format into structured data."""
    results = []
    for line in text.strip().split("\n"):
        # Format: "Strategy-Name: Sharpe=X.XX, Trades=N, Win=XX.X%"
        if "Sharpe=" in line and "Trades=" in line:
            try:
                # Extract name
                parts = line.split(":")
                name = parts[0].strip()

                # Extract metrics
                rest = ":".join(parts[1:])
                sharpe = float(rest.split("Sharpe=")[1].split(",")[0])
                trades = int(rest.split("Trades=")[1].split(",")[0])
                win_rate = float(rest.split("Win=")[1].split("%")[0])

                results.append(
                    {
                        "name": name,
                        "sharpe": sharpe,
                        "trades": trades,
                        "win_rate": win_rate,
                    }
                )
            except (IndexError, ValueError):
                continue
    return results


if __name__ == "__main__":
    # Example usage
    example_results = [
        {"name": "Golden-Cross-50-200", "sharpe": 0.0, "trades": 1, "win_rate": 0},
        {"name": "TF-10-50", "sharpe": 4.79, "trades": 10, "win_rate": 10},
        {"name": "Silver-Cross-20-100", "sharpe": 5.61, "trades": 8, "win_rate": 25},
        {
            "name": "RSI-Oversold-Reversal",
            "sharpe": -3.75,
            "trades": 20,
            "win_rate": 35,
        },
        {"name": "BB-Lower-Bounce", "sharpe": -3.75, "trades": 20, "win_rate": 35},
        {"name": "MACD-Signal-Cross", "sharpe": -3.75, "trades": 20, "win_rate": 35},
    ]

    print(format_backtest_summary(example_results))
    print()
    print("=" * 50)
    print("NOTE: Most strategies show Sharpe=-3.75 which indicates a bug")
    print("in the backtest calculation, not actual performance.")
