#!/usr/bin/env python3
"""Auto-tune OpenClaw+Polymarket config using resolved journal entries."""

from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path
from typing import Any, Dict, List, Mapping, Optional, Sequence, Tuple


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
if str(BASE_DIR) not in sys.path:
    sys.path.insert(0, str(BASE_DIR))

from tools.polymarket_openclaw_report import load_journal_records, load_settlements


def parse_grid(value: str) -> List[float]:
    out: List[float] = []
    for token in value.split(","):
        text = token.strip()
        if not text:
            continue
        out.append(float(text))
    return out


def _normalize_outcome(value: Any) -> Optional[str]:
    text = str(value or "").strip().upper()
    if text in {"YES", "Y", "TRUE", "1"}:
        return "YES"
    if text in {"NO", "N", "FALSE", "0"}:
        return "NO"
    return None


def _iter_resolved_entries(records: Sequence[Mapping[str, Any]], settlements: Mapping[str, str], *, date: str = ""):
    for row in records:
        if str(row.get("type", "")) != "entry":
            continue
        if date and str(row.get("date", "")) != date:
            continue
        market_id = str(row.get("market_id", "")).strip()
        if not market_id:
            continue
        winner = _normalize_outcome(settlements.get(market_id))
        side = _normalize_outcome(row.get("side"))
        if winner is None or side is None:
            continue
        yield row, winner


def simulate_params(
    *,
    records: Sequence[Mapping[str, Any]],
    settlements: Mapping[str, str],
    min_edge: float,
    kelly_scale: float,
    base_kelly_scale: float,
    fee_bps_per_side: float,
    slippage_bps_per_side: float,
    min_trades: int,
    date: str = "",
) -> Dict[str, float]:
    cost_rate = max(0.0, 2.0 * (fee_bps_per_side + slippage_bps_per_side) / 10000.0)
    stake_ratio = (kelly_scale / base_kelly_scale) if base_kelly_scale > 0.0 else kelly_scale
    trades = 0
    stake_total = 0.0
    expected_total = 0.0
    realized_total = 0.0
    for row, winner in _iter_resolved_entries(records, settlements, date=date):
        edge = float(row.get("edge", 0.0) or 0.0)
        if edge < min_edge:
            continue
        entry_price = float(row.get("entry_price", 0.0) or 0.0)
        if entry_price <= 0.0 or entry_price >= 1.0:
            continue
        stake = float(row.get("stake_usd", 0.0) or 0.0) * max(0.0, stake_ratio)
        if stake <= 0.0:
            continue
        expected = float(row.get("expected_value_usd", 0.0) or 0.0) * max(0.0, stake_ratio)
        side = _normalize_outcome(row.get("side"))
        if side is None:
            continue
        is_win = side == winner
        gross_pnl = (stake * ((1.0 / entry_price) - 1.0)) if is_win else -stake
        net_pnl = gross_pnl - (stake * cost_rate)
        trades += 1
        stake_total += stake
        expected_total += expected
        realized_total += net_pnl

    valid = trades >= max(1, min_trades)
    realized_return = (realized_total / stake_total) if stake_total > 0.0 else 0.0
    return {
        "min_edge": float(min_edge),
        "kelly_scale": float(kelly_scale),
        "trades": float(trades),
        "stake_usd": round(stake_total, 4),
        "expected_value_usd": round(expected_total, 4),
        "realized_pnl_usd": round(realized_total, 4),
        "realized_return_on_stake": round(realized_return, 6),
        "score": round(realized_total, 6) if valid else float("-inf"),
    }


def grid_search(
    *,
    records: Sequence[Mapping[str, Any]],
    settlements: Mapping[str, str],
    edge_grid: Sequence[float],
    kelly_grid: Sequence[float],
    base_kelly_scale: float,
    fee_bps_per_side: float,
    slippage_bps_per_side: float,
    min_trades: int,
    date: str = "",
) -> Tuple[Optional[Dict[str, float]], List[Dict[str, float]]]:
    leaderboard: List[Dict[str, float]] = []
    best: Optional[Dict[str, float]] = None
    for min_edge in edge_grid:
        for kelly_scale in kelly_grid:
            result = simulate_params(
                records=records,
                settlements=settlements,
                min_edge=float(min_edge),
                kelly_scale=float(kelly_scale),
                base_kelly_scale=base_kelly_scale,
                fee_bps_per_side=fee_bps_per_side,
                slippage_bps_per_side=slippage_bps_per_side,
                min_trades=min_trades,
                date=date,
            )
            leaderboard.append(result)
            if best is None or result["score"] > best["score"]:
                best = result
    leaderboard.sort(key=lambda item: item["score"], reverse=True)
    if best is not None and best["score"] == float("-inf"):
        best = None
    return best, leaderboard


def _default_edge_grid(base: float) -> List[float]:
    steps = [-0.02, -0.01, -0.005, 0.0, 0.005, 0.01, 0.02]
    out = sorted({round(max(0.001, base + step), 6) for step in steps})
    return out


def _default_kelly_grid(base: float) -> List[float]:
    steps = [-0.2, -0.1, -0.05, 0.0, 0.05, 0.1, 0.2]
    out = sorted({round(max(0.01, base + step), 6) for step in steps})
    return out


def _apply_candidate_to_config(base_config: Mapping[str, Any], candidate: Mapping[str, float]) -> Dict[str, Any]:
    out = dict(base_config)
    out["min_edge"] = float(candidate["min_edge"])
    out["kelly_scale"] = float(candidate["kelly_scale"])
    return out


def main() -> None:
    parser = argparse.ArgumentParser(description="Auto-tune polymarket_openclaw config from realized results")
    parser.add_argument("--journal-file", required=True)
    parser.add_argument("--settlements-file", required=True)
    parser.add_argument("--base-config-file", required=True)
    parser.add_argument("--date", default="", help="Optional YYYY-MM-DD filter")
    parser.add_argument("--edge-grid", default="", help="CSV (e.g. 0.01,0.02,0.03)")
    parser.add_argument("--kelly-grid", default="", help="CSV (e.g. 0.2,0.3,0.4)")
    parser.add_argument("--min-trades", type=int, default=5)
    parser.add_argument("--fee-bps-per-side", type=float, default=-1.0)
    parser.add_argument("--slippage-bps-per-side", type=float, default=-1.0)
    parser.add_argument("--write-report", default="")
    parser.add_argument("--write-candidate-config", default="")
    args = parser.parse_args()

    records = load_journal_records(Path(args.journal_file))
    settlements = load_settlements(Path(args.settlements_file))
    base_config: Dict[str, Any] = json.loads(Path(args.base_config_file).read_text(encoding="utf-8"))
    base_min_edge = float(base_config.get("min_edge", 0.02))
    base_kelly_scale = float(base_config.get("kelly_scale", 0.35))
    fee = float(base_config.get("fee_bps_per_side", 0.0)) if args.fee_bps_per_side < 0 else args.fee_bps_per_side
    slippage = (
        float(base_config.get("slippage_bps_per_side", 0.0))
        if args.slippage_bps_per_side < 0
        else args.slippage_bps_per_side
    )

    edge_grid = parse_grid(args.edge_grid) if args.edge_grid.strip() else _default_edge_grid(base_min_edge)
    kelly_grid = parse_grid(args.kelly_grid) if args.kelly_grid.strip() else _default_kelly_grid(base_kelly_scale)

    best, leaderboard = grid_search(
        records=records,
        settlements=settlements,
        edge_grid=edge_grid,
        kelly_grid=kelly_grid,
        base_kelly_scale=base_kelly_scale,
        fee_bps_per_side=fee,
        slippage_bps_per_side=slippage,
        min_trades=max(1, args.min_trades),
        date=args.date.strip(),
    )

    output: Dict[str, Any] = {
        "date_filter": args.date.strip() or "ALL",
        "base": {"min_edge": base_min_edge, "kelly_scale": base_kelly_scale},
        "best": best,
        "leaderboard_top10": leaderboard[:10],
    }
    if best is not None:
        output["candidate_config"] = _apply_candidate_to_config(base_config, best)

    text = json.dumps(output, ensure_ascii=False, indent=2)
    print(text)

    if args.write_report:
        Path(args.write_report).write_text(text + "\n", encoding="utf-8")
    if args.write_candidate_config and best is not None:
        Path(args.write_candidate_config).write_text(
            json.dumps(output["candidate_config"], ensure_ascii=False, indent=2) + "\n",
            encoding="utf-8",
        )


if __name__ == "__main__":
    main()
