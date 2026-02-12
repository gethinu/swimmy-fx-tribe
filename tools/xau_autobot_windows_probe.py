#!/usr/bin/env python3
"""Windows/MT5 probe: measure live spread and evaluate xau_autobot readiness."""

from __future__ import annotations

import argparse
import json
import time
from typing import Dict, List

try:
    from tools.xau_autobot_cost_guard import (
        effective_roundtrip_cost,
        required_max_spread_points,
        required_max_spread_points_for_ratio,
        roundtrip_cost_verdict,
    )
except Exception:
    from xau_autobot_cost_guard import (  # type: ignore
        effective_roundtrip_cost,
        required_max_spread_points,
        required_max_spread_points_for_ratio,
        roundtrip_cost_verdict,
    )

try:
    import MetaTrader5 as mt5  # type: ignore
except Exception:
    mt5 = None


def compute_spread_points(*, ask: float, bid: float, point: float) -> float:
    if point <= 0.0:
        return 0.0
    return (ask - bid) / point


def percentile(values: List[float], p: float) -> float:
    if not values:
        return 0.0
    if p <= 0.0:
        return min(values)
    if p >= 100.0:
        return max(values)
    s = sorted(values)
    idx = (len(s) - 1) * (p / 100.0)
    lo = int(idx)
    hi = min(lo + 1, len(s) - 1)
    frac = idx - lo
    return s[lo] * (1.0 - frac) + s[hi] * frac


def summarize_spreads(spread_points: List[float]) -> Dict[str, float]:
    if not spread_points:
        return {"count": 0.0, "min": 0.0, "median": 0.0, "p95": 0.0, "max": 0.0, "mean": 0.0}
    total = sum(spread_points)
    return {
        "count": float(len(spread_points)),
        "min": float(min(spread_points)),
        "median": float(percentile(spread_points, 50.0)),
        "p95": float(percentile(spread_points, 95.0)),
        "max": float(max(spread_points)),
        "mean": float(total / len(spread_points)),
    }


def evaluate_spread(
    *,
    spread_points: float,
    price: float,
    point: float,
    break_even_roundtrip_pct: float,
    commission_roundtrip_pct: float,
    slippage_roundtrip_pct: float,
    safety_margin: float,
) -> Dict[str, object]:
    effective_pct = effective_roundtrip_cost(
        spread_points=spread_points,
        price=price,
        point=point,
        commission_roundtrip_pct=commission_roundtrip_pct,
        slippage_roundtrip_pct=slippage_roundtrip_pct,
    )
    verdict = roundtrip_cost_verdict(
        effective_roundtrip_pct=effective_pct,
        break_even_roundtrip_pct=break_even_roundtrip_pct,
    )
    return {
        "spread_points": spread_points,
        "effective_roundtrip_pct": effective_pct,
        "verdict": verdict,
        "safety_pass": effective_pct <= (break_even_roundtrip_pct * safety_margin),
        "go_pass": effective_pct <= (break_even_roundtrip_pct * 0.7),
    }


def _load_readiness(path: str) -> Dict[str, object]:
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def _probe_mt5_spreads(symbol: str, samples: int, interval_ms: int) -> Dict[str, object]:
    if mt5 is None:
        raise RuntimeError("MetaTrader5 package is unavailable in this environment")
    if not mt5.initialize():
        raise RuntimeError(f"mt5.initialize failed: {mt5.last_error()}")
    try:
        if not mt5.symbol_select(symbol, True):
            raise RuntimeError(f"symbol_select failed for {symbol}: {mt5.last_error()}")
        info = mt5.symbol_info(symbol)
        if info is None:
            raise RuntimeError(f"symbol_info unavailable for {symbol}")
        point = float(info.point or 0.0)
        if point <= 0.0:
            raise RuntimeError("invalid point size")

        spreads: List[float] = []
        last_price = 0.0
        for _ in range(max(1, samples)):
            tick = mt5.symbol_info_tick(symbol)
            if tick is not None:
                ask = float(tick.ask)
                bid = float(tick.bid)
                if ask > 0.0 and bid > 0.0:
                    spreads.append(compute_spread_points(ask=ask, bid=bid, point=point))
                    last_price = (ask + bid) / 2.0
            time.sleep(max(0.01, interval_ms / 1000.0))

        if not spreads:
            raise RuntimeError("no valid spread samples collected")

        return {
            "symbol": symbol,
            "point": point,
            "price": last_price,
            "spreads": spreads,
            "summary": summarize_spreads(spreads),
        }
    finally:
        mt5.shutdown()


def main() -> None:
    parser = argparse.ArgumentParser(description="Probe MT5 spread and evaluate readiness cost guard")
    parser.add_argument("--readiness-report", required=True)
    parser.add_argument("--symbol", default="XAUUSD")
    parser.add_argument("--samples", type=int, default=120)
    parser.add_argument("--interval-ms", type=int, default=500)
    parser.add_argument("--price", type=float, default=0.0, help="Override price (uses MT5 mid-price if omitted)")
    parser.add_argument("--point", type=float, default=0.0, help="Override point size (uses MT5 symbol point if omitted)")
    parser.add_argument("--spread-points", type=float, default=-1.0, help="Override spread points")
    parser.add_argument("--commission-roundtrip-pct", type=float, default=0.02)
    parser.add_argument("--slippage-roundtrip-pct", type=float, default=0.01)
    parser.add_argument("--safety-margin", type=float, default=0.8)
    parser.add_argument("--write-report", default="")
    args = parser.parse_args()

    readiness = _load_readiness(args.readiness_report)
    break_even_roundtrip_pct = float(readiness.get("break_even_roundtrip_cost", 0.0)) * 100.0

    spreads: List[float] = []
    point = args.point
    price = args.price

    if args.spread_points >= 0.0:
        spreads = [args.spread_points]
        if point <= 0.0:
            point = 0.01
    else:
        mt5_result = _probe_mt5_spreads(args.symbol, args.samples, args.interval_ms)
        spreads = list(mt5_result["spreads"])
        if point <= 0.0:
            point = float(mt5_result["point"])
        if price <= 0.0:
            price = float(mt5_result["price"])

    if price <= 0.0:
        raise RuntimeError("price is required (either via MT5 probe or --price)")
    if point <= 0.0:
        raise RuntimeError("point is required (either via MT5 probe or --point)")

    summary = summarize_spreads(spreads)
    selected_spread = float(summary["p95"] if len(spreads) > 1 else spreads[0])
    evaluation = evaluate_spread(
        spread_points=selected_spread,
        price=price,
        point=point,
        break_even_roundtrip_pct=break_even_roundtrip_pct,
        commission_roundtrip_pct=args.commission_roundtrip_pct,
        slippage_roundtrip_pct=args.slippage_roundtrip_pct,
        safety_margin=args.safety_margin,
    )

    max_spread_points_go = required_max_spread_points_for_ratio(
        break_even_roundtrip_pct=break_even_roundtrip_pct,
        ratio_limit=0.7,
        price=price,
        point=point,
        commission_roundtrip_pct=args.commission_roundtrip_pct,
        slippage_roundtrip_pct=args.slippage_roundtrip_pct,
    )
    max_spread_points_safe = required_max_spread_points(
        break_even_roundtrip_pct=break_even_roundtrip_pct,
        price=price,
        point=point,
        commission_roundtrip_pct=args.commission_roundtrip_pct,
        slippage_roundtrip_pct=args.slippage_roundtrip_pct,
        safety_margin=args.safety_margin,
    )
    max_spread_points_break_even = required_max_spread_points_for_ratio(
        break_even_roundtrip_pct=break_even_roundtrip_pct,
        ratio_limit=1.0,
        price=price,
        point=point,
        commission_roundtrip_pct=args.commission_roundtrip_pct,
        slippage_roundtrip_pct=args.slippage_roundtrip_pct,
    )

    output: Dict[str, object] = {
        "readiness_report": args.readiness_report,
        "symbol": args.symbol,
        "price": price,
        "point": point,
        "break_even_roundtrip_pct": break_even_roundtrip_pct,
        "commission_roundtrip_pct": args.commission_roundtrip_pct,
        "slippage_roundtrip_pct": args.slippage_roundtrip_pct,
        "safety_margin": args.safety_margin,
        "max_spread_points_go": max_spread_points_go,
        "max_spread_points_safe": max_spread_points_safe,
        "max_spread_points_break_even": max_spread_points_break_even,
        "spread_summary": summary,
        "selected_spread_points": selected_spread,
        "evaluation": evaluation,
    }

    print(json.dumps(output, ensure_ascii=True))
    if args.write_report:
        with open(args.write_report, "w", encoding="utf-8") as f:
            json.dump(output, f, ensure_ascii=True, indent=2)
            f.write("\n")
        print(json.dumps({"written_report": args.write_report}, ensure_ascii=True))


if __name__ == "__main__":
    main()
