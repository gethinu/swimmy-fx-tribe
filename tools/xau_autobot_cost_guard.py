#!/usr/bin/env python3
"""Cost guard CLI for xau_autobot readiness decisions."""

from __future__ import annotations

import argparse
import json
from typing import Dict, List

try:
    import yfinance as yf
except Exception:
    yf = None


def effective_roundtrip_cost(
    *,
    spread_points: float,
    price: float,
    point: float,
    commission_roundtrip_pct: float,
    slippage_roundtrip_pct: float,
) -> float:
    if price <= 0.0 or point <= 0.0:
        return 0.0
    spread_pct = ((spread_points * point) / price) * 100.0
    return spread_pct + commission_roundtrip_pct + slippage_roundtrip_pct


def required_max_spread_points(
    *,
    break_even_roundtrip_pct: float,
    price: float,
    point: float,
    commission_roundtrip_pct: float,
    slippage_roundtrip_pct: float,
    safety_margin: float,
) -> float:
    if price <= 0.0 or point <= 0.0:
        return 0.0
    effective_budget_pct = break_even_roundtrip_pct * safety_margin
    spread_budget_pct = effective_budget_pct - commission_roundtrip_pct - slippage_roundtrip_pct
    if spread_budget_pct <= 0.0:
        return 0.0
    return (spread_budget_pct / 100.0) * price / point


def required_max_spread_points_for_ratio(
    *,
    break_even_roundtrip_pct: float,
    ratio_limit: float,
    price: float,
    point: float,
    commission_roundtrip_pct: float,
    slippage_roundtrip_pct: float,
) -> float:
    if price <= 0.0 or point <= 0.0:
        return 0.0
    budget_pct = break_even_roundtrip_pct * ratio_limit
    spread_budget_pct = budget_pct - commission_roundtrip_pct - slippage_roundtrip_pct
    if spread_budget_pct <= 0.0:
        return 0.0
    return (spread_budget_pct / 100.0) * price / point


def roundtrip_cost_verdict(*, effective_roundtrip_pct: float, break_even_roundtrip_pct: float) -> str:
    if break_even_roundtrip_pct <= 0.0:
        return "NO_GO"
    ratio = effective_roundtrip_pct / break_even_roundtrip_pct
    if ratio <= 0.7:
        return "GO"
    if ratio <= 1.0:
        return "CAUTION"
    return "NO_GO"


def build_spread_scenarios(
    *,
    spread_points_values: List[float],
    price: float,
    point: float,
    break_even_roundtrip_pct: float,
    commission_roundtrip_pct: float,
    slippage_roundtrip_pct: float,
    safety_margin: float,
) -> List[Dict[str, object]]:
    out: List[Dict[str, object]] = []
    for spread_points in spread_points_values:
        effective_pct = effective_roundtrip_cost(
            spread_points=spread_points,
            price=price,
            point=point,
            commission_roundtrip_pct=commission_roundtrip_pct,
            slippage_roundtrip_pct=slippage_roundtrip_pct,
        )
        out.append(
            {
                "spread_points": spread_points,
                "effective_roundtrip_pct": effective_pct,
                "verdict": roundtrip_cost_verdict(
                    effective_roundtrip_pct=effective_pct,
                    break_even_roundtrip_pct=break_even_roundtrip_pct,
                ),
                "safety_pass": effective_pct <= (break_even_roundtrip_pct * safety_margin),
                "go_pass": effective_pct <= (break_even_roundtrip_pct * 0.7),
            }
        )
    return out


def _load_readiness_report(path: str) -> Dict[str, object]:
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def _fetch_latest_price(ticker: str) -> float:
    if yf is None:
        raise RuntimeError("yfinance is unavailable; pass --price explicitly.")
    df = yf.download(ticker, period="1d", interval="1m", auto_adjust=False, progress=False, threads=False)
    if len(df) == 0:
        df = yf.download(ticker, period="5d", interval="5m", auto_adjust=False, progress=False, threads=False)
    if len(df) == 0:
        raise RuntimeError(f"could not fetch price for {ticker}")
    close_col = None
    for col in df.columns:
        if isinstance(col, tuple) and col[0] == "Close":
            close_col = col
            break
    if close_col is None:
        close_col = df.columns[0]
    return float(df[close_col].dropna().iloc[-1])


def _parse_spread_grid(value: str) -> List[float]:
    out: List[float] = []
    for token in value.split(","):
        token = token.strip()
        if token:
            out.append(float(token))
    return out


def main() -> None:
    parser = argparse.ArgumentParser(description="Cost guard for xau_autobot readiness report")
    parser.add_argument("--readiness-report", required=True)
    parser.add_argument("--price", type=float, default=0.0, help="Current market price")
    parser.add_argument("--price-ticker", default="GC=F", help="Ticker used only when --price is not provided")
    parser.add_argument("--point", type=float, default=0.01, help="Symbol point size (XAUUSD often 0.01)")
    parser.add_argument("--spread-points", type=float, default=-1.0, help="Observed spread in points")
    parser.add_argument("--commission-roundtrip-pct", type=float, default=0.0, help="Roundtrip commission in percent")
    parser.add_argument("--slippage-roundtrip-pct", type=float, default=0.0, help="Roundtrip slippage allowance in percent")
    parser.add_argument("--safety-margin", type=float, default=0.8, help="Use only this fraction of break-even cost")
    parser.add_argument("--spread-grid", default="", help="Comma-separated spread points scenarios (e.g. 10,25,50,75)")
    parser.add_argument("--write-report", default="")
    args = parser.parse_args()

    report = _load_readiness_report(args.readiness_report)
    break_even_roundtrip_cost = float(report.get("break_even_roundtrip_cost", 0.0))
    break_even_roundtrip_pct = break_even_roundtrip_cost * 100.0
    price = args.price if args.price > 0.0 else _fetch_latest_price(args.price_ticker)

    max_spread_points = required_max_spread_points(
        break_even_roundtrip_pct=break_even_roundtrip_pct,
        price=price,
        point=args.point,
        commission_roundtrip_pct=args.commission_roundtrip_pct,
        slippage_roundtrip_pct=args.slippage_roundtrip_pct,
        safety_margin=args.safety_margin,
    )
    max_spread_points_go = required_max_spread_points_for_ratio(
        break_even_roundtrip_pct=break_even_roundtrip_pct,
        ratio_limit=0.7,
        price=price,
        point=args.point,
        commission_roundtrip_pct=args.commission_roundtrip_pct,
        slippage_roundtrip_pct=args.slippage_roundtrip_pct,
    )
    max_spread_points_break_even = required_max_spread_points_for_ratio(
        break_even_roundtrip_pct=break_even_roundtrip_pct,
        ratio_limit=1.0,
        price=price,
        point=args.point,
        commission_roundtrip_pct=args.commission_roundtrip_pct,
        slippage_roundtrip_pct=args.slippage_roundtrip_pct,
    )

    output: Dict[str, object] = {
        "readiness_report": args.readiness_report,
        "price": price,
        "point": args.point,
        "break_even_roundtrip_pct": break_even_roundtrip_pct,
        "safety_margin": args.safety_margin,
        "commission_roundtrip_pct": args.commission_roundtrip_pct,
        "slippage_roundtrip_pct": args.slippage_roundtrip_pct,
        "max_spread_points_go": max_spread_points_go,
        "max_spread_points_safe": max_spread_points,
        "max_spread_points_break_even": max_spread_points_break_even,
        "max_spread_price_distance": max_spread_points * args.point,
    }

    if args.spread_points >= 0.0:
        effective_pct = effective_roundtrip_cost(
            spread_points=args.spread_points,
            price=price,
            point=args.point,
            commission_roundtrip_pct=args.commission_roundtrip_pct,
            slippage_roundtrip_pct=args.slippage_roundtrip_pct,
        )
        output["spread_points"] = args.spread_points
        output["effective_roundtrip_pct"] = effective_pct
        output["verdict"] = roundtrip_cost_verdict(
            effective_roundtrip_pct=effective_pct,
            break_even_roundtrip_pct=break_even_roundtrip_pct,
        )
        output["safety_pass"] = effective_pct <= (break_even_roundtrip_pct * args.safety_margin)

    if args.spread_grid:
        output["scenarios"] = build_spread_scenarios(
            spread_points_values=_parse_spread_grid(args.spread_grid),
            price=price,
            point=args.point,
            break_even_roundtrip_pct=break_even_roundtrip_pct,
            commission_roundtrip_pct=args.commission_roundtrip_pct,
            slippage_roundtrip_pct=args.slippage_roundtrip_pct,
            safety_margin=args.safety_margin,
        )

    print(json.dumps(output, ensure_ascii=True))
    if args.write_report:
        with open(args.write_report, "w", encoding="utf-8") as f:
            json.dump(output, f, ensure_ascii=True, indent=2)
            f.write("\n")
        print(json.dumps({"written_report": args.write_report}, ensure_ascii=True))


if __name__ == "__main__":
    main()
