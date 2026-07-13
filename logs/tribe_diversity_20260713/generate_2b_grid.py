#!/usr/bin/env python3
"""2b decisive experiment: do NON-TREND primitives have a CPCV-robust edge?

The kill_oos_cpcv bin dispatches indicator_type to REAL signal generators
(verified in backtester.rs:818-843):
  rsi  -> generate_rsi_signals  : buy oversold-bounce(30) / sell(70)  [MEAN-REVERSION]
  bb   -> generate_bb_signals   : buy lower-band bounce / exit middle [MEAN-REVERSION]
  stoch-> generate_stoch_signals: oversold(20)/overbought(80)          [MEAN-REVERSION]
  macd -> generate_macd_signals : signal-line cross                    [MOMENTUM]
Period comes from sma_short (rsi>=7, bb>=20, stoch>=14); thresholds/dev are
hardcoded in the engine, so we sweep period + timeframe + pip-scaled SL/TP.

This is a genuine test of §4 diversity by BEHAVIOR (non-:TREND) and by SYMBOL
(non-USDJPY), because the engine actually runs the mean-reversion logic (unlike
the SMA-collapse of the live emit path). Run each symbol with its pip-matched
--slippage. No honest_gate/§4 floor is touched.
"""
from __future__ import annotations
import argparse, json

# (indicator_type, category-label, [periods], [timeframes_min])
PRIMS = [
    ("rsi",   ":REVERSION", [7, 14, 21],      [15, 60, 240]),
    ("bb",    ":REVERSION", [20, 30, 50],     [15, 60, 240]),
    ("stoch", ":REVERSION", [14, 21],         [15, 60, 240]),
    ("macd",  ":MOMENTUM",  [12],             [15, 60, 240]),  # macd ignores period
]
BARRIERS_PIPS = [(30, 60), (50, 100), (50, 50), (100, 100)]  # MR: incl symmetric/tight-TP

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--symbol", required=True)
    ap.add_argument("--pip", type=float, required=True, help="0.01 JPY pairs, 0.0001 non-JPY")
    ap.add_argument("--out", required=True)
    args = ap.parse_args()
    m = []
    for (it, cat, periods, tfs) in PRIMS:
        for period in periods:
            for tf in tfs:
                for (slp, tpp) in BARRIERS_PIPS:
                    m.append({
                        "name": f"2B-{args.symbol}-{it}-p{period}-tf{tf}-sl{slp}tp{tpp}",
                        "symbol": args.symbol,
                        "category": cat,
                        "indicator_type": it,
                        "timeframe_min": tf,
                        "tf_seconds": tf * 60,
                        "sma_short": period,      # period for rsi/bb/stoch; macd ignores
                        "sma_long": period * 2,   # unused by MR gens; kept sane
                        "sl": round(slp * args.pip, 8),
                        "tp": round(tpp * args.pip, 8),
                        "volume": 0.01,
                        "indicators_raw": f"({it.upper()} {period})",
                        "rank": "GRID2B",
                        "is_trades": None, "is_pf": None, "is_sharpe": None,
                    })
    json.dump(m, open(args.out, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
    from collections import Counter
    print(f"wrote {args.out}: {len(m)} configs (symbol={args.symbol}) "
          f"by prim={dict(Counter(e['indicator_type'] for e in m))}")

if __name__ == "__main__":
    main()
