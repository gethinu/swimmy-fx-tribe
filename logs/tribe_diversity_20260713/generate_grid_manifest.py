#!/usr/bin/env python3
"""Generate a clean per-symbol SMA grid manifest with pip-scaled barriers.

Unlike relabel (which ports USDJPY-overfit configs), this sweeps a fresh SMA
grid so we can ask, fairly and per-symbol:

  "Does ANY SMA(short/long)+SL/TP config on <SYM>'s OWN 2015-2024 data clear
   the UNCHANGED §4 robustness bar (CPCV pass_rate>=0.6 & median<2.0 &
   trades>=200 & PF>=1.10 & penalized_sharpe>=0.3)?"

Barriers are specified in PIPS and converted to the symbol's price units
(JPY pairs pip=0.01, non-JPY majors pip=0.0001). Run the produced manifest
with the matching --slippage (JPY 0.01, non-JPY 0.0001) so cost is a true
2-pip round-trip on every symbol. No honest_gate floor is touched.
"""
from __future__ import annotations
import argparse, json

TFS = [60, 240, 1440]                      # H1, H4, D1
SMAS = [(10, 30), (15, 50), (20, 60), (30, 80),
        (50, 100), (50, 150), (80, 150), (100, 200)]
BARRIERS_PIPS = [(30, 60), (50, 100), (80, 160), (150, 300)]  # (sl, tp), trend R:R

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--symbol", required=True)
    ap.add_argument("--pip", type=float, required=True, help="0.01 JPY pairs, 0.0001 non-JPY")
    ap.add_argument("--out", required=True)
    args = ap.parse_args()
    m = []
    for tf in TFS:
        for (s, l) in SMAS:
            for (slp, tpp) in BARRIERS_PIPS:
                m.append({
                    "name": f"GRID-{args.symbol}-tf{tf}-{s}_{l}-sl{slp}tp{tpp}",
                    "symbol": args.symbol,
                    "category": ":GRID",
                    "indicator_type": "sma",
                    "timeframe_min": tf,
                    "tf_seconds": tf * 60,
                    "sma_short": s,
                    "sma_long": l,
                    "sl": round(slp * args.pip, 8),
                    "tp": round(tpp * args.pip, 8),
                    "volume": 0.01,
                    "indicators_raw": f"(SMA {s})(SMA {l})",
                    "rank": "GRID",
                    "is_trades": None, "is_pf": None, "is_sharpe": None,
                })
    json.dump(m, open(args.out, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
    print(f"wrote {args.out}: {len(m)} grid configs (symbol={args.symbol}, pip={args.pip})")

if __name__ == "__main__":
    main()
