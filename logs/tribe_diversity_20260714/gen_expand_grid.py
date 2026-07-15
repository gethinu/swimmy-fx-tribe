#!/usr/bin/env python3
"""Part 1 broad sweep: the REAL diverse primitives (mean-reversion + momentum)
across a wide TF and barrier space, for the forward-robust search. Uses the
existing kill_oos_cpcv harness (no backbone change). Volume is synthetic (const
1.0) in this data, so volume-based prims (vwap/volsma/vpoc/vwapvr) are EXCLUDED
as degenerate. Barriers pip-scaled per symbol; wide sweep approximates ATR/vol
scaling and longer TFs give longer holding. score with score_forward.py which
joins the selection(2021-24) and holdout(2015-20) runs."""
from __future__ import annotations
import argparse, json

# (indicator_type, regime-label, [periods])   -- macd ignores period
PRIMS = [
    ("rsi",   ":REVERSION", [7, 14, 21]),
    ("bb",    ":REVERSION", [20, 30, 50]),
    ("stoch", ":REVERSION", [14, 21]),
    ("macd",  ":MOMENTUM",  [12]),
]
TFS = [60, 120, 240, 360, 480]          # H1..H8 -> longer holding at higher TF
BARRIERS_PIPS = [(30, 60), (50, 50), (50, 100), (80, 80), (120, 120), (100, 200)]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--symbol", required=True)
    ap.add_argument("--pip", type=float, required=True)
    ap.add_argument("--out", required=True)
    a = ap.parse_args()
    m = []
    for (it, cat, periods) in PRIMS:
        for period in periods:
            for tf in TFS:
                for (slp, tpp) in BARRIERS_PIPS:
                    m.append({
                        "name": f"EX-{a.symbol}-{it}-p{period}-tf{tf}-sl{slp}tp{tpp}",
                        "symbol": a.symbol, "category": cat, "indicator_type": it,
                        "timeframe_min": tf, "tf_seconds": tf * 60,
                        "sma_short": period, "sma_long": period * 2,
                        "sl": round(slp * a.pip, 8), "tp": round(tpp * a.pip, 8),
                        "volume": 0.01, "indicators_raw": f"({it.upper()} {period})",
                        "rank": "EXPAND", "is_trades": None, "is_pf": None, "is_sharpe": None,
                    })
    json.dump(m, open(a.out, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
    from collections import Counter
    print(f"wrote {a.out}: {len(m)} configs (symbol={a.symbol}) "
          f"prims={dict(Counter(e['indicator_type'] for e in m))}")

if __name__ == "__main__":
    main()
