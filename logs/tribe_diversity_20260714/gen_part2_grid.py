#!/usr/bin/env python3
"""Part 2 grid for primitive_scan (the self-contained, engine-validated bin):
the primitives the live engine does NOT have. ATR-normalized barriers (sl/tp are
ATR multiples set per-trade at entry), variable-dev Bollinger, Keltner channel,
Donchian breakout. Higher TFs = longer holding. Same forward gate (selection
2021-24 + holdout 2015-20) applied by score_forward.py."""
from __future__ import annotations
import argparse, json

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--symbol", required=True)
    ap.add_argument("--out", required=True)
    a = ap.parse_args()
    TF_MR = [240, 360]        # H4, H6  (mean-reversion / longer holding)
    TF_BO = [240, 360, 480]   # H4, H6, H8 (breakout)
    ATR_BARRIERS = [(1.5, 1.5), (2.0, 2.0), (2.0, 3.0), (3.0, 3.0)]  # (sl_mult, tp_mult)
    m = []
    def add(prim, regime, period, dev, tf):
        for (slm, tpm) in ATR_BARRIERS:
            m.append({
                "name": f"P2-{a.symbol}-{prim}-p{period}-d{dev}-tf{tf}-atr{slm}x{tpm}",
                "symbol": a.symbol, "regime": regime, "prim": prim,
                "period": period, "dev": dev, "atr_period": 14,
                "tf_seconds": tf * 60, "barrier_mode": "atr", "sl": slm, "tp": tpm, "max_hold": 0,
            })
    # variable-dev Bollinger mean-reversion
    for p in (20, 30, 50):
        for dev in (1.5, 2.0, 2.5, 3.0):
            for tf in TF_MR:
                add("bb", ":REVERSION", p, dev, tf)
    # Keltner channel mean-reversion (dev = ATR multiple for the band)
    for p in (20, 30, 50):
        for mult in (1.5, 2.0, 2.5):
            for tf in TF_MR:
                add("keltner", ":REVERSION", p, mult, tf)
    # Donchian breakout (dev unused)
    for p in (20, 30, 55):
        for tf in TF_BO:
            add("donchian", ":BREAKOUT", p, 0.0, tf)
    json.dump(m, open(a.out, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
    from collections import Counter
    print(f"wrote {a.out}: {len(m)} configs (symbol={a.symbol}) "
          f"prims={dict(Counter(e['prim'] for e in m))}")

if __name__ == "__main__":
    main()
