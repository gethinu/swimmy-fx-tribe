#!/usr/bin/env python3
"""Narrow confirm around the Part-2 forward-robust winners: EURUSD Keltner p~50
H4 (dev 1.5-2.0) and BB p30 dev1.5 H6, ATR barriers. Is it a coherent REGION
(neighbors also survive selection+holdout) or 4 lucky configs? Also run on GBPUSD
and EURJPY to test cross-symbol transfer. primitive_scan schema."""
from __future__ import annotations
import argparse, json

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--symbol", required=True)
    ap.add_argument("--out", required=True)
    a = ap.parse_args()
    ATR = [(2.0, 2.0), (2.0, 3.0), (3.0, 3.0)]
    m = []
    def add(prim, regime, period, dev, tf):
        for (slm, tpm) in ATR:
            m.append({"name": f"C2-{a.symbol}-{prim}-p{period}-d{dev}-tf{tf}-atr{slm}x{tpm}",
                "symbol": a.symbol, "regime": regime, "prim": prim, "period": period, "dev": dev,
                "atr_period": 14, "tf_seconds": tf * 60, "barrier_mode": "atr", "sl": slm, "tp": tpm, "max_hold": 0})
    # Keltner neighborhood
    for p in (40, 45, 50, 55, 60):
        for dev in (1.5, 2.0, 2.5):
            for tf in (180, 240, 300):
                add("keltner", ":REVERSION", p, dev, tf)
    # BB low-dev neighborhood
    for p in (25, 30, 35):
        for dev in (1.25, 1.5, 1.75, 2.0):
            for tf in (300, 360, 420):
                add("bb", ":REVERSION", p, dev, tf)
    json.dump(m, open(a.out, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
    from collections import Counter
    print(f"wrote {a.out}: {len(m)} configs (symbol={a.symbol}) prims={dict(Counter(e['prim'] for e in m))}")

if __name__ == "__main__":
    main()
