#!/usr/bin/env python3
"""Relabel the 489-config gonogo manifest onto a different symbol.

We take the EXACT evolved SMA configs the USDJPY population produced
(manifest_full.json) and re-point their `symbol` label at a target symbol.
The kill_oos_cpcv bin scores against whatever --data CSV it is given; the
symbol field only drives the diversity label in score_gate.py. So running
this relabeled manifest against <SYM>_M1.csv answers, honestly:

  "If these exact evolved trend-following configs were traded on <SYM>
   instead of USDJPY, would any pass the UNCHANGED honest_gate + §4 bar?"

No floors are touched. Category is left as-is (already cosmetic); the
diversity that matters here is the genuinely different price series.
"""
from __future__ import annotations
import argparse, json

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", dest="inp", required=True)
    ap.add_argument("--symbol", required=True, help="target symbol label, e.g. EURUSD")
    ap.add_argument("--sl-tp-scale", type=float, default=1.0,
                    help="multiply sl/tp price-distances by this (÷ by 100 = 0.01 to convert "
                         "JPY-pip distances to non-JPY-pip distances, since pip 0.01->0.0001)")
    ap.add_argument("--out", required=True)
    args = ap.parse_args()
    m = json.load(open(args.inp, encoding="utf-8"))
    for e in m:
        e["symbol"] = args.symbol
        if args.sl_tp_scale != 1.0:
            e["sl"] = e["sl"] * args.sl_tp_scale
            e["tp"] = e["tp"] * args.sl_tp_scale
    json.dump(m, open(args.out, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
    print(f"relabeled {len(m)} configs -> symbol={args.symbol} -> {args.out}")

if __name__ == "__main__":
    main()
