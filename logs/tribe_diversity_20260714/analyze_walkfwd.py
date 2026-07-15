#!/usr/bin/env python3
"""Walk-forward temporal-stability analysis of the forward-robust Keltner/BB region.
For each forward-robust config (from c2_fwd_<sym>.json) pull its OOS PF / pen-sharpe /
CPCV across 5 disjoint 2-year windows (2015-16 .. 2023-24). Count in how many windows
it stays profitable (PF>=1.10) and CPCV-positive. NB: a 2-year H4 window holds ~100
trades (< the 200 floor), so these are DIAGNOSTIC stability metrics, not §4 PASSes —
the floor-respecting verdict remains the 200+trade 2-window (2021-24 / 2015-20) test."""
from __future__ import annotations
import json, sys
D = "logs/tribe_diversity_20260714"
WINS = ["w1", "w2", "w3", "w4", "w5"]
WLBL = {"w1": "2015-16", "w2": "2017-18", "w3": "2019-20", "w4": "2021-22", "w5": "2023-24"}

def load_win(sym, w):
    return {s["name"]: s for s in json.load(open(f"{D}/wf_{sym}_{w}.json", encoding="utf-8"))["strategies"]}

def main():
    for sym in ("EURUSD", "GBPUSD"):
        try:
            fr = json.load(open(f"{D}/c2_fwd_{sym}.json", encoding="utf-8"))["forward_robust"]
        except FileNotFoundError:
            continue
        names = [r["name"] for r in fr]
        if not names:
            print(f"=== {sym}: 0 forward-robust configs, skipping ===\n"); continue
        wins = {w: load_win(sym, w) for w in WINS}
        print(f"=== {sym}: walk-forward of {len(names)} forward-robust configs across 5x 2yr windows ===")
        print(f"    {'config':44} " + " ".join(f"{WLBL[w]:>9}" for w in WINS) + "  #PF>=1.1 #pen>0")
        allpos = 0
        for nm in names:
            pfs, pens, trs = [], [], []
            for w in WINS:
                s = wins[w].get(nm)
                pf = s["oos"]["pf"] if s else 0.0
                pen = s["oos"]["penalized_sharpe"] if s else 0.0
                tr = s["oos"]["trades"] if s else 0
                pfs.append(pf); pens.append(pen); trs.append(tr)
            n_pf = sum(1 for p in pfs if p >= 1.10)
            n_pen = sum(1 for p in pens if p > 0.0)
            if n_pf == 5: allpos += 1
            cells = " ".join(f"{p:9.3f}" for p in pfs)
            print(f"    {nm[:44]:44} {cells}   {n_pf}/5     {n_pen}/5   (trades {min(trs)}-{max(trs)})")
        print(f"    >>> {sym}: forward-robust configs profitable (PF>=1.10) in ALL 5 windows: {allpos}/{len(names)}")
        # region-wide: how many of ALL c2 configs are PF>=1.10 in >=4 of 5 windows
        allcfg = set()
        for w in WINS: allcfg |= set(wins[w].keys())
        ge4 = 0
        for nm in allcfg:
            n = sum(1 for w in WINS if (wins[w].get(nm) or {}).get("oos", {}).get("pf", 0) >= 1.10)
            if n >= 4: ge4 += 1
        print(f"    >>> {sym}: ALL {len(allcfg)} region configs with PF>=1.10 in >=4/5 windows: {ge4}\n")

if __name__ == "__main__":
    main()
