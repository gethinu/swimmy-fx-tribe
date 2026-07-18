#!/usr/bin/env python3
"""Decompose CPCV folds: is each failing fold density-starved (<20 trades) or a genuine loser (>=20 trades but PF<1.10 / pen<0.30)?"""
import json, sys
from datetime import datetime, timezone

# CPCV blocks are equal slices of the full input range. Print approximate calendar span per block.
def block_span(name):
    return name

for path in sys.argv[1:]:
    with open(path) as f:
        d = json.load(f)
    print(f"\n================= {path} =================")
    for s in d["strategies"]:
        oos = s["oos"]; cp = s["cpcv"]
        print(f"\n### {s['name']}  (sl={s['sl']} tp={s['tp']} mode={s['barrier_mode']})")
        print(f"  OOS 2021-2025: trades={oos['trades']}  PF={oos['pf']:.3f}  penSharpe={oos['penalized_sharpe']:.3f}  "
              f"[gate: t>=200 {'PASS' if oos['trades']>=200 else 'FAIL'} | PF>=1.10 {'PASS' if oos['pf']>=1.10 else 'FAIL'} | pen>=0.30 {'PASS' if oos['penalized_sharpe']>=0.30 else 'FAIL'}]")
        print(f"  CPCV: valid_folds={cp['valid_folds']} passing={cp['passing_folds']} pass_rate={cp['pass_rate']:.2f} median_sharpe={cp['median_sharpe']:.3f}")
        print(f"  {'blk':>3} {'trades':>7} {'PF':>7} {'penSh':>7}  reason-if-fail")
        dens_fail = subst_fail = 0
        for fo in cp["folds"]:
            t = fo["trades"]; pf = fo["pf"]; pen = fo["penalized_sharpe"]; ok = fo["pass"]
            if ok:
                reason = "PASS"
            elif t < 20:
                reason = f"density (<20 tr)"; dens_fail += 1
            else:
                bad = []
                if pf < 1.10: bad.append(f"PF{pf:.2f}<1.10")
                if pen < 0.30: bad.append(f"pen{pen:.2f}<0.30")
                reason = "SUBSTANCE: " + ",".join(bad); subst_fail += 1
            print(f"  {fo['block']:>3} {t:>7} {pf:>7.3f} {pen:>7.3f}  {reason}")
        print(f"  --> failing folds: density-starved={dens_fail}  genuine-loser(>=20tr)={subst_fail}")
