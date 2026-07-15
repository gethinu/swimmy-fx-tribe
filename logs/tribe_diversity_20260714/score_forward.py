#!/usr/bin/env python3
"""Join a selection-window run (OOS 2021-24, CPCV full) with a holdout-window run
(OOS 2015-20, CPCV within 2015-20) of the SAME manifest, and count FORWARD-ROBUST
configs. A config is forward-robust only if it clears the §4 bar on the selection
window AND still survives on the untouched holdout — the exact kill-switch that
retired the EURUSD BB-MR H4 candidate. No floor is touched.

selection-robust : sel oos_qualified (t>=200 & PF>=1.10 & pen>=0.3) AND sel CPCV pr>=0.6
forward-robust   : selection-robust AND holdout oos_qualified AND holdout CPCV pr>=0.6
forward-lite     : selection-robust AND holdout OOS PF>=1.10 AND holdout CPCV median>0
diverse          : symbol!=USDJPY OR category!=TREND
"""
from __future__ import annotations
import argparse, json, re
from collections import Counter

def load(path):
    d = json.load(open(path, encoding="utf-8"))
    return {s["name"]: s for s in d["strategies"]}

def regime(name, cat):
    c = str(cat).lstrip(":").upper()
    return c

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--selection", required=True)
    ap.add_argument("--holdout", required=True)
    ap.add_argument("--out", required=True)
    a = ap.parse_args()
    sel, hol = load(a.selection), load(a.holdout)
    rows = []
    for name, s in sel.items():
        h = hol.get(name)
        if h is None:
            continue
        sel_robust = s["oos_qualified"] and (s["cpcv"]["pass_rate"] >= 0.60)
        hol_qual = h["oos_qualified"]
        hol_cpcv_ok = h["cpcv"]["pass_rate"] >= 0.60
        hol_pf = h["oos"]["pf"]; hol_med = h["cpcv"]["median_sharpe"]
        fwd_robust = sel_robust and hol_qual and hol_cpcv_ok
        fwd_lite = sel_robust and (hol_pf >= 1.10) and (hol_med > 0)
        cat = regime(name, s.get("category", s.get("regime", ":?")))
        diverse = (s["symbol"] != "USDJPY") or (cat != "TREND")
        prim = s.get("indicator_type", s.get("prim", "?"))
        rows.append({
            "name": name, "symbol": s["symbol"], "regime": cat, "prim": prim,
            "tf": s["timeframe_min"], "period": s.get("sma_short", s.get("period")),
            "sel_oos_t": s["oos"]["trades"], "sel_oos_pf": round(s["oos"]["pf"], 3),
            "sel_pen": round(s["oos"]["penalized_sharpe"], 3),
            "sel_cpcv_pr": round(s["cpcv"]["pass_rate"], 2), "sel_cpcv_med": round(s["cpcv"]["median_sharpe"], 3),
            "hol_oos_t": h["oos"]["trades"], "hol_oos_pf": round(hol_pf, 3),
            "hol_pen": round(h["oos"]["penalized_sharpe"], 3),
            "hol_cpcv_pr": round(h["cpcv"]["pass_rate"], 2), "hol_cpcv_med": round(hol_med, 3),
            "sel_robust": sel_robust, "fwd_robust": fwd_robust, "fwd_lite": fwd_lite, "diverse": diverse,
        })
    sel_rob = [r for r in rows if r["sel_robust"]]
    fwd_rob = [r for r in rows if r["fwd_robust"]]
    fwd_lite = [r for r in rows if r["fwd_lite"]]
    fwd_rob_div = [r for r in fwd_rob if r["diverse"]]
    summary = {
        "n": len(rows),
        "selection_robust": len(sel_rob),
        "forward_robust(sel+holdout both pass CPCV)": len(fwd_rob),
        "forward_robust_AND_diverse": len(fwd_rob_div),
        "forward_lite(sel_robust + holdout PF>=1.1 & med>0)": len(fwd_lite),
        "by_symbol_selrobust": dict(Counter(r["symbol"] for r in sel_rob)),
    }
    out = {"summary": summary,
           "forward_robust": sorted(fwd_rob, key=lambda r: -r["hol_oos_pf"]),
           "forward_lite": sorted(fwd_lite, key=lambda r: -r["hol_oos_pf"]),
           "selection_robust": sorted(sel_rob, key=lambda r: -r["sel_oos_pf"]),
           "all_rows": rows}
    json.dump(out, open(a.out, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
    print(f"=== FORWARD JOIN {a.selection.split('/')[-1]} ===")
    for k, v in summary.items():
        print(f"  {k}: {v}")
    if sel_rob:
        print(f"  --- selection-robust (survive selection window) [{len(sel_rob)}] ---")
        for r in sel_rob[:30]:
            tag = "FWD-ROBUST" if r["fwd_robust"] else ("fwd-lite" if r["fwd_lite"] else "holdout-FAIL")
            print(f'    {r["name"][:34]:34} {r["prim"]:5} sel(pf={r["sel_oos_pf"]},pr={r["sel_cpcv_pr"]}) '
                  f'HOLD(t={r["hol_oos_t"]},pf={r["hol_oos_pf"]},pr={r["hol_cpcv_pr"]},med={r["hol_cpcv_med"]}) -> {tag}')

if __name__ == "__main__":
    main()
