#!/usr/bin/env python3
"""Join selection + holdout scans -> honest 2-window forward-robust verdict.
forward-robust  == (oos_qualified AND cpcv_ok) in BOTH selection and holdout windows.
diverse         == not (symbol==USDJPY and regime==TREND).
No gate constant is touched here; we only read the engine's own oos_qualified/cpcv_ok flags.
"""
import json, sys, collections
D=sys.argv[1]
SYMS=["EURUSD","GBPUSD","EURJPY","USDJPY"]

def load(p):
    with open(p) as f: return {s["name"]:s for s in json.load(f)["strategies"]}

allfwd=[]; allselrob=[]
per_sym={}
for sym in SYMS:
    try:
        sel=load(f"{D}/grid_sel_{sym}.json"); hol=load(f"{D}/grid_hol_{sym}.json")
    except FileNotFoundError:
        print(f"[skip {sym}: files missing]"); continue
    sel_rob=[n for n,s in sel.items() if s["oos_qualified"] and s["cpcv_ok"]]
    fwd=[]
    for n in sel_rob:
        h=hol.get(n)
        if h and h["oos_qualified"] and h["cpcv_ok"]:
            s=sel[n]
            fwd.append(dict(name=n,symbol=s["symbol"],regime=s["regime"].strip(":"),prim=s["prim"],
                period=s["period"],dev=s["dev"],tf=s["timeframe_min"],sl=s["sl"],
                sel_pf=round(s["oos"]["pf"],3),sel_pr=s["cpcv"]["pass_rate"],
                hol_t=h["oos"]["trades"],hol_pf=round(h["oos"]["pf"],3),hol_pr=h["cpcv"]["pass_rate"],
                hol_med=round(h["cpcv"]["median_sharpe"],2),
                diverse=(not (s["symbol"]=="USDJPY" and s["regime"].strip(":").upper()=="TREND"))))
    per_sym[sym]=dict(n_configs=len(sel),sel_robust=len(sel_rob),forward_robust=len(fwd),fwd=fwd)
    allfwd+=fwd; allselrob+=[(sym,n) for n in sel_rob]

print("="*78)
print("SEARCH COVERAGE:", sum(v["n_configs"] for v in per_sym.values()),
      "configs total (", len(SYMS),"symbols x ~2352 each )")
print("="*78)
for sym in SYMS:
    if sym not in per_sym: continue
    v=per_sym[sym]
    print(f"\n### {sym}:  configs={v['n_configs']}  selection-robust(sel oos+cpcv)={v['sel_robust']}  FORWARD-ROBUST(2-window)={v['forward_robust']}")
    for r in sorted(v["fwd"], key=lambda x:-x["hol_pr"]):
        div="DIVERSE" if r["diverse"] else "usdjpy-trend"
        print(f"    {r['prim']:8} {r['regime']:9} p{r['period']} d{r['dev']} tf{r['tf']}m sl{r['sl']} | "
              f"sel(pf={r['sel_pf']},pr={r['sel_pr']:.2f}) HOLD(t={r['hol_t']},pf={r['hol_pf']},pr={r['hol_pr']:.2f},med={r['hol_med']}) [{div}]")

print("\n"+"="*78)
print("AGGREGATE — forward-robust (2-window) individuals:", len(allfwd))
div=[r for r in allfwd if r["diverse"]]
print("  of which DIVERSE (non-USDJPY-TREND):", len(div))
print("  by symbol :", dict(collections.Counter(r["symbol"] for r in allfwd)))
print("  by prim   :", dict(collections.Counter(r["prim"] for r in allfwd)))
print("  by regime :", dict(collections.Counter(r["regime"] for r in allfwd)))
print("  by (sym,prim,regime):", dict(collections.Counter((r["symbol"],r["prim"],r["regime"]) for r in allfwd)))
print("  by tf(min):", dict(collections.Counter(r["tf"] for r in allfwd)))
print("\nSelection-robust total (pre-holdout):", len(allselrob),
      " -> shrinks to", len(allfwd), "after holdout (multiple-testing / forward filter)")

# ---- CLUSTER analysis: distinct diverse EDGES vs parameter-variants of one edge ----
def tfband(m):
    return "M15" if m<60 else "H1-H2" if m<=120 else "H4-H8" if m<=480 else "H12-D1"
print("\n"+"-"*78)
print("DISTINCT DIVERSE EDGES (cluster = symbol x prim x regime x TF-band):")
clus=collections.defaultdict(list)
for r in div:
    clus[(r["symbol"],r["prim"],r["regime"],tfband(r["tf"]))].append(r)
if not clus:
    print("   (none)")
for k,v in sorted(clus.items(), key=lambda kv:-len(kv[1])):
    periods=sorted(set(x["period"] for x in v)); devs=sorted(set(x["dev"] for x in v))
    print(f"   {k[0]}/{k[1]}/{k[2]}/{k[3]}: {len(v)} variants  periods={periods} devs={devs}")
print(f"\n=> {len(clus)} distinct diverse edge-cluster(s); {len(div)} total diverse forward-robust variants")
# dump machine-readable
json.dump(dict(per_sym={k:{kk:vv for kk,vv in v.items()} for k,v in per_sym.items()},
               aggregate=dict(forward_robust=len(allfwd),diverse=len(div),
               by_sym=dict(collections.Counter(r["symbol"] for r in allfwd)),
               by_prim=dict(collections.Counter(r["prim"] for r in allfwd)))),
          open(f"{D}/SCORE_SUMMARY.json","w"), indent=1)
print("\n-> wrote", f"{D}/SCORE_SUMMARY.json")
