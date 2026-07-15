#!/usr/bin/env python3
"""Apply honest_gate (AS-IS, no floor tampering) to the real-cost OOS/CPCV results.

Two lenses, both using tools.tribe.honest_gate.evaluate unchanged:

  A. PRODUCTION-FAITHFUL (what feed/export.py emits after an honest OOS backfill):
       floor  = reported IS trades / pf / sharpe   (the stored SQL columns)
       valid  = REAL 2pip OOS sharpe / CPCV pass_rate / CPCV median  (this experiment)
     -> PASS = IS_trades>=200 & IS_pf>=1.10 & IS_sharpe>=0.10 & validated

  B. ALL-REAL FORWARD (stricter honesty: gate the real 2pip OOS window itself):
       floor  = REAL OOS trades / pf / sharpe
       valid  = REAL OOS sharpe / CPCV
     -> PASS = OOS_trades>=200 & OOS_pf>=1.10 & OOS_sharpe>=0.10 & validated

We also tag, for every production-PASS, whether it is a *robust real edge*
(real OOS PF>=1.10 AND CPCV median_sharpe>0) and whether it is diverse
(non-USDJPY OR non-:TREND), and re-state the §4 decisive verdict.
"""
from __future__ import annotations
import json, sys, argparse
sys.path.insert(0, ".")
from tools.tribe.honest_gate import evaluate, GateConfig

def prod_candidate(s):
    return {
        "trades": s.get("reported_is_trades"),
        "profit_factor": s.get("reported_is_pf"),
        "sharpe": s.get("reported_is_sharpe"),
        "oos_sharpe": s["oos"]["sharpe"],
        "cpcv_pass_rate": s["cpcv"]["pass_rate"],
        "cpcv_median_sharpe": s["cpcv"]["median_sharpe"],
        "entry": "SMA-CROSS",   # guardian real path always has entry logic
    }

def allreal_candidate(s):
    return {
        "trades": s["oos"]["trades"],
        "profit_factor": s["oos"]["pf"],
        "sharpe": s["oos"]["sharpe"],
        "oos_sharpe": s["oos"]["sharpe"],
        "cpcv_pass_rate": s["cpcv"]["pass_rate"],
        "cpcv_median_sharpe": s["cpcv"]["median_sharpe"],
        "entry": "SMA-CROSS",
    }

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--results", required=True)
    ap.add_argument("--out", required=True)
    args = ap.parse_args()
    cfg = GateConfig()  # DEFAULTS: min_trades=200, min_pf=1.10, min_sharpe=0.10
    d = json.load(open(args.results, encoding="utf-8"))
    strats = d["strategies"]
    rows = []
    for s in strats:
        pv = evaluate(prod_candidate(s), cfg).verdict
        av = evaluate(allreal_candidate(s), cfg).verdict
        real_edge = (s["oos"]["pf"] >= 1.10) and (s["cpcv"]["median_sharpe"] > 0)
        # category is stored WITHOUT the leading colon ("TREND"); normalise so the
        # diversity test is real (the §4 bin's ":TREND" compare was a false-positive bug).
        cat_norm = str(s["category"]).lstrip(":").upper()
        diverse = (s["symbol"] != "USDJPY") or (cat_norm != "TREND")
        robust = (s["oos"]["pf"] >= 1.10) and (s["cpcv"]["pass_rate"] >= 0.60)
        rows.append({
            "name": s["name"], "symbol": s["symbol"], "category": s["category"],
            "tf": s["timeframe_min"], "sma": f'{s["sma_short"]}/{s["sma_long"]}',
            "is_trades": s.get("reported_is_trades"), "is_pf": s.get("reported_is_pf"),
            "is_sharpe": s.get("reported_is_sharpe"),
            "oos_trades": s["oos"]["trades"], "oos_pf": round(s["oos"]["pf"], 3),
            "oos_sharpe": round(s["oos"]["sharpe"], 3),
            "oos_pen_sharpe": round(s["oos"]["penalized_sharpe"], 3),
            "cpcv_pr": round(s["cpcv"]["pass_rate"], 2),
            "cpcv_median_sharpe": round(s["cpcv"]["median_sharpe"], 3),
            "prod_verdict": pv, "allreal_verdict": av,
            "real_edge": real_edge, "robust": robust, "diverse": diverse,
            "oos_qualified": s["oos_qualified"], "cpcv_ok": s["cpcv_ok"],
        })

    def cnt(key, val=True):
        return sum(1 for r in rows if r[key] == val)
    prod_pass = [r for r in rows if r["prod_verdict"] == "PASS"]
    allreal_pass = [r for r in rows if r["allreal_verdict"] == "PASS"]
    prod_pass_realedge = [r for r in prod_pass if r["real_edge"]]
    prod_pass_robust = [r for r in prod_pass if r["robust"]]
    prod_pass_diverse = [r for r in prod_pass if r["diverse"]]
    s4_qualified = [r for r in rows if r["oos_qualified"]]
    s4_qual_cpcv = [r for r in s4_qualified if r["cpcv_ok"]]
    s4_diverse = [r for r in s4_qualified if r["diverse"]]

    summary = {
        "population_n": len(rows),
        "symbols": {},
        "PROD_honest_gate_PASS": len(prod_pass),
        "PROD_PASS_real_edge(OOS_PF>=1.10 & CPCV_median>0)": len(prod_pass_realedge),
        "PROD_PASS_robust(OOS_PF>=1.10 & CPCV_pass_rate>=0.6)": len(prod_pass_robust),
        "PROD_PASS_diverse(non-USDJPY or non-TREND)": len(prod_pass_diverse),
        "ALLREAL_forward_honest_gate_PASS": len(allreal_pass),
        "S4_oos_qualified(t>=200&PF>=1.10&pen>=0.3)": len(s4_qualified),
        "S4_qualified_and_cpcv_ok": len(s4_qual_cpcv),
        "S4_qualified_and_diverse": len(s4_diverse),
        "S4_decisive_PASS(>=3 qualified & diverse>=1 & cpcv)": (len(s4_qualified) >= 3 and len(s4_diverse) >= 1 and len(s4_qual_cpcv) >= 1),
    }
    from collections import Counter
    summary["symbols"] = dict(Counter(r["symbol"] for r in rows))

    out = {
        "summary": summary,
        "prod_pass": sorted(prod_pass, key=lambda r: -(r["oos_pf"])),
        "allreal_pass": sorted(allreal_pass, key=lambda r: -(r["oos_pf"])),
        "s4_qualified": sorted(s4_qualified, key=lambda r: -(r["oos_pen_sharpe"])),
        "all_rows": rows,
    }
    json.dump(out, open(args.out, "w", encoding="utf-8"), indent=1, ensure_ascii=False)

    print("=== HONEST GATE SCORING (real 2pip cost, purge/embargo CPCV) ===")
    for k, v in summary.items():
        print(f"  {k}: {v}")
    print()
    print(f"--- PRODUCTION honest_gate PASS ({len(prod_pass)}) ---")
    for r in prod_pass[:40]:
        print(f'  {r["name"][:38]:38} {r["symbol"]}/{r["category"]:10} tf={r["tf"]:5} sma={r["sma"]:8} '
              f'IS(t={r["is_trades"]},pf={r["is_pf"]},sh={r["is_sharpe"]}) '
              f'OOS(t={r["oos_trades"]},pf={r["oos_pf"]},sh={r["oos_sharpe"]}) '
              f'CPCV(pr={r["cpcv_pr"]},med={r["cpcv_median_sharpe"]}) '
              f'{"REAL-EDGE" if r["real_edge"] else "weak-oos"} {"DIVERSE" if r["diverse"] else "mono"}')

if __name__ == "__main__":
    main()
