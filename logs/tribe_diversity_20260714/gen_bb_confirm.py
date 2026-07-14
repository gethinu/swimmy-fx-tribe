#!/usr/bin/env python3
"""Focused BB mean-reversion confirm grid around the sole honest-2b robust hit
(EURUSD-bb-p50-tf240). Is it a coherent region or a lone fluke? BB dev is
hardcoded=2.0 in the engine, so sweep period x timeframe x pip-scaled barriers."""
import argparse, json
ap = argparse.ArgumentParser()
ap.add_argument("--symbol", required=True)
ap.add_argument("--pip", type=float, required=True)
ap.add_argument("--out", required=True)
a = ap.parse_args()
PERIODS=[20,25,30,40,50,60]
TFS=[120,180,240,360]
BARRIERS=[(30,60),(50,100),(50,50),(50,80),(80,80),(60,60)]
m=[]
for p in PERIODS:
  for tf in TFS:
    for (slp,tpp) in BARRIERS:
      m.append({"name":f"BBc-{a.symbol}-bb-p{p}-tf{tf}-sl{slp}tp{tpp}","symbol":a.symbol,
        "category":":REVERSION","indicator_type":"bb","timeframe_min":tf,"tf_seconds":tf*60,
        "sma_short":p,"sma_long":p*2,"sl":round(slp*a.pip,8),"tp":round(tpp*a.pip,8),
        "volume":0.01,"indicators_raw":f"(BB {p})","rank":"BBCONFIRM",
        "is_trades":None,"is_pf":None,"is_sharpe":None})
json.dump(m,open(a.out,"w",encoding="utf-8"),indent=1,ensure_ascii=False)
print(f"wrote {a.out}: {len(m)} BB configs symbol={a.symbol}")
