#!/usr/bin/env python3
"""Comprehensive neighborhood grid over the tribe's representable, non-degenerate space.
Emits one manifest per symbol; the actual OHLC comes from --data at scan time, so the
`symbol` field here is only the diversity_ok label (must match the CSV fed).

Representable primitives (from primitive_scan.rs signals()):
  keltner  -> :REVERSION (EMA +/- dev*ATR band bounce)
  bb       -> :REVERSION (variable-dev Bollinger bounce)
  rsi      -> :REVERSION (30/70 cross)
  stoch    -> :REVERSION (20/80 cross)
  donchian -> :BREAKOUT  (channel breakout)
  sma      -> :TREND     (fast/slow crossover)
Volume primitives (vwap/vwapvr/...) excluded: dataset volume is synthetic constant 1.0.
Honest gate is compiled into primitive_scan.exe and is NOT touched here.
"""
import json, sys

# TF in seconds. M15..D1 — spans the region where the known keltner edge lives (H4/H6/D1)
# plus lower TFs for coverage breadth. M1/M5 excluded (degenerate trade counts, doc found 0).
TFS = [900, 3600, 7200, 14400, 21600, 43200, 86400]   # M15,H1,H2,H4,H6,H12,D1
BARR = [2.0, 2.5, 3.0]                                 # symmetric ATR sl=tp multiples
ATRP = 14

def keltner(sym):
    out=[]
    for p in [20,30,40,45,50,55,60,70,80,100]:
        for dev in [1.5,2.0,2.5,3.0]:
            for tf in TFS:
                for b in BARR:
                    out.append(dict(name=f"K-{sym}-keltner-p{p}-d{dev}-tf{tf//60}-atr{b}x{b}",
                        symbol=sym,regime=":REVERSION",prim="keltner",period=p,dev=dev,
                        atr_period=ATRP,tf_seconds=tf,barrier_mode="atr",sl=b,tp=b,max_hold=0))
    return out

def bb(sym):
    out=[]
    for p in [15,20,25,30,35,40,50]:
        for dev in [1.25,1.5,1.75,2.0,2.5]:
            for tf in TFS:
                for b in BARR:
                    out.append(dict(name=f"B-{sym}-bb-p{p}-d{dev}-tf{tf//60}-atr{b}x{b}",
                        symbol=sym,regime=":REVERSION",prim="bb",period=p,dev=dev,
                        atr_period=ATRP,tf_seconds=tf,barrier_mode="atr",sl=b,tp=b,max_hold=0))
    return out

def rsi(sym):
    out=[]
    for p in [7,10,14,21,28]:
        for tf in TFS:
            for b in BARR:
                for mh in [0,20]:
                    out.append(dict(name=f"R-{sym}-rsi-p{p}-tf{tf//60}-atr{b}x{b}-mh{mh}",
                        symbol=sym,regime=":REVERSION",prim="rsi",period=p,dev=2.0,
                        atr_period=ATRP,tf_seconds=tf,barrier_mode="atr",sl=b,tp=b,max_hold=mh))
    return out

def stoch(sym):
    out=[]
    for p in [14,21,28]:
        for tf in TFS:
            for b in BARR:
                for mh in [0,20]:
                    out.append(dict(name=f"S-{sym}-stoch-p{p}-tf{tf//60}-atr{b}x{b}-mh{mh}",
                        symbol=sym,regime=":REVERSION",prim="stoch",period=p,dev=2.0,
                        atr_period=ATRP,tf_seconds=tf,barrier_mode="atr",sl=b,tp=b,max_hold=mh))
    return out

def donchian(sym):
    out=[]
    for p in [7,10,15,20,30,40,55,70]:
        for tf in TFS:
            for b in BARR:
                for mh in [0,40]:
                    out.append(dict(name=f"D-{sym}-donchian-p{p}-tf{tf//60}-atr{b}x{b}-mh{mh}",
                        symbol=sym,regime=":BREAKOUT",prim="donchian",period=p,dev=2.0,
                        atr_period=ATRP,tf_seconds=tf,barrier_mode="atr",sl=b,tp=b,max_hold=mh))
    return out

def sma(sym):
    out=[]
    for p in [10,20,30,50,80]:
        for tf in TFS:
            for b in BARR:
                out.append(dict(name=f"M-{sym}-sma-p{p}-tf{tf//60}-atr{b}x{b}",
                    symbol=sym,regime=":TREND",prim="sma",period=p,dev=2.0,
                    atr_period=ATRP,tf_seconds=tf,barrier_mode="atr",sl=b,tp=b,max_hold=0))
    return out

if __name__=="__main__":
    outdir=sys.argv[1]
    for sym in ["EURUSD","GBPUSD","EURJPY","USDJPY"]:
        m = keltner(sym)+bb(sym)+rsi(sym)+stoch(sym)+donchian(sym)+sma(sym)
        with open(f"{outdir}/grid_{sym}.json","w") as f:
            json.dump(m,f)
        # per-primitive counts
        from collections import Counter
        c=Counter(x["prim"] for x in m)
        print(f"{sym}: {len(m)} configs  {dict(c)}")
