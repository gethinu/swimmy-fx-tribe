# tribe-2d — real bundle-edge founder import (measured 2026-07-19)

**Question (owner):** the prior diversity test seeded the tribe with *synthetic* SMA crosses the
operator picked by hand. That was not a fair test of the hypothesis *"mt5_Bundle-of-edges contains
many real, battle-tested non-USDJPY edges across pairs/indices/commodities; founding the tribe with
those should produce a diverse robust universe."* This run re-does it honestly: import **real bundle
edges** as founders, run them through the tribe's **own** honest CPCV/OOS gate (no floor lowered), and
measure two axes:

- **(A)** do imported non-USDJPY bundle edges survive the tribe honest gate (reach CPCV-robust ≥0.6)?
- **(B)** starting from them, does 6-gen breeding grow a diverse robust universe (symbol/regime spread,
  diverse-robust count, USDJPY% trend, vs A / B-5 / B-3 / synthetic-seed runs)?

**Verdict up front (measured, not assumed):** the hypothesis is **NOT supported**, and *not* because the
synthetic seed was "weak." The real bundle non-USDJPY edges are almost all **un-importable** (mechanism
outside the tribe genome/scorer, or symbol with no tribe data), and the representable-with-data subset
**fails the tribe honest CPCV gate** (0/9 robust). The synthetic keltner seeds — hand-centred on the
tribe's own forward-robust region — actually produced *more* diverse-robust (3) than the real bundle
edges (0). The bundle's diversity is real but lives precisely in the dimensions the tribe cannot ingest.

Guardrails honored: redirect harness only (no repo-root `run-all-tests`); **live `swimmy.db` never
touched** (pure-read copy → `C:\tmp\swimmy_bundle_verify.db`); mt5_Bundle read-only; flag-gated,
default OFF, **no source modified** (flag flipped ON only at harness runtime); unrelated dirty
(LEGEND/guardian) untouched.

---

## 1. State-of-truth reconcile

**Tribe data (the hard gate):** `data/historical/` holds M1 for **exactly four symbols** — EURJPY,
EURUSD, GBPUSD, USDJPY (EUR/GBP added 2026-07-13). No GBPJPY, EURCHF, USDCAD, XAU, DE40/F40, bonds.
So the non-USDJPY symbols the tribe can *score at all* are **{EURUSD, GBPUSD, EURJPY}**.

**Tribe representability is two-layered:**
- Genome (`IndicatorType`, `guardian/src/backtester.rs`): Sma, Ema, Rsi, **Macd**, Bb, Stoch, Keltner,
  + volume primitives (degenerate — dataset volume is a synthetic constant 1.0).
- Honest CPCV scorer (`guardian/src/bin/primitive_scan.rs`, the engine the verification harness uses):
  **only** `sma, rsi, stoch, bb, keltner, donchian`. Any other `prim` → `_ => no-signal` → 0 trades →
  auto-fail (this is the "Keltner mis-scored as SMA" bug class the 2c work fixed by being explicit).
- **Intersection that is genome-expressible AND honestly-scorable AND non-degenerate:** `sma, rsi,
  stoch, bb, keltner` (+ `donchian`, scorable but not a live IndicatorType).

**Bundle state-of-truth** (`README.md` register + `docs/portfolio_audit_20260712.md`): the live/
forward-demo book is dominated by MACD/CCI/EMA/SuperTrend and calendar/session/event/external-anchor
mechanisms. The 2026-07-12 audit itself rates 11/14 live strategies as THIN/UNJUSTIFIED and only
n0130/n0131 as carrying a passing independent GO review — so even by the bundle's own standard these
labels are register decisions, not independently re-verified fills.

## 2. Inventory — non-USDJPY honest-verified edges → representability × data

| id | sym | TF | mechanism | bucket | representable? | tribe data? |
|----|-----|----|-----------|--------|----------------|-------------|
| n0006 | EURCHF | D1 | Bollinger(20,2σ) pullback + SMA20/200 filter | (d) BB | **yes** (core) | **NO** |
| s132 | GBPJPY | D1 | Donchian-7 breakout + EMA50/200 gate + ATR trail | (f) Donchian | **yes** (core) | **NO** |
| n0116 | USDCAD | H4 | **TLT<p10** anchor → NbarBreak-5 long | (k)→(f) residue | residue only | **NO** |
| n0119 | GBPUSD | H4 | **BNO>p90** anchor → NbarBreak-5 short hold5 | (k)→(f) residue | residue only | **yes** |
| n0109 | GBPJPY/EURJPY | H4 | **BoJ×FOMC** week → NbarBreak-5 short | (k)→(f) residue | residue only | JPY-leg: partial |
| n0132 | EURJPY/GBPJPY | H4 | MACD(12,26,9) cross + low-vol gate | (g) MACD | **no** | yes (EURJPY) |
| n0134 | EURJPY | H4 | CCI(14) ±100 cross + low-vol gate | (h) CCI | **no** | yes |
| n0065 | GBPJPY | H4 | EMA(50/200) golden cross | (i) EMA | **no** | NO |
| n0014 | EURJPY | M30 | SuperTrend consensus + vol gate | (j) SuperTrend | **no** | yes |
| n0041 | EURJPY | D1 | ATR-range-percentile vol-spike reversion | custom | **no** | yes |
| n0126 | GBPUSD/EURUSD | M15 | month-end London-fix fade + drift-size gate | (k) calendar | **no** | yes |
| n0047 | EURUSD | D1 | weekend gap-up fade | (k) session | **no** | yes |
| n0001 | GBPUSD | D1 | month-start SELL | (k) calendar | **no** | yes |
| n0046 | EURJPY | D1 | weekend gap-down fade | (k) session | **no** | yes |
| n0056 | GBPJPY | H1 | Tokyo-handoff burst fade | (k) session | **no** | NO |
| n0130 | XAU/DE40/F40/bonds | D1 | macro trend-pullback held-low | (k)/compound | **no** | NO |
| n0131 | EURJPY/GBPJPY/AUDJPY | D1 | realized-skew momentum + vol regime | (k)/statistic | **no** | partial |

**The intersection {cleanly representable} ∩ {tribe data} ∩ {non-USDJPY} is EMPTY.** The two clean
representable edges (n0006 BB, s132 Donchian-7) are on symbols tribe has **no data** for (EURCHF,
GBPJPY). The only representable-with-data item is **n0119's *unconditioned Donchian residue*** on
GBPUSD — i.e. n0119 minus the BNO oil anchor that is its actual edge.

So faithfully, only **one** real bundle non-USDJPY edge is importable-and-scorable, and importing it
means discarding the very thing that makes it an edge. To give the hypothesis the widest fair test, we
also **transplanted** the two representable mechanism *families* (s132 Donchian-7 D1; n0006 BB-20 D1)
onto the three data-available symbols, clearly labelled `XPLANT` (not faithful imports).

## 3. Axis A — do imported bundle edges pass the tribe honest gate?

Scored two ways, agreeing: (i) `primitive_scan.exe` directly on the CSVs; (ii) the **real** breeding
pipeline on a copy DB (add-to-kb → honest CPCV score → Phase-1 → §4). Gates: OOS ≥200 trades, PF ≥1.10,
pen-Sharpe ≥0.30; CPCV pass-rate ≥0.60 with ≥20 trades/fold.

| founder (source) | sym | reg | OOS trades | PF | pen-Sharpe | CPCV pass-rate | Phase-1 | robust? |
|---|---|---|---|---|---|---|---|---|
| n0119 **FAITHFUL** (anchor stripped) | GBPUSD | breakout | 573–828 | **0.80** | 0.00 | 0.20–0.30 | FAIL | **no** |
| s132 XPLANT donch7-d1 | GBPUSD | breakout | 58–80 | 0.74–0.86 | 0.00 | 0.00 | FAIL | no |
| s132 XPLANT donch7-d1 | EURUSD | breakout | 52–70 | **1.19–1.22** | 0.25 | **0.00** | PASS (weak) | **no** |
| s132 XPLANT donch7-d1 | EURJPY | breakout | 60–80 | 0.79 | 0.00 | 0.00 | FAIL | no |
| n0006 XPLANT bb20-d1 | EURUSD | reversion | 62 | 0.99 | 0.00 | 0.00 | FAIL | no |
| n0006 XPLANT bb20-d1 | EURJPY | reversion | 57 | **1.37** | **0.41** | **0.00** | PASS (weak) | **no** |
| n0006 XPLANT bb20-d1 | GBPUSD | reversion | 65 | 0.98 | 0.00 | 0.00 | FAIL | no |
| n0119 XPLANT donch5-h4 | EURUSD | breakout | 567–815 | 0.74–0.84 | 0.00 | 0.10–0.20 | FAIL | no |
| n0119 XPLANT donch5-h4 | EURJPY | breakout | 579–828 | 0.91–0.95 | 0.00 | 0.10–0.30 | FAIL | no |

**Result: 0 of 9 reach CPCV-robust.** Real-pipeline honest-gate: **injected=9, honest-PASS=2 (weak
Phase-1), non-USDJPY-TREND-PASS=0, graveyard=7.** The two "passes" (s132-xplant EURUSD PF 1.19;
n0006-xplant EURJPY PF 1.37) clear the weak Phase-1 screen but have **CPCV pass-rate 0.00** — not
robust, and not breeding-eligible (breeding needs CPCV ≥0.55). This is the identical "weak-Phase-1 /
zero-robust" signature the synthetic escape-route seeds produced.

**Two distinct, honest failure modes:**
1. **The edge lived in the stripped conditioner.** Where density is adequate (H4 Donchian residue, 570–
   830 trades), the unconditioned strategy is a **net loser** (PF 0.80) — the BNO/anchor carried it.
2. **Density/regime mismatch.** Where the mechanism looks good (n0006 BB on EURJPY: PF 1.37, pen-Sharpe
   0.41 — would pass the PF and Sharpe gates), it is a **D1 low-density** edge that cannot clear the
   tribe gate's ≥200-OOS-trade / ≥20-per-CPCV-fold floor (57 trades, 0/10 folds). The bundle validates
   such edges on rare-event / small-N *sealed* windows; the tribe demands high-density CPCV-fold
   robustness. Same edge, incompatible verification regime — this is "试して落ちた," not "试せなかった."

## 4. Axis B — 6-generation breeding from bundle founders

Copy DB = pristine live monoculture. Flag ON (harness only). Same machinery as the synthetic-seed run.

| gen | active | USDJPY% | TREND% | diverse-robust | non-USDJPY-TREND robust | bred |
|-----|--------|---------|--------|----------------|------------------------|------|
| GEN0 (pre-founder) | 488 | 99.6 | 100.0 | 0 | 0 | — |
| GEN0b (post 9 founders) | 490 | 99.2 | 99.6 | 0 | 0 | — |
| GEN1 | 64 | 96.9 | 96.9 | 0 | 0 | **0** |
| GEN2–5 | 64 | 96.9 | 96.9 | 0 | 0 | **0** |
| GEN6 | 64 | 96.9 | 96.9 | **0** | **0** | **0** |

**0 breeding events across all 6 generations.** No strategy is breeding-CPCV-eligible: the 486-member
USDJPY-TREND monoculture is CPCV-poor (the §B-6 pre-gate excludes it — this is the known "the mono pool
is never bred" wall), and the 2 bundle survivors have CPCV 0.00. The population simply ages down to its
62 immortal LEGEND core + 2 inert non-USDJPY rank-B founders (EURUSD Donch-7, EURJPY BB), which never
become robust and never breed. **diverse-robust = 0 throughout.**

## 5. Comparison to prior interventions

| run | founders | GEN6 diverse-robust | non-USDJPY-TREND robust | monoculture broken? |
|-----|----------|--------------------|------------------------|--------------------|
| A / B-3 / B-5 | none (machinery only) | 0 | 0 | no |
| synthetic escape-route seed | hand-tuned keltner/bb/sma/donchian | **3** (EURUSD keltner REVERSION) | 0 | no |
| **bundle founders (this run)** | **real n0006/s132/n0119 params** | **0** | 0 | **no** |

The counterintuitive, honest kicker: the **synthetic** keltner seeds (deliberately centred on the
tribe's own forward-robust region, commit f6042ee4) beat the **real** bundle edges at the tribe gate
(3 diverse-robust vs 0). The bundle edges are not weaker as trading edges — they are simply not located
in the tribe scorer's robust region on the tribe's four-symbol, high-density, 6-primitive gate.

## 6. Hypothesis verdict + why

**Owner hypothesis — REJECTED (measured).** Founding the tribe with real bundle non-USDJPY edges does
**not** yield a diverse robust universe. The synthetic-seed test was not "unfairly weak": it was, if
anything, *better* adapted to the tribe gate than the real edges. The failure is structural, in three
separable layers (honestly distinguishing "couldn't try" from "tried and failed"):

- **Couldn't try (representability):** MACD (n0132), CCI (n0134), EMA-cross (n0065), SuperTrend
  (n0014), calendar/session/event/external-anchor (n0126, n0109, n0116, n0047, n0001, n0046, n0056,
  n0121, n0130, n0131), custom vol-spike (n0041) — none expressible in the tribe's genome/honest scorer.
- **Couldn't try (data):** the two *cleanly* representable edges (n0006 BB / EURCHF; s132 Donchian-7 /
  GBPJPY) are on symbols with **no tribe data**. USDCAD, XAU, DE40, bonds likewise absent.
- **Tried and failed (verification regime):** the representable-with-data subset (n0119 residue faithful
  + s132/n0006 transplants) scored **0/9 CPCV-robust** — losers once the anchor is stripped, or killed
  by the ≥200-trade/≥20-per-fold density floor despite favourable PF/Sharpe.

**Deploy: NO. Flag stays OFF.** Nothing here promotes anything; the live book is untouched.

## 7. Next moves (owner decision)

1. **The real lever is the tribe gate, not the seed.** To ingest bundle-class diversity the tribe would
   need (a) more symbols in `data/historical/` (GBPJPY, USDCAD, EURCHF, XAU…), (b) primitives in the
   honest scorer (MACD/CCI/EMA-cross/SuperTrend, and calendar/session/anchor conditioners), and
   (c) a density/robustness bar that admits rare-event D1 edges (small-N sealed instead of ≥200-trade
   CPCV). Each is a scorer/data change, not a seeding change.
2. **Cheapest single test of the hypothesis's best case:** load GBPJPY M1 and import s132 (Donchian-7
   D1, its faithful symbol) — the one clean representable edge blocked only by data. It will still hit
   the D1 density floor (~13 trades/yr), so expect a density fail; but it isolates data-vs-mechanism.
3. Otherwise: accept that the tribe's honest gate and the bundle's edge-validation regime are different
   instruments, and stop trying to seed one from the other.

---

### Reproduce
- Axis A (no DB, no WSL): `logs/tribe_2d_bundle/{GBPUSD,EURUSD,EURJPY}.json` →
  `./target/release/primitive_scan.exe --data data/historical/<SYM>_M1.csv --manifest … --out …`.
- Axis B (WSL Ubuntu-22.04 userspace SBCL): `cp data/memory/swimmy.db C:\tmp\swimmy_bundle_verify.db`
  (strip -wal/-shm), then `source ~/swenv.sh && sbcl --non-interactive --load ~/run_bundle.lisp`
  (loads `logs/tribe_2d_bundle/bundle_founder_verify.lisp`). Log: `logs/tribe_2d_bundle/bundle_run.log`.
