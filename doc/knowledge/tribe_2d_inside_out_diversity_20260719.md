# tribe-2d — inside-out diversity: can the tribe grow forward-robust diversity from its OWN yardstick? (measured 2026-07-19)

**Question (owner).** The bundle-import route is closed (three walls — data, representability, per-regime
CPCV — settled in `tribe_2d_bundle_founder_import_20260719.md` `1db94384` and
`tribe_2d_data_wall_s132_n0006_20260719.md` `c20098d8`). The only thing that has *ever* passed the tribe's
own honest CPCV outside the USDJPY-TREND core is the synthetic **Keltner/BB mean-reversion EURUSD-REVERSION**
region. So: **starting from that region, can the tribe grow forward-robust diversity organically from the
inside** — its own representable primitives, its own local data, its own per-regime CPCV — enough to honestly
crack the monoculture? Or is the current yardstick (6 primitives × 4 symbols × per-regime CPCV) exhausted, so
that diversification *requires* yardstick expansion (new primitive / new data)?

**Verdict up front (measured): the inside-out ceiling is exactly ONE diverse edge-family — Keltner/BB
mean-reversion on the EUR/USD-quoted majors, H4/H6.** An exhaustive honest scan of the *entire* representable,
non-degenerate space (9,408 configs) finds forward-robust diverse edges in that one region and **nowhere else**
— not in any other primitive, symbol, regime, or timeframe — and this holds even at **zero trading cost**
(the frictionless upper bound). Seeding that region into the live breeding pipeline with the diversity
machinery ON (B-3/B-4/B-5, flag ON, copy DB) **does** engage: it dilutes the monoculture **99.6% → 84.9%**
USDJPY over 6 generations and sustains **2** honest diverse-robust members — but **diverse-robust never
multiplies past what is seeded** (breeding manufactures no *new* CPCV-robust diverse individual), and the §4
floor holds the USDJPY core at **62** (majority never broken). **Growing ONE diverse niche from inside = YES
(measured, reversible). Growing genuine multi-family diversity from inside = NO (measured, exhaustively).
Beyond the single MR-EUR/USD niche is a yardstick problem, not a search-depth or breeding-machinery problem.**

Guardrails honored: redirect harness only (`primitive_scan.exe` + `daemon_verify.lisp` on a **copy** DB with
library writes redirected to `/mnt/c/tmp/verify_library`); no repo-root `run-all-tests`; live `swimmy.db`
untouched (mtime unchanged); honest floor **not** lowered (compiled constants read, not edited); diversity
code all flag-gated, flag set ON **only in-process** in the harness (no tracked source modified, no production
flip); repo left at baseline (391 pre-existing dirty = session start, LEGEND/guardian/RECRUIT noise untouched);
grid CSVs derived from existing `data/historical/` (nothing new committed; large JSONs gitignored).

---

## 1. The yardstick (read, never modified)

`guardian/src/bin/primitive_scan.rs` — the honest engine (same one the daemon and the data-wall run use).
Gates are **compiled constants**, confirmed unchanged this run:

| gate | value |
|---|---|
| OOS window (selection) | 2021-01-01 .. 2025-01-01 |
| OOS min trades / PF / pen-Sharpe | **200 / 1.10 / 0.30** |
| CPCV | 10 equal-index blocks; fold min-trades **20**, PF **1.10**, pen **0.30** |
| CPCV pass-rate gate / median-sharpe max | **0.60 / 2.0** |
| `diversity_ok` | `symbol≠USDJPY OR regime≠TREND` |

Representable, non-degenerate primitives (volume-based excluded — dataset volume is a synthetic constant 1.0):
`keltner`→REVERSION, `bb`(variable-dev)→REVERSION, `rsi`→REVERSION, `stoch`→REVERSION, `donchian`→BREAKOUT,
`sma`→TREND. Symbols with real local M1 data: **EURUSD, GBPUSD, EURJPY, USDJPY** (2015-01-02 .. 2024-12-31).

**forward-robust** ≡ `oos_qualified AND cpcv_ok` in **both** the selection window (OOS 2021-25, CPCV full-span)
**and** the holdout window (OOS+CPCV 2015-21) — the tribe's own 2-window bar from
`tribe_expanded_primitive_forward_20260714.md`. Pipeline byte-reproduces that doc's baseline exactly
(keltner-p55 sel pr 0.60 / HOLD pr 0.70 med +0.80; keltner-p50 0.60/0.60; bb-p30-d1.75 0.80/0.60).

## 2. Inventory — what is honestly alive in the LIVE population (read-only copy of `swimmy.db`)

18,507 strategies. **Exactly ONE clears CPCV ≥ 0.60:**

| what | count | note |
|---|---|---|
| total strategies | 18,507 | 18,505 USDJPY, 1 EURUSD, 1 GBPUSD |
| **CPCV-robust (≥0.60)** | **1** | `UT-S-STAGE2` — USDJPY / TREND / M1, `indicators=entry=exit=NIL` (the SMA-crossover **seed** anchor), pr 0.90, PF 1.90, 120 tr |
| bred USDJPY | 18,505 | **all pr = 0.00** — incl. PF-9.8 / 34-trade overfits, correctly rejected by CPCV |
| non-USDJPY | 2 | `Hunted-D1-VWAPVR-*` (EURUSD, GBPUSD) — degenerate volume primitive, pr 0.00 |

The "3 synthetic Keltner EURUSD-REVERSION" are **offline-research artifacts** — they have never been persisted
to `swimmy.db`. So the tribe's *live* honest-robust inventory is a **monoculture of one**: the USDJPY-TREND
seed. Everything else scores 0.

## 3. Supply — exhaustive neighborhood scan (does the honest gate admit anything diverse & new?)

**Coverage: 9,408 configs** = 4 symbols × 2,352 (keltner 840, bb 735, donchian 336, rsi 210, stoch 126,
sma 105) spanning periods, band/ATR multiples, ATR barriers {2.0,2.5,3.0}, max_hold, and **7 timeframes**
(M15,H1,H2,H4,H6,H12,D1). Each scored on both windows. This is the neighborhood of the known region **and**
the full breadth of every other primitive/symbol/regime.

### 3.1 Realistic cost (2-pip round-trip)

| symbol | configs | selection-robust | **forward-robust (2-window)** | families |
|---|---:|---:|---:|---|
| **EURUSD** | 2,352 | 38 | **13** | keltner-MR (12) + bb-MR (1), all H4/H6 |
| **GBPUSD** | 2,352 | 6 | **2** | keltner-MR H4 (thin transfer) |
| **EURJPY** | 2,352 | 0 | **0** | — |
| **USDJPY** | 2,352 | 6 | **0** | (selection-only artifacts; none survive holdout) |
| **total** | 9,408 | 50 | **15** | |

**All 15 forward-robust are diverse; all 15 are REVERSION; all 15 are Keltner/BB mean-reversion on
EURUSD/GBPUSD at H4/H6.** Zero `rsi` / `stoch` / `donchian` / `sma` survivors anywhere. Zero at
M15/H1/H2/H12/D1. Distinct edge-**families** (symbol×prim×regime×TF-band): 3 clusters, but 2 are the same
keltner-MR H4 mechanism (EURUSD + a thin GBPUSD transfer) and the third is its bb-MR sibling — **mechanically
one family: band mean-reversion on the EUR/USD-quoted majors, H4/H6, ATR barrier.**

The doc's headline "3" was a *representative subset*; the exhaustive scan raises the **variant** count to 15 —
but adds **zero new edge-families**. Denser sampling of one niche, not new diversity.

### 3.2 Frictionless upper bound (slip = 0) — is anything hiding behind cost?

To separate "no edge" from "edge eaten by the spread," the whole grid was re-run at **zero cost**:

| region | frictionless forward-robust |
|---|---:|
| EURUSD/GBPUSD Keltner+BB **REVERSION** H4/H6 | **62** |
| USDJPY sma-TREND H1 / USDJPY rsi / USDJPY donchian / EURUSD sma-TREND / GBPUSD bb | 5 (cost-boundary artifacts) |
| **EURJPY (any)** | **0** |
| total | 67 |

Even with **all friction removed**, 62 of 67 survivors are the *same* Keltner/BB-MR EUR/USD family; the 5
non-EUR/USD-MR extras exist **only** frictionless and score **0** under 2-pip cost — they are exactly what the
spread eats, not deployable edges. **EURJPY is 0 even frictionless** → the JPY-cross absence is *mechanism*,
not cost. The search is therefore not merely "deep enough" — it is the **cost-free ceiling**, and the ceiling
is still the one MR-EUR/USD family.

## 4. Propagation — breeding the region with the diversity machinery ON (daemon, copy DB, 6 gens)

`daemon_verify.lisp` on a **copy** of `swimmy.db`: `*enable-primitive-diversity* = t` (B-3 symbol-mutation +
niche-quota, B-4 behavioural distance, B-5 fitness-sharing overflow cull all engage under this flag), 2f
pre-gate 0.55, `inject-diversity-seeds` (18 seeds incl. the Keltner/BB-REVERSION region), each seed **honestly
scored** by the same `primitive_scan` engine and driven through the real Phase-1 + §4 floor. No live orders,
no socket; library writes redirected to scratch.

| GEN | active | USDJPY | USDJPY % | TREND % | **diverse-robust** | non-USDJPY-TREND survive / robust |
|---|---:|---:|---:|---:|---:|---:|
| 0 (loaded monoculture) | 488 | 486 | **99.6** | 100.0 | **0** | 2 / 0 |
| 0b (18 seeded → honest-PASS 10, 6 non-USDJPY-TREND) | 498 | 486 | 97.6 | 99.2 | **2** | 8 / 0 |
| 1 | 70 | 62 | 88.6 | 94.3 | 2 | 4 / 0 |
| 2 | 70 | 62 | 88.6 | 94.3 | 2 | 4 / 0 |
| 3 | 71 | 62 | 87.3 | 93.0 | 2 | 4 / 0 |
| 4 | 71 | 62 | 87.3 | 93.0 | 2 | 4 / 0 |
| 5 | 72 | 62 | 86.1 | 91.7 | 2 | 4 / 0 |
| **6** | 73 | **62** | **84.9** | 90.4 | **2** | 4 / **0** |

Read-out:
- **Dilution is real:** USDJPY 99.6% → **84.9%** over 6 gens — the machinery engages, culls redundant USDJPY
  (fitness-sharing / overflow), and keeps the diverse members. Reversible and flag-gated.
- **diverse-robust does NOT multiply:** it goes 0 → 2 (the Keltner/BB-REVERSION seeds that clear CPCV ≥ 0.60)
  and **plateaus at 2** through GEN6. Breeding produces ~1 diverse child/gen; they survive at rank-B but
  **0 ever reach CPCV ≥ 0.60** (`robust = 0` every generation). The honest gate admits no *bred* diverse edge.
- **Majority never breaks:** USDJPY absolute count holds at **62** — the §4 live-edge floor protects the core;
  it never falls below it, so USDJPY stays the plurality (84.9%).

This is exactly what §3 predicts: there is one diverse edge-family to seed, it seeds and dilutes, but there is
no *second* robust diverse edge in the representable space for breeding to find or manufacture, so the count
cannot grow.

## 5. Honest verdict — inside-out or yardstick-expansion?

| question | measured answer |
|---|---|
| New diverse-robust edge-**family** from inside (any primitive/symbol/regime/TF, honest 2-window)? | **NO** — 1 family (Keltner/BB-MR EUR/USD H4/H6), realistic **and** frictionless |
| Did diverse-robust "increase from 3"? | Offline **variant** census 3 → **15**, but still **1 family** (denser, not new). Live breeding population 0 → **2**, then plateaus |
| Does breeding the region dilute the monoculture? | **YES** — USDJPY 99.6% → 84.9% over 6 gens (reversible, flag-gated) |
| Does breeding *break* the monoculture / grow robust diversity? | **NO** — diverse-robust caps at seeded 2; USDJPY holds §4 floor (62); majority 84.9% |
| "Not found" vs "search too shallow"? | **Not found**, well-grounded: 9,408 configs across all 6 primitives × 4 symbols × 7 TFs × full param neighborhoods, 2-window forward gate, **plus** the frictionless upper bound. Everything outside MR-EUR/USD is empty even cost-free |

**Conclusion.** The tribe can grow, seed, and *sustain* its one honest diverse niche from the inside, and doing
so measurably (but partially and reversibly) dilutes the USDJPY monoculture. It **cannot** grow *genuine
multi-family* diversity from the inside: the current primitive × data × per-regime-CPCV space contains exactly
**one** forward-robust diverse edge-family, and neither exhaustive scanning nor breeding produces a second.
Everything beyond the single MR-EUR/USD niche is therefore a **yardstick problem** — new primitive types
and/or new data — **not** a search-depth problem and **not** a breeding-machinery problem (both are already
adequate: the scan is exhaustive to the cost-free ceiling; the machinery already dilutes 99.6→84.9% when fed
the one edge that exists).

## 6. Next move (owner decision)

1. **Bank the one real inside-out win, keep the flag OFF.** The Keltner/BB-MR EUR/USD H4 niche is genuinely
   forward-robust and, seeded + flag-ON, dilutes the monoculture to ~85% on a copy DB. It remains **below the
   deployment gate** (holdout PF ~1.13-1.20, median-sharpe ~0.5-0.8) — a diversifier, not a live edge. No
   production flip is justified by this run.
2. **To go past one niche, expand the yardstick — and be specific about which lever:**
   - **New primitive/feature** (the 6 representable are exhausted; the only edge is mean-reversion). A
     *mechanically distinct* family would need something not representable today — session/time-of-day, a
     microstructure/order-flow feature, or a multi-timeframe / cross-pair signal. This is the lever with
     headroom.
   - **New data** is the *weaker* lever on its own: the data-wall run already showed importing real bundle D1
     edges (GBPJPY/EURCHF) fails per-regime CPCV on *substance*, so more symbols alone won't manufacture
     robustness for non-robust mechanisms. New data only helps in combination with a primitive that has an
     edge there.
3. **Stop re-litigating the bundle-import and seed-count routes.** Three independent runs
   (`1db94384`, `fed9bf7b`, `c20098d8`) and this exhaustive census agree: inside-out tops out at the single
   MR-EUR/USD niche. The open question is a *yardstick* question.

---

### Reproduce
`logs/tribe_2d_inside_out/`: `gen_grid.py` (9,408-config generator), `score.py` (2-window forward join +
cluster analysis), `SCORE_SUMMARY.json`, `forward_robust_realistic.txt`. Breeding: copy `data/memory/swimmy.db`
→ `/mnt/c/tmp/swimmy_daemon_verify.db`; from a WSL Ubuntu shell,
`cd /mnt/c/Repos/swimmy-fx-tribe && sbcl --dynamic-space-size 5120 --non-interactive --load ~/run_harness.lisp`
(loads `~/daemon_verify.lisp`, flag ON in-process). Grid: `primitive_scan.exe --data data/historical/<SYM>_M1.csv
--manifest logs/tribe_2d_inside_out/grid_<SYM>.json --slippage <0.0001|0.01|0> [--oos-start 1420070400
--oos-end 1609459200 --cpcv-start 1420070400 --cpcv-end 1609459200]`. Large grid JSONs kept in scratch
(gitignored), derivable from the generator.
