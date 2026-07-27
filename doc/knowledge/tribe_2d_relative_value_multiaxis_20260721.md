# tribe-2d — yardstick EXPANSION ③: FOUR parallel routes at the 2-window-robustness free dimension (measured 2026-07-21)

**Question (owner).** ⑩ (hold-to-barrier) left the wall as the **joint box `{trades≥200 ∧ 2pip-cost-survivable ∧
2-window-CPCV-robust}`** with exactly one interior point (MR-EUR/USD H4/H6), and named the remaining free
dimension as **2-window robustness itself** (stability across 2015-21 *and* 2021-25). The owner asked to stop
running one axis at a time and **run several routes in parallel**, each first stating *which part of the joint box
it attacks*, chief among them **cross-pair relative-value / structural spread** (未踏: the 6 primitives only ever
touched a single series). This run implements and measures **four** routes, both at **realistic** and **frictionless**
cost, on the tribe's 2-window forward gate.

**Verdict up front (measured — the sharpest characterisation yet of why the box has one interior point):**
- **Route A (cross-pair 2-leg spread instrument) — the most promising, and it delivers a genuine first.**
  Frictionless, a structural spread contains a **NEW 2-window-robust diverse family** — `SPR_EJ_UJ bb-MR-H4`
  (EURJPY − 0.973·USDJPY, corr 0.94): SEL 207/1.359/cpcv0.60, HOLD 320/1.127/cpcv0.70, **both qualified**. This is
  the **first mechanism to clear the 2-window free dimension via structure** (not an entry gate). **But at realistic
  2-LEG cost it dies (0 forward-robust)**: PF 1.359→1.250 (sel) / 1.127→1.043 (hold) → CPCV+holdout fail. The wall
  **re-moved to COST** (a spread pays *both* legs ≈ 2× a single pair), not 2-window robustness (cleared), not density
  (207/320 ≥ 200).
- **Route D (single-leg RV-divergence gate) — the intended fix for A's cost wall, and it exposes the deepest
  finding.** Trade ONE real leg (single-leg cost, HALF of A) with entries driven by the spread z-score. Single-leg
  cost **is** gentle (EURJPY-RV PF 1.340→1.277, only −0.06 — the cost fix *works*). **But de-hedging destroys the
  2-window robustness**: the single-leg RV edge **fails holdout even frictionless** (HOLD 368/1.064 < 1.10). ⇒ **The
  2-window robustness and the low cost are COUPLED opposite:** the *hedge* (market-neutrality) is what makes the spread
  MR regime-stable across both windows, and the hedge is exactly what doubles the cost. Keep it → pay 2 legs (A dies);
  drop it → lose 2-window robustness (D dies frictionless). **Neither corner of the trade-off is in the joint box.**
- **Routes B (vol-regime gate) & C (multi-TF confluence) — entry-gate family, 0 as ⑨ predicted.** Both flag-gated,
  both fire, both applied to FAT H4/D1 (not thin intraday). **0 gated forward-robust, 0 load-bearing, at any cost.**
  They cut trades (density) without stabilising any counter-regime edge; only the known keltner-EURUSD-MR-H4 baseline
  survives. Honest caveat for B: vol-*normalised* entries/targets were **already** swept (bb std-bands, keltner
  ATR-bands, atr barriers, ⑧ census); the only NEW increment (the regime *gate*) contributes 0.

**Net:** the joint box still has **exactly one interior point** (MR-EUR/USD H4/H6). The cross-pair route genuinely
*can* clear the free dimension frictionlessly — the first to do so — but the clearance is **inseparable from a doubled
cost**, and the entry-gate routes add nothing. No monoculture break at the deployment gate.

Guardrails honored: redirect harness only (standalone `primitive_scan.exe` on `data/historical/*_M1.csv` + derived
series); **no repo-root `run-all-tests`**; live `swimmy.db` **never opened**; honest floor constants
(`200 / 1.10 / 0.30 / 0.60 / 2.0`) **read, never modified** (confirmed: 0 floor-constant lines in the diff);
both new binary axes are **flag-gated to byte-identity** — a manifest with no new keys is SHA256-identical to the
pre-edit engine (`73394ee4…`, proven after *both* the B/C change and the D change); **no genome gene populated**, flag
**not** flipped (offline research scorer only); unrelated dirty (LEGEND / guardian runtime json / RECRUIT) untouched;
synthetic/derived series live in gitignored `logs/` (**no brought-in data committed**). **Honesty note:** a
name-collision bug in the Route-B grid generator (dev omitted from the config name) initially masked the interior
point — caught by cross-checking a discrepant trade count, fixed, and Route B re-run before scoring (see §6).

---

## 1. The four routes and which part of the joint box each attacks

| route | lever | box part attacked | new binary code? |
|---|---|---|---|
| **A** cross-pair 2-leg **spread instrument** | apply MR/breakout primitives to a synthetic spread/ratio of two real pairs | **2-window robustness** (a structural spread MR is regime-stable by construction) **+ new symbol family** | **NO** — pure derived data through the UNCHANGED scorer |
| **B** **vol-regime** entry gate | enter only when trailing ATR percentile ∈ [lo,hi) | **2-window robustness** (a regime-invariant band, not a price level) | yes, flag-gated |
| **C** **multi-TF confluence** entry gate | enter only when a higher-TF SMA trend agrees | **2-window robustness** (drop counter-regime whipsaws that differ across windows) | yes, flag-gated |
| **D** **single-leg RV-divergence** signal | trade ONE real leg on the cross-pair spread z-score | **all three jointly** — the 2-window spread MECHANISM at single-leg COST at fat DENSITY | yes, flag-gated |

Route A is deliberately pure-data: a synthetic instrument is just a price series, so the honest engine / floor
constants / 2-window gate are literally the same compiled binary — byte-identity is trivial (binary untouched). Routes
B/C/D are one flag-gated binary change each (B+C share one edit), default-off ⇒ byte-identical.

## 2. Route A — cross-pair relative-value (pure data, no binary change)

**Instruments** (synthetic M1 OHLC, aligned inner-join of two real legs; widest-bound intrabar high/low ⇒ conservative
ATR; β = OLS on the 2015-18 train slice, and a β=1 equal-weight variant):

| instrument | def | corr(levels) | note |
|---|---|---|---|
| `SPR_EJ_UJ` | EURJPY − 0.973·USDJPY | **0.942** | genuine structural spread — **NEW family** |
| `RAT_EU_GU` | EURUSD / GBPUSD = EUR/GBP | 0.450 | a real cross the tribe lacks — NEW symbol |
| `SPR1_EU_GU` | EURUSD − GBPUSD (β=1) | 0.450 | USD-major spread; Brexit decoupled the levels ⇒ weak |
| `SPR_EU_GU`, `RAT_EJ_UJ` | β=0.03 ≈ EURUSD; ≈ EURUSD | — | **controls** — should re-derive the known EURUSD-MR (pipeline sanity) |

**HONEST COST:** a spread trade executes TWO legs ⇒ pays BOTH spreads. Realistic per-side slip = `slipA + β·slipB`
(≈ 2× a single pair): `SPR_EJ_UJ` 0.0197, `SPR1_EU_GU` 0.0002, `RAT_EU_GU` 0.00014. **Not** understated to single-pair
2-pip (that would cheat in favour of this route). Grid: 26 cores (keltner/bb MR neighbourhoods + donchian/sma/rsi/
stoch) × {H1,H4,D1} = 78/instrument.

**Frictionless (mechanism exists?): 3 forward-robust.**

| config | SEL (t, pf, cpcv) | HOLD (t, pf, cpcv) | reading |
|---|---|---|---|
| `SPR_EJ_UJ bb-MR-H4` | 207, 1.359, 0.60 | 320, 1.127, 0.70 | **NEW 2-window-robust diverse family** (JPY structural-spread MR) — first spread to clear the gate |
| `SPR_EJ_UJ sma-TREND-H1` | 667, 1.126, 0.60 | 967, 1.204, 0.60 | thin/high-freq (cost-fragile) |
| `SPR_EU_GU keltner-MR-H4` (control ≈EURUSD) | 233, 1.303, 0.70 | 373, 1.250, 0.60 | re-derives the KNOWN EURUSD-MR niche — pipeline sanity **PASS** |

**Realistic 2-leg cost (deployable?): 0 forward-robust.** The new family's cost gap:

| | SEL fric → 2-leg-real | HOLD fric → 2-leg-real |
|---|---|---|
| `SPR_EJ_UJ bb-MR-H4` | 207/1.359/cpcv0.60 → 207/**1.250**/cpcv**0.40** (cpcv fails) | 320/1.127/cpcv0.70 → 322/**1.043**/cpcv0.30 (pf<1.10) |

At 2-leg cost the JPY-spread MR retains a **positive OOS edge** (sel pf 1.250) but loses CPCV robustness and holdout
qualification. `sma-H1` collapses outright (1.126→0.939). **Wall for Route A = COST** (2-leg execution), **not**
2-window robustness (cleared) and **not** density (207/320 ≥ 200) — the ⑨ pattern, but now the mechanism *also* cleared
the free dimension frictionlessly.

## 3. Route D — single-leg RV-divergence gate (flag-gated: `prim="rvspread"`, `--aux`)

The natural fix for A's cost wall: trade ONE real leg (single-leg cost = HALF) with entries from the spread z-score
(`spread = leg − rv_beta·aux`; long leg when z<−dev, short when z>+dev, exit at z→0). 4 leg/partner scans × 27 configs
(period {20,50,100} × dev {2.0,2.5,3.0} × {H1,H4,D1}), realistic = SINGLE-leg cost (JPY 0.01 / USD 0.0001).

**Frictionless: 0 forward-robust. Realistic: 0.** The strong leg (`EURJPYonUSDJPY`, corr 0.94) is the exemplar:

| `rv-EURJPYonUSDJPY-p20-d2.5-H4` | SEL | HOLD |
|---|---|---|
| frictionless | 245/1.340/cpcv0.60 **qual** | 368/**1.064**/cpcv0.40 **fails (pf<1.10)** |
| single-leg real | 246/**1.277**/cpcv0.50 | 367/1.009/cpcv0.40 |

Two facts, together the deepest result of the whole investigation:
1. **Single-leg cost IS gentle** — PF 1.340→1.277 (only −0.06), vs A's 2-leg gap; the cost fix **works**.
2. **But de-hedging destroys the 2-window robustness** — the single-leg RV edge **fails holdout even frictionless**
   (1.064 / 1.029 < 1.10). Trading one directional leg re-imports that leg's regime-dependent drift (EURJPY trended
   2021-25, not 2015-21), which the market-neutral 2-leg spread had cancelled.

⇒ **Robustness (via the hedge) and low cost (via single-leg) are the SAME knob pulled opposite ways.** The hedge is
what makes the spread MR regime-stable across both windows; the hedge is exactly what doubles the cost. On this
4-pair data, no corner of that trade-off lands inside `{2pip-survivable ∧ 2-window-robust}`.

## 4. Routes B & C — entry-gate axes (flag-gated, byte-identical OFF)

Both **fire** (verified not-inert: sma-H4 56→25 trades with a D1-trend filter; keltner-H4 242→163/140 in the low/high
vol bands — and EURUSD keltner-MR is *better* in the high-vol band, pf 1.311 vs 1.135, a real regime signal). Both
applied to **fat H4/D1** to avoid ⑨'s thin-cost wall. Each gated config has a NOFILTER/NOGATE sibling for a
**load-bearing** test.

| route | coverage | frictionless fwd-robust | realistic fwd-robust | **gated** | **load-bearing** |
|---|---|---|---|---|---|
| **B** vol-regime | 4×60 | 1 (known keltner-EURUSD base) | 1 (same) | **0** | **0** |
| **C** multi-TF confluence | 4×42 | 1 (known keltner-EURUSD base) | 1 (same) | **0** | **0** |

Both gates only cut trades (density); neither stabilises a counter-regime edge into 2-window robustness. **0
monoculture-break candidates.** Honest caveat (B): vol-*normalisation* is already representable (bb std-bands = a
vol-normalised entry, keltner ATR-bands, `barrier_mode=atr` = a vol-normalised target) and was swept by ⑧ — the only
genuinely new increment is the regime *gate*, which is 0.

## 5. Honest verdict — the joint box, more sharply

| question | measured answer |
|---|---|
| Does cross-pair structure clear the **2-window robustness** free dimension frictionlessly? | **YES** — `SPR_EJ_UJ bb-MR-H4` is 2-window robust frictionless. **First** structural (non-entry-gate) mechanism to do so. |
| Does it survive **realistic** cost? | **NO** — 2-leg execution ≈ doubles cost; the fat H4 spread-MR drops below the CPCV/holdout gate (0 forward-robust). |
| Can single-leg execution recover the cost? | **YES for cost** (PF −0.06 only) **but NO for robustness** — de-hedging makes the RV edge fail holdout *even frictionless*. |
| Do vol-regime / multi-TF entry gates create a robust diverse edge? | **NO — 0 gated, 0 load-bearing** at any cost (⑨'s entry-gate family; they only cut density). |
| Net interior points of the joint box on this data? | **Still exactly one** — MR-EUR/USD H4/H6. |
| Deepest structural reason | **2-window robustness and low cost are coupled opposite through the hedge.** The market-neutrality that buys regime-stability is the same thing that doubles execution cost; removing it to save cost removes the stability. |

**Conclusion.** "Try more routes" was answered with four, honestly measured at both cost levels. The most promising
(cross-pair) is the **first lever in the entire 2d program to clear the 2-window free dimension by structure** — a real
advance in understanding — yet it cannot be converted into a *deployable* diverse edge, because the clearance is
mechanically inseparable from a doubled cost (Route A), and de-hedging to escape that cost re-imports single-leg
regime drift that destroys the 2-window stability (Route D). Entry-gate axes (B, C) add nothing, exactly as ⑨
predicted. The monoculture is unbroken at the deployment gate.

## 6. The one honesty incident (disclosed in full)

While scoring Route B, the interior-point config `keltner-EURUSD-MR-H4` reported **197 trades** where every other
context reported **242**. Rather than report it, I isolated the config: run alone it gave **242** (correct), but inside
the 60-config grid the scorer's name-keyed dict returned 197. Cause: the Route-B grid generator put only `period` in
the config name, but the keltner core has two devs (d2.0 **and** d2.5) at the same period ⇒ two configs shared the
name `keltner-EURUSD-p50-tf240-NOG`, and the scorer's `{name: strat}` dict silently kept the last (d2.5, 197). The
scan itself computed both correctly; only the *scoring* collapsed them. Fixed the generator to include `dev` in the
name, verified **all** route grids are collision-free (unique == total), and **re-ran Route B** before scoring. Routes
A/C were unaffected (names include `dev` / no same-(prim,period) collision). This is exactly the class of silent error
that manufactures a false number — it was caught by chasing a discrepant count, not by assuming.

## 7. Next move (owner decision)

1. **Bank the coupling finding; keep all flags OFF.** The A↔D coupling is the load-bearing conclusion: on the current
   4-pair data, **2-window robustness and cost-survivability cannot be co-satisfied by any cross-pair construction**
   (hedged = robust-but-2-leg-costly; unhedged = cheap-but-not-robust). Do **not** lower the density/CPCV/cost floors
   to admit the near-misses (`SPR_EJ_UJ bb-MR-H4`, `EURJPY-RV`); each pass would be a single-window or cost-subsidised
   artifact.
2. **The remaining lever narrows to NEW DATA with a *native* 2-window-robust edge** — an instrument whose *single-series,
   single-leg-cost* edge is intrinsically regime-stable (a different asset class with different regime dynamics: metals,
   an index, rates), **not** a derived combination of the existing four (which this run closes) and **not** just more
   FX pairs to feed non-robust mechanisms (the ⑦ data-wall already showed that fails). This is heavier and gated on
   acquiring clean M1 for such an instrument, which the tribe does not have.
3. **All axes are shippable code (flag OFF, byte-identical, floor-neutral).** Merge is safe; a production flag-ON for
   "monoculture dilution" remains **unjustified by measurement**. Flag flip is an owner decision (not done).

---

### Reproduce
`logs/tribe_2d_relval/` (Route A, pure data): `gen_relval_series.py` (synthetic instruments + honest 2-leg slips),
`gen_relval_grid.py`, `run_relval.sh`, `score_relval.py`, `ROUTE_A_NOTES.md`, `id_OLD/id_NEW*` (byte-identity).
`logs/tribe_2d_vol/` (B), `logs/tribe_2d_htf/` (C): `gen_*_grid.py`, `logs/run_gate_symbol.sh`, `logs/score_gate.py`.
`logs/tribe_2d_rvgate/` (D): `gen_rvgate_grid.py` (per-leg β + runplan), `run_rvgate.sh`, `score_rvgate.py`.
Binary: `guardian/src/bin/primitive_scan.rs` — Route A needs no change; B/C add `htf_*` + `vol_*`; D adds
`rvspread` + `rv_*` + `--aux`. Byte-identity: run the identity manifest with NO new keys, diff SHA (73394ee4…).
All large derived CSVs / grid JSONs are gitignored (derivable from the generators).
