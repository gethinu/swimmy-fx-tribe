# tribe-2d — yardstick EXPANSION ②: the hold-to-barrier / fat-per-trade axis. Does letting winners run beat the cost wall? (measured 2026-07-21)

**Question (owner).** Stage ⑨ (session axis, `tribe_2d_session_axis_yardstick_20260719.md` `c5cd0f3d`) found that an
orthogonal *entry* gate surfaces NEW diverse *mechanism* frictionlessly (18 edges / 14 families) but **all die at
realistic 2-pip cost** — thin, high-frequency M15/H1 edges the spread eats. It concluded the binding wall had moved
from *mechanism* to *COST*, and that the honest bar rewards **fewer, larger trades** (the one deployable family,
MR-EUR/USD, survives because it is H4/H6 = low-frequency, fat-per-trade). This run tests that conclusion **directly**
with the most direct fat-per-trade lever, and asks: **does an expanded yardstick that makes trades FATTER contain a
new forward-robust (cpcv≥0.60, 2-window) diverse edge outside the MR-EUR/USD niche, and does per-trade thickness beat
the 2-pip cost wall?**

**Verdict up front (measured — and it is a genuinely new failure mode, distinct from the session axis):**
- **Realistic 2-pip: 0 new.** Exactly **one** 2-window forward-robust edge across 768 configs — and it is the *known*
  `keltner-EURUSD-MR-H4` in **signal** (pre-hold) mode. Hold-to-barrier contributes **0** forward-robust edges and is
  **0 load-bearing** (there is no config where the barrier exit revives an edge the signal exit killed).
- **Frictionless upper bound: still 0 load-bearing.** Only **2** forward-robust, both in the *same* `EURUSD/keltner/
  REVERSION/H4` family (1 signal + 1 barrier, and the barrier one's signal sibling is *also* robust ⇒ not
  load-bearing). **No new family appears even at zero cost** — unlike the session axis, which surfaced 14.
- **BUT the lever does the one thing the session axis could not: it beats COST.** The fat barrier trades **survive the
  2-pip spread** — PF drops only ~0.06–0.12 (e.g. `sma-EURUSD-H1-barrier` fric 1.348 → real 1.225; `bb-GBPUSD-H4`
  1.304 → 1.234; `donchian-USDJPY-H4` 1.230 → 1.175), where every session M15 edge collapsed below 1.0. The wall for
  hold-to-barrier is **not cost** — it is the **density floor (≥200 trades)** and **2-window CPCV robustness**.

**So the honest three-wall picture is now complete:** ⑨ session axis → mechanism exists, **COST** kills it (thin/
high-freq). ⑩ hold-to-barrier → **beats cost** (fat trades survive spread), but **DENSITY + 2-window robustness** kill
it. The only interior point of {trades≥200, cost-survivable-fat, 2-window-CPCV-robust} on this 4-symbol × 6-primitive
data remains the single `MR-EUR/USD H4/H6` niche — and it needs neither new axis.

Guardrails honored: redirect harness only (standalone `primitive_scan.exe` on `data/historical/*_M1.csv`); **no
repo-root `run-all-tests`**; live `swimmy.db` **never opened** (no DB pipeline / daemon this run); honest floor
constants **read, not edited** (200 / 1.10 / 0.30 OOS, 0.60 / 2.0 CPCV — compiled, confirmed unchanged); the axis is
**flag-gated to byte-identity** (no `hold_mode` key ⇒ SHA256-identical to the pre-hold *and* pre-session binary — hash
`0cfa3445…`, matching the ⑨ identity proof); no genome gene populated (no realistic-cost edge to carry — same
"don't build carriage ahead of the edge" discipline as ⑨); flag **not** flipped; unrelated dirty (LEGEND / guardian /
RECRUIT) untouched; grid CSVs derived from existing `data/historical/` (nothing new committed; scan JSONs in
gitignored scratch/`logs/`).

---

## 1. Axis selection — why hold-to-barrier, data-driven (1 axis this run)

The owner shortlist (from ⑨ §6, aimed at *low-frequency / fat-per-trade*) was: (a) day-of-week / intra-month
seasonality (`dow_mask` already wired), (b) event/regime-conditional entry gate, (c) multi-day / hold-to-barrier
swing. The session run's decisive finding is that the binding quantity is **per-trade thickness** (cost ÷ per-trade
edge). Scoring the three candidates against that quantity **and** against "is it already representable":

| candidate | attacks *fewer*? | attacks *larger* (per-trade thickness)? | already representable? |
|---|---|---|---|
| (a) dow / intra-month seasonality | YES (entry gate) | **NO** — same trade thickness, just fewer days | `dow_mask` already wired; it is the **same clock-gate family as the session axis that already failed at cost** |
| (b) event/regime entry gate | YES (entry gate) | **NO directly** — picks entries, does not change exit thickness | entry rarity/conviction already tunable via `dev`/`period` |
| (c) **hold-to-barrier / swing exit** | YES (a multi-day position blocks re-entry) | **YES — directly**: lets a winner ride to a wide barrier / time-stop | **NO** — see below |

Two measured discriminators make (c) the only defensible pick:

1. **Only (c) attacks per-trade thickness** — the exact quantity ⑨ identified as binding. (a) and (b) are *entry
   gates* of the same family as the session axis, which already hit the 2-pip wall with **0** survivors. Reducing
   entry frequency without thickening each trade cannot clear a *cost* wall.
2. **Only (c) is a genuine representational gap.** The scorer's exit was **unconditionally**
   `sl || tp || indicator-exit || timeout` (`primitive_scan.rs` backtest loop) — the indicator exit (`ex_long`/
   `ex_short`) is *always* OR'd in, so every mean-reversion trade is force-closed at the mid **regardless of how wide
   `tp` is set**. That caps per-trade edge at *band→mid* and is exactly why fat MR trades were only reachable at high
   TF (H4/H6, where the price-space band is large). Wide ATR barriers (`barrier_mode:"atr"`, `sl`/`tp` as ATR
   multiples) and a `max_hold` time-stop **already existed and were already swept by the census** — the *one* missing
   degree of freedom is **disabling the forced fast exit** so a trade can ride the move. Entry rarity/conviction — the
   essence of (a)/(b) — is *already* representable via `dev`/`period` (a high-`dev` band touch **is** a rare
   high-conviction entry), so folding "low-frequency" into the sweep keeps the new degree of freedom **minimal and
   honest**: exactly one new field.

Decision: **(c) hold-to-barrier only**, not paired with a redundant (b) regime gate — conviction/rarity is already in
the sweep (`dev`, `period`, long `max_hold`, higher TFs). Pairing would add a second new degree of freedom (more
multiple-testing surface, weaker honest claim) for a lever already available. If (c) had shown a frictionless signal
dying only to entry-chop, a regime gate would then be the justified next increment. It did not (§4).

## 2. Implementation — one flag-gated field (`hold_mode`), byte-identical OFF

**Scorer** `guardian/src/bin/primitive_scan.rs` (the same honest engine the daemon and every prior 2d run use). One
optional manifest field:

| field | default | meaning |
|---|---|---|
| `hold_mode` | `""` | `""` / `"signal"` ⇒ indicator-exit **ON** (pre-hold behavior). `"barrier"` ⇒ indicator-exit **OFF**: a winner rides to `sl`/`tp`/`timeout` only |

The change is a single gated term in both position arms: `… || (ex_long && !hold_barrier) || timeout`, where
`hold_barrier = m.hold_mode == "barrier"`. Honest floor constants (`OOS_MIN_TRADES=200`, `PF_GATE=1.10`,
`PEN_SHARPE_GATE=0.30`, `CPCV_PASS_RATE_GATE=0.60`, `CPCV_MEDIAN_SHARPE_MAX=2.0`) **read, never modified**.

**Byte-identity proof.** Pre-hold (backed-up) binary vs new binary on the **same 102 no-`hold_mode` configs**
(the ⑨ `identity_check.json`):
```
id_OLD.json  sha256 = 0cfa344531b150e5939d6d81a180ad92b6d681166a985b7bc80026faaa30ab08
id_NEW.json  sha256 = 0cfa344531b150e5939d6d81a180ad92b6d681166a985b7bc80026faaa30ab08   (IDENTICAL ✓)
```
This is the **same hash the session-axis doc recorded**, so flag-OFF is byte-for-byte the pre-session *and* pre-hold
engine. **Gate-fires / not-inert check:** for `keltner-EURUSD-H4-p50-d2.0` (sl3/tp8, max_hold=30) — `nofield` ≡
`signal` (identical: 237 trades, PF 1.279, pen 0.662), while `barrier` diverges (180 trades, PF 0.955, pen 0.000). It
correctly *hurts* a mean-reversion config (holding past the reversion gives the edge back), confirming (i) the mask
works and (ii) hold-to-barrier is a *continuation* tool, not an MR tool — as the grid then adjudicates across all six.

**No genome gene populated.** Same discipline as ⑨: with no realistic-cost hold-to-barrier edge to carry (§4),
populating a live exit-mode gene would be building carriage ahead of the edge. The scorer axis is the deliverable;
the genome side is deliberately untouched (even more clearly unwarranted here than for ⑨, which at least had 14
frictionless diverse families as future candidates — this axis has 0 new families).

## 3. The honest CPCV hold-to-barrier scan — coverage & method

**768 configs** = 4 symbols × **192**, where 192 = **16 entry-cores × 3 TF {H1, H4, D1} × 4 exits**. The 4 exits per
(core, TF): one **SIG** baseline (signal exit, atr sl=tp=2.5, no time-stop — reproduces the census/session baseline and
is the load-bearing *sibling*) + three **barrier** exits (`Bs2t5h3` = sl2/tp5 ATR, 3-day stop; `Bs25t8h7` = sl2.5/tp8,
7-day stop; `Bs3t6h0` = sl3/tp6, no time-stop). Entry cores: donchian-BREAKOUT {p20,40,55}, sma-TREND {p20,50,100},
keltner-MR {p30,50}×{dev2.0,2.5}, bb-MR {p20,40}×{dev2.0,2.5}, rsi-MR p14, stoch-MR p14. TFs **H1/H4/D1** — where a
multi-day hold is meaningful (H1 tests whether hold-to-barrier can **decouple fatness from TF**; H4 brackets the known
survivor; D1 = genuine swing, expected to hit the density floor). Each config scored on the tribe's **2-window forward
gate** — `oos_qualified AND cpcv_ok` in **both** selection (OOS 2021-25, CPCV full-span) **and** holdout (OOS+CPCV
2015-21) — at **realistic 2-pip** (EUR/GBP slip 0.0001; JPY 0.01) **and frictionless** (slip 0). Harness cross-check:
the realistic scan reproduces the census's one survivor (`keltner-EURUSD-MR-H4`, signal) exactly.

## 4. Results

### 4.1 Realistic 2-pip cost — the deployment bar

| symbol | configs | sel-robust | **forward-robust (2-window)** | of which barrier-mode | of which load-bearing |
|---|---:|---:|---:|---:|---:|
| EURUSD | 192 | 3 | **1** | 0 | 0 |
| GBPUSD | 192 | 0 | **0** | 0 | 0 |
| EURJPY | 192 | 0 | **0** | 0 | 0 |
| USDJPY | 192 | 0 | **0** | 0 | 0 |
| **total** | **768** | 3 | **1** | **0** | **0** |

The single forward-robust edge is `keltner-EURUSD-p50-d2.0-H4-**SIG**` (signal exit — the known MR family). The other
two EURUSD selection-robust are a **diverse hold-to-barrier near-miss** and a barrier re-parameterization of the same
keltner niche, both **failing the holdout**:

| config | mode | SEL (t, pf, pen, cpcv) | HOLD (t, pf, pen, cpcv) | fate |
|---|---|---|---|---|
| `keltner-EURUSD-MR-H4-SIG` | signal | 242, 1.223, 0.57, 0.60 | 377, 1.162, 0.43, 0.60 | **PASS** (known family) |
| `sma-EURUSD-TREND-H1-Bs3t6h0` | **barrier** | 263, **1.225**, 0.59, **0.60** | 378, **1.086**, 0.23, 0.50 | **FAIL holdout** |
| `keltner-EURUSD-MR-H4-Bs2t5h3` | barrier | 276, 1.153, 0.41, 0.70 | 421, 1.115, 0.30, 0.80 | FAIL holdout (same family) |

**The closest thing to success is `sma-EURUSD-TREND-H1-barrier`:** a genuinely *diverse* (EURUSD-TREND, not the
MR-EUR/USD family), *hold-to-barrier-driven*, *cost-surviving* edge that is **selection-robust at realistic 2-pip**
(pf 1.225, cpcv 0.60) — the fat-per-trade lever cleared the cost wall. But it **fails the holdout** (2015-21: pf
1.086 < 1.10, pen 0.23 < 0.30): the EURUSD trend-following fat-swing worked 2021-25 and not 2015-21. A single-window
artifact, not 2-window robust.

### 4.2 Frictionless upper bound — is a mechanism hiding behind cost?

| | frictionless forward-robust | diverse | **load-bearing** (SIG sibling dead) | new families |
|---|---:|---:|---:|---:|
| EURUSD | 2 | 2 | 0 | 0 |
| GBPUSD / EURJPY / USDJPY | 0 | 0 | 0 | 0 |
| **total** | **2** | 2 | **0** | **0** |

Both frictionless survivors are the **same** `EURUSD/keltner/REVERSION/H4` family (1 signal + 1 barrier), and the
barrier one's signal sibling is *also* robust ⇒ **not load-bearing**. **No new forward-robust family appears even at
zero cost** — the sharpest contrast with the session axis (14 new frictionless families). Hold-to-barrier does not
*create* a robust mechanism; it re-parameterizes the one that already exists.

### 4.3 Why it fails — the density floor, and cost is *not* the wall

Fate of the **576** frictionless barrier configs (per symbol-window), and the "frequent-AND-fat" quadrant:

| barrier-config fate (frictionless) | count | share |
|---|---:|---:|
| **fail: < 200 trades (DENSITY)** | **358** | **62%** |
| reach ≥200 but pf < 1.10 (no edge) | 192 | 33% |
| oos-qualified but CPCV fail | 18 | 3% |
| sel-robust but HOLDOUT fail | 6 | 1% |
| ≥200 & pf ok but pen_sharpe < 0.30 | 1 | — |
| **2-window forward-robust** | **1** | — |

- **Density is the dominant killer (62%).** Fat ⟹ rare: the "let winners run" continuation edge is real but
  concentrates into *few, huge* trades — e.g. `sma-EURUSD-D1-barrier` PF **1.29 → 30.3** on **8 trades**,
  `sma-USDJPY-D1` 1.49 → 7.25 on **3 trades** (barrier beats its signal sibling on PF in **235/573** pairs, mean
  +0.013, all big gains are trend-following on D1). These are statistically empty — nowhere near the 200-trade floor.
- **26** configs are both frequent (≥200) *and* fat (pf≥1.10) frictionless — including *diverse* ones
  (`donchian-USDJPY-BREAKOUT-H4` t201/pf1.23, `donchian-EURJPY-H1` t339/pf1.14, `bb-GBPUSD-H4` t296/pf1.30). The
  mechanism **exists** in this quadrant. But only **1** clears the full 2-window CPCV gate (the known keltner family);
  the rest have CPCV pass-rates 0.20–0.50 and fail cross-validation / holdout.
- **Cost is NOT the wall** (the key difference from ⑨). The fat trades survive 2-pip almost intact — cost gap on the
  diverse near-misses: `bb-GBPUSD-H4` pf 1.304→1.234, `donchian-USDJPY-H4` 1.230→1.175, `sma-EURUSD-H1` 1.348→1.225.
  They die on **CPCV pass-rate (0.30–0.50, never 0.60 both windows)** and **holdout**, not on the spread.

## 5. Honest verdict

| question | measured answer |
|---|---|
| Is hold-to-barrier a genuine new degree of freedom (not already swept)? | **YES** — the forced `\|\| indicator-exit` capped per-trade thickness; disabling it is the one gap. Byte-identical OFF (SHA match). |
| Does it make trades fatter / lower-frequency? | **YES** — 237→180 trades, per-trade PF up on continuation entries (235/573 barrier configs beat their signal sibling). |
| Does per-trade thickness beat the **2-pip cost wall** (the session axis's killer)? | **YES** — fat trades survive the spread (PF drops only ~0.06–0.12; every session M15 edge had collapsed below 1.0). |
| New forward-robust diverse family at **realistic 2-pip**? | **NO — 0.** Closest is `sma-EURUSD-TREND-H1-barrier`: diverse, cost-surviving, **selection-robust**, but **fails the holdout**. |
| New forward-robust diverse family **frictionless**? | **NO — 0 load-bearing, 0 new families** (vs 14 for the session axis). Only the known MR-EUR/USD niche, re-parameterized. |
| What is the binding wall for this axis? | **DENSITY floor (62% < 200 trades) + 2-window CPCV robustness** — NOT cost. Fat ⟹ rare collides with the ≥200-trade honest floor; the frequent-and-fat survivors are not cross-validation-stable. |
| "Not found" vs "search too shallow"? | **Not found at the deployment bar**, well-grounded: 768 targeted configs, 2-window gate, realistic **and** frictionless; the frictionless ceiling is 0 load-bearing (not a cost artifact I under-powered). Targeted, not exhaustive — but the failure is *structural* (density floor), not a needle in a denser grid. |

**Conclusion.** The most direct fat-per-trade lever **does** solve the problem the session axis exposed — fat trades
survive the 2-pip spread that annihilated every thin intraday edge. But it **does not** produce a new forward-robust
diverse family, at any cost level, because it collides with a *different* honest-floor constraint: **per-trade fatness
and the ≥200-trade density floor are in direct tension**, and the few configs that are simultaneously frequent, fat,
and cost-surviving are **not stable across both validation windows** (`sma-EURUSD-TREND-H1` is the exemplar: 2-pip
selection-robust, holdout-dead). The tribe's anti-overfit machinery — the very density floor and 2-window CPCV that
keep lucky-few-trade strategies out — is exactly what forbids "make each trade fat" from manufacturing diversity. The
sole point where {density≥200, cost-survivable-fat, 2-window-robust} co-satisfy on this data remains the one
`MR-EUR/USD H4/H6` niche, and it needs neither the session gate nor the barrier exit.

**The two orthogonal levers ⑨ pointed at — *fewer* (session/clock gate) and *fatter* (hold-to-barrier) — have now each
been implemented and measured, and each fails for a distinct, honestly-named reason** (⑨: cost; ⑩: density +
2-window robustness). Neither breaks the monoculture at the deployment gate.

## 6. Next move (owner decision)

1. **Bank the finding; keep the flag OFF; do not mine the near-miss.** `sma-EURUSD-TREND-H1-barrier` is one denser
   param-grid from a *possible* holdout pass — but that pass would be a multiple-testing artifact of a
   single-window edge, not robust diversity. **Do not** lower the density or CPCV floor to admit it.
2. **The lesson for the yardstick program updates.** The wall is no longer any single axis — it is the **joint honest
   box {trades≥200, cost-survivable, 2-window-CPCV-robust}**, which has exactly one interior point on the current
   data. Two axes have now demonstrated that expanding *one* dimension (entry-orthogonality; per-trade thickness)
   relocates the failure to *another* floor rather than escaping the box. A next axis should target the **remaining
   free dimension: the 2-window robustness itself** — i.e. an orthogonal feature whose diverse edge is *stable across
   both 2015-21 and 2021-25 regimes*, not merely present in one. Candidates that could plausibly deliver *stability*
   (not just presence): cross-pair relative-value / divergence (a *structural* spread that persists across regimes),
   or a genuinely new data axis (a symbol/instrument whose edge family is absent from the current four). Both are
   heavier; both are now better-targeted by knowing cost and density are *not* the free dimension.
3. **Do NOT chase (a) seasonality or (b) a standalone regime gate as separate runs.** Both are entry gates of the
   session family (⑨) and would inherit its cost wall unless *combined* with fatness — and this run shows fatness
   itself hits density. A `dow_mask`/regime gate stacked on hold-to-barrier would *compound* the density problem
   (each gate cuts trades further below 200). Deprioritized with reason, not merely untried.
4. **Both axes are shippable code (flag OFF, byte-identical, honest floor neutral).** Merge is safe; a production
   flag-ON for "monoculture dilution" remains **unjustified by measurement**. Flag flip is an owner decision (not done).

---

### Reproduce
`logs/tribe_2d_hold_barrier/` (gitignored): `gen_hold_grid.py` (192-config/symbol generator: 16 cores × 3 TF × 4
exits), `run_symbol.sh` (4 scans/symbol: {sel,hol}×{real,fric}), `score_hold.py` (2-window forward join + SIG-sibling
load-bearing test + family cluster), `verdict_{real,fric}.txt`, `cost_gap.txt`, `HOLD_SUMMARY_{real,fric}.json`.
Byte-identity: run the pre-hold binary and the new binary on the ⑨ `identity_check.json`, diff (identical SHA above).
Scan: `primitive_scan.exe --data data/historical/<SYM>_M1.csv --manifest grid_<SYM>.json --out <..>.json
--slippage <0.0001|0.01|0> [holdout: --oos-start 1420070400 --oos-end 1609459200 --cpcv-start 1420070400
--cpcv-end 1609459200]`. Large grid JSONs kept in scratch (derivable from the generator).
