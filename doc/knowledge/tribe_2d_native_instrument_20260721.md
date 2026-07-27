# tribe-2d — yardstick EXPANSION ④: NATIVE new-instrument (different asset class) — the one lever ⑪ left open (measured 2026-07-21)

**Question (owner).** ⑪ closed the cross-pair *derived* route and characterised the wall as the **joint box
`{trades≥200 ∧ realistic-cost-survivable ∧ 2-window-CPCV-robust}`** with exactly **one** interior point (MR-EUR/USD
H4/H6). Its §7.2 named the sole remaining lever: **a *native* single-series instrument whose edge is intrinsically
2-window-robust at single-leg cost — a different asset class with different regime dynamics (metals / index / rates),
NOT a derived combination of the existing four FX pairs (⑪ closes that) and NOT just more FX pairs fed to non-robust
mechanisms (⑦ data-wall closes that).** This run acquires real native data for those asset classes, canonicalises it to
Rule-A M1, sets an honest per-instrument cost, and runs the tribe's unchanged 2-window forward gate.

**Verdict up front (measured):**
- **The native-instrument route is the FIRST lever in the entire 2d program to add a box interior point beyond
  MR-EUR/USD.** At realistic (conservative, sourced) cost, one native index config is 2-window forward-robust:
  **`UK100 keltner-REVERSION p50 d3.0 H2`** (FTSE-100 CFD) — SEL 278/1.172/cpcv0.70, HOLD 300/1.174/cpcv0.60. It is on
  **true M1**, survives a conservative full-spread cost (RT ≈2.7 bps), and is a genuinely **new instrument / asset
  class** (GBP equity index; maximal symbol distance from both the USDJPY-TREND monoculture and the EURUSD-MR point).
- **BUT it is the SAME mechanism family (keltner mean-reversion), at the SAME sub-deployment quality** (pf ~1.17,
  median-sharpe ~0.5) as the known point — a **new, uncorrelated instrument for a known mechanism, not a new mechanism
  family and not a deployable-grade edge.** Return correlation with MR-EUR/USD is ~0 (daily **+0.019**, monthly
  **+0.061**): a genuine diversifier despite the shared mechanism.
- **The box grows from 1 interior point to 2 — both keltner-MR, on uncorrelated instruments (EURUSD FX + UK100 index).**
  Multi-*family* diversity is still **NO**: the one genuinely new *mechanism* surfaced (JP225 **sma-TREND** H2, frictionless
  1.213/1.241) **dies at realistic cost** — the ⑨ COST wall again. Metals (gold) = **0 at any cost**. Rates = **no
  intraday data exists** (untested gap; see §6).
- **Net:** monoculture still unbroken **at the deployment gate**, but for the first time the diverse-robust set is
  measurably **>1 uncorrelated instrument** (both MR, both sub-deploy). This directly, but only partially, touches §4-2.

Guardrails honored: the native SCAN used the **UNCHANGED** compiled scanner (a native instrument is pure data, like ⑪
Route A); the only binary edit is a **verification-only** flag-gated `--dump-daily` (off ⇒ `--out` **SHA256-identical**,
proven: `3bea89ca…` before==after), used solely offline to extract PnL streams for the correlation/regime checks. Honest
floor constants (`200 / 1.10 / 0.30 / 0.60 / 2.0`) **read, never modified**. The bundle repo (`mt5_Bundle-of-edges`) was
**READ-ONLY** — only its CSV/parquet were read; every output went to gitignored `logs/tribe_2d_native/`. **No brought-in
data committed** (data/historical/ and logs/ are gitignored). Live `swimmy.db` never opened; no genome gene, no flag flip.
**No paid/irreversible fetch** — all data was already on disk at zero charge. Unrelated dirty (LEGEND/guardian) untouched.

---

## 1. Data acquired (what, period, quality)

Everything below was **already on disk in the read-only bundle at zero charge**; canonicalised into the gitignored
scratch through a Rule-A canonicaliser that **refuses to write** any source whose median bar size ≠ 60 s unless a native
sub-hourly size is *declared* (so pre-aggregated D1/H1 cannot be smuggled past the M1→TF resample — the phantom-PASS trap
that bit twice before). The scorer does the M1→TF resample itself; feeding it genuine M1 exercises that path honestly.

| asset class | instrument(s) | source | bars | period | granularity | quality |
|---|---|---|---|---|---|---|
| **index** | US500, DE40, F40, JP225, UK100 | IC-Markets MT5 CFD M1 (`data/ohlc_index_icm/*_m1_*.csv`) | 1.79–2.64 M each | **2017-01-03 → 2024-12-31** | **true M1** (median Δ=60 s, min 60 s) | 0 dup ts, 100% OHLC-consistent, frac-60s 0.97–0.99 (rest = overnight/weekend closes, expected for index CFDs) |
| **metals** | XAUUSD | Dukascopy M15 20-yr parquet (`data/ohlc_cache/xauusd_m15.parquet`) | 249 k | **2014-06 → 2024-12** | **M15-base** (declared) | 0 dup, 100% consistent; used only at TF≥H1 (≥4×M15) — **granularity validated bit-identical, §3** |
| **rates** | — | none | — | — | — | **no intraday OHLC exists** in the bundle (D1 ETFs TLT/IEF only); untested (§6) |

- **Index HOL window caveat (disclosed):** dense index M1 begins 2017 (2010–2016 are stubs), so the holdout OOS
  (defined 2015-2021) runs effectively on **2017-2021** and selection on 2021-2024 — two genuine, distinct regimes
  (2017-21: melt-up→2018 vol→COVID; 2021-24: 2022 bear→2023-24 bull). Gold spans **both** windows fully (2014-2024).
- **Timezone:** the ICM `datetime` is naive (broker/UTC); with **no session gate** and year-boundary windows a 1–3 h
  offset shifts a handful of bars among millions — immaterial to the resample or the 2015/2021/2025 splits.

## 2. Cost model (honest, per-instrument, sourced)

FX 2-pip is **not** reused. Each instrument's realistic per-side slip is set in its **own price units** from bundle specs.
Frictionless is run alongside every instrument (mechanism-exists vs deployable).

| instrument | realistic per-side slip | source | RT cost (bps) | note |
|---|---|---|---|---|
| US500 | 0.40 pt | median `spread_med` in `us500_m5.parquet` (2017-24) | ~2.1 | full-spread each side |
| DE40 | 1.20 pt | `de40_m5.parquet` spread_med | ~1.8 | |
| F40 | 1.00 pt | `f40_m5.parquet` | ~3.3 | |
| JP225 | 7.00 pt | `jp225_m5.parquet` | ~5.3 | widest CFD |
| UK100 | 1.00 pt | `uk100_m5.parquet` | ~2.7 | |
| XAUUSD | 0.125 ($/oz) | `scan_adapter_d1.py` `PAIR_COSTS[xauusd]=25p` retail_global RT, pip 0.01 | ~1.2 | IC-Markets std tier |

Per-side slip = the **full** median `spread_med`, so round-trip = 2×spread — matching the bundle's own index-cost
convention ("deduct spread on entry AND exit", `docs/index_intraday_probe_prespec_20260708.md`) and **comparable to /
slightly more conservative than** FX's 2-pip (~1.8 bps RT). The scanner applies slip on both fills (RT = 2×slip). This is
**not** understated; if anything it is on the conservative side (see the cost-sensitivity sweep, §4).

## 3. Rule-A + M15-granularity validation (honesty for the gold track)

The gold 20-yr history exists only at M15. Feeding M15 as the canonical base is legitimate **only** because the scorer
resamples it to the trade TF (H1–D1, each ≥4×M15) — never read at or below M15. To prove the granularity does not
manufacture a verdict, the KNOWN interior config was scored on EURUSD both ways (EURUSD is true M1; resample it to M15):

| keltner-EURUSD-MR-H4 @ realistic 2-pip | M1-base | M15-base |
|---|---|---|
| SEL | t=242, pf=1.223, cpcv=0.60, qual=**True** | t=242, pf=1.223, cpcv=0.60, qual=**True** |
| HOLD | t=377, pf=1.162, cpcv=0.60, qual=**True** | t=377, pf=1.162, cpcv=0.60, qual=**True** |

**Bit-identical.** H4 signals are close-based and the resampled H4 open/close are identical (high/low differ negligibly,
not enough to flip an ATR barrier). The gold M15-base track is therefore trustworthy at H1+.

**Harness self-validation:** the generic native harness, run on the existing EURUSD M1, re-derives the **sole known box
interior point** `keltner-EURUSD-MR-H4` (real SEL pf 1.223 / HOLD pf 1.162) and nothing else — matching rollup §0.4 and
⑧/⑪. Frictionless sel-robust=8 collapses to realistic 2-window=1: cost + holdout do real work.

## 4. Results — 2-window forward gate (SEL 2021-25 / HOL 2015-21, honest floor unchanged)

**Frictionless (does the native mechanism exist?):**

| instrument | 2-window forward-robust (frictionless) |
|---|---|
| US500 | 1 — `stoch-MR-H2` (985/1.124, 1006/1.181) |
| **JP225** | 1 — **`sma-TREND-H2`** (335/1.213/cpcv0.70, 339/1.241/cpcv0.60) — a genuinely NEW *mechanism* (trend, not MR) |
| UK100 | 4 — `keltner-MR` H1/H2 neighbourhood |
| DE40, F40, XAUUSD | 0 |

**Realistic cost (deployable ⇒ NEW BOX INTERIOR POINT?):**

| instrument | 2-window forward-robust (realistic) |
|---|---|
| **UK100** | **1 — `keltner-REVERSION p50 d3.0 H2`: SEL 278/1.172/cpcv0.70/med0.56, HOLD 300/1.174/cpcv0.60/med0.44** |
| US500, DE40, F40, JP225, XAUUSD | 0 |

- **JP225 sma-TREND** is the first native *mechanism*-diverse edge to clear 2-window **frictionless**, but is 0 at
  realistic cost — the ⑨ COST wall (thin per-trade edge eaten by spread). Mechanism exists; not deployable.
- **Gold (metals): 0 at any cost** — frictionless only 2 sel-robust (sma-TREND-H1, donchian-BREAKOUT-H2), both fail
  holdout. An honest negative for the canonical "different asset class."

**Cost-robustness sweep of the UK100 interior point** (slip in price pts; the point is IN-BOX iff 2-window-robust):

| slip | RT bps | 2-window-robust configs | interior SEL/HOLD pf | in box? |
|---|---:|---:|---|---|
| 0.0 (fric) | 0.00 | 4 | 1.243/1.237 | yes |
| 0.5 (½ spread) | 1.36 | 1 | 1.203/1.203 | **yes** |
| **1.0 (full spread = realistic)** | **2.71** | **1** | **1.172/1.174** | **yes** |
| 1.5 | 4.07 | 0 | 1.149/1.126 | no |
| 2.0 | 5.42 | 0 | 1.117/1.098 | no |

The point survives from frictionless through full-spread cost with margin, and dies at 1.5–2× cost — a bounded, honest
cost margin (comparable to MR-EUR/USD's own margin). Not a subsidised near-miss.

## 5. Is the UK100 point "really a different family"? (owner's §4 requirement)

| dimension | measured | reading |
|---|---|---|
| **mechanism family** | keltner-REVERSION (band mean-reversion) | **SAME** as the sole known point — NOT a new mechanism |
| **instrument / asset class** | UK100 = GBP equity-index CFD vs EURUSD = FX | **DIFFERENT** — maximal symbol distance; B-4 (symbol/TF/behaviour) > 0.20 trivially (different symbol *and* asset class) |
| **return correlation vs MR-EUR/USD** (exact backtest PnL, `--dump-daily`, common 2017-25) | daily Pearson **+0.019**, monthly **+0.061**; traded together only 144/883 days | **~uncorrelated** — a genuine diversifier despite the shared mechanism |
| **regime dependence** (UK100 yearly PnL) | positive **6/8** years; loses only 2023 (strong low-vol melt-up, where MR *should* lose) + flat 2019; strong 2018/2020/2021/2022 | broad, MR-coherent; **not** a single-regime artifact (consistent with passing BOTH windows) |

**Honest characterisation:** a **new, ~uncorrelated, different-asset-class instrument** for the **known mean-reversion
mechanism**, at **sub-deployment** quality. It expands the box from 1→2 interior points (both MR, uncorrelated). It is
**not** a new mechanism family and **not** a deployable-grade live edge. Single-feed caveat: UK100 is one IC-Markets CFD
feed; a different index feed could shift the marginal config (the point sits pf 1.17 with cpcv exactly 0.60 on holdout).

## 6. Rates — the untested gap

The bundle has **no intraday rates OHLC** (only D1 ETFs TLT/IEF/… and FRED daily yields — feeding D1 to an M1 scorer is
the exact Rule-A phantom-PASS violation). The only acquisition path is `drafts/fetch_dukascopy_bars_20260711.py`
(`ust10/bund/ukgilt`), which is **free** (Dukascopy public feed) but **never executed**, Windows-only, with symbol
strings and price scales that are **explicit GUESSES needing `--verify` first**, and writes into the read-only bundle. It
is not a charging decision, but it is a flaky, unverified fetch with real phantom-data risk. Given a complete 2-class
answer already exists, this run **does not attempt it** and surfaces it as an owner go/no-go (a tiny probe-first fetch to
scratch, schema+scale validated, would be the disciplined way if pursued).

## 7. Honest verdict + relationship to §4-2

- **⑫ is the first stage to expand the joint box beyond MR-EUR/USD.** The native route delivers a genuine 2nd interior
  point (UK100 keltner-MR-H2): a new asset class, ~uncorrelated returns, cost-robust at a conservative sourced spread —
  but the **same MR mechanism** at **sub-deployment** quality.
- **Monoculture still unbroken at the DEPLOYMENT gate.** No native config reaches deploy-grade (pf/median-sharpe), and
  the one new *mechanism* (JP225 TREND) dies at realistic cost. So the deeper "true multi-*family* diversity" requirement
  is still unmet; what grew is the count of uncorrelated diverse-robust **instruments within the MR family** (1→2).
- **§4-2 (diverse ≥1 robust):** already ≥1 (MR-EUR/USD); now there are **2 uncorrelated diverse-robust instruments**.
  Still both MR and both below deploy, so §4-2's *decisive* bar (a deployable diverse edge) remains unmet — but the
  diverse-robust set is, for the first time, measurably plural and cross-asset-class.
- **Levers now measured & closed:** derived cross-pair (⑪), entry-gates/session/hold-barrier (⑨/⑩/⑪ B-C), bundle-import
  (⑥), FX data-wall (⑦), inside-out census (⑧). Native asset-class data (⑫) is the last structural lever — it *adds a
  same-family diversifier* but does not produce a new deployable or new-family edge. **Metals: 0. Rates: no data.**

**Deploy NO, flag OFF.** The canonicaliser + native harness + `--dump-daily` are shippable (off ⇒ byte-identical output).
Merge is safe; a production flag-on / deployment of the UK100 edge is **not** justified (sub-deployment quality). Whether
to acquire native rates M1 (free Dukascopy, unverified) is an owner decision (§6).

---

### Reproduce
`logs/tribe_2d_native/` (all gitignored): `canonicalize.py` (Rule-A guard, format auto-detect, parquet), `build_index.sh`
(5 index CFDs 2017-24), `gen_native_grid.py` (31 cores × 6 TFs H1–D1, balanced MR/TREND/BREAKOUT), `run_native.sh`
(sel/hol × real/fric, single-leg slip), `score_native.py` (2-window gate + box-interior test), `m15_validation.py`
(M15↔M1 granularity), `BASELINE.md` (harness self-validation). Cost sourced from `*_m5.parquet` `spread_med` +
`scan_adapter_d1.py`. Binary: `guardian/src/bin/primitive_scan.rs` — native scan needs NO change; `--dump-daily` is
verification-only (off ⇒ SHA256-identical `--out`, proven `3bea89ca…`). Interior configs / PnL streams:
`daily_UK100.jsonl`, `daily_EURUSD.jsonl`; cost sweep `g_{sel,hol}_s{0.5,1.5,2.0}_UK100.json`.
