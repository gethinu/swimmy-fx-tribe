# tribe-2d — yardstick EXPANSION: the session/time-of-day axis. Does a new orthogonal primitive grow forward-robust diversity? (measured 2026-07-19)

**Question (owner).** The inside-out census (`tribe_2d_inside_out_diversity_20260719.md` `6ee78541`) settled that the
current 6-primitive × 4-symbol × per-regime-CPCV yardstick contains exactly **one** forward-robust diverse
edge-family (Keltner/BB mean-reversion on the EUR/USD-quoted majors, H4/H6), realistic **and** frictionless — so
"beyond one niche is a **yardstick** problem, not a search-depth or breeding problem." This run actually *widens
the yardstick*: it adds a **new orthogonal primitive axis — a trading-SESSION / time-of-day entry gate** — to the
honest scorer, and asks the concrete question: **does an expanded yardstick contain a new forward-robust
(cpcv ≥ 0.60, 2-window) diverse edge-family outside the MR-EUR/USD niche?**

**Verdict up front (measured, and it is a genuinely new kind of result):**
- **Frictionless: YES — the session axis is truly orthogonal and surfaces NEW diverse *mechanism* the 24h
  yardstick is structurally blind to.** At zero cost the 2-window gate admits **18 diverse forward-robust edges
  across 14 distinct new families** — including **EURJPY** band-MR in the Asian session (EURJPY was **0 even
  frictionless** in the whole 24h census), a **USDJPY donchian-BREAKOUT** in the London/NY overlap (a *diverse*,
  non-TREND USDJPY edge — the monoculture's own symbol in a new regime), and **EURUSD trend in the NY session**.
  The session→regime alignment is mechanistically coherent (Asian range → mean-reversion, ×7 survivors across 3
  symbols; NY → trend; London-open → GBPUSD), i.e. real signal, not multiple-testing noise. This is the **first
  lever in the whole 2d line to produce mechanism-diversity beyond the single MR-EUR/USD family.**
- **Realistic 2-pip cost: NO — 0 forward-robust, everywhere.** Every one of those session edges lives *below* the
  transaction-cost floor. They are thin, high-frequency M15/H1 edges (PF 1.10–1.33 frictionless) and the spread
  eats them decisively: e.g. `bb-EURUSD-ASIA-M15` PF **1.12 → 0.65** (pass-rate 0.60 → 0.00) at 2-pip;
  `keltner-EURUSD-ASIA` → 0.77; `bb-EURJPY-ASIA` → 0.87. The single closest-to-deployable session edge
  (`keltner-GBPUSD-MR` in ASIA/OVERLAP) stays selection-robust at 2-pip but **fails the holdout** — and it is the
  *same* band-MR family, not new diversity.

**So: "widening the yardstick produces diversity?" → frictionless YES, deployable NO.** The session axis expands
the *representable mechanism* space for real (measured: EURJPY, breakout, trend all appear where the 24h scan had
nothing), but the new mechanisms do not clear the honest deployment bar at realistic cost. **The binding wall for
the session axis is COST, not mechanism** — the mirror image of the data-wall verdict (`c20098d8`), where the wall
was mechanism, not data. The monoculture is not broken at the deployment gate; but for the first time we can *name
concrete diverse mechanisms that provably exist frictionlessly* — a more tractable failure mode than "nothing
exists anywhere."

Guardrails honored: redirect harness only (standalone `primitive_scan.exe` on `data/historical/*_M1.csv`); **no
repo-root `run-all-tests`**; live `swimmy.db` **never opened** (no DB pipeline, no daemon this run); honest floor
constants **read, not edited** (200/1.10/0.30 OOS, 0.60/2.0 CPCV — compiled, confirmed unchanged); the session axis
is **flag-gated to byte-identity** (no-session input ⇒ SHA256-identical output vs the pre-session binary); the
genome-side B-4 session term is a **dormant hook, proven numerically inert** for every current genome; flag **not**
flipped in production; unrelated dirty (LEGEND / guardian / RECRUIT) untouched; grid CSVs derived from existing
`data/historical/` (nothing new committed; large scan JSONs kept in gitignored scratch/`logs/`).

---

## 1. Axis selection — why SESSION over cross-pair (data-driven, 1 axis this run)

The owner's shortlist was (a) session/time-of-day, (b) cross-pair, (c) multi-TF, (d) new indicator family; with the
census's own guidance that same-family MR/trend indicators (d) don't add diversity, so bet on the orthogonal axes.
**This run bets the whole budget on (a) session**, for four measured reasons:

1. **Maximal orthogonality.** All 6 existing primitives are pure functions of OHLC. A session gate is a pure
   function of the **clock** (`timestamp`), independent of price — it multiplies the existing signal by a time mask
   without touching the signal. It cannot be a "denser sampling" of an existing primitive; it is a genuinely new
   axis of the representation.
2. **Strongest documented real mechanism.** Intraday FX seasonality (Asian-range mean-reversion, London-open
   breakout, London/NY-overlap volatility, NY trend/reversal) is one of the most replicated microstructure
   regularities — it is a *structural* consequence of which participants are active by the clock, not a data-mined
   coincidence. If any orthogonal axis has a real edge, this is the prior favourite.
3. **It attacks the scan's exact blind spot.** The honest scan integrates PnL over all 24h, so any edge that lives
   in a 3–4h window and is flat/negative elsewhere is **averaged to ~0 and rejected**. The census found `EURJPY = 0
   even frictionless`; a session gate is precisely the instrument to reveal whether that is "no edge" or "no
   *24h-average* edge." (Measured below: EURJPY does have a frictionless Asian-session edge.)
4. **Cleanest honest implementation, no new data.** A UTC-hour window is computable from the existing `timestamp`
   with a provably byte-identical no-op default. Cross-pair (b) needs two-symbol timestamp alignment across
   resampled timeframes and a signal definition — heavier, higher-risk, and not finishable as a *rigorous* 2-window
   scan in one session. Deferred to the next hand (§6), now better-targeted by this run's cost finding.

## 2. Implementation — flag-gated session gate (byte-identical OFF)

**Scorer** `guardian/src/bin/primitive_scan.rs` (the same honest engine the daemon and every prior 2d run use).
Added optional manifest fields, all with byte-identical defaults:

| field | default | meaning |
|---|---|---|
| `sess_start` | 0 | entry-allowed window start, UTC hour |
| `sess_end` | 24 | window end (exclusive); `start>end` wraps past midnight |
| `dow_mask` | 0 | day-of-week bitmask (0 ⇒ all days) |
| `session` | "" | human label only (never read by backtest logic; omitted from output when empty) |

`in_session(m, ts)` is a pure function of the bar timestamp; it gates **entries only** (exits/SL/TP unaffected), so
it is a clean multiplicative mask on the existing signal. The default `(0,24,0)` short-circuits to `true`, so a
manifest **without** session keys takes exactly the entries it took before. Honest floor constants
(`OOS_MIN_TRADES=200`, `PF_GATE=1.10`, `PEN_SHARPE_GATE=0.30`, `CPCV_PASS_RATE_GATE=0.60`,
`CPCV_MEDIAN_SHARPE_MAX=2.0`) are **read, never modified**.

**Byte-identity proof.** Old (pre-session) binary vs new binary on the **same 102 no-session configs**:
```
id_OLD.json  sha256 = 0cfa344531b150e5939d6d81a180ad92b6d681166a985b7bc80026faaa30ab08
id_NEW.json  sha256 = 0cfa344531b150e5939d6d81a180ad92b6d681166a985b7bc80026faaa30ab08   (IDENTICAL ✓)
```
Flag-OFF (no session field) is byte-for-byte the pre-session engine. Gate-fires check: `keltner-p55-H1` trades
879 (24H) → 686 (LONDON) → 452 (OVERLAP) → 287 (ASIA), and its edge *concentrates* — dead over 24h (PF 1.001) but
positive session-gated (ASIA PF 1.068 pen 0.206) — confirming the mask works and that 24h-averaging hides structure.

**Genome (B-4) first-class hook.** `src/lisp/school/school-genome.lisp` gains `*b4-w-session* = 0.10` and a
session term in `b4-genetic-distance`, added with the **exact dormant-hook discipline of the existing `:behavior`
term**: it contributes only when *both* genomes carry a `:session` gene, otherwise its weight renormalises away.
Proven numerically inert (cl-user standalone, no system/DB): session-less genomes score `0.7881642600` **with or
without** the term (identical); populated genomes fire (`ASIA` vs `LONDON` → 0.822 ≠ 0.788). Combined with flag-OFF
dispatching to the untouched `legacy-genetic-distance`, the distance is byte-identical for every live strategy
(flag OFF *and* ON). No live `strategy` struct field was added — deliberately: with no realistic-cost session edge
to carry (§4), populating a live session gene would be building carriage ahead of the edge, which the regen design
explicitly avoids. The hook is *ready*, not *populated*.

## 3. The honest CPCV session scan — coverage & method

**2,448 configs** = 4 symbols × 612, where 612 = 6 primitives (keltner/bb/rsi/stoch → MR, donchian → BREAKOUT,
sma → TREND) × trimmed param neighbourhoods × **3 timeframes {M15, H1, H2}** × **6 session windows**:

| session | UTC window | rationale |
|---|---|---|
| `24H` | none (control) | the pre-session baseline, *same config, no gate* |
| `ASIA` | 00–08 | Asian range → mean-reversion |
| `LONDON` | 07–16 | London hours → trend |
| `NY` | 12–21 | New York hours → trend/reversal |
| `OVERLAP` | 12–16 | London/NY overlap → high volatility |
| `LONOPEN` | 07–10 | London open → breakout |

TFs are **M15/H1/H2 only** — the band where UTC-hour gating is well-posed (M15/H1 exact, H2 to 2h). H4/H6 are
excluded *by construction*: a 3–4h session collapses to one bucket-start-hour there, and the 200-trade floor is
unclearable. **This is exactly why the known MR-EUR/USD H4/H6 family is out of scope for the session axis** — and it
is the right scoping, because the census already established forward-robust = **0 at M15/H1/H2 over 24h**. So the
test is clean: *the 24h baseline at these TFs is empty (cross-validated below); does gating them to a session revive
anything?* Each config scored on the tribe's own **2-window forward gate** — `oos_qualified AND cpcv_ok` in **both**
the selection window (OOS 2021-25, CPCV full-span) **and** the holdout (OOS+CPCV 2015-21) — at **realistic cost**
(EUR/GBP 2-pip = slip 0.0001; JPY pairs 2-pip = slip 0.01) **and frictionless** (slip 0).

Harness cross-check: the 24H baseline reproduces the census — forward-robust ≈ 0 at M15/H1/H2 (only a single
`sma-USDJPY-24H-H2` frictionless cost-boundary artifact, matching the census's "USDJPY sma-TREND" frictionless
extra). The session engine agrees with the 24h engine where they overlap.

## 4. Results

### 4.1 Realistic 2-pip cost — the deployment bar

| symbol | configs | selection-robust | **forward-robust (2-window)** |
|---|---:|---:|---:|
| EURUSD | 612 | 0 | **0** |
| GBPUSD | 612 | 3 | **0** (all 3 fail holdout) |
| EURJPY | 612 | 0 | **0** |
| USDJPY | 612 | 0 | **0** |
| **total** | **2,448** | 3 | **0** |

**Zero forward-robust diverse (or any) session edge at realistic cost.** The 3 GBPUSD selection-robust are all
`keltner-MR` in ASIA/OVERLAP — the same band-MR family — and each fails the holdout (hol pass-rate 0.40–0.60, hol
PF ~1.02–1.06, just under the 1.10 gate).

### 4.2 Frictionless upper bound — is a mechanism hiding behind cost?

| | frictionless forward-robust | diverse | session-ONLY (24h sibling dead) |
|---|---:|---:|---:|
| EURUSD | 5 | 5 | 5 |
| GBPUSD | 10 | 10 | 10 |
| EURJPY | **2** | 2 | 2 |
| USDJPY | 3 | 1 | 3 |
| **total** | **20** | **18** | **19** |

**18 of 20 are diverse and session-ONLY** (their 24H sibling is *not* forward-robust ⇒ the session gate is
load-bearing, i.e. the edge is invisible to the 24h scan). **14 distinct new diverse families**, the notable ones:

| family | note |
|---|---|
| `EURJPY / bb-MR / ASIA / M15` (2) | **EURJPY was 0 even frictionless in the entire 24h census** — the session axis finds it |
| `USDJPY / donchian-BREAKOUT / OVERLAP / H2` | a **diverse (non-TREND) USDJPY** edge — the monoculture's own symbol, new regime |
| `EURUSD / sma-TREND / NY / H1` (2) | a **trend** edge (new regime for EURUSD), NY-session-specific |
| `GBPUSD / {keltner,bb,rsi}-MR / {LONOPEN,OVERLAP,ASIA}` (7) | coherent GBPUSD session-MR cluster |

By session: `ASIA 7, LONOPEN 5, NY 3, OVERLAP 3, LONDON 1`. By regime: `REVERSION 14, TREND 5, BREAKOUT 1`. The
**session→regime coupling is mechanistically sensible** (Asian range → MR dominates; NY → the trend edges), and the
*same* Asian-MR mechanism appears across EURUSD, EURJPY and GBPUSD — cross-symbol coherence a noise process would
not produce. This is genuine orthogonal mechanism, not a denser MR-EUR/USD resampling.

### 4.3 Cost gap — what 2-pip does to each frictionless winner (same config)

| frictionless winner | fric sel/hol pr | REAL sel (PF, pr) | REAL hol (PF, pr) |
|---|---|---|---|
| `bb-EURUSD-ASIA-M15` | 0.60/0.60 | 0.65, 0.00 | 0.73, 0.00 |
| `keltner-EURUSD-ASIA-M15` | 0.80/0.60 | 0.77, 0.00 | 0.81, 0.00 |
| `bb-EURJPY-ASIA-M15` | 0.80/0.70 | 0.87, 0.00 | 0.85, 0.10 |
| `donchian-USDJPY-OVERLAP-H2` | 0.60/0.60 | 1.10, 0.50 | 1.13, 0.70 |
| `keltner-GBPUSD-ASIA-H1` | 0.70/0.60 | 1.11, **0.70** | 1.02, 0.50 |
| `keltner-GBPUSD-OVERLAP-H2` | 0.70/0.60 | 1.25, **0.60** | 1.06, 0.60 |

The M15 mean-reversion edges (the bulk) collapse to **PF < 1.0** at 2-pip — the per-trade edge is smaller than the
spread. Of 20 frictionless survivors, only **2** remain selection-robust at 2-pip (the two GBPUSD keltner-MR rows),
and **both fail the holdout PF gate**. There is no realistic-cost 2-window survivor. The wall is cost.

## 5. Honest verdict

| question | measured answer |
|---|---|
| Is the session axis truly orthogonal (new mechanism, not denser MR-EUR/USD)? | **YES** — new symbols (EURJPY), new regimes (BREAKOUT/TREND non-USDJPY), coherent session→regime coupling |
| New forward-robust diverse family **frictionless**? | **YES — 14 families, 18 variants** the 24h scan cannot see |
| New forward-robust diverse family at **realistic 2-pip cost**? | **NO — 0**, everywhere |
| What is the binding wall? | **COST**, not mechanism (mirror of the data-wall's mechanism verdict). Session edges are thin/high-freq M15-H1; the spread eats them |
| Closest-to-deployable session edge? | `keltner-GBPUSD-MR` ASIA/OVERLAP — sel-robust @2pip, fails holdout; and it is the *known* band-MR family, not new |
| "Not found" vs "search too shallow"? | **Not found at the deployment bar**, well-grounded: 2,448 configs, 2-window gate, realistic **and** frictionless; frictionless ceiling explicitly measured (a mechanism *does* exist, cost kills it) |

**Conclusion.** Expanding the yardstick with a genuinely orthogonal primitive (session/time-of-day) **does** enlarge
the representable *mechanism* space — measurably, for the first time in the 2d line: EURJPY, breakout, and trend
edges that had zero representation over 24h now appear, frictionless, with coherent session structure. But **none of
them clears the honest 2-window CPCV bar at realistic 2-pip cost** — they are thin, high-frequency, and eaten by the
spread. So diversification via the session axis is **real in mechanism, empty at the deployment gate**. The 2d
lesson updates: the one deployable family (MR-EUR/USD) survives because it is **H4/H6 = low-frequency, fat-per-trade**;
the session axis pushes edges to **M15/H1 = high-frequency, thin-per-trade**, straight into the cost floor. The
honest bar rewards *fewer, larger* trades — that, not orthogonality per se, is the real filter.

## 6. Next move (owner decision)

1. **Bank the mechanism finding, keep the flag OFF, don't chase the near-miss.** The `keltner-GBPUSD-MR-ASIA`
   near-miss is one denser param-grid away from a *possible* 2-pip holdout pass — but that pass would be a
   multiple-testing artifact of the same band-MR family, not new diversity. **Do not** lower the cost floor or mine
   it to a pass. The genome B-4 session hook is in place (dormant) for the day a session edge legitimately clears.
2. **Re-aim the next axis at LOW-FREQUENCY / FAT-per-trade, given this run's cost finding.** The session result says
   the binding constraint is now *cost*, so the highest-value next lever is not "more intraday signals" (cross-pair
   intraday would hit the same 2-pip wall) but an orthogonal feature that fires **rarely with conviction**:
   day-of-week / week-of-month seasonality (the `dow_mask` is already wired), event/regime-conditional gates, or a
   swing/position-holding (multi-day) barrier family. Fewer, larger trades are what survive 2-pip.
3. **Cross-pair (axis b) remains open** but is now *lower* priority than (2): it is heavier to implement and, unless
   framed as a low-frequency divergence/relative-value signal, it inherits the same cost wall this run exposed.
4. **Stop re-litigating "does a diverse edge exist frictionlessly."** It does (session axis, measured). The open
   question is now sharply narrower: **is there any orthogonal feature whose diverse edge is fat enough per trade to
   clear 2-pip?** — a *cost-survival* question, not an existence question.

---

### Reproduce
`logs/tribe_2d_session_axis/` (gitignored): `gen_session_grid.py` (612-config/symbol generator + no-session
`identity_check.json`), `run_symbol.sh` (4 scans/symbol: {sel,hol}×{real,fric}), `score_session.py` (2-window
forward join + 24h-sibling load-bearing test + family cluster), `cost_gap.py` (frictionless→realistic fate),
`verdict_{real,fric}.txt`, `cost_gap.txt`, `SESSION_SUMMARY_{real,fric}.json`. Byte-identity: run the pre-session
binary and the new binary on `identity_check.json`, diff (identical SHA above). Scan:
`primitive_scan.exe --data data/historical/<SYM>_M1.csv --manifest logs/tribe_2d_session_axis/grid_<SYM>.json
--out <sel|hol>_<real|fric>.json --slippage <0.0001|0.01|0> [holdout: --oos-start 1420070400 --oos-end 1609459200
--cpcv-start 1420070400 --cpcv-end 1609459200]`. Large grid JSONs kept in scratch (derivable from the generator).
