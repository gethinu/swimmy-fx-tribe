# tribe 2d regen — diversity-seed ESCAPE ROUTE (measured 2026-07-18)

Branch `claude/2d-behavioral-distance`. Flag-gated, default OFF, **NOT merged / NOT deployed**.
Canonical record for the "final resort" of the tribe diversity investigation: seed non-USDJPY-TREND
clones **directly into the population, bypassing the breeding CPCV pre-gate**, and measure whether
the diversity machinery (B-4 distance / B-5 fitness-sharing sort / B-3 niche quota + symbol mutation)
can then break the single-symbol monoculture — **without ever relaxing the honest verification floor.**

Related: [[tribe-2d-regen-b3-niche-quota]] [[tribe-2d-regen-b5-overflow]] [[tribe-2a-daemon-isolation]]
[[tribe-2d-behavioral-distance]] [[tribe-diversity-engine-rebuild]].

---

## 0. Why this was the only remaining escape route

Today's B-3/B-4/B-5 daemon_verify runs proved the diversity machinery is **correct but never
ENGAGES** on the USDJPY-TREND monoculture, because that pool is **never bred**:

- §B-6 CPCV breeding pre-gate (`breeding-cpcv-eligible-p`, cpcv≥0.55) excludes the CPCV-poor mono;
- B-4 clone-block (dist<0.02) forbids USDJPY-TREND × USDJPY-TREND pairs.

So symbol mutation (§B-3(2)) never fires on a TREND parent → no non-USDJPY-TREND child is ever born
→ the TREND pool stays a single (symbol×regime) niche → the niche quota (§B-3(3)) and the
fitness-sharing overflow sort (§B-5) have no 2nd niche to bite on. **The only way to put a
non-USDJPY-TREND member into that pool is a path that does not require breeding.**

## 1. The honest line (owner-specified, strictly held)

Relax **exactly one** thing: the breeding CPCV pre-gate (the ENTRY). A seed enters the population
without first having been a CPCV≥0.55 parent.

Relax **nothing** about the honest verification floor. A seed carries **no fabricated performance
metrics** (cpcv-pass-rate / sharpe / pf / trades left at fresh defaults 0.0; oos-sharpe nil). To reach
rank B / A / diverse-robust it must be honestly backtested (real OOS + CPCV on real **local** price
data) and pass the **same** Phase-1 screening (`*phase1-min-sharpe*`=0.1, pf≥1.0) + §4 live-edge floor
(`*live-edge-guard-min-trades*`=20) as every other strategy. **"Give the entry, never fabricate the
verdict."** Symbols restricted to those with real local M1 data (EURUSD/GBPUSD/EURJPY).

This also **corrects a latent honesty defect in the earlier harness**: the prior A/B-5/B-3 runs seeded
20 EUR/GBP **REVERSION** clones with cpcv=0.7/sharpe=0.7 **pre-set** and `:require-bt nil` — i.e. they
never touched the honest gate and were **cosmetic by construction**. Their "diverse-robust ~20" was an
artifact. This run scores every seed honestly.

## 2. Implementation (flag-gated, OFF byte-identical)

- **`src/lisp/school/school-seed.lisp`** (new, pure module — FFI-free-testable):
  - `*enable-diversity-seed-injection*` (sub-flag, default t; consulted only under the master flag).
  - `*diversity-seed-specs*` — 18 templates, **TREND-first (12/18)**, 3 symbols, pip+ATR barriers,
    plus a REVERSION minority (Keltner/BB-MR — the one prior honest forward-robust region) and a
    BREAKOUT minority (Donchian). Indicators map 1:1 to primitive_scan's honest primitives
    {sma, keltner, bollinger→bb, donchian}. **Not** the earlier EUR/GBP-REVERSION mini-monoculture.
  - `build-diversity-seed` — clones a valid base for a well-formed entry AST, re-points
    symbol/regime/indicator genes, **writes no performance number.**
  - `inject-diversity-seeds` — flag-gated. OFF (master flag nil OR sub-flag nil) ⇒ **NIL (no-op,
    byte-identical legacy)**. ON ⇒ the diverse seed candidate list. Never promotes/ranks/fabricates.
- **`swimmy.asd`** — new file after `school-genome`.
- **`logs/tribe_2c_wsl/daemon_verify.lisp`** — replaced the fabricated-metric seeding with the honest
  path: `inject-diversity-seeds` → `add-to-kb :require-bt t` (real dedup/correlation/graveyard gates
  + Phase-1 deferral) → **score via primitive_scan (guardian's real OOS/CPCV engine)** →
  `handle-v2-result` (real Phase-1 gate) → explicit DB persistence of the honest verdict.
  `db-distro` gained the escape-route metric: **non-USDJPY-TREND survive / robust**.

Master flag `swimmy.core:*enable-primitive-diversity*` config default OFF (config.lisp:124) **unchanged**.

## 3. Offline test (hermetic, FFI-free)

`tests/diversity_seed_offline_test.lisp` — `sbcl --script`, loads only pure school-genome + school-seed
against a minimal struct. **20/20 PASS** (native SBCL 2.6.4). Asserts: flag OFF ⇒ no seeds
(byte-identical); flag ON ⇒ 18 seeds, all non-USDJPY, 3 symbols, 3 regimes, **TREND majority (12/18)**;
**no fabricated perf** (cpcv=0/sharpe=0/pf=0/trades=0/oos=nil/rank=nil — every seed below the 0.55
pre-gate and 0.6 robust bar, so it MUST earn the verdict); TREND seeds are SMA (honestly scored as
crossovers); B-4 distance sees a EURUSD-TREND seed as a non-clone (0.4952 vs 0.02 floor; 0.0577 for a
same-symbol period tweak) and is blind flag-OFF.

## 4. Measurement — 6-generation daemon_verify (copy DB, redirected library, WSL Ubuntu-22.04)

Isolation: copy of `swimmy.db` at `/mnt/c/tmp` (live DB never opened), library WRITE path redirected to
scratch, flag ON (harness only), 2f pre-gate bootstrap 0.55. Scoring = prebuilt `primitive_scan.exe`
(guardian's engine) over real local M1. **honest floor untouched.** 0 repo pollution.

### Seed honest-gate result (the crux)

| Phase-1 screen (Sharpe≥0.1 ∧ PF≥1.0) | count | of which non-USDJPY-TREND |
|---|---|---|
| **PASS** | 10/18 | **6** (GBPUSD sma20/40/50/60, EURUSD sma50/60) |
| graveyard | 8/18 | short-period TREND, both EURJPY, both Donchian |

**But the CPCV-robust bar (cpcv≥0.6) is decisive:**
- **non-USDJPY-TREND robust = 0 in EVERY generation. Max cpcv among them = 0.20.**
- The only honestly-robust diverse members are **EURUSD REVERSION** (Keltner/BB-MR): diverse-robust 2→3.

### Per-generation distribution

| Gen | active | USDJPY | USDJPY% | TREND% | diverse-robust | non-USD-TREND surv/robust |
|-----|--------|--------|---------|--------|----------------|---------------------------|
| GEN0  | 488 | 486 | 99.6% | 100.0% | 0 | 2 / 0 |
| GEN0b | 498 | 486 | 97.6% | 99.2%  | 2 | 8 / 0 |
| GEN1  | 71  | 62  | 87.3% | 93.0%  | 2 | 4 / 0 |
| GEN2  | 71  | 62  | 87.3% | 93.0%  | 2 | 4 / 0 |
| GEN3  | 72  | 62  | 86.1% | 91.7%  | 2 | 4 / 0 |
| GEN4  | 72  | 62  | 86.1% | 91.7%  | 2 | 4 / 0 |
| GEN5  | 73  | 62  | 84.9% | 90.4%  | 2 | 4 / 0 |
| GEN6  | 74  | 62  | 83.8% | 89.2%  | 3 | 4 / 0 |

Final GEN6 non-USDJPY members (12): 8 REVERSION (4 surviving DVSEED seeds cpcv 0.5–0.6 + **4 bred
children** `Bred-DVSEE-*-Gen1` cpcv 0.5–0.6 — the seeds actually **bred** honest-passing offspring),
4 TREND (DVSEED EURUSD/GBPUSD sma50/60, cpcv 0.1–0.2).

### 4-way comparison (GEN6 USDJPY%)

| run | GEN6 USDJPY% | diverse-robust | notes |
|-----|--------------|----------------|-------|
| **A** (2a) | 72.9% | ~21 **(cosmetic)** | 20 fabricated REVERSION seeds inflated the denominator |
| **B-5** | 75.6% | 20 **(cosmetic)** | sort-key alone insufficient |
| **B-3** | 75.6% | 20 **(cosmetic)** | niche quota no-op (TREND pool 62/62 USDJPY) |
| **seed (this)** | **83.8%** | **3 (HONEST)** | honest scoring removes the cosmetic diversity |

## 5. Verdict — did it break the majority? Real or cosmetic?

**Majority NOT broken. USDJPY% = 83.8% at GEN6 — the WORST of the four runs.**

**Why it looks worse than the prior runs (and why that is the honest result):** the USDJPY *count* is
invariant at **62** in every run — that is the §4 honest-floor equilibrium for this monoculture
(486→62 driven by the §4 floor + fitness-sharing cull, NOT by 2c/2d/seed). What varies is the
non-USDJPY *denominator*. The prior runs propped it up with ~20 **fabricated** REVERSION seeds; with
those scored honestly they largely vanish, so USDJPY% rises. **The prior "dilution to 72.9–75.6%" was
partly a cosmetic-seed artifact.**

**Real vs cosmetic — split by regime:**
- **non-USDJPY-TREND (the escape-route target): COSMETIC AT THE ROBUST BAR.** 6 seeds pass the *weak*
  Phase-1 screen and 4 persist to GEN6, so they are not instantly dead — but **0 ever reach
  CPCV-robust (max cpcv 0.20)** and none reach the 0.55 breeding pre-gate, so they never breed and
  never certify. **The heavy conclusion stands: honest has no forward-robust non-USDJPY-TREND edge**
  — SMA-crossover trend on EUR/GBP/EURJPY clears only in-sample screening, not out-of-sample
  robustness. (Consistent with the rebuild finding: per-symbol SMA CPCV-robust = 0 on all symbols.)
- **non-USDJPY-REVERSION (EURUSD Keltner/BB-MR): GENUINELY REAL, but small.** 2 seeds hit cpcv 0.6 and
  **bred 4 diverse children** (1 robust) — diverse-robust 3. This is the only honest diversification,
  and it was already known; it plateaus far below the 62-USDJPY core.

**The escape route mechanically works** (it injects non-USDJPY-TREND members into the monoculture's own
pool bypassing the breeding pre-gate; the niche quota protects the sparse off-niche — surv=4 stable;
seeds compose with B-4/B-5/B-3 and breed). **But it cannot break the monoculture**, because the honest
floor correctly refuses to certify the non-robust non-USDJPY-TREND edges, and the robust USDJPY core
sits at its §4-floor equilibrium.

## 6. Residual & deploy decision

- **Deploy: NO (owner-gated).** Flag stays default OFF. Shipping the seed path would not break the
  monoculture and would, if anything, *reduce* apparent diversity vs the cosmetic prior runs. There is
  no honest non-USDJPY-TREND edge to seed toward; the only honest diverse edge (EURUSD MR) is already
  reachable and thin.
- **What ships safely (code):** the seed path is reversible, honest-floor-neutral, flag-OFF
  byte-identical, and offline-proven. It is a correct *mechanism*; it is simply not *justified* as a
  monoculture-breaker by this measurement.
- **The real bottleneck is unchanged and is NOT a mechanism gap:** it is the **absence of honest
  forward-robust non-USDJPY-TREND edges** in the available primitives/data. No entry-widening fixes
  that — only genuinely new, honestly-robust diverse edges would.

Guardrails honored: verification via redirected `daemon_verify.lisp` only (no repo `run-all-tests`);
live `swimmy.db` never opened (copy only); all flag-gated, OFF byte-identical; flag NOT flipped;
unrelated dirty (LEGEND/guardian) untouched.
