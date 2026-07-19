# Swimmy as a Strategy Factory (蠱毒 / Gu-poison)

> The value of this repository is **not** any single strategy. Every individual
> strategy here has been correctly rejected for live use (the best headline
> result, EURUSD +5.9%/mo, only exists at a ruinous 45% drawdown — i.e. ~1.6%/mo
> at the 12% drawdown budget, below the 3% KPI). The value is the **machine that
> keeps producing and culling candidates**: the 蠱毒 jar. This document reframes
> the repo around that asset and defines how its output is consumed.

## 1. The thesis

Two repos, complementary strengths:

| | bundle-of-edge | swimmy-fx-tribe (this repo) |
|---|---|---|
| Strength | Curated, hand-verified edges | A factory that *continuously generates* strategies |
| Weakness | Finite; runs dry; no generation | Generates faster than it validates |

The 蠱毒 (Gu-poison) metaphor: throw many venomous creatures in one jar, let them
fight, the survivor is the strongest. Swimmy throws many strategies into a
breeding + culling loop and keeps the survivors. **But "survived the jar" ≠
"verified edge."** A survivor that never left in-sample data is a plausible
hypothesis, not a deployable edge. The factory's job is therefore not just to
*generate* but to hand the consumer an **honestly-labelled** feed.

## 2. The pipeline

```
            ┌──────────────────────── this repo (Lisp / SBCL) ────────────────────────┐
generate -> │  school-evolution / school-breeder / school-genome   (breed candidates) │
            │  school-rank-system / school-voting / school-scoring (rank)             │
cull     -> │  school-validation / school-monte-carlo / cemetery   (kill the weak)    │
            │  data/library/{S,A,B,LEGEND}/*.lisp                  (survivors)         │
            └──────────────────────────────────┬──────────────────────────────────────┘
                                                │
                          tools/tribe/export.py │  (honest gate + ranking)
                                                v
                                   feed/strategies_feed.json   <-- machine-readable
                                   feed/CATALOG.md              <-- human-readable
                                                │
                        ┌───────────────────────┴───────────────────────┐
                        v                                                v
              bundle-of-edge (external                    guardian/  (vendored arena,
              downstream consumer)                        Rust; OOS/CPCV backtest + exec)
```

### The arena is vendored at `guardian/`
The Rust evaluation/execution engine (**guardian**) is now **vendored into this
repo** at [`guardian/`](../guardian/) via `git subtree` (full history preserved),
built as one Cargo workspace (root [`Cargo.toml`](../Cargo.toml) →
`members = ["guardian"]`). Brain (generation + selection + survivors) and the
arena therefore move together, so the ZMQ S-expr contract in
[`docs/llm/INTERFACES.md`](../docs/llm/INTERFACES.md) can change atomically on
both sides. `bundle-of-edge` stays a **separate** downstream consumer; only
guardian is vendored. Consequences:

- `data/historical/` and `data/reports/` are still git-ignored — they live on the
  host (SSD); vendoring the *engine* does not vendor the *data*. See
  [`doc/OPERATIONS.md`](OPERATIONS.md) for storage layout + the subtree runbook.
- Most survivors here still carry `OOS-SHARPE 0.0` / `CPCV-* 0.0`: the arena has
  not (yet) re-scored them out-of-sample. The feed marks these **UNVALIDATED**.
  Running the vendored guardian over the PROVISIONAL set is what produces the real
  OOS/CPCV numbers that would promote strategies into PASS.

## 3. The feed contract (`feed/`)

Regenerate any time the library changes:

```bash
python3 -m tools.tribe.export                 # -> feed/strategies_feed.json + feed/CATALOG.md
```

`strategies_feed.json` (`schema: swimmy.tribe.feed/v1`):

```jsonc
{
  "schema": "swimmy.tribe.feed/v1",
  "generated_utc": "...",
  "gate_config": { "dd_budget_pct": 12, "kpi_monthly_pct": 3, "min_trades": 200, ... },
  "counts": { "total": N, "PASS": x, "PROVISIONAL": y, "REJECT": z },
  "strategies": [
    {
      "name": "...", "library_rank": "B|A|S|LEGEND",
      "symbol": "...", "timeframe": 30, "category": ":TREND",
      "metrics": { "profit_factor": ..., "sharpe": ..., "trades": ..., "oos_sharpe": ... },
      "logic": { "indicators": "(...)", "entry": "(...)", "exit": "(...)" },
      "gate": { "verdict": "PASS|PROVISIONAL|REJECT", "score": ..., "checks": [...], "normalized": {...} }
    }
  ]
}
```

### Verdict semantics (the honest gate)
Defined and unit-tested in `tools/tribe/honest_gate.py`:

- **PASS** — clears sample-size + PF + Sharpe floors **and** has OOS/CPCV
  validation. A genuine deployable candidate. A consumer may ingest directly.
- **PROVISIONAL** — clears the hard floors but is **IS-only / thin**. A
  hypothesis worth forward-testing; **never** route to live on this label alone.
- **REJECT** — fails a hard floor: under-sampled (`trades < min`), `PF < floor`,
  or — for headline-return candidates — fails the **drawdown-budget** check
  (the return only exists at a drawdown above budget).

### Risk normalisation (why headline returns are not trusted)
Return and drawdown both scale with position size. A candidate is scored at the
firm's **drawdown budget (12%)**, not at whatever leverage produced the headline:

```
budgeted_monthly = headline_monthly × (dd_budget / dd_observed)
```

Example — the EURUSD optimisation: `5.9136%/mo @ DD 45.28%` → `1.57%/mo @ DD 12%`
→ below the 3% KPI → **REJECT**. Run it yourself:

```bash
python3 -m tools.tribe.honest_gate --monthly 5.9136 --dd 45.28 --trades 21 --forward-trades 21 --pf 2.06
```

## 4. Current state of the feed (snapshot)

As of the latest export of `data/library/` (168 survivors):

- **PASS: 0** — *nothing here has cleared out-of-sample validation.*
- **PROVISIONAL: 61** — IS-only survivors with PF ≈ 1.1–1.3, Sharpe ≈ 0.3, mostly USDJPY.
- **REJECT: 107** — under-sampled or PF below floor (incl. the `S/TestStrat` placeholder).

The actionable read for bundle-of-edge: **do not consume PROVISIONAL as edge.**
The next high-value work is to run the vendored guardian arena (`guardian/`) over
the PROVISIONAL set to produce real OOS/CPCV numbers, which is what would move
strategies into PASS. Until then the factory's honest output is "61 hypotheses,
0 verified."

> **Note (2026-07-19, hygiene audit) — the "PASS" name collides; this snapshot is stale, 要再測定.**
> Two things about the counts above:
> 1. **Disambiguate "PASS".** This section's **PASS** is the *strict OOS/CPCV-verified* gate
>    (`honest_gate.py`, still **0**). The `KILL_CRITERIA_20260703.md §4` go/no-go's
>    **"honest PASS = 60"** is a *different axis* — the **hard-floor continue-trigger** — which
>    corresponds to this section's **PROVISIONAL (≈61)**, *not* to PASS. Mapped correctly the two
>    numbers **agree** (hard-floor pass ≈ 60–61; OOS/CPCV-verified = 0); the apparent conflict is a
>    name collision, not a contradiction.
> 2. **The 168 / 0 / 61 / 107 snapshot is undated and now stale, and cannot be cleanly re-measured
>    right now.** `data/library/` currently carries **~320 untracked `RECRUIT-RND-*` daemon-churn
>    files** (uniform mtime 2026-07-18, not committed) that `os.walk`-based `python3 -m tools.tribe.export`
>    would count, inflating every total; the `--source sql` mode instead reads the live `swimmy.db`
>    (off-limits this pass). **Refresh these counts by re-running the export only after the library
>    churn is cleaned** (see the hygiene note in the tribe-2d rollup).

## 5. Files

| Path | Role |
|---|---|
| `tools/tribe/sexpr.py` | Minimal reader for `#S(STRATEGY …)` library files (no eval) |
| `tools/tribe/honest_gate.py` | Promotion gate + risk normalisation (importable + CLI) |
| `tools/tribe/export.py` | Library → JSON + Markdown feed |
| `tools/tribe/tests/test_tribe.py` | Unit tests for parser + gate |
| `feed/strategies_feed.json` | Machine-readable feed (consumer entrypoint) |
| `feed/CATALOG.md` | Human-readable catalogue |
