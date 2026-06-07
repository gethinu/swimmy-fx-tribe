# Edges Bundle — Swimmy Strategy Library Export

Portable export of the **entire** Swimmy strategy library (`data/library/<TIER>/*.lisp`)
for reuse in **`mt5_bundle_of_edges`**, where these candidates are refined / promoted
into tradeable edges.

> These are *candidates*, not edges yet. In-sample/backtest metrics from Swimmy are
> included so you can triage, but **re-validate out-of-sample before trusting any of them.**

Regenerate with:

```bash
python3 tools/export_edges_bundle.py
```

## Contents

| File | What |
|------|------|
| `edges_library.json` | Full structured records + `meta` header (counts, caveats, tier meanings). |
| `edges_index.csv`    | Flat triage table (one row per strategy) for spreadsheets / quick sort. |
| `README.md`          | This file. |

## Inventory (this export)

| Tier | Count | Meaning |
|------|------:|---------|
| LEGEND | 64 | Protected / immortal (never culled). |
| S | 1 | Research-elite (Sharpe≥0.75, PF≥1.70, CPCV pass≥70%). *Currently a test stub.* |
| A | 9 | OOS-validated (Sharpe≥0.45, PF≥1.30, OOS Sharpe≥0.35). Mostly bred + tests. |
| B | 93 | Phase-1 backtest passed (Sharpe≥0.15, PF≥1.05). Working candidate pool. |
| RETIRED | 1 | Archived (low-weight learning data). |
| **Total** | **168** | `likely_test_artifact=true` flags 6 non-genuine entries. |

## Record schema (`edges_library.json` → `strategies[]`)

```jsonc
{
  "name": "Bladerunner",
  "source_tier": "LEGEND",          // folder it came from
  "rank": "LEGEND",                  // strategy's own :RANK field
  "status": "ACTIVE",
  "category": "TREND",
  "symbol": "USDJPY",
  "timeframe_minutes": 10080,
  "timeframe_label": "W1",           // MT5-style label
  "direction": "BOTH",               // BUY | SELL | BOTH
  "indicators": [["EMA", 20]],       // normalized [name, ...params]
  "entry_lisp":  "(AND (> CLOSE EMA-20) (CROSS-ABOVE LOW EMA-20))",  // normalized source
  "entry_ast":   ["AND", [">", "CLOSE", "EMA-20"], ["CROSS-ABOVE", "LOW", "EMA-20"]],
  "entry_pseudo":"(CLOSE > EMA-20 AND LOW crosses above EMA-20)",    // human infix
  "exit_lisp": "...", "exit_ast": [...], "exit_pseudo": "...",
  "sl": 0.2, "tp": 0.4, "volume": 0.01,
  "metrics": { "sharpe": ..., "profit_factor": ..., "win_rate": ...,
               "trades": ..., "max_dd": ..., "oos_sharpe": ...,
               "cpcv_pass_rate": ..., "cpcv_median_*": ... },
  "provenance": { "generation": ..., "breeding_count": ..., "parents": ...,
                  "hash": "...", "immortal": true, "creation_time_iso": "...",
                  "confidence_estimator": "edge_ratio_v1" },
  "likely_test_artifact": false,
  "source_file": "data/library/LEGEND/Bladerunner.lisp"
}
```

Use **`entry_ast` / `exit_ast`** for programmatic translation to MQL5 — it is a clean
operator-prefix tree. Keep `entry_lisp` for fidelity and `entry_pseudo` for review.

## DSL glossary

### Operators (head of an AST node)

| Token | Meaning |
|-------|---------|
| `AND` / `OR` | logical combinator over child clauses |
| `>` `<` `>=` `<=` `=` `/=` | numeric comparison `(op LHS RHS)` |
| `CROSS-ABOVE` / `CROSS-OVER` | LHS crosses up through RHS this bar |
| `CROSS-BELOW` / `CROSS-UNDER` | LHS crosses down through RHS this bar |
| `:RSI-BELOW n` / `:RSI-ABOVE n` | keyword-style RSI threshold clause |
| `:CROSS-OVER :SMA a :SMA b` | keyword-style MA cross (a over b) |

A few legacy entries store the rule as a raw string (e.g. `"CROSS SMA 5 20"`); these
surface in `entry_lisp`/`entry_pseudo` verbatim and need manual interpretation.

### Operands / price & indicator refs

| Token | Meaning |
|-------|---------|
| `CLOSE` `OPEN` `HIGH` `LOW` | current bar price |
| `EMA-n` `SMA-n` | moving average of period n |
| `RSI` `RSI-n` | RSI (period in `indicators`) |
| `MACD-LINE` `SIGNAL-LINE` | MACD components |
| `BB-UPPER` `BB-MIDDLE` `BB-LOWER` | Bollinger Bands |
| `STOCH-K` `STOCH-D` | Stochastic %K / %D |
| `ATR-14` `CCI-14` `PSAR` | volatility / trend indicators |
| `PNL` `TP` `SL` | open-trade PnL vs target/stop (used in bred exits) |

`indicators` lists the configured periods, e.g. `[["MACD", 12, 26, 9]]`,
`[["BB", 20, 2]]`, `[["PSAR", 0.0227, 0.1829]]`.

`sl`/`tp` are in price units (≈ pips×0.01 for JPY pairs in the source system).

## How to consume in `mt5_bundle_of_edges`

1. **Load** `edges_library.json`; filter to what you want to refine (e.g.
   `source_tier in {LEGEND, A, B}`, `likely_test_artifact == false`,
   `metrics.trades >= 100` to drop sparse-sample overfits).
2. **Translate** each `entry_ast` / `exit_ast` recursively into MQL5 conditions
   (a small visitor: comparisons → `>`/`<`, `CROSS-ABOVE` → prev/cur compare,
   indicator refs → `iMA`/`iRSI`/`iBands`/`iStochastic`/`iATR`/`iSAR`/`iCCI`).
3. **Re-backtest** in MT5 Strategy Tester per symbol/timeframe (use
   `timeframe_label`), then keep only what survives OOS + cost + spread.
4. Treat surviving candidates as edges; archive the rest.

## Caveats (read before using)

- **In-sample metrics.** `metrics.*` are Swimmy backtest values. High Sharpe with low
  `trades` (e.g. Sharpe≈18 on 35 trades) is almost certainly overfit — the source
  system deliberately shrinks these by trade-evidence. Sort by `trades` too.
- **Duplicates.** The same strategy can exist in multiple tier folders (e.g. a `Bred-*`
  in both `A/` and `B/`). The bundle keeps one record per *file*; dedupe by
  `provenance.hash` or `name` downstream if needed.
- **Timeframe units.** Most `timeframe_minutes` are minute buckets, but a few legacy
  reversion legends store seconds (e.g. `300` → M5). `timeframe_label` already maps
  the common cases; spot-check oddities.
- **Test artifacts.** `likely_test_artifact=true` marks `TestStrat`, `RECRUIT-RND-*`,
  `TEST-REFRESH-*` etc. Exclude them from edge work.
- **USDJPY-centric.** Most records carry `symbol: USDJPY`; re-fit periods/SL/TP for
  other symbols.
