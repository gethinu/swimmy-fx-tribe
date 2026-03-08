# Legend Full Walk-Forward Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Run a minimum 3-fold MT5 walk-forward optimization for the three surviving Legend/Historical-S candidates and publish the fold-level evidence into the knowledge docs.

**Architecture:** Reuse the existing portable MT5 tester root and compiled `.ex5` artifacts. For each shortlisted job, generate a dedicated optimization `.set` file with narrow parameter ranges, generate a tester `.ini` with `Optimization=2` and `ForwardMode=2`, run three sequential folds, then parse the resulting `.xml` and `.forward.xml` reports into repo-side summary JSON so the docs can cite stable artifacts.

**Tech Stack:** `python3`, `tools/mt5_inventory_tester.py` helpers, `tools/mt5_ih_promotion_gate.py` XML parser, portable MT5 Strategy Tester, Markdown docs

---

### Task 1: Fix the walk-forward contract

**Files:**
- Read: `doc/GPT要件定義.txt`
- Read: `doc/knowledge/legend_mt5_followup_validation_20260307.md`
- Read: `doc/knowledge/legend_s_rank_inventory_20260307.md`
- Output: `data/reports/mt5/legend_walkforward/<run-id>/manifest.json`

**Step 1: Keep the minimum fold requirement explicit**

Use at least `3` folds because `doc/GPT要件定義.txt` requires `Walk-forward 最低 3 fold（rolling window）`.

**Step 2: Use a common split rule**

Use MT5 built-in forward optimization with:

```ini
Optimization=2
ForwardMode=2
Model=4
```

Treat `ForwardMode=2` as the same `Forward=1/3` contract already used by the InstitutionalHunter runs in `implementation_plan_v50.7.md`.

**Step 3: Keep the candidate set frozen**

Run only:

- `legend-macd-above-zero-cross`
- `legend-pullback-breakout`
- `historical-s-bred940-trend-core`

### Task 2: Generate optimization inputs

**Files:**
- Read: `src/mt5/legend_batch2/Legend_MACDAboveZeroCross.mq5`
- Read: `src/mt5/legend_batch1/Legend_PullbackBreakout.mq5`
- Read: `src/mt5/historical_s_batch3/HistS_Bred940TrendCore.mq5`
- Read: `/mnt/c/Users/stair/AppData/Local/SwimmyMT5Portable/inventory_tester/MQL5/Profiles/Tester/Legend_MACDAboveZeroCross.set`
- Read: `/mnt/c/Users/stair/AppData/Local/SwimmyMT5Portable/inventory_tester/MQL5/Profiles/Tester/Legend_PullbackBreakout.set`
- Read: `/mnt/c/Users/stair/AppData/Local/SwimmyMT5Portable/inventory_tester/MQL5/Profiles/Tester/HistS_Bred940TrendCore.set`
- Output: `data/reports/mt5/legend_walkforward/<run-id>/generated_inputs/`

**Step 1: Narrow the optimization search**

For each job, optimize only the parameters that plausibly move edge or risk:

- `legend-macd-above-zero-cross`: `InpStopLossPrice`, `InpTakeProfitPrice`, `InpFastEMA`, `InpSlowEMA`, `InpSignalPeriod`
- `legend-pullback-breakout`: `InpStopLossPrice`, `InpTakeProfitPrice`, `InpEMAPeriod`, `InpLongRSIMin`, `InpShortRSIMax`
- `historical-s-bred940-trend-core`: `InpStopLossPrice`, `InpTakeProfitPrice`, `InpFastMAPeriod`, `InpSlowMAPeriod`, `InpLongRSIMin`, `InpShortRSIMax`

**Step 2: Keep ranges narrow and human-auditable**

Do not use the raw auto-generated max values from the stock `.set` files when they are obviously unrealistic. Generate fresh `.set` files with small bounded ranges around the current defaults and store copies under the repo artifact directory.

### Task 3: Execute three folds per survivor

**Files:**
- Use: portable MT5 root `C:\Users\stair\AppData\Local\SwimmyMT5Portable\inventory_tester`
- Output: `data/reports/mt5/legend_walkforward/<run-id>/<job>/<fold>/`

**Step 1: Use three sequential total windows**

Run these windows:

1. `2024.01.01` to `2024.12.31`
2. `2024.07.01` to `2025.06.30`
3. `2025.03.01` to `2026.03.07`

Each run uses MT5 built-in `ForwardMode=2`, so the final third of each total window is the OOS segment.

**Step 2: Persist every config**

For every `(job, fold)` store:

- generated `.set`
- generated `.ini`
- raw `.xml`
- raw `.forward.xml`

### Task 4: Summarize fold outcomes

**Files:**
- Read: `data/reports/mt5/legend_walkforward/<run-id>/<job>/<fold>/*.forward.xml`
- Output: `data/reports/mt5/legend_walkforward/<run-id>/<job>/walkforward_summary.json`

**Step 1: Parse all forward candidates**

Use `tools/mt5_ih_promotion_gate.py:load_rows_from_mt5_xml` or equivalent XML parsing.

**Step 2: Select one candidate per fold**

Use a simple fail-closed filter:

- `Trades >= 5`
- `Profit Factor >= 1.0`
- `Equity DD % <= 20.0`
- `Sharpe Ratio > 0`

If no candidate survives a fold, record the fold as failed and keep the best available row separately for diagnostics.

**Step 3: Publish aggregate counts**

For each job, report:

- surviving folds
- total forward net profit
- median forward profit
- mean forward PF
- total forward trades

### Task 5: Update the knowledge docs

**Files:**
- Modify: `doc/knowledge/legend_mt5_followup_validation_20260307.md`
- Modify: `doc/knowledge/legend_s_rank_inventory_20260307.md`

**Step 1: Add a full walk-forward section**

Document:

- the exact three total windows
- the `ForwardMode=2` / `Forward=1/3` contract
- the limited optimization ranges
- fold-level outcomes per survivor

**Step 2: Re-rank the shortlist**

State whether:

- `legend-macd-above-zero-cross` stays the stability-first leader
- `legend-pullback-breakout` remains regime-dependent
- `historical-s-bred940-trend-core` improves enough to challenge the lead

**Step 3: Tighten the open-risk list**

After the WFO section, leave only unresolved items that still lack evidence:

- broker variance with a true second-source snapshot
- optional parameter retune beyond the narrow ranges
- repo cleanup boundary decisions

### Task 6: Verify

**Files:**
- Verify: `doc/knowledge/legend_mt5_followup_validation_20260307.md`
- Verify: `doc/knowledge/legend_s_rank_inventory_20260307.md`
- Verify: `tools/tests/test_mt5_inventory_tester.py`
- Verify: `tools/tests/test_legend_mq5_docs.py`
- Verify: `tools/tests/test_historical_s_mq5_docs.py`

**Step 1: Run the doc/test suite**

Run:

```bash
python3 -m unittest \
  tools.tests.test_mt5_inventory_tester \
  tools.tests.test_legend_mq5_docs \
  tools.tests.test_historical_s_mq5_docs \
  -v
```

**Step 2: Report fresh evidence only**

Include:

- final walk-forward run id
- per-job fold pass counts
- key forward metrics
- any fold that timed out, produced no forward candidates, or stayed sample-thin
