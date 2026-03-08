# MT5 Latest Validation Closeout Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extend the frozen MT5 validation set with a fresh 2026 latest-window run for the shortlisted survivors and fold the result into the knowledge docs.

**Architecture:** Reuse `tools/mt5_inventory_tester.py` and the portable MT5 tester setup that already produced the 2026-03-07 baseline and followup artifacts. First run a single-job probe to confirm 2026 date coverage, then run the shortlisted jobs over the same latest window, summarize the resulting JSON metrics, and update the inventory/followup notes.

**Tech Stack:** `python3`, `tools/mt5_inventory_tester.py`, MT5 Strategy Tester, Markdown docs

---

### Task 1: Probe 2026 date coverage

**Files:**
- Use: `tools/mt5_inventory_tester.py`
- Output: `data/reports/mt5/inventory_tester/<new-run-id>/legend-macd-above-zero-cross/`

**Step 1: Run a single-job latest-window probe**

Run:

```bash
python3 tools/mt5_inventory_tester.py \
  --job-set all \
  --job legend-macd-above-zero-cross \
  --from-date 2026.01.01 \
  --to-date 2026.03.07 \
  --timeout-sec 1800
```

**Step 2: Confirm report generation**

Verify that the run directory contains a summary JSON and HTML report for `legend-macd-above-zero-cross`.

### Task 2: Run fresh validation for shortlisted survivors

**Files:**
- Use: `tools/mt5_inventory_tester.py`
- Output: `data/reports/mt5/inventory_tester/<new-run-id>/legend-macd-above-zero-cross/`
- Output: `data/reports/mt5/inventory_tester/<new-run-id>/legend-pullback-breakout/`
- Output: `data/reports/mt5/inventory_tester/<new-run-id>/historical-s-bred940-trend-core/`

**Step 1: Execute the latest-window validation set**

Run:

```bash
python3 tools/mt5_inventory_tester.py \
  --job-set all \
  --job legend-macd-above-zero-cross \
  --job legend-pullback-breakout \
  --job historical-s-bred940-trend-core \
  --from-date 2026.01.01 \
  --to-date 2026.03.07 \
  --timeout-sec 1800
```

**Step 2: Extract the decision metrics**

Read each summary JSON and capture:
- `total_net_profit`
- `profit_factor`
- `total_trades`
- `sharpe_ratio`

### Task 3: Publish the closeout note

**Files:**
- Modify: `doc/knowledge/legend_mt5_followup_validation_20260307.md`
- Modify: `doc/knowledge/legend_s_rank_inventory_20260307.md`

**Step 1: Add the latest-window section**

Document:
- exact date window `2026.01.01` to `2026.03.07`
- the three shortlisted jobs
- which candidate remains the cleanest stability-first survivor
- whether `legend-pullback-breakout` stayed regime-dependent
- whether `historical-s-bred940-trend-core` recovered or stayed unstable

**Step 2: Tighten the remaining-risk language**

After adding the fresh run, limit the unresolved items to:
- full walk-forward optimization
- parameter retune experiments
- broker variance
- repo cleanup decisions

### Task 4: Verify

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

**Step 2: Report the fresh evidence**

Include:
- new run id(s)
- key metrics per shortlisted job
- any blocker if 2026 history is unavailable
