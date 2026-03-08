# Legend MQ5 Batch 2 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add the second incremental batch of Japanese strategy writeups and standalone MQ5 ports from the LEGEND inventory.

**Architecture:** Reuse the batch-1 EA structure, source logic from current DB `data_sexp`, and keep the documentation in Japanese for the operator-facing natural-language layer. Limit batch-2 to three clean strategies before touching the library-only Legends or the historical S-rank recovery set.

**Tech Stack:** Markdown, SQLite-backed strategy metadata, MetaTrader 5 / MQL5, Python `unittest`.

---

### Task 1: Failing verification test

**Files:**
- Create: `tools/tests/test_legend_mq5_docs.py`

**Step 1: Assert batch-1 Japanese wording exists**

Require:

- `自然言語版`
- `市場アイデア`

**Step 2: Assert batch-2 files exist**

Require:

- `doc/knowledge/legend_mq5_batch2_20260307.md`
- `src/mt5/legend_batch2/Legend_TrendPullbackEntry.mq5`
- `src/mt5/legend_batch2/Legend_SweetChariotSMA40.mq5`
- `src/mt5/legend_batch2/Legend_MACDAboveZeroCross.mq5`

**Step 3: Run test and confirm failure**

Run:

```bash
python3 -m unittest tools.tests.test_legend_mq5_docs -v
```

Expected: fail because batch-1 is still English and batch-2 artifacts are missing.

### Task 2: Japanese operator catalog

**Files:**
- Modify: `doc/knowledge/legend_mq5_batch1_20260307.md`
- Create: `doc/knowledge/legend_mq5_batch2_20260307.md`

**Step 1: Translate batch-1 natural-language sections into Japanese**

**Step 2: Add batch-2 strategy entries**

Strategies:

- `Trend-Pullback-Entry`
- `Sweet-Chariot-SMA-40`
- `MACD-Above-Zero-Cross`

**Step 3: Record exact logic**

For each:

- indicators
- entry
- exit
- timeframe
- SL / TP
- metrics
- mirrored short inference

### Task 3: MQ5 ports

**Files:**
- Create: `src/mt5/legend_batch2/Legend_TrendPullbackEntry.mq5`
- Create: `src/mt5/legend_batch2/Legend_SweetChariotSMA40.mq5`
- Create: `src/mt5/legend_batch2/Legend_MACDAboveZeroCross.mq5`

**Step 1: Reuse batch-1 EA pattern**

Each file includes:

- `CTrade`
- new-bar gate
- `magic`-scoped single-position management
- indicator handles
- broker-side SL / TP

**Step 2: Implement closed-bar logic**

Map:

- `Trend-Pullback-Entry`: trend filter + RSI pullback
- `Sweet-Chariot-SMA-40`: close/SMA cross
- `MACD-Above-Zero-Cross`: MACD main/signal cross above zero

**Step 3: Verify green**

Run:

```bash
python3 -m unittest tools.tests.test_legend_mq5_docs -v
```

Expected: all tests pass.
