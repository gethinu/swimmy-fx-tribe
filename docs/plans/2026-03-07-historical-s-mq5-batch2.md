# Historical S MQ5 Batch 2 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add the second small batch of historical S-rank candidates, focusing on three bred variants that share the same signal core but differ in risk envelope and attached indicator debris.

**Architecture:** Reuse the historical batch-1 trend-core EA shape, treat `data_sexp` entry/exit as the executable truth, and document that the three candidates are not three independent ideas but three execution variants of the same bred trend core. Keep the operator-facing catalog in Japanese.

**Tech Stack:** Markdown, SQLite-backed strategy metadata, MetaTrader 5 / MQL5, Python `unittest`.

---

### Task 1: Failing verification test

**Files:**
- Modify: `tools/tests/test_historical_s_mq5_docs.py`

**Step 1: Require batch-2 doc**

Assert that:

- `doc/knowledge/historical_s_mq5_batch2_20260307.md` exists
- it contains `自然言語版`
- it contains `同一シグナルコア`
- it contains the three strategy names

**Step 2: Require batch-2 MQ5 files**

Assert that:

- `src/mt5/historical_s_batch2/HistS_Bred508TrendCore.mq5`
- `src/mt5/historical_s_batch2/HistS_Bred794TrendCore.mq5`
- `src/mt5/historical_s_batch2/HistS_Bred128TrendCore.mq5`

exist and contain their markers.

**Step 3: Run the test to confirm failure**

Run:

```bash
python3 -m unittest tools.tests.test_historical_s_mq5_docs -v
```

Expected: batch-2 assertions fail before files are created.

### Task 2: Japanese extraction catalog

**Files:**
- Create: `doc/knowledge/historical_s_mq5_batch2_20260307.md`

**Step 1: Document the shared signal core**

Explain that all three candidates use:

- long: `close > sma20 > sma50 and rsi > 55`
- short: mirrored bearish form
- exit: effective broker-side risk exit

**Step 2: Document per-strategy differences**

Include:

- SL / TP
- current metrics
- attached indicators that no longer match the entry expression

**Step 3: Emphasize that this is one family, not three distinct ideas**

### Task 3: MQ5 ports

**Files:**
- Create: `src/mt5/historical_s_batch2/HistS_Bred508TrendCore.mq5`
- Create: `src/mt5/historical_s_batch2/HistS_Bred794TrendCore.mq5`
- Create: `src/mt5/historical_s_batch2/HistS_Bred128TrendCore.mq5`

**Step 1: Reuse batch-1 bred template**

Each file should:

- use `PERIOD_CURRENT`
- expose candidate-specific SL / TP
- preserve the original strategy name in the source

**Step 2: Add candidate-specific notes**

- `508`: strongest PF/OOS profile
- `794`: PSAR appears in stored indicators but is not used by entry expression
- `128`: wide-stop / shallow-target skew

### Task 4: Verify green

**Files:**
- Verify: `tools/tests/test_historical_s_mq5_docs.py`

**Step 1: Run verification**

Run:

```bash
python3 -m unittest tools.tests.test_historical_s_mq5_docs -v
```

Expected: all tests pass.
