# Legend MQ5 Batch 3 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add the third incremental batch of Japanese strategy writeups and standalone MQ5 ports for one core LEGEND trend strategy and the two library-only external Legend candidates.

**Architecture:** Reuse the batch-1/2 EA template, keep operator-facing natural-language documentation in Japanese, and treat current library/DB rows as the primary source for the external Legends. Where older source files disagree with current library rows, note the divergence explicitly in the catalog and choose a practical MQ5 interpretation.

**Tech Stack:** Markdown, SQLite-backed strategy metadata, MetaTrader 5 / MQL5, Python `unittest`.

---

### Task 1: Failing verification test

**Files:**
- Modify: `tools/tests/test_legend_mq5_docs.py`

**Step 1: Require batch-3 doc**

Assert that `doc/knowledge/legend_mq5_batch3_20260307.md` exists and contains:

- `自然言語版`
- `MACD-Signal-Cross`
- `Legend-London-Breakout-V1`
- `Legend-RSI-Reversion-V1`

**Step 2: Require batch-3 MQ5 files**

Assert that these files exist and contain their strategy markers:

- `src/mt5/legend_batch3/Legend_MACDSignalCross.mq5`
- `src/mt5/legend_batch3/Legend_LondonBreakoutV1.mq5`
- `src/mt5/legend_batch3/Legend_RSIReversionV1.mq5`

**Step 3: Run test and confirm failure**

Run:

```bash
python3 -m unittest tools.tests.test_legend_mq5_docs -v
```

Expected: batch-3 assertions fail before files are created.

### Task 2: Japanese batch-3 catalog

**Files:**
- Create: `doc/knowledge/legend_mq5_batch3_20260307.md`

**Step 1: Add Japanese natural-language writeups**

Strategies:

- `MACD-Signal-Cross`
- `Legend-London-Breakout-V1`
- `Legend-RSI-Reversion-V1`

**Step 2: Record exact metadata**

For each strategy, include:

- indicators
- entry
- exit
- symbol
- timeframe
- SL / TP
- current metrics

**Step 3: Note source divergence**

For the two external Legends, document that:

- current library/DB rows are treated as the operational source
- older Lisp source files disagree on timeframe or exit semantics

### Task 3: MQ5 ports

**Files:**
- Create: `src/mt5/legend_batch3/Legend_MACDSignalCross.mq5`
- Create: `src/mt5/legend_batch3/Legend_LondonBreakoutV1.mq5`
- Create: `src/mt5/legend_batch3/Legend_RSIReversionV1.mq5`

**Step 1: Implement MACD signal cross**

Use closed-bar MACD main/signal crossover logic.

**Step 2: Implement London breakout**

Use a daily session-range capture, breakout-after-range logic, and time-based exit.

**Step 3: Implement RSI reversion**

Use RSI(2) threshold entry/exit with mirrored short inference.

### Task 4: Verify green

**Files:**
- Verify: `tools/tests/test_legend_mq5_docs.py`

**Step 1: Run verification**

Run:

```bash
python3 -m unittest tools.tests.test_legend_mq5_docs -v
```

Expected: all tests pass.
