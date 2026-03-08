# Historical S MQ5 Batch 1 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extract the first small batch of historical S-rank candidates from DB `data_sexp`, document them in Japanese, and create corresponding MQ5 ports.

**Architecture:** Treat `data_sexp` as the executable truth for each candidate, but explicitly document when stored `indicators` diverge from the `entry` / `exit` expression. Use one representative high-Sharpe bred trend candidate, one bred variant with stronger historical validation shape, and one simpler recruit cross strategy.

**Tech Stack:** Markdown, SQLite-backed strategy metadata, MetaTrader 5 / MQL5, Python `unittest`.

---

### Task 1: Failing verification test

**Files:**
- Create: `tools/tests/test_historical_s_mq5_docs.py`

**Step 1: Require the Japanese catalog**

Assert that:

- `doc/knowledge/historical_s_mq5_batch1_20260307.md` exists
- it contains `自然言語版`
- it contains the three strategy names
- it contains `実行正本`

**Step 2: Require the MQ5 files**

Assert that these exist:

- `src/mt5/historical_s_batch1/HistS_Bred222TrendCore.mq5`
- `src/mt5/historical_s_batch1/HistS_Bred723TrendCore.mq5`
- `src/mt5/historical_s_batch1/HistS_RecruitRndTrendCross.mq5`

**Step 3: Run the test to confirm failure**

Run:

```bash
python3 -m unittest tools.tests.test_historical_s_mq5_docs -v
```

Expected: fail because batch-1 artifacts do not exist yet.

### Task 2: Japanese extraction catalog

**Files:**
- Create: `doc/knowledge/historical_s_mq5_batch1_20260307.md`

**Step 1: Document the extracted candidates**

Strategies:

- `Bred-Bred--222-Gen30-N3980040329-718`
- `Bred-Bred--723-Gen29-N3980038311-278`
- `RECRUIT-RND-1768781166-12`

**Step 2: Explain source ambiguity**

For bred strategies, record that:

- stored `indicators` do not match the actual `entry` rule
- MQ5 translation follows the `entry` / `exit` expression as the executable truth
- original timeframe `3600` is not a standard MT5 timeframe

**Step 3: Add operator-friendly Japanese descriptions**

For each strategy include:

- symbol
- timeframe
- entry / exit semantics
- SL / TP
- key metrics
- implementation note

### Task 3: MQ5 ports

**Files:**
- Create: `src/mt5/historical_s_batch1/HistS_Bred222TrendCore.mq5`
- Create: `src/mt5/historical_s_batch1/HistS_Bred723TrendCore.mq5`
- Create: `src/mt5/historical_s_batch1/HistS_RecruitRndTrendCross.mq5`

**Step 1: Implement the bred trend core ports**

Use:

- `close > sma20 > sma50 and rsi > 55` for long
- mirrored short logic
- broker-side SL / TP as the effective exit

**Step 2: Implement the recruit trend cross port**

Use:

- entry: SMA5 / SMA20 cross with mirrored long/short
- exit: any SMA13 / SMA120 cross

**Step 3: Note timeframe assumptions in code comments**

The bred candidates require chart-attached custom timeframe use if exact `3600`-minute replication is desired.

### Task 4: Verify green

**Files:**
- Verify: `tools/tests/test_historical_s_mq5_docs.py`

**Step 1: Run verification**

Run:

```bash
python3 -m unittest tools.tests.test_historical_s_mq5_docs -v
```

Expected: all tests pass.
