# Historical S MQ5 Batch 3 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Finish the historical S-rank extraction by documenting the remaining seven bred candidates in Japanese and adding one MQ5 port for each.

**Architecture:** Treat the backup DB `data_sexp` / library strategy body as the executable truth. Batch-3 intentionally groups the seven remaining candidates because they share the exact same entry/exit core and differ only in `SL/TP`, validation metrics, and stored indicator debris. Keep operator-facing docs in Japanese and reuse the existing historical bred trend-core MQ5 shape.

**Tech Stack:** Markdown, Python unittest, MQ5 Expert Advisors

---

### Task 1: Extend red tests for batch-3 coverage

**Files:**
- Modify: `tools/tests/test_historical_s_mq5_docs.py`
- Test: `tools/tests/test_historical_s_mq5_docs.py`

**Step 1: Write the failing test**

Add assertions that:

- `doc/knowledge/historical_s_mq5_batch3_20260307.md` exists
- the doc contains `自然言語版`, `同一シグナルコア`, `duplicate pair`, and all seven remaining strategy names
- all seven MQ5 files exist under `src/mt5/historical_s_batch3/`

**Step 2: Run test to verify it fails**

Run:

```bash
python3 -m unittest tools.tests.test_historical_s_mq5_docs -v
```

Expected: FAIL because batch-3 doc and MQ5 files do not exist yet.

### Task 2: Write the operator-facing Japanese catalog

**Files:**
- Create: `doc/knowledge/historical_s_mq5_batch3_20260307.md`

**Step 1: Write the document**

Document:

- `Bred-Bred--187-Gen23-N3980038264-261`
- `Bred-Bred--436-Gen32-N3980040463-744`
- `Bred-Bred--940-Gen31-N3980039835-605`
- `Bred-Bred--586-Gen29-N3980038495-317`
- `Bred-Bred--458-Gen32-N3980040289-704`
- `Bred-Bred--139-Gen11-N3979972567-6`
- `Bred-Bred--139-Gen11-N3979972610-6`

Include:

- the shared bred trend-core entry/exit logic
- explicit note that all 12 historical bred S candidates now map to the same executable core
- the `Bred-Bred--139...` pair as a `duplicate pair`
- per-strategy `SL/TP`, metrics, and indicator divergence notes

### Task 3: Add MQ5 ports for the seven remaining bred candidates

**Files:**
- Create: `src/mt5/historical_s_batch3/HistS_Bred187TrendCore.mq5`
- Create: `src/mt5/historical_s_batch3/HistS_Bred436TrendCore.mq5`
- Create: `src/mt5/historical_s_batch3/HistS_Bred940TrendCore.mq5`
- Create: `src/mt5/historical_s_batch3/HistS_Bred586TrendCore.mq5`
- Create: `src/mt5/historical_s_batch3/HistS_Bred458TrendCore.mq5`
- Create: `src/mt5/historical_s_batch3/HistS_Bred139TrendCore_3979972567.mq5`
- Create: `src/mt5/historical_s_batch3/HistS_Bred139TrendCore_3979972610.mq5`

**Step 1: Write minimal implementation**

For each file:

- reuse the batch-2 bred trend-core EA structure
- keep `SMA20`, `SMA50`, `RSI14` execution logic
- use `PERIOD_CURRENT` because original `timeframe=3600` is non-native in MT5
- encode strategy-specific `SL`, `TP`, magic number, and source strategy name

### Task 4: Verify green

**Files:**
- Verify: `tools/tests/test_historical_s_mq5_docs.py`
- Verify: `tools/tests/test_legend_mq5_docs.py`

**Step 1: Run focused historical tests**

```bash
python3 -m unittest tools.tests.test_historical_s_mq5_docs -v
```

Expected: PASS

**Step 2: Run full doc test suite**

```bash
python3 -m unittest tools.tests.test_legend_mq5_docs tools.tests.test_historical_s_mq5_docs -v
```

Expected: PASS
