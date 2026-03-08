# Legend MQ5 Batch 1 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Deliver the first incremental batch of strategy writeups and MQ5 ports from the LEGEND inventory.

**Architecture:** Use current DB `data_sexp` as the strategy truth source for the first three clean LEGEND candidates, write a human-readable catalog for operators, and add one standalone MQ5 EA per strategy. Defer historical S-rank ports to a later batch because backup rows need extra consistency checks before translation.

**Tech Stack:** Markdown, SQLite-backed strategy metadata, MetaTrader 5 / MQL5.

---

### Task 1: Batch-1 strategy catalog

**Files:**
- Create: `doc/knowledge/legend_mq5_batch1_20260307.md`

**Step 1: Write the batch scope**

Document the first three strategies:

- `Perfect-Order-SMA`
- `Simple-Momentum-Sync`
- `Pullback-Breakout`

**Step 2: Record source-of-truth fields**

For each strategy, capture:

- indicators
- entry rule
- exit rule
- symbol
- timeframe
- SL / TP
- headline metrics

**Step 3: Add operator-friendly interpretation**

For each strategy, explain:

- what market structure it is trying to exploit
- the exact long-side logic
- the mirrored short-side logic used for MQ5 translation
- where the implementation file lives

### Task 2: MQ5 ports

**Files:**
- Create: `src/mt5/legend_batch1/Legend_PerfectOrderSMA.mq5`
- Create: `src/mt5/legend_batch1/Legend_SimpleMomentumSync.mq5`
- Create: `src/mt5/legend_batch1/Legend_PullbackBreakout.mq5`

**Step 1: Add standalone EA skeletons**

Each EA should include:

- `CTrade`
- managed-position filtering by `magic`
- new-bar execution gate
- symbol / timeframe inputs
- fixed-price SL / TP inputs mapped from the strategy definition

**Step 2: Implement entry and exit rules**

Map the Lisp rules into deterministic MQL5 logic using closed bars only.

**Step 3: Mirror short-side rules explicitly**

Because current Legend rules are mostly expressed on the bullish side, implement a symmetric bearish mapping and document that this is an inference.

**Step 4: Verify artifacts**

Run file-level review and consistency checks between the Markdown catalog and the MQL5 implementations.

### Task 3: Deferred next batch

**Files:**
- Update later: `doc/knowledge/legend_mq5_batch1_20260307.md`

**Step 1: Flag deferred work**

Call out:

- 13 historical S-rank candidates from backup
- `Legend-London-Breakout-V1`
- `Legend-RSI-Reversion-V1`
- exclusion of `TEST-REFRESH-ACTIVE-SEXP-SYNC`
