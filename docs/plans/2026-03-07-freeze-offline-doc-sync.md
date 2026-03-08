# Freeze Offline Documentation Sync Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Sync Legend/MQ5 documentation with the current frozen-runtime state and the already completed offline MQ5 batches.

**Architecture:** Limit work to documentation that can be validated without restarting Swimmy. Use local process/report inspection as the runtime source of truth, then update the inventory and batch documents so their "next steps" reflect the MQ5 artifacts already present in the repository.

**Tech Stack:** Markdown, local shell inspection, Python `unittest` doc checks.

---

### Task 1: Capture frozen-runtime source of truth

**Files:**
- Modify: `doc/knowledge/legend_s_rank_inventory_20260307.md`

1. Record that the main Swimmy runtime is intentionally frozen/stopped.
2. Replace stale "pipeline still alive" wording with the freeze-aware status.
3. Keep report-file timestamps/counters only as the latest pre-freeze evidence.

### Task 2: Sync MQ5 progress documentation

**Files:**
- Modify: `doc/knowledge/legend_s_rank_inventory_20260307.md`
- Modify: `doc/knowledge/legend_mq5_batch1_20260307.md`

1. Update the recommended next steps so they no longer describe batch-2/3 and historical S work as future work if those artifacts already exist.
2. Reframe the remaining work around offline-safe tasks: MT5 compile/test, packaging, and source-divergence audit.
3. Preserve the original strategy rationale while correcting outdated sequencing.

### Task 3: Verify documentation contract

**Files:**
- Verify: `tools/tests/test_legend_mq5_docs.py`
- Verify: `tools/tests/test_historical_s_mq5_docs.py`

1. Run the doc existence/content tests after edits.
2. Report only the remaining risks that still require MT5 or live environment access.
