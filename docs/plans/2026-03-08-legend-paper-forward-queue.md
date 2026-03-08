# Legend Paper-Forward Queue Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Turn the Legend MT5 bridge output into an offline paper-forward queue artifact without directly mutating deployment gates or rank evidence.

**Architecture:** Extend the bridge to aggregate multiple inventory manifests for the same shortlist candidates, then add one queue generator that reads the bridge JSON and emits prioritized `QUEUED` paper-forward items with the canonical forward thresholds from the current Swimmy contract.

**Tech Stack:** Python 3, `json`, `argparse`, `glob`, `pathlib`, `unittest`

---

### Task 1: Fix the queue contract in docs

**Files:**
- Modify: `docs/llm/STATE.md`
- Create: `docs/plans/2026-03-08-legend-paper-forward-queue.md`

**Step 1: Add the queue contract**

Document:
- bridge multi-manifest aggregation is allowed
- queue artifact path family
- queue item fields
- the rule that queue generation must not upsert `deployment_gate_status`

**Step 2: Verify INTERFACES stays unchanged**

Run: `rg -n "Legend paper-forward queue artifact|Legend MT5 evidence bridge" docs/llm/STATE.md docs/llm/INTERFACES.md`
Expected: new contract text is only in `STATE.md`.

### Task 2: Write failing tests first

**Files:**
- Modify: `tools/tests/test_legend_mt5_evidence_bridge.py`
- Create: `tools/tests/test_legend_mt5_paper_forward_queue.py`

**Step 1: Add a multi-manifest aggregation test**

Create a bridge test that feeds two inventory manifests for the same strategy and asserts:
- `windows_total` increments
- `positive_windows` reflects both manifests
- the latest window snapshot comes from the newest manifest

**Step 2: Add a queue generation test**

Create a queue test that feeds a minimal bridge payload and asserts:
- items are sorted by `shortlist_rank`
- only `requires_paper_forward=true` candidates are queued
- each item carries `target_forward_days=30`, `target_forward_trades=300`, `target_forward_sharpe=0.70`, `target_forward_pf=1.50`
- status is `QUEUED`

**Step 3: Run tests to verify RED**

Run: `python3 -m unittest tools.tests.test_legend_mt5_evidence_bridge tools.tests.test_legend_mt5_paper_forward_queue -q`
Expected: FAIL because the queue tool does not exist and the bridge does not yet reflect the new aggregation expectation.

### Task 3: Implement the minimal code

**Files:**
- Modify: `tools/legend_mt5_evidence_bridge.py`
- Create: `tools/legend_mt5_paper_forward_queue.py`

**Step 1: Extend bridge input handling**

Add:
- helper to collect manifest paths from repeated files or recursive roots
- aggregation logic that summarizes all provided inventory windows per strategy

**Step 2: Implement queue builder**

Build queue payload with:
- `schema_version`
- `generated_at_utc`
- `bridge_source`
- `targets`
- `items`

Each item should include:
- strategy metadata from bridge
- canonical queue thresholds
- queue reason
- status `QUEUED`

**Step 3: Add CLI**

Support:
- `--bridge-report` required
- `--output` optional

If omitted, output goes to `data/reports/mt5/legend_paper_forward_queue_<timestamp>.json`.

**Step 4: Run tests to verify GREEN**

Run: `python3 -m unittest tools.tests.test_legend_mt5_evidence_bridge tools.tests.test_legend_mt5_paper_forward_queue -q`
Expected: PASS

### Task 4: Generate fresh artifacts from current freeze-era inputs

**Files:**
- Read: `data/reports/mt5/inventory_tester/run_20260307_*/inventory_tester_manifest.json`
- Read: `data/reports/mt5/legend_walkforward/run_20260307_113707/`
- Read: `data/reports/mt5/legend_walkforward/run_20260307_123443_wide/`
- Read: `data/reports/mt5/legend_walkforward/run_20260307_132228_b940_medium/`
- Create: `data/reports/mt5/legend_evidence_bridge_*.json`
- Create: `data/reports/mt5/legend_paper_forward_queue_*.json`

**Step 1: Regenerate bridge with multiple manifests**

Run a bridge command that includes:
- baseline short window
- latest window
- rolling windows

Expected:
- `windows_total` is greater than `1` for all shortlist strategies
- shortlist order remains unchanged

**Step 2: Generate queue artifact**

Run the queue tool on the fresh bridge report.

Expected:
- 3 queue items
- order `1 -> 2 -> 3`
- all items remain offline-only and `QUEUED`

**Step 3: Sanity-check JSON**

Run:
- `python3 -m json.tool <bridge>.json >/dev/null`
- `python3 -m json.tool <queue>.json >/dev/null`

Expected: both succeed.
