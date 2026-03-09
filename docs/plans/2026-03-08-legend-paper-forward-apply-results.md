# Legend Paper-Forward Apply Results Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Apply offline executor result batches onto staged Legend paper-forward manifests without mutating deployment gates or live runtime state.

**Architecture:** Add one Python tool that reads a stage report plus a result batch, updates matching `run_manifest.json` files in place, and writes a single apply report summarizing updated and missing runs. Keep the tool fail-closed for unknown run IDs and leave DB / `deployment_gate_status` untouched.

**Tech Stack:** Python 3, `json`, `argparse`, `pathlib`, `unittest`

---

### Task 1: Document the contract

**Files:**
- Modify: `docs/llm/STATE.md`
- Create: `docs/plans/2026-03-08-legend-paper-forward-apply-results.md`

**Step 1: Add apply-results contract text**

Document:
- tool name and output path family
- required result batch item fields
- in-place `run_manifest.json` update rule
- explicit no-DB-write rule

**Step 2: Verify INTERFACES stays unchanged**

Run: `rg -n "Legend paper-forward result apply artifact" docs/llm/STATE.md docs/llm/INTERFACES.md`
Expected: contract text only appears in `STATE.md`.

### Task 2: Write the failing test

**Files:**
- Create: `tools/tests/test_legend_mt5_paper_forward_apply_results.py`

**Step 1: Add apply-results test**

Feed:
- a minimal stage report with two staged runs
- two `run_manifest.json` files
- a result batch that updates one run and references one unknown run

Assert:
- matching manifest is updated in place
- unmatched result item is reported as missing
- apply report includes `updated_count`, `missing_count`, `status_counts`
- untouched manifest remains `STAGED`

**Step 2: Run test to verify RED**

Run: `python3 -m unittest tools.tests.test_legend_mt5_paper_forward_apply_results -q`
Expected: FAIL because the new tool does not exist yet.

### Task 3: Implement the minimal tool

**Files:**
- Create: `tools/legend_mt5_paper_forward_apply_results.py`

**Step 1: Implement manifest updater**

Read:
- `--stage-report`
- `--results`

Write:
- updated `run_manifest.json` files
- `data/reports/mt5/legend_paper_forward_apply_results_<timestamp>.json`

**Step 2: Keep fail-closed behavior**

For unknown `paper_run_id`:
- do not create new manifests
- count them in `missing_count`
- include them in report items with `apply_status=missing`

**Step 3: Run test to verify GREEN**

Run: `python3 -m unittest tools.tests.test_legend_mt5_paper_forward_apply_results -q`
Expected: PASS

### Task 4: Run regression verification

**Files:**
- Read: `tools/tests/test_legend_mt5_*`

**Step 1: Run Legend MT5 tool test suite**

Run: `python3 -m unittest tools.tests.test_legend_mt5_evidence_bridge tools.tests.test_legend_mt5_paper_forward_queue tools.tests.test_legend_mt5_paper_forward_runner_input tools.tests.test_legend_mt5_paper_forward_stage tools.tests.test_legend_mt5_paper_forward_status tools.tests.test_legend_mt5_paper_forward_apply_results tools.tests.test_mt5_inventory_tester -q`
Expected: PASS

**Step 2: Verify real artifacts are left untouched without real executor results**

Do not apply fake results to `data/reports/mt5/legend_paper_forward_runs/*/run_manifest.json`.

Expected:
- no dummy forward metrics are written into canonical staged manifests
- verification relies on tests and contract checks only
