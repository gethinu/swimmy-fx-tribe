# Legend Paper-Forward Result Template Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Generate a canonical executor-fillable result batch template from staged Legend paper-forward runs without mutating manifests or deployment state.

**Architecture:** Add one Python tool that reads a stage report, normalizes items into the same shape expected by `legend_mt5_paper_forward_apply_results.py`, and writes a single template JSON under `data/reports/mt5/`. Keep it offline-only, deterministic, and read-only with respect to manifests and DB.

**Tech Stack:** Python 3, `json`, `argparse`, `pathlib`, `unittest`

---

### Task 1: Document the contract

**Files:**
- Modify: `docs/llm/STATE.md`
- Create: `docs/plans/2026-03-09-legend-paper-forward-result-template.md`

**Step 1: Add result-template contract text**

Document:
- tool name and output path family
- required template item fields
- explicit read-only rule
- `INTERFACES.md` unchanged

**Step 2: Verify INTERFACES stays unchanged**

Run: `rg -n "Legend paper-forward result template artifact" docs/llm/STATE.md docs/llm/INTERFACES.md`
Expected: contract text only appears in `STATE.md`.

### Task 2: Write the failing test

**Files:**
- Create: `tools/tests/test_legend_mt5_paper_forward_result_template.py`

**Step 1: Add template-generation test**

Feed a minimal stage report with unsorted items and assert:
- output items are sorted by `shortlist_rank`
- template items carry `paper_run_id`, `manifest_path`, `planned_run_root`
- `observed_forward_*` defaults are zero/null
- `status=STAGED`, `status_reason=fill_from_executor`

**Step 2: Run test to verify RED**

Run: `python3 -m unittest tools.tests.test_legend_mt5_paper_forward_result_template -q`
Expected: FAIL because the new tool does not exist yet.

### Task 3: Implement the minimal tool

**Files:**
- Create: `tools/legend_mt5_paper_forward_result_template.py`

**Step 1: Implement template generator**

Read:
- `--stage-report`

Write:
- `data/reports/mt5/legend_paper_forward_result_template_<timestamp>.json`

**Step 2: Keep it read-only**

Do not modify:
- `run_manifest.json`
- SQLite
- `deployment_gate_status`

**Step 3: Run test to verify GREEN**

Run: `python3 -m unittest tools.tests.test_legend_mt5_paper_forward_result_template -q`
Expected: PASS

### Task 4: Generate artifact and run regression verification

**Files:**
- Read: `data/reports/mt5/legend_paper_forward_stage_*.json`
- Create: `data/reports/mt5/legend_paper_forward_result_template_*.json`

**Step 1: Generate a fresh template artifact**

Run the template tool on the latest stage report.

Expected:
- 3 template items
- order matches shortlist rank 1/2/3

**Step 2: Run Legend MT5 tool test suite**

Run: `python3 -m unittest tools.tests.test_legend_mt5_evidence_bridge tools.tests.test_legend_mt5_paper_forward_queue tools.tests.test_legend_mt5_paper_forward_runner_input tools.tests.test_legend_mt5_paper_forward_stage tools.tests.test_legend_mt5_paper_forward_status tools.tests.test_legend_mt5_paper_forward_apply_results tools.tests.test_legend_mt5_paper_forward_result_template tools.tests.test_mt5_inventory_tester -q`
Expected: PASS

**Step 3: Sanity-check JSON and manifest immutability**

Run:
- `python3 -m json.tool <template>.json >/dev/null`
- inspect `data/reports/mt5/legend_paper_forward_runs/*/run_manifest.json`

Expected:
- template JSON is valid
- canonical manifests remain `STAGED` with zero/null observed metrics
