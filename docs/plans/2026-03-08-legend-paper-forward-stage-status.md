# Legend Paper-Forward Stage And Status Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Materialize the offline Legend paper-forward runner input into staged run manifests and a status report without mutating deployment gates or live runtime state.

**Architecture:** Add one staging tool that writes deterministic `run_manifest.json` files under each planned run root plus an aggregate stage report, and one status tool that reads those manifests back into a single current-status artifact. Keep both tools offline-only and fail-closed.

**Tech Stack:** Python 3, `json`, `argparse`, `pathlib`, `unittest`

---

### Task 1: Document the contracts

**Files:**
- Modify: `docs/llm/STATE.md`
- Create: `docs/plans/2026-03-08-legend-paper-forward-stage-status.md`

**Step 1: Add stage/status contract text**

Document:
- stage report path family
- per-run manifest location and required fields
- status report path family
- explicit no-DB-write rule

**Step 2: Verify INTERFACES stays unchanged**

Run: `rg -n "Legend paper-forward stage manifests|Legend paper-forward status artifact" docs/llm/STATE.md docs/llm/INTERFACES.md`
Expected: contract text only appears in `STATE.md`.

### Task 2: Write failing tests

**Files:**
- Create: `tools/tests/test_legend_mt5_paper_forward_stage.py`
- Create: `tools/tests/test_legend_mt5_paper_forward_status.py`

**Step 1: Add staging test**

Feed a minimal runner-input JSON and assert:
- one `run_manifest.json` per item is created
- aggregate stage report contains `status_counts`
- staged items start with zero observed forward metrics
- staged items use `status=STAGED`

**Step 2: Add status aggregation test**

Feed a minimal stage report plus manifests and assert:
- `status_counts` are grouped correctly
- `progress_summary` reflects observed vs target forward metrics

**Step 3: Run tests to verify RED**

Run: `python3 -m unittest tools.tests.test_legend_mt5_paper_forward_stage tools.tests.test_legend_mt5_paper_forward_status -q`
Expected: FAIL because the new tools do not exist yet.

### Task 3: Implement the minimal tools

**Files:**
- Create: `tools/legend_mt5_paper_forward_stage.py`
- Create: `tools/legend_mt5_paper_forward_status.py`

**Step 1: Implement stage writer**

Read runner-input items and write:
- `<planned_run_root>/run_manifest.json`
- `data/reports/mt5/legend_paper_forward_stage_<timestamp>.json`

**Step 2: Implement status aggregator**

Read the stage report and manifests and write:
- `data/reports/mt5/legend_paper_forward_status_<timestamp>.json`

**Step 3: Run tests to verify GREEN**

Run: `python3 -m unittest tools.tests.test_legend_mt5_paper_forward_stage tools.tests.test_legend_mt5_paper_forward_status -q`
Expected: PASS

### Task 4: Generate fresh artifacts

**Files:**
- Read: `data/reports/mt5/legend_paper_forward_runner_input_*.json`
- Create: `data/reports/mt5/legend_paper_forward_stage_*.json`
- Create: `data/reports/mt5/legend_paper_forward_status_*.json`

**Step 1: Generate stage report**

Run the staging tool on the latest runner-input artifact.

Expected:
- 3 manifests written
- stage report shows `STAGED=3`

**Step 2: Generate status report**

Run the status tool on the new stage report.

Expected:
- `status_counts.STAGED=3`
- progress ratios are all zero because no executor has run yet

**Step 3: Sanity-check JSON**

Run:
- `python3 -m json.tool <stage>.json >/dev/null`
- `python3 -m json.tool <status>.json >/dev/null`

Expected: both succeed.
