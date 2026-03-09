# Legend Paper-Forward Runner Input Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Convert the offline Legend paper-forward queue into runner-ready JSON seed artifacts without touching deployment gates or live execution state.

**Architecture:** Add one small Python tool that reads `legend_paper_forward_queue_*.json`, expands each queued candidate into a deterministic `PAPER` runner input item, and writes a single JSON artifact for downstream orchestration. Keep it fail-closed and offline-only.

**Tech Stack:** Python 3, `json`, `argparse`, `pathlib`, `unittest`

---

### Task 1: Document the runner-input contract

**Files:**
- Modify: `docs/llm/STATE.md`
- Create: `docs/plans/2026-03-08-legend-paper-forward-runner-input.md`

**Step 1: Add contract text**

Document:
- runner-input artifact path family
- required item fields
- explicit `execution_mode=PAPER`
- the rule that runner input does not write `deployment_gate_status`

**Step 2: Verify INTERFACES stays unchanged**

Run: `rg -n "Legend paper-forward runner input|Legend paper-forward queue artifact" docs/llm/STATE.md docs/llm/INTERFACES.md`
Expected: only `STATE.md` changes.

### Task 2: Write the failing tests

**Files:**
- Create: `tools/tests/test_legend_mt5_paper_forward_runner_input.py`

**Step 1: Add a runner-input generation test**

Feed a minimal queue JSON and assert:
- items remain sorted by `shortlist_rank`
- each item gets `execution_mode=PAPER`
- each item gets a deterministic `paper_comment_prefix`
- each item gets a deterministic `paper_run_id`
- each item gets `status=READY_FOR_PAPER_FORWARD`

**Step 2: Run tests to verify RED**

Run: `python3 -m unittest tools.tests.test_legend_mt5_paper_forward_runner_input -q`
Expected: FAIL because the runner-input tool does not exist yet.

### Task 3: Implement the minimal tool

**Files:**
- Create: `tools/legend_mt5_paper_forward_runner_input.py`

**Step 1: Parse queue payload**

Read:
- `bridge_source`
- queue `items`
- forward target thresholds

**Step 2: Build runner-input payload**

For each queue item, add:
- `paper_run_id`
- `execution_mode`
- `paper_comment_prefix`
- `planned_run_root`
- `status`

**Step 3: Add CLI**

Support:
- `--queue-report` required
- `--output` optional

If omitted, write to `data/reports/mt5/legend_paper_forward_runner_input_<timestamp>.json`.

**Step 4: Run tests to verify GREEN**

Run: `python3 -m unittest tools.tests.test_legend_mt5_paper_forward_runner_input -q`
Expected: PASS

### Task 4: Generate a fresh runner-input artifact

**Files:**
- Read: `data/reports/mt5/legend_paper_forward_queue_*.json`
- Create: `data/reports/mt5/legend_paper_forward_runner_input_*.json`

**Step 1: Run the tool on the current queue artifact**

Expected:
- 3 items
- order `1 -> 2 -> 3`
- all items `READY_FOR_PAPER_FORWARD`

**Step 2: Sanity-check JSON**

Run: `python3 -m json.tool <runner-input>.json >/dev/null`
Expected: success
