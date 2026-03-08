# Legend MT5 Evidence Bridge Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Preserve the freeze-era Legend MT5 validation trail as a contract-aware JSON bridge that can feed Swimmy's next paper-forward queue without falsely satisfying rank or deployment gates.

**Architecture:** Add one offline Python tool that reads MT5 inventory manifests and walk-forward summaries, chooses the canonical shortlist evidence defined in current docs, and emits a single machine-readable report under `data/reports/mt5/`. The tool must remain read-only with respect to SQLite, Lisp rank state, and `deployment_gate_status`.

**Tech Stack:** Python 3, `json`, `argparse`, `pathlib`, `unittest`, existing MT5 report artifacts under `data/reports/mt5/`

---

### Task 1: Document the bridge contract before code

**Files:**
- Modify: `docs/llm/STATE.md`
- Create: `docs/plans/2026-03-08-legend-mt5-evidence-bridge.md`

**Step 1: Update STATE**

Add a new `Legend MT5 evidence bridge` entry that fixes:
- the tool name
- artifact inputs and output path family
- the freeze-era shortlist ordering
- the rule that MT5 walk-forward is proxy evidence and must not directly satisfy `RankTradeEvidence` or `deployment_gate_status`

**Step 2: Verify INTERFACES remains unchanged**

Run: `rg -n "Legend MT5 evidence bridge|deployment_gate_status|paper_forward" docs/llm/STATE.md docs/llm/INTERFACES.md`
Expected: the new bridge contract appears in `STATE.md`; `INTERFACES.md` needs no change because no ZMQ/HTTP/schema boundary changes are introduced.

### Task 2: Write the failing tests

**Files:**
- Create: `tools/tests/test_legend_mt5_evidence_bridge.py`
- Reference: `tools/tests/test_mt5_inventory_tester.py`
- Reference: `tools/tests/test_mt5_ih_promotion_gate.py`

**Step 1: Write a fixture-driven parser test**

Create a test that builds temporary `inventory_tester_manifest.json` and `walkforward_summary.json` files for:
- `legend-macd-above-zero-cross` with both narrow and `wide` walk-forward summaries
- `historical-s-bred940-trend-core` with a `medium` walk-forward summary
- `legend-pullback-breakout` with a `wide` walk-forward summary

Assert that the bridge output:
- keeps shortlist order `macd -> bred940 -> pullback`
- prefers `wide` for `legend-macd-above-zero-cross` and `legend-pullback-breakout`
- prefers `medium` for `historical-s-bred940-trend-core`
- marks all strategies `requires_paper_forward=true`

**Step 2: Write a contract-alignment test**

Assert that the bridge output:
- does not set any strategy as `rank_evidence_satisfied`
- does not set any strategy as `deployment_gate_ready`
- records reason codes showing MT5 artifacts are proxy-only evidence

**Step 3: Run the tests to verify RED**

Run: `python3 -m unittest tools.tests.test_legend_mt5_evidence_bridge -q`
Expected: FAIL with import or attribute errors because the bridge tool does not exist yet.

### Task 3: Implement the bridge tool

**Files:**
- Create: `tools/legend_mt5_evidence_bridge.py`
- Test: `tools/tests/test_legend_mt5_evidence_bridge.py`

**Step 1: Implement minimal parsing helpers**

Add helpers to:
- load inventory manifest entries from repeated manifest paths
- discover walk-forward summaries under repeated run roots
- normalize MT5 numeric strings into floats/ints
- choose canonical walk-forward summaries using the freeze-era preference map

**Step 2: Implement report assembly**

Build a JSON report with:
- `schema_version`
- `generated_at_utc`
- `contract`
- `inputs`
- `shortlist`
- `strategies`

For each strategy, include:
- job metadata
- shortlist rank and role
- inventory snapshot summary
- canonical walk-forward proxy summary
- contract-alignment flags and reason codes
- `next_action` pointing to paper-forward canonical follow-up

**Step 3: Add CLI**

Support:
- `--inventory-manifest` repeated
- `--walkforward-root` repeated
- `--output` optional

If `--output` is absent, write to `data/reports/mt5/legend_evidence_bridge_<timestamp>.json`.

**Step 4: Run the tests to verify GREEN**

Run: `python3 -m unittest tools.tests.test_legend_mt5_evidence_bridge -q`
Expected: PASS

### Task 4: Verify on current freeze-era artifacts

**Files:**
- Read: `data/reports/mt5/inventory_tester/run_20260307_105823/inventory_tester_manifest.json`
- Read: `data/reports/mt5/legend_walkforward/run_20260307_113707/`
- Read: `data/reports/mt5/legend_walkforward/run_20260307_123443_wide/`
- Read: `data/reports/mt5/legend_walkforward/run_20260307_132228_b940_medium/`
- Create: `data/reports/mt5/legend_evidence_bridge_*.json`

**Step 1: Generate a real bridge report**

Run:

```bash
python3 tools/legend_mt5_evidence_bridge.py \
  --inventory-manifest data/reports/mt5/inventory_tester/run_20260307_105823/inventory_tester_manifest.json \
  --walkforward-root data/reports/mt5/legend_walkforward/run_20260307_113707 \
  --walkforward-root data/reports/mt5/legend_walkforward/run_20260307_123443_wide \
  --walkforward-root data/reports/mt5/legend_walkforward/run_20260307_132228_b940_medium
```

Expected:
- a new `data/reports/mt5/legend_evidence_bridge_*.json` file is written
- shortlist rank `1/2/3` matches the freeze-era conclusion
- all three strategies still show `requires_paper_forward=true`

**Step 2: Sanity-check the output**

Run: `python3 -m json.tool data/reports/mt5/legend_evidence_bridge_*.json >/dev/null`
Expected: command succeeds

**Step 3: Do not commit automatically**

The current repository is already dirty. Leave changes unstaged unless the user explicitly asks for commit or cleanup.
