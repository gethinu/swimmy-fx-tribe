# XAU AutoBot Full Automation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Deliver end-to-end XAU AutoBot operation with 90-day re-optimization, Windows one-command execution, and periodic automated cycle with logs/report outputs.

**Architecture:** Add a lightweight orchestrator script that shells existing tools (`xau_autobot_backtest.py`, `xau_autobot_optimize.py`, `xau_autobot_readiness.py`, `xau_autobot_cost_guard.py`) and emits a single summary JSON. Wrap this flow for Windows (`.ps1` + `.bat`) and Linux cron/systemd usage. Keep risk gating unchanged; only improve execution ergonomics and repeatability.

**Tech Stack:** Python 3, existing tools under `tools/`, unittest, Bash, PowerShell, Windows batch.

---

### Task 1: Add Orchestration Unit Tests (TDD)

**Files:**
- Create: `tools/tests/test_xau_autobot_cycle.py`
- Test: `tools/tests/test_xau_autobot_cycle.py`

**Step 1: Write failing tests for helper behavior**
- Validate command argument builder returns expected command vectors.
- Validate JSONL parser picks target records (`preset=tuned`, `segment=all`).
- Validate summary composer includes key fields and timestamp.

**Step 2: Run tests to verify failure**
- Run: `./.venv/bin/python -m unittest tools/tests/test_xau_autobot_cycle.py -v`
- Expected: FAIL due to missing module/functions.

**Step 3: Implement minimal helpers in new script**
- Add helper functions with deterministic output.

**Step 4: Run tests to verify pass**
- Run the same command and confirm all pass.

### Task 2: Implement Cycle Script

**Files:**
- Create: `tools/xau_autobot_cycle.py`
- Modify: `tools/tests/test_xau_autobot_cycle.py`

**Step 1: Implement CLI and subprocess execution**
- Inputs: periods/intervals/costs/output paths.
- Execute backtest -> optimize -> readiness -> cost_guard sequentially.

**Step 2: Add result parsing and unified summary**
- Parse JSON/JSONL outputs from each command.
- Write summary JSON report and optional text line.

**Step 3: Keep failure behavior explicit**
- Non-zero subprocess exit should fail fast with stderr surfaced.

**Step 4: Verify tests pass**
- `./.venv/bin/python -m unittest tools/tests/test_xau_autobot_cycle.py -v`

### Task 3: Add Windows One-Command Runners

**Files:**
- Create: `tools/windows/xau_autobot_run_all.ps1`
- Create: `tools/windows/xau_autobot_run_all.bat`

**Step 1: Write PowerShell wrapper**
- Run cycle script + MT5 probe script with sensible defaults.
- Create output folder if missing.

**Step 2: Add batch wrapper**
- Delegate to `.ps1` with execution-policy bypass.

**Step 3: Local syntax sanity checks**
- Verify files are plain text and parameter names align with Python scripts.

### Task 4: Execute 90-Day Artifacts

**Files:**
- Create/update reports under `data/reports/`
- Create/update config: `tools/configs/xau_autobot.tuned_auto_gc_m5_90d.json`

**Step 1: Run optimize with `--period 90d`**
- Save stdout JSONL report.
- Save best config.

**Step 2: Run readiness against 90d config**
- Save readiness JSON report.

**Step 3: Run cost_guard against readiness**
- Save cost guard report with spread scenarios.

### Task 5: Documentation Update

**Files:**
- Modify: `doc/knowledge/xau_autobot_quickstart.md`

**Step 1: Add one-command sections**
- Linux cycle command
- Windows `.bat`/`.ps1` command

**Step 2: Add periodic run examples**
- Cron entry
- systemd timer/service usage note

### Task 6: Verification Before Completion

**Files:**
- N/A

**Step 1: Run targeted tests**
- `./.venv/bin/python -m unittest tools/tests/test_xau_autobot_cycle.py -v`
- Existing suite:
  - `./.venv/bin/python -m unittest tools/tests/test_xau_autobot.py tools/tests/test_xau_autobot_optimize.py tools/tests/test_xau_autobot_readiness.py tools/tests/test_xau_autobot_cost_guard.py tools/tests/test_xau_autobot_windows_probe.py -v`

**Step 2: Run smoke execution of cycle script**
- Use small/normal settings and confirm summary JSON is generated.

**Step 3: Report exact outputs**
- Mention generated file paths and key metrics.
