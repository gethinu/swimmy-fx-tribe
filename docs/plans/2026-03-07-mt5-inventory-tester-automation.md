# MT5 Inventory Tester Automation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a fully automatic MT5 Strategy Tester runner for the MQ5 inventory ports that uses an isolated portable terminal and does not interfere with the currently running live terminal.

**Architecture:** Implement a Python runner that prepares a portable MT5 root under the Windows user profile, copies the required MT5 binaries/config/expert files, generates tester `.ini` files, launches `terminal64.exe /portable /config`, waits for shutdown, then copies HTML/PNG artifacts plus a JSON summary back into the repo. Keep runtime isolation simple: separate portable root, shared machine, sequential job execution.

**Tech Stack:** Python 3 stdlib, PowerShell launch via `powershell.exe`, MT5 portable terminal, `unittest`

---

### Task 1: Add the failing contract tests

**Files:**
- Create: `tools/tests/test_mt5_inventory_tester.py`
- Test: `python3 -m unittest tools.tests.test_mt5_inventory_tester -v`

**Step 1: Write the failing test**

Cover these behaviors:
- `select_jobs("legend")` returns the 9 legend jobs and includes `Legend_LondonBreakoutV1.ex5`
- `select_jobs("historical_s")` returns the 13 historical-s jobs and includes `HistS_Bred222TrendCore.ex5`
- `render_tester_ini(...)` emits the expected MT5 `[Tester]` config keys
- `parse_report_summary(...)` decodes a UTF-16 HTML report and extracts `initial_deposit`, `total_net_profit`, `profit_factor`, and `total_trades`
- `find_report_artifacts(...)` returns the `.htm` and image artifacts for a report prefix

**Step 2: Run test to verify it fails**

Run: `python3 -m unittest tools.tests.test_mt5_inventory_tester -v`

Expected: FAIL because `tools.mt5_inventory_tester` does not exist yet.

### Task 2: Implement the isolated tester runner

**Files:**
- Create: `tools/mt5_inventory_tester.py`
- Modify: `tools/tests/test_mt5_inventory_tester.py`

**Step 1: Write minimal implementation**

Implement:
- `TesterJob` dataclass and static registry for `legend`, `historical_s`, and `all`
- path helpers for Windows/WSL conversion
- portable-root bootstrap that copies:
  - `terminal64.exe`, `metatester64.exe`, `MetaEditor64.exe`, `Terminal.ico`
  - source terminal `config/*`
  - requested expert `.ex5`
- `.ini` generator with `ShutdownTerminal=1`, `UseLocal=1`, `UseRemote=0`, `UseCloud=0`
- PowerShell launcher that waits for portable `terminal64.exe` exit
- report artifact discovery and UTF-16 HTML summary parsing
- JSON manifest output into `data/reports/mt5/inventory_tester/<run-id>/`

**Step 2: Run test to verify it passes**

Run: `python3 -m unittest tools.tests.test_mt5_inventory_tester -v`

Expected: PASS

### Task 3: Document the automation contract

**Files:**
- Create: `doc/knowledge/legend_mt5_tester_automation_20260307.md`
- Modify: `doc/knowledge/legend_mt5_compile_report_20260307.md`

**Step 1: Document**

Document:
- why live terminal reuse is unsafe
- why portable isolation is the chosen path
- default portable root and report output location
- default job sets and how `timeframe=3600` historical rows are mapped to `Period=H1`
- one-command usage examples

**Step 2: Add/adjust tests if doc contracts need coverage**

Run: `python3 -m unittest tools.tests.test_legend_mq5_docs -v`

Expected: PASS after any doc assertions are updated.

### Task 4: Verify with a fresh real MT5 run

**Files:**
- Generate only: `data/reports/mt5/inventory_tester/...`

**Step 1: Run a real isolated smoke job**

Run a single legend job first, for example:

```bash
python3 tools/mt5_inventory_tester.py \
  --job-set legend \
  --job legend-perfect-order-sma \
  --from-date 2025.01.01 \
  --to-date 2025.01.15
```

Expected:
- live terminal remains running
- portable terminal launches from a different path
- report `.htm` and `.png` files are copied into repo output
- JSON summary is written

**Step 2: Run the complete verification set**

Run:

```bash
python3 -m unittest tools.tests.test_mt5_inventory_tester tools.tests.test_legend_mq5_docs tools.tests.test_historical_s_mq5_docs -v
```

Expected: PASS
