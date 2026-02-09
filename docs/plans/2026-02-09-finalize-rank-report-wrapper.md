# Finalize Rank Report Wrapper Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a reliable manual wrapper to generate the Evolution Factory Report with sufficient SBCL heap to avoid OOM.

**Architecture:** A small Bash wrapper in `tools/ops/` sources `tools/sbcl_env.sh` to set `SWIMMY_SBCL_DYNAMIC_SPACE_MB` and invokes the existing Lisp script. A lightweight shell test asserts the wrapper exists and uses the standard env path. State docs are updated before implementation.

**Tech Stack:** Bash, SBCL, Lisp ops script.

---

### Task 1: Update State Documentation (Pre-Implementation)

**Files:**
- Modify: `docs/llm/STATE.md`

**Step 1: Add a new bullet under the memory/ops notes**

Add a bullet near the existing SBCL memory notes to document the wrapper and env usage. Example text:

- `tools/ops/finalize_rank_report.sh` は `tools/sbcl_env.sh` を読み込み、`SWIMMY_SBCL_DYNAMIC_SPACE_MB`（未指定時 4096MB）で `finalize_rank_report.lisp` を実行する。

**Step 2: Save and review**

**Step 3: Commit**

```bash
git add docs/llm/STATE.md
git commit -m "docs: document rank report wrapper memory"
```

---

### Task 2: Add Wrapper + Test (TDD)

**Files:**
- Create: `tools/ops/finalize_rank_report.sh`
- Create: `tools/tests/test_finalize_rank_report_wrapper.sh`

**Step 1: Write the failing test**

Create `tools/tests/test_finalize_rank_report_wrapper.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT/tools/ops/finalize_rank_report.sh"

if [[ ! -x "$SCRIPT" ]]; then
  echo "missing executable: $SCRIPT"
  exit 1
fi

grep -q "sbcl_env.sh" "$SCRIPT"
grep -q "SWIMMY_SBCL_DYNAMIC_SPACE_MB" "$SCRIPT"
grep -q "finalize_rank_report.lisp" "$SCRIPT"
```

**Step 2: Run the test to verify it fails**

Run:
```bash
bash tools/tests/test_finalize_rank_report_wrapper.sh
```
Expected: FAIL with "missing executable".

**Step 3: Implement the wrapper**

Create `tools/ops/finalize_rank_report.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

if [[ -f "$ROOT/tools/sbcl_env.sh" ]]; then
  # shellcheck source=tools/sbcl_env.sh
  source "$ROOT/tools/sbcl_env.sh"
fi

: "${SWIMMY_SBCL_DYNAMIC_SPACE_MB:=4096}"
export SWIMMY_SBCL_DYNAMIC_SPACE_MB
export SWIMMY_HOME="${SWIMMY_HOME:-$ROOT}"

exec sbcl --dynamic-space-size "$SWIMMY_SBCL_DYNAMIC_SPACE_MB" --script "$ROOT/tools/ops/finalize_rank_report.lisp"
```

Make it executable:
```bash
chmod +x tools/ops/finalize_rank_report.sh
```

**Step 4: Run the test to verify it passes**

Run:
```bash
bash tools/tests/test_finalize_rank_report_wrapper.sh
```
Expected: PASS (exit 0).

**Step 5: Commit**

```bash
git add tools/ops/finalize_rank_report.sh tools/tests/test_finalize_rank_report_wrapper.sh
git commit -m "ops: add finalize rank report wrapper"
```

---

### Task 3: Manual Verification (Optional Smoke)

**Files:**
- None

**Step 1: Run wrapper with explicit memory (optional)**

```bash
SWIMMY_HOME=/home/swimmy/swimmy SWIMMY_SBCL_DYNAMIC_SPACE_MB=4096 tools/ops/finalize_rank_report.sh
```

Expected: report generation completes and `data/reports/evolution_factory_report.txt` timestamp updates.

**Step 2: Commit (only if step 1 reveals changes that should be committed)**

---

## Notes
- No INTERFACES changes required.
- STATE must be updated before adding the wrapper.
- If the optional smoke run modifies report files, do not commit generated outputs.
