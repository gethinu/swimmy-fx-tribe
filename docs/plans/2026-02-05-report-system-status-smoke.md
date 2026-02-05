# Report System Status Smoke Test Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a safe, environment-agnostic smoke test for `tools/report_system_status.py` via a `--dry-run` path that builds the payload without requiring Discord credentials or a live ZMQ notifier.

**Architecture:** Extend the script with a simple `--dry-run` flag to bypass webhook validation and ZMQ send while still computing stats and constructing the embed payload. Add a subprocess-based test that runs the script in dry-run mode and asserts exit code + expected output markers.

**Tech Stack:** Python 3, standard library (`subprocess`, `sys`, `os`), existing CLI script.

---

### Task 1: Add failing smoke test (RED)

**Files:**
- Create: `tools/test_report_system_status_smoke.py`

**Step 1: Write the failing test**

```python
import subprocess
import sys

def main() -> int:
    result = subprocess.run(
        [sys.executable, "tools/report_system_status.py", "--dry-run"],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert "Stats (Sharded)" in result.stdout
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
```

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_report_system_status_smoke.py`  
Expected: FAIL (script exits non-zero because `SWIMMY_DISCORD_REPORTS` is not set).

**Step 3: Commit (if desired; ask first)**  
`git add tools/test_report_system_status_smoke.py && git commit -m "test: add report_system_status dry-run smoke"`

---

### Task 2: Implement `--dry-run` path (GREEN)

**Files:**
- Modify: `tools/report_system_status.py`

**Step 1: Implement minimal flag handling**
- Detect `--dry-run` via `sys.argv`.
- Skip webhook env requirement when dry-run is true.
- Skip ZMQ send when dry-run is true.
- Still compute stats + build payload.
- Print a short confirmation and keep existing “Stats (Sharded)” output.

**Step 2: Run test to verify it passes**

Run: `python3 tools/test_report_system_status_smoke.py`  
Expected: PASS

**Step 3: Commit (if desired; ask first)**  
`git add tools/report_system_status.py && git commit -m "feat: add report_system_status --dry-run"`

---

### Task 3: Optional broader verification

**Files:** none

**Step 1: Run existing test suite (if needed)**

Run: `sbcl --script tests/test_runner.lisp`  
Expected: PASS

**Step 2: Commit (if desired; ask first)**  
`git commit -m "chore: verify tests"`

