# System Audit Automation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Provide a system-level `tools/system_audit.sh` that runs audits and performs safe auto-repair (daemon-reload + enable + restart), with dry-run support and reliable SBCL memory settings.

**Architecture:** A Bash orchestrator runs systemd health checks and audit scripts in sequence, collects pass/warn/fail results, writes a summary log, and returns structured exit codes. It sources `tools/sbcl_env.sh` to standardize SBCL dynamic space size for deep audits.

**Tech Stack:** Bash, systemd (system scope via `sudo systemctl`), Python, SBCL.

---

### Task 1: Update STATE doc before implementation

**Files:**
- Modify: `docs/llm/STATE.md`

**Step 1: Update STATE**
Add a bullet documenting `tools/system_audit.sh`, system-scope operation, auto-repair behavior, and `DRY_RUN=1` behavior.

**Step 2: Commit**
```bash
git add docs/llm/STATE.md
git commit -m "docs: document system audit automation"
```

---

### Task 2: Add failing test for system_audit

**Files:**
- Create: `tools/test_system_audit.sh`

**Step 1: Write the failing test**
```bash
#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT="$ROOT/tools/system_audit.sh"

if [[ ! -x "$SCRIPT" ]]; then
  echo "Missing $SCRIPT" >&2
  exit 1
fi

help_output="$($SCRIPT --help)"
echo "$help_output" | grep -q "Usage:" || { echo "Missing usage" >&2; exit 1; }

run_output="$(DRY_RUN=1 $SCRIPT)"
echo "$run_output" | grep -q "Summary" || { echo "Missing summary" >&2; exit 1; }
```

**Step 2: Run test to verify it fails**
Run: `bash tools/test_system_audit.sh`
Expected: FAIL with "Missing .../tools/system_audit.sh"

---

### Task 3: Implement system_audit.sh (minimal)

**Files:**
- Create: `tools/system_audit.sh`

**Step 1: Write minimal implementation**
```bash
#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOG_DIR="$ROOT/logs"
LOG_FILE="$LOG_DIR/system_audit.log"

mkdir -p "$LOG_DIR"

usage() {
  cat <<'USAGE'
Usage: tools/system_audit.sh [--help]
Environment:
  DRY_RUN=1  Skip auto-repair steps
  SWIMMY_AUDIT_SERVICES="svc1 svc2"  Override default services
USAGE
}

if [[ "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

services_default=(swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-notifier)
if [[ -n "${SWIMMY_AUDIT_SERVICES:-}" ]]; then
  # shellcheck disable=SC2206
  services=($SWIMMY_AUDIT_SERVICES)
else
  services=("${services_default[@]}")
fi

log() {
  printf '%s %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*" | tee -a "$LOG_FILE"
}

warn_count=0
fail_count=0

mark_warn() { warn_count=$((warn_count + 1)); }
mark_fail() { fail_count=$((fail_count + 1)); }

summary() {
  echo "Summary: FAIL=$fail_count WARN=$warn_count"
}

log "[AUDIT] Starting system audit (system scope)"

# systemctl status (best effort)
if sudo -n true 2>/dev/null; then
  for svc in "${services[@]}"; do
    if ! sudo systemctl status "$svc" >/dev/null 2>&1; then
      log "[WARN] service not healthy: $svc"
      mark_warn
    fi
  done
else
  log "[WARN] sudo not available; skipping systemctl status"
  mark_warn
fi

# auto-repair
if [[ "${DRY_RUN:-0}" == "1" ]]; then
  log "[AUDIT] DRY_RUN=1; skipping auto-repair"
else
  if sudo -n true 2>/dev/null; then
    log "[REPAIR] daemon-reload"
    sudo systemctl daemon-reload || mark_fail

    for svc in "${services[@]}"; do
      log "[REPAIR] enable $svc"
      sudo systemctl enable "$svc" || mark_fail
      log "[REPAIR] restart $svc"
      sudo systemctl restart "$svc" || mark_fail
    done
  else
    log "[FAIL] sudo not available for auto-repair"
    mark_fail
  fi
fi

summary

if [[ $fail_count -gt 0 ]]; then
  exit 1
fi
if [[ $warn_count -gt 0 ]]; then
  exit 2
fi
exit 0
```

**Step 2: Run test to verify it passes**
Run: `bash tools/test_system_audit.sh`
Expected: PASS

**Step 3: Commit**
```bash
git add tools/system_audit.sh tools/test_system_audit.sh
git commit -m "feat: add system audit automation script"
```

---

### Task 4: Expand system_audit to full workflow

**Files:**
- Modify: `tools/system_audit.sh`

**Step 1: Add audit steps**
Implement:
- `python3 tools/dashboard.py`
- `tail -n 200 logs/notifier.log`
- `python3 tools/test_notifier_direct.py`
- `sbcl --dynamic-space-size "$SWIMMY_SBCL_DYNAMIC_SPACE_MB" --script tools/broadcast_test_v2.lisp`
- `tail -n 120 data/reports/evolution_factory_report.txt`
- `sbcl --dynamic-space-size "$SWIMMY_SBCL_DYNAMIC_SPACE_MB" --script tools/integrity_audit.lisp`
- `sbcl --dynamic-space-size "$SWIMMY_SBCL_DYNAMIC_SPACE_MB" --script tools/deep_audit.lisp`

Each step should mark WARN/FAIL on non-zero exit, but continue.

**Step 2: Run test to verify it passes**
Run: `bash tools/test_system_audit.sh`
Expected: PASS

**Step 3: Commit**
```bash
git add tools/system_audit.sh
git commit -m "feat: run full system audit pipeline"
```
