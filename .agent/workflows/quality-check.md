---
description: Run quality check and auto-commit if tests pass
---
# Quality Check Workflow

This workflow runs quality gate tests and auto-commits if everything passes.

## Steps

// turbo-all

1. **Run Quality Gate (Tests + Integrity Check)**
   - Runs Unit Tests (Lisp/Rust)
   - Runs System Integrity Checks (Lisp/Python/Env/JSON/Systemd)
```bash
cd /home/swimmy/swimmy && ./tools/quality_gate.sh
```


2. **Run SRP/Complexity check**
   - Scans for God Classes (>600 lines)
```bash
cd /home/swimmy/swimmy && python3 tools/check_srp.py
```

3. **If tests pass, stage all changes**
```bash
cd /home/swimmy/swimmy && git add -A
```

3. **Create auto-commit with timestamp**
```bash
cd /home/swimmy/swimmy && git commit -m "🤖 Auto-commit: Quality Gate Passed $(date '+%Y-%m-%d %H:%M')"
```

4. **Report result**
Report to the user whether the commit was successful or if there were any issues.

## Notes
- This workflow should be run after completing a set of code modifications
- If quality-gate fails, DO NOT commit - fix the issues first
- The `// turbo-all` annotation means all steps auto-run without user confirmation
