---
name: quality-check
description: Runs strict quality gates (tests, SRP check, integrity) and auto-commits if successful.
---

# Quality Check Skill

This skill ensures the codebase maintains high standards before committing changes. It runs unit tests, system integrity checks, and complexity analysis.

## When to use
- After making ANY code changes.
- Before notifying the user of task completion.
- When the user asks to "check quality" or "verify code".
- If you are about to `notify_user` sending code for review, run this FIRST.

## Steps

### 1. Run Quality Gate (Tests + Integrity)
Execute the project's main quality gate command. This runs Lisp/Rust unit tests and environment checks.

```bash
cd /home/swimmy/swimmy && make quality-gate
```

### 2. Run Complexity & SRP Check
Scan for maintenance issues like God Classes or God Methods.

```bash
cd /home/swimmy/swimmy && python3 tools/check_srp.py
```

### 3. Check Results
- If **ANY** test fails, **STOP**. Do not commit. Fix the issues first.
- If all tests pass, proceed to commit.

### 4. Auto-Commit (If Success)
Stage and commit all changes with a timestamped message.

```bash
cd /home/swimmy/swimmy && git add -A
```

```bash
cd /home/swimmy/swimmy && git commit -m "🤖 Auto-commit: Quality Gate Passed $(date '+%Y-%m-%d %H:%M')"
```

### 5. Report
Inform the user of the result (Success/Failure).
