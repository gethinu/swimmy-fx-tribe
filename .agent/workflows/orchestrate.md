---
description: Phase/Progress Management (PLAN -> DO -> CHECK -> ACT)
---

# Orchestrate Workflow

This workflow defines the standard operating procedure for Swimmy development.

## Phase 1: PLAN (Planning Mode)

// turbo
1. Call `task_boundary(Mode=PLANNING)`
2. Analyze requirements -> Write `implementation_plan.md`
3. Break down tasks -> Write `task.md` (5-10 min granularity)
4. Call `notify_user` to get approval.

**DO NOT Implement yet.**

## Phase 2: DO (Execution Mode)

For each task:

// turbo
1. `task_boundary(Mode=EXECUTION, TaskStatus="Task Name")`
2. Implement using tools.
3. Update `task.md` (`[x]`).
4. Update `task_boundary` summary every ~5 tool calls.

**On Error**: Notify user immediately.

## Phase 3: CHECK (Verification Mode)

// turbo
1. `task_boundary(Mode=VERIFICATION)`
2. Run `/quality-check` workflow (if applicable) or manual tests.
3. Use `browser_subagent` if UI testing is needed.
4. Pass -> Go to Phase 4. Fail -> Go to Phase 4 (Act on failure).

## Phase 4: ACT (Improvement/Completion)

**If Failed**:
1. Analyze root cause.
2. Add fix task to `task.md`.
3. Return to Phase 2 (EXECUTION).

**If Succeeded**:
1. Write/Update `walkthrough.md`.
2. `notify_user` with completion report.
