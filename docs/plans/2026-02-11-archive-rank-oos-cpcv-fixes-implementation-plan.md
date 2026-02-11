# Archive Rank / OOS / CPCV Reporting Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Prevent accidental resurrection of `GRAVEYARD/RETIRED` strategies, stop stale OOS `sent` rows, and make CPCV `failed` counts explainable in the report.

**Architecture:** Keep DB as source of truth for counts while adding guardrails at update points. Enforce archive-rank protection in DB upsert, tighten OOS enqueue semantics to only mark sent on accepted dispatch, and split CPCV failure counters into criteria-fail vs transport/runtime fail.

**Tech Stack:** Common Lisp (Swimmy core/school), SQLite, existing Swimmy unit test framework (`deftest`).

---

### Task 1: Document state changes first

**Files:**
- Modify: `docs/llm/STATE.md`

**Step 1: Update current state bullets**
- Add archive rank protection rule and OOS dispatch accounting rule.
- Add CPCV status counter semantics (criteria vs error split).

**Step 2: Update change log**
- Add 2026-02-11 entry summarizing the behavior changes.

### Task 2: Add failing regression tests (RED)

**Files:**
- Modify: `src/lisp/tests/backtest-db-tests.lisp`
- Modify: `src/lisp/tests/school-split-tests.lisp`
- Modify: `src/lisp/tests/telemetry-tests.lisp`

**Step 1: Add archive rank regression test**
- Existing DB row `:RETIRED` + upsert with nil/active rank must remain `:RETIRED`.

**Step 2: Add OOS stale sent regression test**
- When `request-backtest` dispatch is rejected, queue row must not remain `sent`.

**Step 3: Add CPCV summary regression test**
- Summary line should expose send-failed, criteria-failed, and result-error separately.

**Step 4: Run targeted tests and confirm failures**
- Run minimal test invocation and verify new tests fail before implementation.

### Task 3: Implement minimal production fixes (GREEN)

**Files:**
- Modify: `src/lisp/school/school-db.lisp`
- Modify: `src/lisp/school/school-validation.lisp`
- Modify: `src/lisp/school/school-backtest-utils.lisp`
- Modify: `src/lisp/school/school-backtest.lisp`
- Modify: `src/lisp/core/message-dispatcher.lisp`

**Step 1: Protect archive ranks in DB upsert**
- Preserve existing DB rank when existing rank is archive and incoming rank is nil/non-archive.

**Step 2: Fix OOS enqueue/dispatch accounting**
- Treat throttle/reject as dispatch failure (queue row moves to error path, not lingering as sent).
- Keep successful/queued dispatch as sent/retry.

**Step 3: Stabilize send return contract**
- Make `send-zmq-msg`/`request-backtest` return explicit success state used by OOS logic.

**Step 4: Split CPCV failure counters**
- Track `criteria_failed` and `result_error` separately and include both in status line.

### Task 4: Verification and operational reconciliation

**Files:**
- Modify: `docs/llm/STATE.md` (if wording adjustments needed after implementation)

**Step 1: Re-run targeted tests**
- Confirm new tests pass and no touched regression tests fail.

**Step 2: Validate live DB drift**
- Run `tools/ops/reconcile_archive_db.py --dry-run` and inspect impact.

**Step 3: Apply safe reconciliation for wrong ranks**
- Run rank correction (`--no-insert`) to recover reset-looking `Retired/Graveyard` counts without massive inserts.

**Step 4: Verify report snippets**
- Confirm evolution report no longer shows archive candidates/NIL and CPCV/OOS lines are coherent.
