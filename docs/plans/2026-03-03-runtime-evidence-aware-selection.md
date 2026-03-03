# Runtime Evidence-Aware Selection Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace single raw-Sharpe global-best runtime selection with evidence-aware selection while preserving current order flow and rank/deployment semantics.

**Architecture:** Keep `process-category-trades` as the orchestration entrypoint, but replace candidate ordering logic with an evidence-aware scorer that evaluates gate readiness, live-edge status, evidence-adjusted quality, stability, and diversification penalty. Emit structured telemetry for selected/rejected candidates for auditability.

**Tech Stack:** Common Lisp (SBCL), existing Swimmy school execution/runtime modules, existing Lisp test harness (`src/lisp/tests.lisp`).

---

### Task 1: Add failing tests for runtime selection policy

**Files:**
- Modify: `src/lisp/tests.lisp`

1. Add tests for evidence-aware winner selection across simultaneous signals.
2. Add test for diversification penalty affecting winner choice.
3. Add test for telemetry reasons containing selected/rejected rationale.
4. Register added tests in `run-all-tests` list.

### Task 2: Implement evidence-aware scoring and telemetry in runtime selection

**Files:**
- Modify: `src/lisp/school/school-execution.lisp`

1. Add helper parameters and functions to compute candidate evidence metrics.
2. Integrate deployment gate status and live-edge guard status into candidate scoring.
3. Introduce evidence-adjusted score + recent stability + diversification penalty.
4. Keep single-winner execution path; swap only candidate selection block.
5. Emit telemetry with `selected`/`rejected` reasons and score breakdown.

### Task 3: Document runtime selection policy

**Files:**
- Modify: `docs/llm/SPEC.md`
- Modify: `docs/llm/STATE.md` (if needed for current-state delta)

1. Add concise runtime selection policy section and rationale.
2. Clarify that rank/deployment gate semantics are unchanged.

### Task 4: Verify

**Files:**
- No additional file change required.

1. Run targeted Lisp tests for new behavior.
2. Ensure red-green cycle is completed for newly added tests.
3. Summarize modified files, rationale, and residual risk.
