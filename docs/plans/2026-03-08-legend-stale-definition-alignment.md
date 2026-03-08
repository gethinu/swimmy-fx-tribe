# Legend Stale Definition Alignment Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align stale split Legend Lisp definitions with the canonical `strategies_v3.lisp` definitions for the reviewed Legend ports so source files stop disagreeing about live logic.

**Architecture:** Keep `strategies_v3.lisp` as the canonical behavioral source and treat split files under `src/lisp/strategies/` as compatibility mirrors. Add a focused regression test that compares selected split definitions against canonical definitions, then update the stale split definition and document the contract in `STATE` before the code change.

**Tech Stack:** Common Lisp, SBCL/ASDF, Swimmy custom test framework, Markdown docs

---

### Task 1: Add a failing regression test for Legend definition drift

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add a test that loads `strategies_v3.lisp`, `src/lisp/strategies/strategies-trend.lisp`, and `src/lisp/strategies/strategies-scalp.lisp`, then compares the selected strategy definitions for:
- `MACD-Above-Zero-Cross`
- `Pullback-Breakout`

Compare:
- indicators
- entry
- exit
- sl
- tp
- timeframe

**Step 2: Run test to verify it fails**

Run:

```bash
sbcl --non-interactive --eval '(load "swimmy.asd")' --eval '(asdf:load-system :swimmy)' --eval '(quit (if (swimmy.tests::test-legend-split-definitions-match-canonical) 0 1))'
```

Expected:
- `MACD-Above-Zero-Cross` fails because split definition still uses stale MACD params.

**Step 3: Write minimal implementation**

Update the stale split definition in `src/lisp/strategies/strategies-trend.lisp` to match canonical `strategies_v3.lisp`.

**Step 4: Run test to verify it passes**

Run the same SBCL command and expect exit `0`.

### Task 2: Document the canonical-source contract

**Files:**
- Modify: `docs/llm/STATE.md`

**Step 1: Record contract before implementation**

Add a short state entry that:
- states `strategies_v3.lisp` remains the canonical Legend source for reviewed MT5 ports
- notes split files are compatibility mirrors and must not drift on reviewed definitions
- states no `INTERFACES.md` change is required

### Task 3: Verify focused scope

**Files:**
- Verify only

**Step 1: Re-run focused regression test**

Use the same SBCL command from Task 1.

**Step 2: Inspect diff**

Run:

```bash
git diff -- docs/llm/STATE.md src/lisp/strategies/strategies-trend.lisp src/lisp/tests.lisp
```

Expected:
- only the contract note, regression test, and stale MACD definition alignment appear
