# Dynamic Cost Pips Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add dynamic `cost_pips` based on bid/ask and per-symbol pip size, and use it as the canonical spread/cost input for EV-first gating.

**Architecture:** Introduce a dedicated cost utility module that defines pip-size per symbol and converts bid/ask to spread pips. Wire execution-time spread filtering to this conversion so EV cost uses a single unit system. Keep logic minimal and testable.

**Tech Stack:** Common Lisp (Swimmy Lisp), SBCL test runner.

---

### Task 1: Add cost utilities (pip-size + spread-to-pips)

**Files:**
- Create: `src/lisp/school/school-cost.lisp`
- Modify: `swimmy.asd`
- Test: `src/lisp/tests/school-v47-tests.lisp`

**Step 1: Write the failing tests**

Add to `src/lisp/tests/school-v47-tests.lisp`:

```lisp
(deftest test-pip-size-by-symbol
  (assert-true (= 0.0001 (swimmy.school:get-pip-size "EURUSD")))
  (assert-true (= 0.0001 (swimmy.school:get-pip-size "GBPUSD")))
  (assert-true (= 0.01 (swimmy.school:get-pip-size "USDJPY"))))

(deftest test-spread-pips-from-bid-ask
  ;; EURUSD: 1 pip = 0.0001
  (assert-true (= 2.0 (swimmy.school:spread-pips-from-bid-ask "EURUSD" 1.1000 1.1002)))
  ;; USDJPY: 1 pip = 0.01
  (assert-true (= 2.0 (swimmy.school:spread-pips-from-bid-ask "USDJPY" 150.00 150.02))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --script tests/test_runner.lisp`
Expected: FAIL with undefined function errors for `get-pip-size` and `spread-pips-from-bid-ask`.

**Step 3: Write minimal implementation**

Create `src/lisp/school/school-cost.lisp`:

```lisp
(in-package :swimmy.school)

(defparameter *pip-size-by-symbol*
  '(("EURUSD" . 0.0001)
    ("GBPUSD" . 0.0001)
    ("USDJPY" . 0.01))
  "Pip size by symbol for bid/ask to pips conversion.")

(defun get-pip-size (symbol)
  (or (cdr (assoc symbol *pip-size-by-symbol* :test #'string=)) 0.0001))

(defun spread-pips-from-bid-ask (symbol bid ask)
  (let ((pip (get-pip-size symbol)))
    (if (and (numberp bid) (numberp ask) (> pip 0))
        (/ (- ask bid) pip)
        0.0)))
```

Ensure `swimmy.asd` includes `school-cost.lisp` in the load order (near other school utilities).

**Step 4: Run test to verify it passes**

Run: `sbcl --script tests/test_runner.lisp`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/school/school-cost.lisp src/lisp/tests/school-v47-tests.lisp swimmy.asd
git commit -m "feat: add pip-size and spread-to-pips utilities"
```

---

### Task 2: Add dynamic cost calculation helper

**Files:**
- Modify: `src/lisp/school/school-cost.lisp`
- Test: `src/lisp/tests/school-v47-tests.lisp`

**Step 1: Write the failing test**

Add to `src/lisp/tests/school-v47-tests.lisp`:

```lisp
(deftest test-cost-pips-defaults
  ;; Default cost = spread only (no slippage/commission/swap)
  (assert-true (= 2.0 (swimmy.school:calculate-cost-pips "EURUSD" 1.1000 1.1002)))
  (assert-true (= 2.0 (swimmy.school:calculate-cost-pips "USDJPY" 150.00 150.02))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --script tests/test_runner.lisp`
Expected: FAIL with undefined function `calculate-cost-pips`.

**Step 3: Write minimal implementation**

In `src/lisp/school/school-cost.lisp`:

```lisp
(defun calculate-cost-pips (symbol bid ask &key (slippage-pips 0.0) (commission-pips 0.0) (swap-pips 0.0))
  (+ (spread-pips-from-bid-ask symbol bid ask)
     (float slippage-pips)
     (float commission-pips)
     (float swap-pips)))
```

**Step 4: Run test to verify it passes**

Run: `sbcl --script tests/test_runner.lisp`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/school/school-cost.lisp src/lisp/tests/school-v47-tests.lisp
git commit -m "feat: add dynamic cost-pips helper"
```

---

### Task 3: Wire dynamic spread filter into execution

**Files:**
- Modify: `src/lisp/school/school-execution.lisp`
- Test: `src/lisp/tests/school-v47-tests.lisp` (optional small unit test)

**Step 1: Write the failing test (optional)**

If feasible, add a unit test that calls a small helper (to be introduced) like `spread-ok-p` using known bid/ask values. If integration-level testing is too heavy, skip and rely on unit tests from Tasks 1–2.

**Step 2: Implement minimal integration**

In `src/lisp/school/school-execution.lisp`, before `verify-signal-authority` is checked in `execute-category-trade`, insert:

```lisp
(let ((spread-pips (swimmy.school:spread-pips-from-bid-ask symbol bid ask)))
  (when (> spread-pips *max-spread-pips*)
    (format t "[EXEC] ⚠️ Spread too wide: ~,2f pips > ~,2f (skip)~%" spread-pips *max-spread-pips*)
    (return-from execute-category-trade nil)))
```

**Step 3: Run tests**

Run: `sbcl --script tests/test_runner.lisp`
Expected: PASS.

**Step 4: Commit**

```bash
git add src/lisp/school/school-execution.lisp
git commit -m "feat: apply dynamic spread filter using pip-size"
```

---

### Task 4: Update canonical docs (SPEC)

**Files:**
- Modify: `docs/llm/SPEC.md`

**Step 1: Update spec**

In the Pattern Similarity Gate section, add a short note that `cost_pips` is computed from bid/ask using per-symbol pip-size and used by EV gating/spread filters.

**Step 2: Commit**

```bash
git add docs/llm/SPEC.md
git commit -m "docs: document dynamic cost-pips" 
```

---

## Test Plan
- `sbcl --script tests/test_runner.lisp`

