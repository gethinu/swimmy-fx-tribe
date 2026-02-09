# Multi-Metric Scoring Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace Sharpe-only ranking decisions with a unified composite score across ranking, breeder selection, and promotion/demotion flows using stage-specific metrics.

**Architecture:** Add a shared scoring helper (normalization + weighted composite), then wire stage-specific metric extraction into ranking, breeder, and promotion logic. Use TDD to validate score behavior and to prove Sharpe-only paths are replaced.

**Tech Stack:** Common Lisp (SBCL), Swimmy test runner (`tests/test_runner.lisp`).

### Task 1: Add Composite Scoring Helpers

**Files:**
- Modify: `src/lisp/school/school-rank-system.lisp` (or a small new helper file if already used for shared rank logic)
- Test: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add unit tests near other scoring-related tests in `src/lisp/tests.lisp`:

```lisp
(deftest test-composite-score-prefers-stable-pf-wr
  "Composite score should favor better PF/WR with acceptable DD even if Sharpe is lower"
  (let* ((fn (find-symbol "SCORE-FROM-METRICS" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "score-from-metrics exists")
    (let* ((a (list :sharpe 0.4 :profit-factor 1.8 :win-rate 0.60 :max-dd 0.08))
           (b (list :sharpe 0.8 :profit-factor 1.1 :win-rate 0.42 :max-dd 0.18))
           (score-a (funcall fn a))
           (score-b (funcall fn b)))
      (assert-true (> score-a score-b) "PF/WR/low-DD should beat Sharpe-only"))))

(deftest test-composite-score-penalizes-high-dd
  "Composite score should penalize high MaxDD even with strong Sharpe"
  (let* ((fn (find-symbol "SCORE-FROM-METRICS" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "score-from-metrics exists")
    (let* ((safe (list :sharpe 1.0 :profit-factor 1.5 :win-rate 0.55 :max-dd 0.08))
           (risky (list :sharpe 1.2 :profit-factor 1.5 :win-rate 0.55 :max-dd 0.22))
           (score-safe (funcall fn safe))
           (score-risky (funcall fn risky)))
      (assert-true (> score-safe score-risky) "High DD should reduce score"))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --script tests/test_runner.lisp
```

Expected: FAIL because `score-from-metrics` does not exist.

**Step 3: Write minimal implementation**

In `src/lisp/school/school-rank-system.lisp` (or a shared scoring module if preferred), add:

```lisp
(defparameter *score-weight-sharpe* 0.45)
(defparameter *score-weight-pf* 0.25)
(defparameter *score-weight-wr* 0.20)
(defparameter *score-weight-maxdd* 0.10)

(defun %clamp (v lo hi)
  (min hi (max lo v)))

(defun %norm (v lo hi)
  (if (<= hi lo) 0.0
      (/ (- (%clamp v lo hi) lo) (- hi lo))))

(defun score-from-metrics (metrics)
  (let* ((sharpe (or (getf metrics :sharpe) 0.0))
         (pf (or (getf metrics :profit-factor) 0.0))
         (wr (or (getf metrics :win-rate) 0.0))
         (dd (or (getf metrics :max-dd) 1.0))
         (n-sharpe (%norm sharpe 0.0 2.0))
         (n-pf (%norm pf 1.0 2.0))
         (n-wr (%norm wr 0.40 0.70))
         (n-dd (%norm dd 0.0 0.20)))
    (+ (* *score-weight-sharpe* n-sharpe)
       (* *score-weight-pf* n-pf)
       (* *score-weight-wr* n-wr)
       (* -1 *score-weight-maxdd* n-dd))))
```

**Step 4: Run tests to verify it passes**

Run:
```bash
sbcl --script tests/test_runner.lisp
```

Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/tests.lisp src/lisp/school/school-rank-system.lisp
git commit -m "feat: add composite scoring helper"
```

### Task 2: Replace Sharpe-Only Sorting in Ranking & Breeder

**Files:**
- Modify: `src/lisp/school/school-ranking.lisp`
- Modify: `src/lisp/school/school-breeder.lisp`
- Test: `src/lisp/tests.lisp`

**Step 1: Write the failing tests**

Add tests asserting composite score ordering is used:

```lisp
(deftest test-b-rank-cull-uses-composite-score
  "Rank B culling should use composite score, not Sharpe only"
  ;; Build B-rank sample with conflicting Sharpe/PF/WR/DD
  ;; Assert the weaker composite is culled even if Sharpe is higher
  )
```

```lisp
(deftest test-breeder-selection-uses-composite-score
  "Breeder selection should sort by composite score"
  ;; Stub a small KB and ensure top selection aligns with composite score
  )
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --script tests/test_runner.lisp
```

Expected: FAIL because Sharpe-only sorting still in place.

**Step 3: Write minimal implementation**

- In `cull-rank-b-pool`, change sort key from Sharpe → `score-from-metrics` with IS metrics:

```lisp
(lambda (s)
  (score-from-metrics (list :sharpe (strategy-sharpe s)
                            :profit-factor (strategy-profit-factor s)
                            :win-rate (strategy-win-rate s)
                            :max-dd (strategy-max-dd s))))
```

- In `school-breeder.lisp`, update the sorting and pool culling to use the same composite score.

**Step 4: Run tests to verify it passes**

Run:
```bash
sbcl --script tests/test_runner.lisp
```

Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/tests.lisp src/lisp/school/school-ranking.lisp src/lisp/school/school-breeder.lisp
git commit -m "feat: use composite score for ranking and breeder selection"
```

### Task 3: Replace Sharpe-Only Promotion/Demotion

**Files:**
- Modify: `src/lisp/school/school-strategy.lisp`
- Modify: `src/lisp/school/school-rank-system.lisp`
- Test: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add a test verifying promotions are driven by composite score:

```lisp
(deftest test-promotion-uses-composite-score
  "Promotion should not rely on Sharpe alone"
  ;; Construct strategy with low Sharpe but strong PF/WR/low DD and confirm promotion
  )
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --script tests/test_runner.lisp
```

Expected: FAIL because check-promotion still uses Sharpe thresholds.

**Step 3: Write minimal implementation**

In `check-promotion`, replace Sharpe thresholds with composite score thresholds:
- Define `*score-min-scout*`, `*score-min-warrior*`, `*score-min-veteran*` (defparameters)
- Use `score-from-metrics` on IS metrics

In `evaluate-a-rank-strategy`, incorporate OOS/CPCV composite score for A/S handling (skip if missing metrics).

**Step 4: Run tests to verify it passes**

Run:
```bash
sbcl --script tests/test_runner.lisp
```

Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/tests.lisp src/lisp/school/school-strategy.lisp src/lisp/school/school-rank-system.lisp
git commit -m "feat: use composite score for promotion and evaluation"
```
