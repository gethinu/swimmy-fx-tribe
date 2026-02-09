# CPCV Median Gate for S-Rank Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Use IS Sharpe as the only IS gate for S-Rank and evaluate PF/WR/MaxDD using CPCV median metrics, with reporting and selection/voting pressure aligned to PF/MaxDD.

**Architecture:** Guardian emits CPCV median metrics in `CPCV_RESULT`; Lisp persists them, uses them in S gating and reporting, and applies PF/MaxDD pressure in selection and voting.

**Tech Stack:** Common Lisp (SBCL/ASDF), Rust (Guardian), SQLite, ZeroMQ S-expressions.

---

### Task 1: Align Docs and Interface Schema

**Files:**
- Modify: `docs/llm/SPEC.md`
- Modify: `docs/llm/INTERFACES.md`
- Modify: `docs/llm/STATE.md`

**Step 1: Update SPEC S-Rank definition**

```markdown
- **S-RANK**: **IS Sharpe ≥ 0.5** + CPCV検証合格（実弾許可、PF/WR/MaxDDはCPCV中央値で判定）
```

```markdown
| **S-Rank** | Verified Elite | **IS Sharpe ≥ 0.5** | **CPCV** ... - Median Sharpe ≥ 0.5 - Median PF ≥ 1.5 - Median WR ≥ 45% - Median MaxDD < 15% - Pass Rate ≥ 50% |
```

**Step 2: Add CPCV_VALIDATE request schema and CPCV_RESULT fields**

Add to `docs/llm/INTERFACES.md` near CPCV definitions:

```lisp
**CPCV_VALIDATE (Request, S-Expression)**:
((action . "CPCV_VALIDATE")
 (strategy_name . "Volvo-Scalp-Gen0")
 (symbol . "USDJPY")
 (candles_file . "/path/to/USDJPY_M1.csv")
 (request_id . "RID-123")
 (strategy_params . ((name . "Volvo-Scalp-Gen0") (sma_short . 10) ...)))
```

Extend CPCV_RESULT example:

```lisp
(median_pf . 1.52)
(median_wr . 0.47)
(median_maxdd . 0.12)
```

**Step 3: STATE decision note**

Add to `docs/llm/STATE.md` Decision section:

```markdown
- **S判定ルール**: **IS Sharpe ≥ 0.5** を必須とし、**PF/WR/MaxDDはCPCV中央値**（`median_pf/median_wr/median_maxdd`）で最終判定する。CPCV gateは `median_sharpe ≥ 0.5` と `pass_rate ≥ 50%` を含む。
```

**Step 4: Commit docs**

```bash
git add docs/llm/SPEC.md docs/llm/INTERFACES.md docs/llm/STATE.md
git commit -m "docs: define S gate with CPCV medians"
```

### Task 2: Guardian CPCV_RESULT Payload Expansion (guardian repo)

**Files:**
- Modify: `/home/swimmy/swimmy/guardian/src/main.rs`

**Step 1: Extend CpcvResultPayload**

```rust
struct CpcvResultPayload {
    strategy_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    request_id: Option<String>,
    median_sharpe: f64,
    median_pf: f64,
    median_wr: f64,
    median_maxdd: f64,
    path_count: usize,
    passed_count: usize,
    failed_count: usize,
    pass_rate: f64,
    is_passed: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}
```

**Step 2: Populate medians in `cpcv_payload_from_aggregate` and error path**

```rust
median_pf: agg.median_pf,
median_wr: agg.median_wr,
median_maxdd: agg.median_maxdd,
```

Error path:

```rust
median_pf: 0.0,
median_wr: 0.0,
median_maxdd: 0.0,
```

**Step 3: Include medians in `cpcv_result_to_sexp`**

```rust
parts.push(format!("(median_pf . {})", result.median_pf));
parts.push(format!("(median_wr . {})", result.median_wr));
parts.push(format!("(median_maxdd . {})", result.median_maxdd));
```

**Step 4: Update Guardian tests**

```rust
assert_eq!(payload.median_pf, agg.median_pf);
assert_eq!(payload.median_wr, agg.median_wr);
assert_eq!(payload.median_maxdd, agg.median_maxdd);
```

Add a test to assert SEXP includes new fields:

```rust
let sexp = cpcv_result_to_sexp(&payload);
assert!(sexp.contains("median_pf"));
assert!(sexp.contains("median_wr"));
assert!(sexp.contains("median_maxdd"));
```

**Step 5: Run tests**

```bash
cd /home/swimmy/swimmy/guardian
cargo test
```

Expected: PASS.

**Step 6: Commit Guardian change**

```bash
git add src/main.rs
git commit -m "feat: include CPCV median pf/wr/maxdd"
```

### Task 3: Lisp Strategy Fields + DB Schema

**Files:**
- Modify: `src/lisp/dsl.lisp`
- Modify: `src/lisp/packages-school.lisp`
- Modify: `src/lisp/school/school-db.lisp`

**Step 1: Add strategy fields**

```lisp
(cpcv-median-sharpe 0.0)
(cpcv-median-pf 0.0)
(cpcv-median-wr 0.0)
(cpcv-median-maxdd 0.0)
(cpcv-pass-rate 0.0)
```

Update `defstrategy` to accept the new keys and set them.

**Step 2: Export accessors**

Add to `src/lisp/packages-school.lisp`:

```lisp
#:strategy-cpcv-median-pf
#:strategy-cpcv-median-wr
#:strategy-cpcv-median-maxdd
```

**Step 3: Extend DB schema and migrations**

In `init-db`:

```sql
cpcv_median_pf REAL,
cpcv_median_wr REAL,
cpcv_median_maxdd REAL,
```

Add ALTER TABLE lines:

```lisp
(execute-non-query "ALTER TABLE strategies ADD COLUMN cpcv_median_pf REAL")
(execute-non-query "ALTER TABLE strategies ADD COLUMN cpcv_median_wr REAL")
(execute-non-query "ALTER TABLE strategies ADD COLUMN cpcv_median_maxdd REAL")
```

**Step 4: Upsert and fetch**

Extend INSERT/REPLACE column list and values with the new fields:

```lisp
(or (strategy-cpcv-median-pf strat) 0.0)
(or (strategy-cpcv-median-wr strat) 0.0)
(or (strategy-cpcv-median-maxdd strat) 0.0)
```

Update SELECTs to include the new columns and map them into the strategy object.

**Step 5: Commit DB/schema changes**

```bash
git add src/lisp/dsl.lisp src/lisp/packages-school.lisp src/lisp/school/school-db.lisp
git commit -m "feat: store CPCV median pf/wr/maxdd"
```

### Task 4: CPCV_RESULT Ingestion + S Gate Logic

**Files:**
- Modify: `src/lisp/core/message-dispatcher.lisp`
- Modify: `src/lisp/school/school-validation.lisp`
- Modify: `src/lisp/school/school-rank-system.lisp`

**Step 1: Parse CPCV_RESULT fields**

Add in `message-dispatcher.lisp` CPCV_RESULT handling:

```lisp
(median-pf (%result-val-normalized result '(median_pf median-pf) 0.0))
(median-wr (%result-val-normalized result '(median_wr median-wr) 0.0))
(median-maxdd (%result-val-normalized result '(median_maxdd median-maxdd) 0.0))
```

Include in `result-plist` and store on strategy:

```lisp
(setf (strategy-cpcv-median-pf strat) median-pf)
(setf (strategy-cpcv-median-wr strat) median-wr)
(setf (strategy-cpcv-median-maxdd strat) median-maxdd)
```

**Step 2: Update S criteria in rank system**

In `school-rank-system.lisp`:

```lisp
(:S :sharpe-min 0.5
    :cpcv-min 0.5 :cpcv-pass-min 0.5
    :cpcv-pf-min 1.5 :cpcv-wr-min 0.45 :cpcv-maxdd-max 0.15)
```

Update `check-rank-criteria` so that for `:S`, PF/WR/MaxDD are evaluated using CPCV medians:

```lisp
((eq target-rank :S)
 (and (>= sharpe (getf criteria :sharpe-min 0))
      (or (not include-cpcv)
          (and (>= (or (strategy-cpcv-median-sharpe strategy) 0.0) (getf criteria :cpcv-min 0))
               (>= (or (strategy-cpcv-pass-rate strategy) 0.0) (getf criteria :cpcv-pass-min 0))
               (>= (or (strategy-cpcv-median-pf strategy) 0.0) (getf criteria :cpcv-pf-min 0))
               (>= (or (strategy-cpcv-median-wr strategy) 0.0) (getf criteria :cpcv-wr-min 0))
               (< (or (strategy-cpcv-median-maxdd strategy) 1.0) (getf criteria :cpcv-maxdd-max 1.0))))))
```

**Step 3: Update validation flow**

In `validate-for-s-rank-promotion`:

- Pre-CPCV check only **IS Sharpe ≥ 0.5**.
- On CPCV result, persist median PF/WR/MaxDD and use updated criteria.

**Step 4: Commit S gate changes**

```bash
git add src/lisp/core/message-dispatcher.lisp src/lisp/school/school-validation.lisp src/lisp/school/school-rank-system.lisp
git commit -m "feat: use CPCV medians for S gate"
```

### Task 5: CPCV Failure Reasons in Evolution Report

**Files:**
- Modify: `src/lisp/school/school-validation.lisp`
- Modify: `src/lisp/school/school-narrative.lisp`

**Step 1: Add CPCV median failure counts**

Add a helper in `school-validation.lisp`:

```lisp
(defun cpcv-median-failure-counts (strategies)
  (let* ((criteria (get-rank-criteria :S))
         (total 0) (pf 0) (wr 0) (maxdd 0))
    (dolist (s (or strategies '()))
      (when (and (strategy-cpcv-median-sharpe s)
                 (> (strategy-cpcv-median-sharpe s) 0.0))
        (incf total)
        (when (< (or (strategy-cpcv-median-pf s) 0.0) (getf criteria :cpcv-pf-min 1.5)) (incf pf))
        (when (< (or (strategy-cpcv-median-wr s) 0.0) (getf criteria :cpcv-wr-min 0.45)) (incf wr))
        (when (>= (or (strategy-cpcv-median-maxdd s) 1.0) (getf criteria :cpcv-maxdd-max 0.15)) (incf maxdd))))
    (list :total total :pf pf :wr wr :maxdd maxdd)))
```

**Step 2: Add report snippet**

In `school-narrative.lisp`, extend CPCV section:

```lisp
(let* ((counts (cpcv-median-failure-counts a-rank-db))
       (failure-line (format nil "CPCV Median Failures: pf<1.5=~d wr<0.45=~d maxdd>=0.15=~d total=~d"
                             (getf counts :pf 0) (getf counts :wr 0) (getf counts :maxdd 0) (getf counts :total 0))))
  (format nil "~a~%~a" cpcv-snippet failure-line))
```

**Step 3: Update S-Rank label text**

Change S-Rank line in report to:

```markdown
S-Rank (IS Sharpe≥0.5 + CPCV median PF/WR/MaxDD + pass_rate)
```

**Step 4: Commit report updates**

```bash
git add src/lisp/school/school-validation.lisp src/lisp/school/school-narrative.lisp
git commit -m "feat: report CPCV median gate failures"
```

### Task 6: Selection/Breeding/Voting PF/MaxDD Pressure

**Files:**
- Modify: `src/lisp/school/school-constants.lisp`
- Modify: `src/lisp/school/school-breeder.lisp`
- Modify: `src/lisp/school/school-voting.lisp`

**Step 1: Add scoring constants**

```lisp
(defparameter *selection-score-sharpe-weight* 0.5)
(defparameter *selection-score-pf-weight* 0.3)
(defparameter *selection-score-maxdd-weight* 0.2)
```

**Step 2: Add composite score helper**

```lisp
(defun strategy-selection-score (s)
  (let* ((sh (or (strategy-sharpe s) 0.0))
         (pf (or (strategy-profit-factor s) 0.0))
         (dd (or (strategy-max-dd s) 1.0)))
    (+ (* *selection-score-sharpe-weight* sh)
       (* *selection-score-pf-weight* pf)
       (* *selection-score-maxdd-weight* (- 1.0 dd)))))
```

**Step 3: Apply in breeder sorting/culling**

Replace Sharpe-only sorting with:

```lisp
: key #'strategy-selection-score
```

**Step 4: Apply in voting weights**

Use composite score to scale weight:

```lisp
(let* ((score (strategy-selection-score strat))
       (base-weight 1.0))
  (cond
    ((> score 1.0) (* base-weight 1.5))
    ((> score 0.3) (* base-weight 1.2))
    ((< score -0.3) (* base-weight 0.5))
    (t base-weight)))
```

**Step 5: Commit selection/voting changes**

```bash
git add src/lisp/school/school-constants.lisp src/lisp/school/school-breeder.lisp src/lisp/school/school-voting.lisp
git commit -m "feat: add PF/MaxDD pressure to selection and voting"
```

### Task 7: Tests for CPCV Medians and S Gate

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/tests/backtest-db-tests.lisp`

**Step 1: Update rank criteria tests**

Extend `test-check-rank-criteria-requires-cpcv-pass-rate` to include median PF/WR/MaxDD fields and ensure failure when below thresholds.

**Step 2: Add a positive S criteria test**

```lisp
(deftest test-check-rank-criteria-cpcv-medians-pass
  (let ((strat (swimmy.school:make-strategy :name "UT-S-OK"
                                            :rank :A
                                            :sharpe 0.6
                                            :cpcv-median-sharpe 0.6
                                            :cpcv-pass-rate 0.6
                                            :cpcv-median-pf 1.6
                                            :cpcv-median-wr 0.5
                                            :cpcv-median-maxdd 0.12)))
    (assert-true (swimmy.school::check-rank-criteria strat :S))))
```

**Step 3: Update DB tests**

In `backtest-db-tests.lisp`, add the new CPCV median fields to `metrics` and assert they persist.

**Step 4: Run Lisp tests**

```bash
sbcl --script tests/test_runner.lisp
```

Expected: PASS.

**Step 5: Commit tests**

```bash
git add src/lisp/tests.lisp src/lisp/tests/backtest-db-tests.lisp
git commit -m "test: cover CPCV medians and S gate"
```

### Task 8: Deprecate Old Architecture Doc

**Files:**
- Modify: `doc/SYSTEM_ARCHITECTURE.md`

**Step 1: Replace with redirect stub**

```markdown
# SYSTEM_ARCHITECTURE.md (Deprecated)

This document is deprecated. The source of truth is:

- `docs/llm/ARCHITECTURE.md`
```

**Step 2: Commit doc cleanup**

```bash
git add doc/SYSTEM_ARCHITECTURE.md
git commit -m "docs: deprecate legacy system architecture doc"
```

### Task 9: Final Verification

**Files:**
- Verify: `docs/llm/SPEC.md`
- Verify: `docs/llm/INTERFACES.md`
- Verify: `docs/llm/STATE.md`

**Step 1: Run targeted Python tests**

```bash
pytest tests/test_backtest_service_request_id.py -q
```

**Step 2: Run Lisp tests**

```bash
sbcl --script tests/test_runner.lisp
```

**Step 3: Summarize migration plan**

- Deploy Guardian changes and restart `swimmy-guardian` and `swimmy-backtest`.
- Restart Lisp services (`swimmy-brain`, `swimmy-school`).
- Trigger CPCV batch or wait for next cycle to populate new medians.
