# CPCV Pipeline Recovery Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore CPCV validation flow by preventing KB init crashes, making legacy library loads resilient, and adding CPCV_VALIDATE handling in Guardian.

**Architecture:** Lisp loads strategies from DB + Library, so we guard against NIL/invalid entries at load time. Guardian gains a CPCV_VALIDATE handler that runs CPCV (or reports an error) and emits a CPCV_RESULT S-expression back to Lisp. Tests cover the persistence fallback and CPCV handler error-path to prevent regression.

**Tech Stack:** Common Lisp (SBCL), Rust (Guardian), ZMQ, SQLite.

---

### Task 1: Add Failing Tests for Library Load + KB Init Guards (Lisp)

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: Write failing test for #S strategy load recovery**

Add a test that writes a temp file containing a `#S(STRATEGY ...)` form (include :INDICATORS/:ENTRY/:EXIT) and asserts `swimmy.persistence:load-strategy` returns a valid strategy with the expected name.

```lisp
(deftest test-load-strategy-recovers-struct-sexp
  (let* ((tmp (format nil "/tmp/swimmy-strat-~a.lisp" (get-universal-time)))
         (content "#S(STRATEGY :NAME \"UT-STRUCT\" :INDICATORS ((SWIMMY.SCHOOL::SMA 5)) :ENTRY (SWIMMY.SCHOOL::CROSS-ABOVE CLOSE OPEN) :EXIT (> CLOSE OPEN) :RANK :LEGEND)")
         (strat nil))
    (unwind-protect
        (progn
          (with-open-file (s tmp :direction :output :if-exists :supersede)
            (format s "~a" content))
          (setf strat (swimmy.persistence:load-strategy tmp))
          (assert-true (and strat (swimmy.school:strategy-p strat)) "Should recover strategy")
          (assert-equal "UT-STRUCT" (swimmy.school:strategy-name strat)))
      (ignore-errors (delete-file tmp)))))
```

**Step 2: Run tests to verify RED**

Run: `scripts/ci-test.sh`

Expected: FAIL with the new test (load-strategy returns NIL).

**Step 3: Write failing test for NIL entries in init-knowledge-base**

```lisp
(deftest test-init-knowledge-base-skips-nil-strategies
  (let ((orig-db (symbol-function 'swimmy.school:fetch-all-strategies-from-db))
        (orig-file (symbol-function 'swimmy.persistence:load-all-strategies))
        (failed nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db)
                (lambda () (list nil (swimmy.school:make-strategy :name "UT-DB"))))
          (setf (symbol-function 'swimmy.persistence:load-all-strategies)
                (lambda () (list nil (swimmy.school:make-strategy :name "UT-FILE"))))
          (handler-case
              (swimmy.school:init-knowledge-base)
            (error () (setf failed t)))
          (assert-false failed "init-knowledge-base should not crash on NIL")
          (assert-true (every #'swimmy.school:strategy-p swimmy.school::*strategy-knowledge-base*)
                       "KB should contain only strategies"))
      (setf (symbol-function 'swimmy.school:fetch-all-strategies-from-db) orig-db)
      (setf (symbol-function 'swimmy.persistence:load-all-strategies) orig-file))))
```

**Step 4: Run tests to verify RED**

Run: `scripts/ci-test.sh`

Expected: FAIL with the new test (init-knowledge-base throws on NIL).

**Step 5: Commit**

```bash
git add src/lisp/tests.lisp
git commit -m "test: add regression coverage for library load and kb init"
```

---

### Task 2: Implement Loader Resilience + KB NIL Guard (Lisp)

**Files:**
- Modify: `src/lisp/core/persistence.lisp`
- Modify: `src/lisp/strategies/strategies.lisp`

**Step 1: Update load-strategy fallback to recover #S content**

Enhance `%read-strategy-from-string` to:
- Detect `(STRATEGY ...)` lists and attempt `(apply #'make-strategy ...)`.
- If that fails, extract core keys (`:NAME`, `:INDICATORS`, `:ENTRY`, `:EXIT`, metrics) and call `%plist->strategy` (from `school-sexp-migration.lisp`).
- Return NIL only if both attempts fail.

**Step 2: Add NIL/invalid filtering in init-knowledge-base**

After merging DB + file strategies, filter with `strategy-p` and log how many entries were dropped.

**Step 3: Run tests to verify GREEN**

Run: `scripts/ci-test.sh`

Expected: PASS (new tests now green).

**Step 4: Commit**

```bash
git add src/lisp/core/persistence.lisp src/lisp/strategies/strategies.lisp
git commit -m "fix: harden library load and skip nil kb entries"
```

---

### Task 3: Add CPCV_VALIDATE Handler in Guardian (Rust)

**Repository:** `/home/swimmy/swimmy/guardian` (separate git repo)

**Files:**
- Modify: `/home/swimmy/swimmy/guardian/src/main.rs`
- Modify: `/home/swimmy/swimmy/guardian/src/cpcv.rs`
- Create: `/home/swimmy/swimmy/guardian/src/cpcv_handler.rs` (if extracting helper)
- Modify: `/home/swimmy/swimmy/guardian/src/main.rs` tests module (or new test file)

**Step 1: Write failing Rust test for CPCV_VALIDATE handler**

Add a unit test that builds a `CpcvRequest` with a non-existent `candles_file` and asserts the handler returns a CPCV_RESULT with:
- `path_count == 0`
- `is_passed == false`
- `error` populated

**Step 2: Run test to verify RED**

Run: `cd /home/swimmy/swimmy/guardian && cargo test --release cpcv_validate_handler -- --nocapture`

Expected: FAIL (handler not implemented).

**Step 3: Implement CPCV_VALIDATE handling**

In `main.rs` command handling section:
- Detect `CPCV_VALIDATE` for JSON and S-expression inputs.
- Parse into `CpcvRequest`.
- Call `cpcv::run_cpcv_validation`.
- Build `CPCV_RESULT` alist with: `strategy_name`, `median_sharpe`, `path_count`, `passed_count`, `failed_count`, `pass_rate`, `is_passed`, and `error` (if any).
- Send CPCV_RESULT back to Lisp with `push_to_brain.send(...)` as an S-expression string.

**Step 4: Run tests to verify GREEN**

Run: `cd /home/swimmy/swimmy/guardian && cargo test --release cpcv_validate_handler -- --nocapture`

Expected: PASS.

**Step 5: Commit**

```bash
cd /home/swimmy/swimmy/guardian
git add src/main.rs src/cpcv.rs src/cpcv_handler.rs
git commit -m "feat: handle CPCV_VALIDATE and emit CPCV_RESULT"
```

---

### Task 4: Build Guardian + Restart Services

**Step 1: Build Guardian**

Run:
```bash
cd /home/swimmy/swimmy/guardian
cargo build --release
```

Expected: `target/release/guardian` updated.

**Step 2: Restart system services (requires sudo)**

```bash
sudo systemctl restart swimmy-guardian swimmy-evolution
```

**Step 3: Verify CPCV flow resumed**

Run:
```bash
cd /home/swimmy/swimmy
cat data/reports/evolution_factory_report.txt | head -40
rg -n "CPCV" logs/evolution_daemon.log | tail -40
```

Expected: CPCV queued/received counts start moving, no init-knowledge-base crashes.

---

### Task 5: Re-Check Pipeline Health Snapshot

Run:
```bash
cd /home/swimmy/swimmy
cat data/reports/oos_status.txt
python3 - <<'PY'
import sqlite3
conn = sqlite3.connect('data/memory/swimmy.db')
cur = conn.cursor()
cur.execute('SELECT status, COUNT(*) FROM oos_queue GROUP BY status ORDER BY status')
print(cur.fetchall())
PY
rg -n "CPCV_RESULT" logs/brain.log | tail -20
```

Expected: OOS queue stable, CPCV_RESULT entries appearing in logs.

