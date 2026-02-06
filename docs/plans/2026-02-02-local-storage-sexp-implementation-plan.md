# Local Storage S-expression Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Migrate local storage (backtest cache, telemetry, live status) to S-expression-only with atomic writes, shared Python parser, and minimal tests.

**Architecture:** Lisp writes `.sexp` files atomically and reads them via safe S-expression reader; Python report tools and Discord bot read `.sexp` through a shared parser. A one-shot migration script converts existing JSON to S-expression and backs up JSON.

**Tech Stack:** Common Lisp (SBCL), Python 3, ZeroMQ tools, SQLite unchanged.

---

### Task 1: Update authoritative docs before code

**Files:**
- Modify: `docs/llm/SPEC.md`
- Modify: `docs/llm/STATE.md`
- (If needed) Modify: `docs/llm/INTERFACES.md`

**Step 1: Update SPEC Local Storage policy**
Edit `docs/llm/SPEC.md` Local Storage section to:
```
- Local Storage: backtest_cache/system_metrics/live_status are S-expression only (.sexp). JSON is not read after migration.
```

**Step 2: Update STATE decision + next action**
Edit `docs/llm/STATE.md` decision to explicitly state immediate S-expression-only and filename changes, and update next action accordingly.

**Step 3: Verify no interface change required**
If no ZMQ/HTTP interface change, leave `docs/llm/INTERFACES.md` untouched; otherwise note rationale.

**Step 4: Commit**
```
git add docs/llm/SPEC.md docs/llm/STATE.md
git commit -m "docs: finalize local storage sexp-only policy"
```

---

### Task 2: Add Lisp S-expression atomic IO helpers

**Files:**
- Create: `src/lisp/core/sexp-io.lisp`
- Modify: `src/lisp/packages.lisp`
- Test: `src/lisp/tests/local-storage-sexp-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write failing Lisp test (RED)**
Create `src/lisp/tests/local-storage-sexp-tests.lisp`:
```lisp
(in-package :swimmy.tests)

(deftest test-sexp-io-roundtrip
  (let* ((tmp (merge-pathnames "sexp_io_test.sexp" #P"/tmp/"))
         (payload '((schema_version . 1) (name . "X") (value . 42))))
    (unwind-protect
        (progn
          (swimmy.core:write-sexp-atomic tmp payload)
          (let ((read (swimmy.core:read-sexp-file tmp :package :swimmy.school)))
            (assert-equal payload read)))
      (when (probe-file tmp) (delete-file tmp)))))
```
Add this test to `run-all-tests` list in `src/lisp/tests.lisp`.

**Step 2: Run tests to confirm failure**
Run: `./ci-test.sh`
Expected: FAIL with undefined `write-sexp-atomic` or `read-sexp-file`.

**Step 3: Implement minimal helper (GREEN)**
Create `src/lisp/core/sexp-io.lisp`:
```lisp
(in-package :swimmy.core)

(defun write-sexp-atomic (path form)
  (let* ((tmp (merge-pathnames (format nil "~a.tmp" (pathname-name path)) path)))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-readably* t)
              (*print-pretty* nil)
              (*print-case* :downcase))
          (write form :stream out))))
    (rename-file tmp path)))

(defun read-sexp-file (path &key (package :swimmy.main))
  (when (probe-file path)
    (let ((content (uiop:read-file-string path)))
      (swimmy.core:safe-read-sexp content :package package))))
```
Export in `src/lisp/packages.lisp`:
```
#:write-sexp-atomic
#:read-sexp-file
```

**Step 4: Run tests to confirm green**
Run: `./ci-test.sh`
Expected: PASS.

**Step 5: Commit**
```
git add src/lisp/core/sexp-io.lisp src/lisp/packages.lisp src/lisp/tests/local-storage-sexp-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add sexp atomic IO helpers"
```

---

### Task 3: Convert backtest cache to S-expression

**Files:**
- Modify: `src/lisp/school/school-backtest-utils.lisp`
- Test: `src/lisp/tests/local-storage-sexp-tests.lisp`

**Step 1: Write failing test (RED)**
Extend `local-storage-sexp-tests.lisp`:
```lisp
(deftest test-backtest-cache-sexp
  (let* ((tmp (merge-pathnames "backtest_cache.sexp" #P"/tmp/"))
         (swimmy.school::*backtest-cache-file* (namestring tmp))
         (swimmy.school::*backtest-cache* (make-hash-table :test 'equal)))
    (setf (gethash "StratA" swimmy.school::*backtest-cache*)
          (list :timestamp 123 :result '((sharpe . 1.0) (trades . 10))))
    (swimmy.school::save-backtest-cache)
    (clrhash swimmy.school::*backtest-cache*)
    (swimmy.school::load-backtest-cache)
    (assert-true (gethash "StratA" swimmy.school::*backtest-cache*))
    (when (probe-file tmp) (delete-file tmp))))
```

**Step 2: Run tests to confirm failure**
Run: `./ci-test.sh`
Expected: FAIL because JSON code still used.

**Step 3: Implement minimal change (GREEN)**
In `school-backtest-utils.lisp`:
- Set `*backtest-cache-file*` to `data/backtest_cache.sexp`.
- Replace JSON parse with:
```lisp
(let ((sexp (swimmy.core:read-sexp-file *backtest-cache-file* :package :swimmy.school)))
  (dolist (entry (cdr (assoc 'entries sexp))) ...))
```
- Replace JSON write with:
```lisp
(swimmy.core:write-sexp-atomic *backtest-cache-file*
  `((schema_version . 1)
    (entries . ,entries-list)))
```

**Step 4: Run tests to confirm green**
Run: `./ci-test.sh`
Expected: PASS.

**Step 5: Commit**
```
git add src/lisp/school/school-backtest-utils.lisp src/lisp/tests/local-storage-sexp-tests.lisp
git commit -m "feat: store backtest cache as sexp"
```

---

### Task 4: Convert telemetry and live status to S-expression

**Files:**
- Modify: `src/lisp/school/school-telemetry.lisp`
- Modify: `src/lisp/shell/notifications.lisp`
- Test: `src/lisp/tests/local-storage-sexp-tests.lisp`

**Step 1: Write failing tests (RED)**
Add tests:
```lisp
(deftest test-telemetry-sexp
  (let* ((tmp (merge-pathnames "system_metrics.sexp" #P"/tmp/"))
         (swimmy.school::*telemetry-file* (namestring tmp)))
    (swimmy.school::save-telemetry-sexp (list :heap 1 :strategy-count 2))
    (let ((data (swimmy.core:read-sexp-file tmp :package :swimmy.school)))
      (assert-equal 1 (cdr (assoc 'schema_version data))))
    (when (probe-file tmp) (delete-file tmp))))

(deftest test-live-status-sexp
  (let* ((tmp (merge-pathnames "live_status.sexp" #P"/tmp/"))
         (swimmy.shell::*live-status-path* (namestring tmp)))
    (swimmy.shell::save-live-status)
    (let ((data (swimmy.core:read-sexp-file tmp :package :swimmy.main)))
      (assert-true (assoc 'daily_pnl data)))
    (when (probe-file tmp) (delete-file tmp))))
```

**Step 2: Run tests to confirm failure**
Run: `./ci-test.sh`
Expected: FAIL (functions not updated).

**Step 3: Implement minimal change (GREEN)**
- In `school-telemetry.lisp`, set `*telemetry-file*` to `data/system_metrics.sexp` and replace `save-telemetry-json` with `save-telemetry-sexp` using `write-sexp-atomic`.
- In `notifications.lisp`, set `*live-status-path*` to `.opus/live_status.sexp` and replace JSON output with an alist + `write-sexp-atomic`.

**Step 4: Run tests to confirm green**
Run: `./ci-test.sh`
Expected: PASS.

**Step 5: Commit**
```
git add src/lisp/school/school-telemetry.lisp src/lisp/shell/notifications.lisp src/lisp/tests/local-storage-sexp-tests.lisp
git commit -m "feat: write telemetry and live status as sexp"
```

---

### Task 5: Python S-expression parser + report tool updates

**Files:**
- Create: `src/python/sexp_utils.py`
- Modify: `tools/report_backtest_summary.py`
- Modify: `tools/report_status.py`
- Modify: `src/python/discord_bot.py`
- Test: `tools/test_sexp_utils.py`

**Step 1: Write failing Python test (RED)**
Create `tools/test_sexp_utils.py`:
```python
from sexp_utils import parse_sexp_alist, parse_sexp_list

assert parse_sexp_alist("((a . 1) (b . 2))")["a"] == 1
assert parse_sexp_list("((name . \"X\") (name . \"Y\"))")[0]["name"] == "X"
try:
    parse_sexp_alist("(")
    raise AssertionError("expected parse error")
except Exception:
    pass
print("OK")
```
Run: `python3 tools/test_sexp_utils.py`
Expected: FAIL (module missing).

**Step 2: Implement minimal parser (GREEN)**
Create `src/python/sexp_utils.py` with tokenizer + recursive parser supporting list, dotted pair, strings, numbers, booleans. Provide:
```python
def parse_sexp(text): ...
def parse_sexp_alist(text): ...
def parse_sexp_list(text): ...
def load_sexp_alist(path): ...
def load_sexp_list(path): ...
```

**Step 3: Update report scripts**
- `report_backtest_summary.py`: read `data/backtest_cache.sexp` via `load_sexp_list`.
- `report_status.py`: read `data/backtest_cache.sexp` and `data/system_metrics.sexp` via parser.
- `discord_bot.py`: read `.opus/live_status.sexp` via parser.

**Step 4: Run tests**
Run: `python3 tools/test_sexp_utils.py`
Expected: PASS.

**Step 5: Commit**
```
git add src/python/sexp_utils.py tools/test_sexp_utils.py tools/report_backtest_summary.py tools/report_status.py src/python/discord_bot.py
git commit -m "feat: python sexp parser and report updates"
```

---

### Task 6: Migration script + minimal smoke test

**Files:**
- Create: `tools/migrate_local_storage_to_sexp.py`
- Test: `tools/test_migrate_local_storage_to_sexp.py`

**Step 1: Write failing test (RED)**
Create `tools/test_migrate_local_storage_to_sexp.py`:
```python
import json, tempfile, os
from migrate_local_storage_to_sexp import migrate_file

with tempfile.TemporaryDirectory() as d:
    src = os.path.join(d, "backtest_cache.sexp")
    with open(src, "w") as f:
        json.dump([{"name": "A", "timestamp": 1, "result": {"sharpe": 0.1}}], f)
    out = migrate_file(src)
    assert out.endswith(".sexp")
    assert os.path.exists(out)
print("OK")
```
Run: `python3 tools/test_migrate_local_storage_to_sexp.py`
Expected: FAIL (module missing).

**Step 2: Implement script (GREEN)**
Create `tools/migrate_local_storage_to_sexp.py` with:
- `migrate_file(path_json) -> path_sexp`
- JSON load, build sexp payload with `(schema_version . 1)`
- Write atomically
- Backup JSON to `.json.bak.YYYYMMDDHHMM`

**Step 3: Run tests**
Run: `python3 tools/test_migrate_local_storage_to_sexp.py`
Expected: PASS.

**Step 4: Commit**
```
git add tools/migrate_local_storage_to_sexp.py tools/test_migrate_local_storage_to_sexp.py
git commit -m "feat: migrate local storage json to sexp"
```

---

### Task 7: Final verification

**Step 1: Run full test suite**
Run: `./ci-test.sh`
Expected: PASS.

**Step 2: Manual smoke (optional)**
Run:
```
python3 tools/report_status.py
python3 tools/report_backtest_summary.py
```
Expected: no crashes when `.sexp` files exist.

**Step 3: Commit (if any remaining)**
```
git status --short
```
Commit any remaining changes.
