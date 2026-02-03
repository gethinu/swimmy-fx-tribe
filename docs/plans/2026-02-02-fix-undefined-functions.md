# Fix Undefined Test Functions Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Ensure `swimmy.tests:run-all-tests` has no undefined function errors by defining the missing tests `test-db-rank-counts` and `test-report-source-drift-detects-mismatch`.

**Architecture:** Add the missing `deftest` definitions to the backtest DB test module so ASDF loads them; keep the existing test runner list intact. Verify via the CI test runner to ensure no undefined-function errors.

**Tech Stack:** Common Lisp (SBCL), ASDF (`swimmy`), `ci-test.sh`.

### Task 1: Reproduce and enumerate undefined test functions

**Files:**
- Modify: none
- Test: `src/lisp/tests.lisp`

**Step 1: Run the test suite to reproduce the undefined-function error**

Run: `./ci-test.sh`
Expected: SBCL halts with `UNDEFINED-FUNCTION` for `SWIMMY.TESTS::TEST-DB-RANK-COUNTS` (or the next missing test), even if the wrapper script prints a pass summary.

**Step 2: Compare run-all-tests list vs deftest definitions**

Run:

```bash
python3 - <<'PY'
import re, pathlib, collections
root = pathlib.Path('.')
# gather all deftest names in repo
names = set()
for path in root.rglob('*.lisp'):
    text = path.read_text(errors='ignore')
    for m in re.finditer(r"\(deftest\s+([\w-]+)", text):
        names.add(m.group(1))

text = (root/'src/lisp/tests.lisp').read_text()
m = re.search(r"\(dolist \(test '\((.*?)\)\)" , text, re.S)
if not m:
    raise SystemExit('run-all-tests list not found')
list_block = re.sub(r";.*", "", m.group(1)).replace('\n', ' ')
syms = re.findall(r"\b([a-zA-Z][\w-]*)\b", list_block)
syms = list(collections.OrderedDict.fromkeys(syms))
missing = [s for s in syms if s not in names]
print(missing)
PY
```

Expected: `['test-db-rank-counts', 'test-report-source-drift-detects-mismatch']`.

### Task 2: Add missing test definitions

**Files:**
- Modify: `src/lisp/tests/backtest-db-tests.lisp`
- Test: `src/lisp/tests/backtest-db-tests.lisp`

**Step 1: Write the missing tests (RED by existence)**

Add the following definitions after `test-map-strategies-from-db-limit`:

```lisp
(deftest test-db-rank-counts
  "DB rank counts should reflect stored ranks (including graveyard and unranked)."
  (let ((tmp-db (format nil "/tmp/swimmy-ranks-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (dolist (spec '(("R-S" :S) ("R-A" :A) ("R-B" :B) ("R-G" :GRAVEYARD) ("R-N" nil)))
              (destructuring-bind (name rank) spec
                (swimmy.school::upsert-strategy
                 (make-strategy :name name :sharpe 0.2 :symbol "USDJPY" :rank rank))))
            (let* ((counts (swimmy.school::get-db-rank-counts))
                   (s (getf counts :s))
                   (a (getf counts :a))
                   (b (getf counts :b))
                   (g (getf counts :graveyard))
                   (u (getf counts :unranked))
                   (total (getf counts :total))
                   (active (getf counts :active)))
              (assert-true (= 1 s) "S count")
              (assert-true (= 1 a) "A count")
              (assert-true (= 1 b) "B count")
              (assert-true (= 1 g) "Graveyard count")
              (assert-true (= 1 u) "Unranked count")
              (assert-true (= 5 total) "Total count")
              (assert-true (= 4 active) "Active excludes graveyard")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-report-source-drift-detects-mismatch
  "Drift check should flag mismatched DB/KB/Library counts."
  (let* ((tmp-db (format nil "/tmp/swimmy-drift-~a.db" (get-universal-time)))
         (tmp-lib (format nil "/tmp/swimmy-lib-drift-~a/" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.persistence::*library-path* (merge-pathnames tmp-lib #P"/"))
          (swimmy.school::*disable-auto-migration* t)
          (*strategy-knowledge-base* nil))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.persistence:init-library)
            ;; DB: one graveyard, one active
            (swimmy.school::upsert-strategy (make-strategy :name "D-A" :sharpe 0.2 :symbol "USDJPY" :rank :B))
            (swimmy.school::upsert-strategy (make-strategy :name "D-G" :sharpe -0.2 :symbol "USDJPY" :rank :graveyard))
            ;; KB: empty
            (setf *strategy-knowledge-base* nil)
            ;; Library: no graveyard files
            (let ((warnings (swimmy.school::report-source-drift)))
              (assert-true (> (length warnings) 0) "Drift warnings should be reported")))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))
```

**Step 2: Run the tests to verify they now exist and execute**

Run: `./ci-test.sh`
Expected: No `UNDEFINED-FUNCTION` errors; the test runner proceeds through `test-db-rank-counts` and `test-report-source-drift-detects-mismatch`.

### Task 3: Commit

**Files:**
- Modify: `src/lisp/tests/backtest-db-tests.lisp`

**Step 1: Commit the changes**

```bash
git add src/lisp/tests/backtest-db-tests.lisp
git commit -m "test: add missing db rank and drift tests"
```
