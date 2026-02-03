# DB-As-Truth Report Consistency Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make Backtest Result and Evolution Factory Report use the SQLite DB as the single source of truth, consolidate DB path usage, and add a drift smoke test.

**Architecture:** Add DB rank-count helpers in `school-db.lisp` and a drift-check helper comparing DB/KB/Library counts. Update Discord backtest summary and Evolution Factory Report to use DB-derived counts (and DB-derived top candidates). Align legacy SQLite path in `core/schema.lisp` with `data/memory/swimmy.db`. Add tests for rank counts and drift detection.

**Tech Stack:** Common Lisp (SBCL), SQLite via `sqlite-manager`, Swimmy test harness (`ci-test.sh`).

---

### Task 1: Add DB rank-count + drift helpers

**Files:**
- Modify: `src/lisp/school/school-db.lisp`
- Modify: `src/lisp/packages-school.lisp`
- Test: `src/lisp/tests/backtest-db-tests.lisp`

**Step 1: Write failing tests for DB rank counts and drift detection**

Add to `src/lisp/tests/backtest-db-tests.lisp` (near other DB tests):

```lisp
(deftest test-db-rank-counts
  "DB rank counts should reflect stored ranks (including graveyard and unranked)."
  (let ((tmp-db (format nil "/tmp/swimmy-ranks-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil))
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

**Step 2: Run test to verify it fails**

Run: `./ci-test.sh`

Expected: FAIL with undefined function `swimmy.school::get-db-rank-counts` / `report-source-drift` (baseline suite currently has pre-existing failures too).

**Step 3: Implement DB rank counts + drift helpers**

Add to `src/lisp/school/school-db.lisp` (after `get-db-stats`):

```lisp
(defun %normalize-db-rank-key (rank)
  (cond
    ((null rank) "NIL")
    ((stringp rank) (string-upcase rank))
    ((symbolp rank) (string-upcase (symbol-name rank)))
    (t (string-upcase (format nil "~a" rank)))))

(defun get-db-rank-counts ()
  "Return plist of rank counts from DB: :total :active :s :a :b :legend :graveyard :incubator :unranked."
  (let* ((rows (execute-to-list "SELECT rank, count(*) FROM strategies GROUP BY rank"))
         (counts (make-hash-table :test 'equal))
         (total 0))
    (dolist (row rows)
      (destructuring-bind (rank count) row
        (let ((key (%normalize-db-rank-key rank)))
          (setf (gethash key counts) count)
          (incf total count))))
    (labels ((count-rank (key) (or (gethash key counts) 0)))
      (let* ((s (count-rank ":S"))
             (a (count-rank ":A"))
             (b (count-rank ":B"))
             (legend (count-rank ":LEGEND"))
             (graveyard (count-rank ":GRAVEYARD"))
             (incubator (count-rank ":INCUBATOR"))
             (unranked (count-rank "NIL"))
             (active (- total graveyard)))
        (list :total total
              :active active
              :s s
              :a a
              :b b
              :legend legend
              :graveyard graveyard
              :incubator incubator
              :unranked unranked)))))

(defun get-library-rank-counts (&optional (root swimmy.persistence:*library-path*))
  "Return plist of library counts by rank dir."
  (labels ((count-dir (dir)
             (length (directory (merge-pathnames (format nil "~a/*.lisp" dir) root)))))
    (list :s (count-dir "S")
          :a (count-dir "A")
          :b (count-dir "B")
          :incubator (count-dir "INCUBATOR")
          :legend (count-dir "LEGEND")
          :graveyard (count-dir "GRAVEYARD"))))

(defun report-source-drift ()
  "Return list of warning strings when DB/KB/Library counts drift."
  (let* ((db (get-db-rank-counts))
         (lib (get-library-rank-counts))
         (kb-active (length *strategy-knowledge-base*))
         (db-active (getf db :active 0))
         (db-grave (getf db :graveyard 0))
         (lib-grave (getf lib :graveyard 0))
         (warnings nil))
    (when (/= db-active kb-active)
      (push (format nil "KB active mismatch (DB=~d KB=~d)" db-active kb-active) warnings))
    (when (/= db-grave lib-grave)
      (push (format nil "Graveyard mismatch (DB=~d Library=~d)" db-grave lib-grave) warnings))
    (nreverse warnings)))
```

**Step 4: Export new helpers**

Add to `src/lisp/packages-school.lisp` exports list:

```lisp
   #:get-db-rank-counts
   #:get-library-rank-counts
   #:report-source-drift
```

**Step 5: Run tests**

Run: `./ci-test.sh`

Expected: New tests pass; pre-existing failures remain unchanged.

**Step 6: Commit**

```bash
git add src/lisp/school/school-db.lisp src/lisp/packages-school.lisp src/lisp/tests/backtest-db-tests.lisp
git commit -m "test: add db rank count + drift helpers"
```

---

### Task 2: Make Backtest Summary + Evolution Report use DB counts

**Files:**
- Modify: `src/lisp/core/discord.lisp`
- Modify: `src/lisp/school/school-narrative.lisp`
- Test: `src/lisp/tests/backtest-db-tests.lisp`

**Step 1: Write failing test for Evolution Report DB counts**

Add to `src/lisp/tests/backtest-db-tests.lisp`:

```lisp
(deftest test-evolution-report-uses-db-counts
  "Evolution report should reflect DB counts, not KB/library drift."
  (let* ((tmp-db (format nil "/tmp/swimmy-report-~a.db" (get-universal-time)))
         (tmp-lib (format nil "/tmp/swimmy-lib-report-~a/" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.persistence::*library-path* (merge-pathnames tmp-lib #P"/"))
          (*strategy-knowledge-base* nil))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.persistence:init-library)
            ;; DB: 1 active (B) + 1 graveyard
            (swimmy.school::upsert-strategy (make-strategy :name "ER-B" :sharpe 0.2 :symbol "USDJPY" :rank :B))
            (swimmy.school::upsert-strategy (make-strategy :name "ER-G" :sharpe -0.2 :symbol "USDJPY" :rank :graveyard))
            ;; KB empty to force mismatch if KB used
            (setf *strategy-knowledge-base* nil)
            (let ((report (swimmy.school::generate-evolution-report)))
              (assert-true (search "Knowledge Base (Active)\n1 Strategies" report)
                           "Active count should be 1 (DB)" )
              (assert-true (search "👻 Graveyard\n1" report)
                           "Graveyard count should be 1 (DB)") ))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))
```

**Step 2: Run test to verify it fails**

Run: `./ci-test.sh`

Expected: FAIL because report still uses KB/library counts.

**Step 3: Update Backtest Summary rank distribution to use DB**

Edit `src/lisp/core/discord.lisp` (inside `notify-backtest-summary`):

```lisp
(let* ((counts (swimmy.school:get-db-rank-counts))
       (s-count (getf counts :s 0))
       (a-count (getf counts :a 0))
       (b-count (getf counts :b 0))
       (grave-count (getf counts :graveyard 0)))
  (setf report-msg (concatenate 'string report-msg
                                (format nil "**Current Rank Distribution:**~%🏆 S-Rank: ~d | 🎯 A-Rank: ~d | 📋 B-Rank: ~d~%⚰️ Graveyard/Pending: ~d~%~%"
                                        s-count a-count b-count grave-count))))
```

**Step 4: Update Evolution Report to use DB counts + DB top candidates**

Edit `src/lisp/school/school-narrative.lisp`:

1) Add a DB-based top-candidates snippet helper near `build-top-candidates-snippet`:

```lisp
(defun %format-db-rank-label (rank)
  (cond
    ((null rank) "UNRANKED")
    ((stringp rank)
     (let ((r (string-upcase rank)))
       (if (and (> (length r) 0) (char= (char r 0) #\:))
           (subseq r 1)
           r)))
    ((symbolp rank) (symbol-name rank))
    (t "UNRANKED")))

(defun build-top-candidates-snippet-from-db ()
  "Build top candidates snippet using DB as source of truth."
  (handler-case
      (let* ((rows (execute-to-list "SELECT name, sharpe, rank FROM strategies ORDER BY sharpe DESC LIMIT 5"))
             (limit (length rows)))
        (with-output-to-string (s)
          (format s "~%🌟 **Top Candidates:**~%")
          (loop for i from 0 below limit
                for row = (nth i rows)
                do (destructuring-bind (name sharpe rank) row
                     (format s "- `~a` (S=~,2f, ~a)~%"
                             (subseq name 0 (min 25 (length name)))
                             (float (or sharpe 0.0))
                             (%format-db-rank-label rank))))))
    (error (e)
      (format nil "~%🌟 **Top Candidates:**~%  - error: ~a" e))))
```

2) Replace report counts in `generate-evolution-report`:

```lisp
(let* ((counts (get-db-rank-counts))
       (active-count (getf counts :active 0))
       (s-rank (getf counts :s 0))
       (a-rank (getf counts :a 0))
       (b-rank (getf counts :b 0))
       (graveyard (getf counts :graveyard 0))
       (one-day-ago (- (get-universal-time) 86400))
       (new-recruits (count-if (lambda (s)
                                 (and (strategy-creation-time s)
                                      (> (strategy-creation-time s) one-day-ago)))
                               swimmy.globals:*strategy-knowledge-base*))
       (top-snippet (build-top-candidates-snippet-from-db))
       (cpcv-snippet (build-cpcv-status-snippet)))
  ... use active-count instead of (length all) ...)
```

**Step 5: Run tests**

Run: `./ci-test.sh`

Expected: `test-evolution-report-uses-db-counts` passes; baseline failures remain unchanged.

**Step 6: Commit**

```bash
git add src/lisp/core/discord.lisp src/lisp/school/school-narrative.lisp src/lisp/tests/backtest-db-tests.lisp
git commit -m "feat: use db counts for reports"
```

---

### Task 3: Consolidate DB path and update docs

**Files:**
- Modify: `src/lisp/core/schema.lisp`
- Modify: `doc/owners_guide.md`

**Step 1: Write failing doc update expectation (optional/manual)**

(No automated test; proceed to implementation.)

**Step 2: Align legacy DB path**

Edit `src/lisp/core/schema.lisp`:

```lisp
(defparameter *db-path* "data/memory/swimmy.db")
```

**Step 3: Update Owner’s Guide to declare DB as truth**

Add a short note near the report section in `doc/owners_guide.md`:

```markdown
> [!IMPORTANT]
> **真実のソースはSQLite（data/memory/swimmy.db）**。
> Libraryは派生スナップショット、In-memory KBはキャッシュとして扱う。
```

**Step 4: Commit**

```bash
git add src/lisp/core/schema.lisp doc/owners_guide.md
git commit -m "docs: declare db as single source of truth"
```

---

### Task 4: Verification

**Step 1: Full test run**

Run: `./ci-test.sh`

Expected: same baseline failures as before; new tests pass.

**Step 2: Manual sanity check**

Run an Evolution report and Backtest summary in a staging env and confirm S/A/B/Graveyard counts match DB.

---

## Notes / Constraints

- Baseline tests currently fail in this repo (missing functions in unrelated tests). Do not mask those; ensure new tests pass and report pre-existing failures separately.
- Avoid long DB scans in reports; use DB counts and top-5 query only.
