# Newborn Pruning Guard + Rank-Scoped Persistence Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Protect newborn strategies from pruning (trades<=0 or age<24h) and allow rank-scoped delete/move operations in persistence.

**Architecture:** Add a single `newborn-protected-p` guard in `school-pruning.lisp` and apply it to low-sharpe, similarity, and hard-cap pruning paths. Extend persistence functions to accept optional rank hints to target the correct storage directory without breaking existing callers.

**Tech Stack:** Common Lisp (SBCL), ASDF system `swimmy`, Swimmy test harness (`bash scripts/ci-test.sh`).

---

### Task 1: Newborn Protection in Pruning

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/school/school-pruning.lisp`

**Step 1: Write the failing tests (and register in `run-all-tests`)**

Add the following tests near other school lifecycle tests in `src/lisp/tests.lisp`, and append the three test symbols to the `run-all-tests` list.

```lisp
(deftest test-prune-low-sharpe-skips-newborn-age
  "Newborn (age<24h) low-sharpe strategies are protected from pruning."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-send (symbol-function 'swimmy.school:send-to-graveyard))
         (now (get-universal-time))
         (strat (swimmy.school:make-strategy :name "UT-NEWBORN-AGE"
                                             :sharpe 0.0
                                             :trades 5)))
    (unwind-protect
        (progn
          (setf (swimmy.school::strategy-creation-time strat) now)
          (setf swimmy.school::*strategy-knowledge-base* (list strat))
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (s reason) (declare (ignore s reason)) nil))
          (let ((removed (swimmy.school:prune-low-sharpe-strategies)))
            (assert-equal 0 removed "Newborn should be protected by age")
            (assert-equal 1 (length swimmy.school::*strategy-knowledge-base*)
                          "KB should keep newborn strategy")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig-send))))

(deftest test-prune-similar-skips-newborn-trades
  "Newborn (trades<=0) strategies are protected from similarity pruning."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-send (symbol-function 'swimmy.school:send-to-graveyard))
         (now (get-universal-time))
         (old-ts (- now (* 2 24 60 60)))
         (strat1 (swimmy.school:make-strategy :name "UT-SIM-A"
                                              :sl 30 :tp 60 :timeframe 60
                                              :indicators '((sma 20)) :symbol "EURUSD"
                                              :sharpe 1.0 :trades 10))
         (strat2 (swimmy.school:make-strategy :name "UT-SIM-B"
                                              :sl 30 :tp 60 :timeframe 60
                                              :indicators '((sma 20)) :symbol "EURUSD"
                                              :sharpe 0.5 :trades 0)))
    (unwind-protect
        (progn
          (setf (swimmy.school::strategy-creation-time strat2) old-ts)
          (setf swimmy.school::*strategy-knowledge-base* (list strat1 strat2))
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (s reason) (declare (ignore s reason)) nil))
          (let ((removed (swimmy.school:prune-similar-strategies)))
            (assert-equal 0 removed "Newborn should be protected by trades")
            (assert-equal 2 (length swimmy.school::*strategy-knowledge-base*)
                          "KB should keep both strategies")))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig-send))))

(deftest test-hard-cap-skips-newborn
  "Hard-cap pruning should skip newborn strategies."
  (let* ((orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-send (symbol-function 'swimmy.school:send-to-graveyard))
         (orig-cap swimmy.school:*kb-hard-cap*)
         (now (get-universal-time))
         (old-ts (- now (* 3 24 60 60)))
         (newborn (swimmy.school:make-strategy :name "UT-CAP-NEW"
                                               :sharpe -1.0 :trades 0))
         (older (swimmy.school:make-strategy :name "UT-CAP-OLD"
                                             :sharpe 0.0 :trades 10)))
    (unwind-protect
        (progn
          (setf swimmy.school:*kb-hard-cap* 1)
          (setf (swimmy.school::strategy-creation-time newborn) now)
          (setf (swimmy.school::strategy-creation-time older) old-ts)
          (setf swimmy.school::*strategy-knowledge-base* (list newborn older))
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (s reason) (declare (ignore s reason)) nil))
          (let ((removed (swimmy.school:enforce-kb-hard-cap)))
            (assert-equal 1 (or removed 0) "Should purge one non-newborn")
            (assert-true (find "UT-CAP-NEW" swimmy.school::*strategy-knowledge-base*
                               :key #'swimmy.school:strategy-name :test #'string=)
                         "Newborn should remain")
            (assert-false (find "UT-CAP-OLD" swimmy.school::*strategy-knowledge-base*
                                :key #'swimmy.school:strategy-name :test #'string=)
                          "Older should be removed")))
      (setf swimmy.school:*kb-hard-cap* orig-cap)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig-send))))
```

Add these symbols to the `run-all-tests` list:

```
                  test-prune-low-sharpe-skips-newborn-age
                  test-prune-similar-skips-newborn-trades
                  test-hard-cap-skips-newborn
```

**Step 2: Run tests to verify they fail**

Run: `bash scripts/ci-test.sh`

Expected: The three new tests FAIL because newborn protection is not yet implemented.

**Step 3: Write minimal implementation**

Edit `src/lisp/school/school-pruning.lisp` to add a helper and guard checks:

```lisp
(defparameter *newborn-protection-seconds* (* 24 60 60)
  "Age window for newborn protection in pruning.")

(defun newborn-protected-p (strat)
  "Return T when strategy should be protected from pruning (newborn)."
  (let* ((trades (or (strategy-trades strat) 0))
         (born (or (strategy-creation-time strat) 0))
         (age-sec (and (> born 0) (- (get-universal-time) born))))
    (or (<= trades 0)
        (and age-sec (< age-sec *newborn-protection-seconds*)))))
```

Apply guard checks in these functions:

- `prune-low-sharpe-strategies` (skip early if `newborn-protected-p`)
- `prune-similar-strategies` (skip marking `strat2` if newborn)
- `enforce-kb-hard-cap` (exclude newborns from `to-purge`)

Minimal guard pattern examples:

```lisp
;; in prune-low-sharpe-strategies
(if (newborn-protected-p strat)
    nil
    (if (< sharpe *prune-sharpe-threshold*) ...))

;; in prune-similar-strategies
(when (and (< (strategy-distance strat1 strat2) *prune-similarity-threshold*)
           (not (member (strategy-rank strat2) *prune-protected-ranks*))
           (not (newborn-protected-p strat2)))
  ...)

;; in enforce-kb-hard-cap
(let* ((sorted ...)
       (to-purge (subseq (remove-if (lambda (s)
                                     (or (member (strategy-rank s) *prune-protected-ranks*)
                                         (newborn-protected-p s)))
                                   sorted)
                         0 (min excess current-size))))
  ...)
```

**Step 4: Run tests to verify they pass**

Run: `bash scripts/ci-test.sh`

Expected: All tests PASS.

**Step 5: Commit**

```bash
git add src/lisp/tests.lisp src/lisp/school/school-pruning.lisp
git commit -m "feat: protect newborn strategies from pruning"
```

---

### Task 2: Rank-Scoped Persistence (delete/move)

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/core/persistence.lisp`

**Step 1: Write the failing tests (and register in `run-all-tests`)**

Add these tests to `src/lisp/tests.lisp` (near other persistence/lifecycle tests), and append the two symbols to `run-all-tests`.

```lisp
(deftest test-delete-strategy-rank-guard
  "delete-strategy should honor :rank when provided."
  (let* ((orig-path swimmy.persistence:*library-path*)
         (tmp (format nil "/tmp/swimmy-test-lib-~a/" (random 1000000)))
         (tmp-dir (uiop:ensure-directory-pathname tmp))
         (strat (swimmy.school:make-strategy :name "UT-DEL-RANK" :rank :B)))
    (unwind-protect
        (progn
          (setf swimmy.persistence:*library-path* tmp-dir)
          (swimmy.persistence:init-library)
          (swimmy.persistence:save-strategy strat)
          (assert-true (swimmy.persistence:strategy-exists-p "UT-DEL-RANK" :B)
                       "Strategy should exist at rank B")
          (swimmy.persistence:delete-strategy strat :rank :A)
          (assert-true (swimmy.persistence:strategy-exists-p "UT-DEL-RANK" :B)
                       "Wrong rank should not delete")
          (swimmy.persistence:delete-strategy strat :rank :B)
          (assert-false (swimmy.persistence:strategy-exists-p "UT-DEL-RANK" :B)
                        "Correct rank should delete"))
      (setf swimmy.persistence:*library-path* orig-path)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

(deftest test-move-strategy-from-rank
  "move-strategy should delete from :from-rank when provided."
  (let* ((orig-path swimmy.persistence:*library-path*)
         (tmp (format nil "/tmp/swimmy-test-lib-~a/" (random 1000000)))
         (tmp-dir (uiop:ensure-directory-pathname tmp))
         (strat (swimmy.school:make-strategy :name "UT-MOVE-RANK" :rank :B)))
    (unwind-protect
        (progn
          (setf swimmy.persistence:*library-path* tmp-dir)
          (swimmy.persistence:init-library)
          (swimmy.persistence:save-strategy strat)
          ;; simulate rank drift so delete must use :from-rank
          (setf (swimmy.school:strategy-rank strat) :A)
          (swimmy.persistence:move-strategy strat :S :from-rank :B)
          (assert-false (swimmy.persistence:strategy-exists-p "UT-MOVE-RANK" :B)
                        "Source rank file should be removed")
          (assert-true (swimmy.persistence:strategy-exists-p "UT-MOVE-RANK" :S)
                       "Target rank file should exist"))
      (setf swimmy.persistence:*library-path* orig-path)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))
```

Add these symbols to the `run-all-tests` list:

```
                  test-delete-strategy-rank-guard
                  test-move-strategy-from-rank
```

**Step 2: Run tests to verify they fail**

Run: `bash scripts/ci-test.sh`

Expected: The two new tests FAIL because `delete-strategy` and `move-strategy` don’t accept these keyword arguments yet.

**Step 3: Write minimal implementation**

Edit `src/lisp/core/persistence.lisp`:

```lisp
(defun delete-strategy (strategy-obj &key rank)
  "Delete the file associated with the strategy."
  (let* ((name (slot-value strategy-obj 'swimmy.school::name))
         (resolved-rank (or rank (strategy-storage-rank strategy-obj)))
         (path (get-strategy-path name resolved-rank)))
    (cond
      ((probe-file path)
       (delete-file path)
       (format t "[LIB] 🗑️ Deleted ~a from ~a~%" name path))
      ;; Legacy tier fallback (pre-migration files)
      ((and (null rank)
            (slot-exists-p strategy-obj 'swimmy.school::tier)
            (let* ((tier (slot-value strategy-obj 'swimmy.school::tier))
                   (legacy-path (get-strategy-path name tier)))
              (when (probe-file legacy-path)
                (delete-file legacy-path)
                (format t "[LIB] 🗑️ Deleted ~a from legacy ~a~%" name legacy-path)
                t))))
      (t
       (format t "[LIB] ⚠️ File not found for deletion: ~a~%" path)))))

(defun move-strategy (strategy-obj new-rank &key (force nil) (from-rank nil))
  "Move strategy to a new rank (delete old file, update slot, save new file).
   V49.3: Fortress Mode - Blocks moving A/S/Legend to Graveyard without :force t."
  ...
  ;; 1. Delete old file using CURRENT rank or explicit :from-rank
  (if from-rank
      (delete-strategy strategy-obj :rank from-rank)
      (delete-strategy strategy-obj))
  ...)
```

**Step 4: Run tests to verify they pass**

Run: `bash scripts/ci-test.sh`

Expected: All tests PASS.

**Step 5: Commit**

```bash
git add src/lisp/tests.lisp src/lisp/core/persistence.lisp
git commit -m "feat: add rank-scoped persistence deletes"
```

