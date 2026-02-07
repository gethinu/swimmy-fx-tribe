# Pair Strategy Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan.

**Goal:** Add pair strategies as independent entities with DB persistence, OOS/CPCV gates, and hybrid slot competition with single strategies while keeping overlay execution intact.

**Architecture:** Add a `pair_strategies` table and pair ranking pipeline in Lisp. Use existing trade_list persistence (plus CPCV) to compute composite OOS/CPCV metrics, update pair ranks, and refresh active overlay pairs with per-symbol/timeframe caps. Integrate as a periodic maintenance task and keep single strategy lifecycle untouched.

**Tech Stack:** Common Lisp (SBCL), SQLite, existing Swimmy test runner (`tests/test_runner.lisp`).

---

### Task 1: Pair Strategy DB Persistence

**Files:**
- Modify: `src/lisp/school/school-db.lisp`
- Test: `src/lisp/tests/backtest-db-tests.lisp`

**Step 1: Write the failing test**

Add a new test in `src/lisp/tests/backtest-db-tests.lisp`:

```lisp
(deftest test-pair-strategy-upsert-fetch
  "pair_strategies should upsert and fetch records"
  (let* ((tmp-db (format nil "/tmp/swimmy-pair-strat-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (*default-pathname-defaults* #P"/tmp/"))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (let ((pair (list :pair-id "P1"
                              :strategy-a "A"
                              :strategy-b "B"
                              :weight-a 0.6
                              :weight-b 0.4
                              :symbol "USDJPY"
                              :timeframe 1
                              :sharpe 0.8
                              :profit-factor 1.5
                              :score 1.0
                              :corr 0.1
                              :rank :A
                              :oos-sharpe 0.7
                              :cpcv-median 0.6
                              :cpcv-pass-rate 0.8)))
              (swimmy.school::upsert-pair-strategy pair)
              (let ((fetched (swimmy.school::fetch-pair-strategy "P1")))
                (assert-equal "P1" (getf fetched :pair-id) "pair_id should match")
                (assert-equal "A" (getf fetched :strategy-a) "strategy_a should match")
                (assert-equal :A (getf fetched :rank) "rank should match")
                (assert-true (> (getf fetched :last-updated) 0) "last_updated should be set"))))
        (ignore-errors (close-db-connection))
        (ignore-errors (delete-file tmp-db))))))
```

**Step 2: Run test to verify it fails**

Run:
```
sbcl --script tests/test_runner.lisp
```
Expected: FAIL with undefined function `upsert-pair-strategy` or missing `pair_strategies` table.

**Step 3: Write minimal implementation**

In `src/lisp/school/school-db.lisp`, add a new table and helper functions:

```lisp
(execute-non-query
 "CREATE TABLE IF NOT EXISTS pair_strategies (
    pair_id TEXT PRIMARY KEY,
    strategy_a TEXT,
    strategy_b TEXT,
    weight_a REAL,
    weight_b REAL,
    symbol TEXT,
    timeframe INTEGER,
    sharpe REAL,
    profit_factor REAL,
    score REAL,
    corr REAL,
    rank TEXT,
    oos_sharpe REAL,
    cpcv_median REAL,
    cpcv_pass_rate REAL,
    last_updated INTEGER
  )")

(defun upsert-pair-strategy (pair)
  (let ((updated-at (get-universal-time)))
    (execute-non-query
     "INSERT OR REPLACE INTO pair_strategies (
        pair_id, strategy_a, strategy_b, weight_a, weight_b,
        symbol, timeframe, sharpe, profit_factor, score, corr,
        rank, oos_sharpe, cpcv_median, cpcv_pass_rate, last_updated
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (getf pair :pair-id)
     (getf pair :strategy-a)
     (getf pair :strategy-b)
     (getf pair :weight-a)
     (getf pair :weight-b)
     (getf pair :symbol)
     (getf pair :timeframe)
     (getf pair :sharpe)
     (getf pair :profit-factor)
     (getf pair :score)
     (getf pair :corr)
     (format nil "~s" (getf pair :rank))
     (getf pair :oos-sharpe)
     (getf pair :cpcv-median)
     (getf pair :cpcv-pass-rate)
     updated-at)))

(defun fetch-pair-strategy (pair-id)
  (let ((row (first (execute-to-list
                     "SELECT pair_id, strategy_a, strategy_b, weight_a, weight_b,
                             symbol, timeframe, sharpe, profit_factor, score, corr,
                             rank, oos_sharpe, cpcv_median, cpcv_pass_rate, last_updated
                      FROM pair_strategies WHERE pair_id = ?" pair-id))))
    (when row
      (destructuring-bind (pid a b wa wb sym tf sharpe pf score corr rank oos cpcv pass updated) row
        (list :pair-id pid :strategy-a a :strategy-b b :weight-a wa :weight-b wb
              :symbol sym :timeframe tf :sharpe sharpe :profit-factor pf :score score :corr corr
              :rank (and rank (swimmy.core:safe-read-sexp rank :package :swimmy.school))
              :oos-sharpe oos :cpcv-median cpcv :cpcv-pass-rate pass :last-updated updated)))))
```

**Step 4: Run test to verify it passes**

Run:
```
sbcl --script tests/test_runner.lisp
```
Expected: PASS.

**Step 5: Commit**

```
git add src/lisp/school/school-db.lisp src/lisp/tests/backtest-db-tests.lisp

git commit -m "feat: add pair strategy persistence"
```

---

### Task 2: Persist CPCV trade_list

**Files:**
- Modify: `src/lisp/core/message-dispatcher.lisp`
- Test: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add to `src/lisp/tests.lisp`:

```lisp
(deftest test-cpcv-result-persists-trade-list
  "CPCV_RESULT should persist trade_list as oos_kind=CPCV"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"AAA\") (request_id . \"RID-CPCV\") (trade_list . (((timestamp . 1) (pnl . 1.0) (symbol . \"USDJPY\")))))))")
           (called nil)
           (orig-record (symbol-function 'swimmy.school:record-backtest-trades)))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:record-backtest-trades)
                  (lambda (rid name kind trades)
                    (setf called (list rid name kind trades))
                    nil))
            (funcall fn msg)
            (assert-true called "record-backtest-trades should be called")
            (assert-equal "RID-CPCV" (first called) "request_id should match")
            (assert-equal "AAA" (second called) "strategy name should match")
            (assert-equal "CPCV" (third called) "oos_kind should be CPCV"))
        (setf (symbol-function 'swimmy.school:record-backtest-trades) orig-record)))))
```

**Step 2: Run test to verify it fails**

Run:
```
sbcl --script tests/test_runner.lisp
```
Expected: FAIL because CPCV_RESULT does not call `record-backtest-trades`.

**Step 3: Write minimal implementation**

In `src/lisp/core/message-dispatcher.lisp`, inside `CPCV_RESULT` handling, add:

```lisp
(let* ((trade-list (%alist-val result '(trade_list trade-list) nil))
       (req-id (%alist-val result '(request_id request-id) nil)))
  (when (and trade-list name)
    (swimmy.school:record-backtest-trades req-id name "CPCV" trade-list)))
```

**Step 4: Run test to verify it passes**

Run:
```
sbcl --script tests/test_runner.lisp
```
Expected: PASS.

**Step 5: Commit**

```
git add src/lisp/core/message-dispatcher.lisp src/lisp/tests.lisp

git commit -m "feat: persist CPCV trade_list"
```

---

### Task 3: Pair Metrics + Gate Evaluation

**Files:**
- Modify: `src/lisp/school/school-pair-composite.lisp`
- Test: `src/lisp/tests/pair-composite-tests.lisp`

**Step 1: Write the failing test**

Add to `src/lisp/tests/pair-composite-tests.lisp`:

```lisp
(deftest test-pair-gate-blocks-on-insufficient-data
  "pair gate should block promotion when trade_list is insufficient"
  (let* ((trades-a '(((timestamp . 1) (pnl . 1.0))))
         (trades-b '(((timestamp . 1) (pnl . -1.0)))) )
    (multiple-value-bind (metrics reason)
        (swimmy.school::pair-metrics-from-trades trades-a trades-b :min-trades 3)
      (assert-true (null metrics) "metrics should be nil")
      (assert-equal :insufficient-data reason "should block with insufficient-data"))))
```

**Step 2: Run test to verify it fails**

Run:
```
sbcl --script tests/test_runner.lisp
```
Expected: FAIL due to undefined function `pair-metrics-from-trades`.

**Step 3: Write minimal implementation**

In `src/lisp/school/school-pair-composite.lisp`:

```lisp
(defun win-rate-from-pnls (pnls)
  (let ((wins 0) (total 0))
    (dolist (p pnls)
      (incf total)
      (when (> p 0) (incf wins)))
    (if (zerop total) 0.0 (/ (float wins) total))))

(defun max-dd-from-pnls (pnls)
  (let ((peak 0.0) (equity 0.0) (maxdd 0.0))
    (dolist (p pnls)
      (incf equity p)
      (setf peak (max peak equity))
      (let ((dd (if (> peak 0) (/ (- peak equity) peak) 0.0)))
        (setf maxdd (max maxdd dd))))
    maxdd))

(defun pair-metrics-from-trades (trades-a trades-b &key (min-trades *pair-min-trades*))
  (when (or (< (length trades-a) min-trades)
            (< (length trades-b) min-trades))
    (return-from pair-metrics-from-trades (values nil :insufficient-data)))
  (multiple-value-bind (xs ys)
      (align-pnl-series (trade-list->series trades-a) (trade-list->series trades-b))
    (multiple-value-bind (w1 w2) (inverse-vol-weights xs ys)
      (let* ((pnls (composite-pnls xs ys w1 w2))
             (score (pair-score-from-pnls pnls))
             (wr (win-rate-from-pnls pnls))
             (maxdd (max-dd-from-pnls pnls)))
        (values (append score (list :weight-a w1 :weight-b w2 :win-rate wr :max-dd maxdd)) nil)))))
```

**Step 4: Run test to verify it passes**

Run:
```
sbcl --script tests/test_runner.lisp
```
Expected: PASS.

**Step 5: Commit**

```
git add src/lisp/school/school-pair-composite.lisp src/lisp/tests/pair-composite-tests.lisp

git commit -m "feat: add pair metrics gating helpers"
```

---

### Task 4: Pair Slot Competition (Hybrid)

**Files:**
- Modify: `src/lisp/school/school-pair-composite.lisp`
- Test: `src/lisp/tests/pair-composite-tests.lisp`

**Step 1: Write the failing test**

Add to `src/lisp/tests/pair-composite-tests.lisp`:

```lisp
(deftest test-pair-slot-competition-cap
  "pair selection should respect per-tf pair slots while competing with singles"
  (let* ((single-a (swimmy.school::make-strategy :name "S1" :symbol "USDJPY" :timeframe 1 :sharpe 0.9 :profit-factor 1.5 :rank :A))
         (single-b (swimmy.school::make-strategy :name "S2" :symbol "USDJPY" :timeframe 1 :sharpe 0.8 :profit-factor 1.4 :rank :A))
         (pairs (list (list :pair-id "P1" :a "S1" :b "S2" :symbol "USDJPY" :timeframe 1 :score 1.2 :weight-a 0.5 :weight-b 0.5)
                      (list :pair-id "P2" :a "S1" :b "S2" :symbol "USDJPY" :timeframe 1 :score 1.1 :weight-a 0.6 :weight-b 0.4))))
    (let ((active (swimmy.school::select-active-pair-defs pairs (list single-a single-b)
                                                          :pair-slots-per-tf 1
                                                          :competition-top-n 2)))
      (assert-equal 1 (length active) "only one pair should survive cap")
      (assert-equal "P1" (getf (first active) :pair-id) "top pair should be selected"))))
```

**Step 2: Run test to verify it fails**

Run:
```
sbcl --script tests/test_runner.lisp
```
Expected: FAIL due to undefined `select-active-pair-defs`.

**Step 3: Write minimal implementation**

In `src/lisp/school/school-pair-composite.lisp`:

```lisp
(defparameter *pair-slots-per-tf* 1
  "Max pair slots per symbol/timeframe in hybrid competition.")

(defparameter *pair-competition-top-n* 2
  "Top-N combined list to consider when activating pairs.")

(defun %single-score (strat)
  (+ (* *pair-score-sharpe-weight* (or (strategy-sharpe strat) 0.0))
     (* *pair-score-pf-weight* (or (strategy-profit-factor strat) 0.0))))

(defun select-active-pair-defs (pair-rows single-strategies
                                 &key (pair-slots-per-tf *pair-slots-per-tf*)
                                      (competition-top-n *pair-competition-top-n*))
  (let ((groups (make-hash-table :test 'equal))
        (out nil))
    (dolist (p pair-rows)
      (let ((key (format nil "~a|~a" (getf p :symbol) (getf p :timeframe))))
        (push (list :type :pair :score (or (getf p :score) 0.0) :pair p) (gethash key groups))))
    (dolist (s single-strategies)
      (let ((key (format nil "~a|~a" (strategy-symbol s) (strategy-timeframe s))))
        (push (list :type :single :score (%single-score s) :single s) (gethash key groups))))
    (maphash
     (lambda (_key entries)
       (declare (ignore _key))
       (let* ((sorted (sort (copy-list entries) #'> :key (lambda (e) (getf e :score))))
              (top (subseq sorted 0 (min competition-top-n (length sorted))))
              (pairs (remove-if-not (lambda (e) (eq (getf e :type) :pair)) top))
              (pairs (subseq pairs 0 (min pair-slots-per-tf (length pairs)))))
         (dolist (p pairs)
           (let* ((pair (getf p :pair)))
             (push (list :pair-id (getf pair :pair-id)
                         :a (getf pair :a)
                         :b (getf pair :b)
                         :weight-a (getf pair :weight-a)
                         :weight-b (getf pair :weight-b)) out)))))
     groups)
    (nreverse out)))
```

**Step 4: Run test to verify it passes**

Run:
```
sbcl --script tests/test_runner.lisp
```
Expected: PASS.

**Step 5: Commit**

```
git add src/lisp/school/school-pair-composite.lisp src/lisp/tests/pair-composite-tests.lisp

git commit -m "feat: add hybrid pair slot selection"
```

---

### Task 5: Pair Evaluation Pipeline + Scheduler Hook

**Files:**
- Modify: `src/lisp/school/school-pair-composite.lisp`
- Modify: `src/lisp/school/school-db.lisp`
- Modify: `src/lisp/core/scheduler.lisp`

**Step 1: Write the failing test**

Add to `src/lisp/tests/pair-composite-tests.lisp` (new test verifies that promotion is blocked when OOS/CPCV trade_lists are missing):

```lisp
(deftest test-pair-promotion-blocked-without-oos-cpcv
  "pair promotion should be blocked if OOS/CPCV data missing"
  (let* ((pair (list :pair-id "P1" :a "A" :b "B"))
         (result (swimmy.school::pair-promotable-p pair :oos-trades-a nil :oos-trades-b nil)))
    (assert-true (null result) "pair should not be promotable without OOS")))
```

**Step 2: Run test to verify it fails**

Run:
```
sbcl --script tests/test_runner.lisp
```
Expected: FAIL due to undefined `pair-promotable-p`.

**Step 3: Write minimal implementation**

In `src/lisp/school/school-db.lisp`, add a helper to convert backtest rows to trade-list:

```lisp
(defun backtest-rows->trade-list (rows)
  (mapcar (lambda (row)
            (destructuring-bind (_rid _name ts pnl sym _dir _ep _xp _sl _tp _vol _hold _rank _tf _cat _reg _kind) row
              (declare (ignore _rid _name _dir _ep _xp _sl _tp _vol _hold _rank _tf _cat _reg _kind))
              (list (cons 'timestamp ts) (cons 'pnl pnl) (cons 'symbol sym))))
          rows))
```

In `src/lisp/school/school-pair-composite.lisp`, add promotion gating and a refresh pipeline:

```lisp
(defun pair-promotable-p (pair &key oos-trades-a oos-trades-b)
  (multiple-value-bind (metrics reason)
      (pair-metrics-from-trades oos-trades-a oos-trades-b :min-trades *pair-min-trades*)
    (declare (ignore reason))
    metrics))

(defun refresh-pair-strategies ()
  (let* ((strategies *strategy-knowledge-base*)
         (trade-map (make-hash-table :test 'equal)))
    (dolist (s strategies)
      (let* ((name (strategy-name s))
             (rows (fetch-backtest-trades name)))
        (when rows
          (setf (gethash name trade-map) (backtest-rows->trade-list rows)))))
    (multiple-value-bind (pairs _diag)
        (select-pair-candidates strategies trade-map)
      (declare (ignore _diag))
      (dolist (pair pairs)
        (let* ((a (getf pair :a))
               (b (getf pair :b))
               (oos-a (backtest-rows->trade-list (fetch-backtest-trades a :oos-kind "OOS")))
               (oos-b (backtest-rows->trade-list (fetch-backtest-trades b :oos-kind "OOS")))
               (cpcv-a (backtest-rows->trade-list (fetch-backtest-trades a :oos-kind "CPCV")))
               (cpcv-b (backtest-rows->trade-list (fetch-backtest-trades b :oos-kind "CPCV")))
               (symbol (strategy-symbol (find-strategy a)))
               (timeframe (strategy-timeframe (find-strategy a))))
          (multiple-value-bind (oos-metrics oos-reason)
              (pair-metrics-from-trades oos-a oos-b :min-trades *pair-min-trades*)
            (multiple-value-bind (cpcv-metrics cpcv-reason)
                (pair-metrics-from-trades cpcv-a cpcv-b :min-trades *pair-min-trades*)
              (declare (ignore oos-reason cpcv-reason))
              (let ((rank (cond ((and cpcv-metrics (>= (getf cpcv-metrics :sharpe) 0.5)) :S)
                                ((and oos-metrics (>= (getf oos-metrics :sharpe) 0.3)) :A)
                                (t :B))))
                (upsert-pair-strategy
                 (append pair
                         (list :symbol symbol :timeframe timeframe :rank rank
                               :oos-sharpe (and oos-metrics (getf oos-metrics :sharpe))
                               :cpcv-median (and cpcv-metrics (getf cpcv-metrics :sharpe))
                               :cpcv-pass-rate (and cpcv-metrics (getf cpcv-metrics :pass-rate)))))))))))))

(defun refresh-pair-active-defs ()
  (let* ((pairs (fetch-pair-strategies))
         (active (select-active-pair-defs pairs *strategy-knowledge-base*)))
    (setf *pair-active-defs* active)))
```

Then, in `src/lisp/core/scheduler.lisp`, hook into daily PnL aggregation:

```lisp
(when (fboundp 'swimmy.school::refresh-pair-strategies)
  (funcall 'swimmy.school::refresh-pair-strategies))
(when (fboundp 'swimmy.school::refresh-pair-active-defs)
  (funcall 'swimmy.school::refresh-pair-active-defs))
```

**Step 4: Run test to verify it passes**

Run:
```
sbcl --script tests/test_runner.lisp
```
Expected: PASS.

**Step 5: Commit**

```
git add src/lisp/school/school-pair-composite.lisp src/lisp/school/school-db.lisp src/lisp/core/scheduler.lisp src/lisp/tests/pair-composite-tests.lisp

git commit -m "feat: add pair evaluation pipeline"
```

---

### Task 6: Documentation Updates

**Files:**
- Modify: `doc/knowledge/implementation_plan_v50.6.md`
- Modify: `doc/owners_guide.md`
- Modify: `docs/llm/STATE.md`

**Step 1: Write the failing test**

No code tests. Proceed with doc updates only.

**Step 2: Update docs**

Add sections describing:
- `pair_strategies` table and required columns
- Hybrid slot competition (`*pair-slots-per-tf*`, `*pair-competition-top-n*`)
- OOS/CPCV composite gates for pairs
- Scheduler hook timing

**Step 3: Commit**

```
git add doc/knowledge/implementation_plan_v50.6.md doc/owners_guide.md docs/llm/STATE.md

git commit -m "docs: document pair strategy lifecycle"
```

---

## Execution Handoff
Plan complete and saved to `docs/plans/2026-02-07-pair-strategy-implementation-plan.md`. Two execution options:

1. Subagent-Driven (this session) - I dispatch fresh subagent per task, review between tasks, fast iteration
2. Parallel Session (separate) - Open new session with executing-plans, batch execution with checkpoints

Which approach?
