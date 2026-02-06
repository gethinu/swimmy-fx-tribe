# Noncorrelated Promotion Notifications Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add daily PnL aggregation and send Discord notifications with a Dalio-style noncorrelation score when a strategy is promoted to A or S rank.

**Architecture:** A daily batch aggregates `trade_logs` into `strategy_daily_pnl`. On promotion, compute correlation from the last 30 trading days of daily PnL and send a Discord report to `SWIMMY_DISCORD_REPORTS`.

**Tech Stack:** Common Lisp (SBCL), SQLite via `sqlite`, existing Discord Notifier (`queue-discord-notification`), ASDF system loading.

---

### Task 1: Add Daily PnL Table + Aggregation Batch

**Files:**
- Modify: `src/lisp/school/school-db.lisp`
- Modify: `src/lisp/tests/backtest-db-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add this test to `src/lisp/tests/backtest-db-tests.lisp`:

```lisp
(deftest test-strategy-daily-pnl-aggregation
  "Daily aggregation should sum pnl per strategy per day"
  (let* ((tmp-db (format nil "/tmp/swimmy-daily-~a.db" (get-universal-time)))
         (t1 (encode-universal-time 0 0 12 1 2 2026))
         (t2 (encode-universal-time 0 30 12 1 2 2026))
         (t3 (encode-universal-time 0 0 12 2 2 2026)))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (dolist (spec (list (list t1 1.0) (list t2 2.0) (list t3 -1.0)))
              (destructuring-bind (ts pnl) spec
                (swimmy.school::record-trade-to-db
                 (swimmy.school::make-trade-record
                  :timestamp ts
                  :strategy-name "TEST"
                  :symbol "USDJPY"
                  :direction :buy
                  :category :trend
                  :regime :trend
                  :pnl pnl
                  :hold-time 10))))
            (swimmy.school::refresh-strategy-daily-pnl)
            (let* ((day1 (swimmy.school::execute-single
                          "SELECT date(datetime(? - 2208988800, 'unixepoch', 'localtime'))" t1))
                   (day2 (swimmy.school::execute-single
                          "SELECT date(datetime(? - 2208988800, 'unixepoch', 'localtime'))" t3))
                   (rows (swimmy.school::execute-to-list
                          "SELECT trade_date, pnl_sum, trade_count FROM strategy_daily_pnl WHERE strategy_name = ?"
                          "TEST"))
                   (table (make-hash-table :test 'equal)))
              (dolist (row rows)
                (setf (gethash (first row) table) (list (second row) (third row))))
              (assert-true (= (length rows) 2) "Expected two daily rows")
              (assert-equal (list 3.0 2) (gethash day1 table) "Day1 sum/count")
              (assert-equal (list -1.0 1) (gethash day2 table) "Day2 sum/count")))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))
```

Add the test to the runner list in `src/lisp/tests.lisp`:

```lisp
                  test-strategy-daily-pnl-aggregation
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --script tests/test_runner.lisp
```
Expected: FAIL with `refresh-strategy-daily-pnl` undefined or missing table.

**Step 3: Write minimal implementation**

Update `src/lisp/school/school-db.lisp`:

1) Add table creation in `init-db`:
```lisp
  (execute-non-query
   "CREATE TABLE IF NOT EXISTS strategy_daily_pnl (
      strategy_name TEXT NOT NULL,
      trade_date TEXT NOT NULL,
      pnl_sum REAL NOT NULL,
      trade_count INTEGER NOT NULL,
      updated_at INTEGER NOT NULL,
      PRIMARY KEY (strategy_name, trade_date)
    )")
```

2) Add batch aggregation + fetch helpers:
```lisp
(defun refresh-strategy-daily-pnl ()
  "Aggregate trade_logs into daily PnL (JST localtime)."
  (init-db)
  (let ((now (get-universal-time)))
    (execute-non-query
     "INSERT INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
      SELECT strategy_name,
             date(datetime(timestamp - 2208988800, 'unixepoch', 'localtime')) AS trade_date,
             SUM(pnl) AS pnl_sum,
             COUNT(*) AS trade_count,
             ? AS updated_at
        FROM trade_logs
       GROUP BY strategy_name, trade_date
      ON CONFLICT(strategy_name, trade_date)
      DO UPDATE SET pnl_sum=excluded.pnl_sum,
                    trade_count=excluded.trade_count,
                    updated_at=excluded.updated_at"
     now)))

(defun fetch-strategy-daily-pnl (strategy-name &key (limit 30))
  "Fetch last N daily pnl rows (date, pnl_sum) for a strategy."
  (init-db)
  (execute-to-list
   "SELECT trade_date, pnl_sum FROM strategy_daily_pnl
     WHERE strategy_name = ?
     ORDER BY trade_date DESC
     LIMIT ?"
   strategy-name limit))
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --script tests/test_runner.lisp
```
Expected: PASS, test `test-strategy-daily-pnl-aggregation` succeeds.

**Step 5: Commit**

```bash
git add src/lisp/school/school-db.lisp src/lisp/tests/backtest-db-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add daily pnl aggregation table and batch"
```

---

### Task 2: Daily Correlation + Noncorrelation Score

**Files:**
- Modify: `src/lisp/school/school-dalio.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add to `src/lisp/tests.lisp`:

```lisp
(deftest test-daily-pnl-correlation
  "Daily PnL correlation should be near 1 for identical series"
  (let* ((tmp-db (format nil "/tmp/swimmy-corr-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            ;; Seed 3 days of identical pnl for A and B
            (dolist (spec '(("2026-02-01" 1.0) ("2026-02-02" 2.0) ("2026-02-03" 3.0)))
              (destructuring-bind (d v) spec
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "A" d v 1 (get-universal-time))
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "B" d v 1 (get-universal-time))))
            (let ((corr (swimmy.school::calculate-daily-pnl-correlation "A" "B" :days 3)))
              (assert-true (and corr (> corr 0.99)) "Correlation should be ~1")))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))
```

Add to run list in `run-all-tests`:

```lisp
                  test-daily-pnl-correlation
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --script tests/test_runner.lisp
```
Expected: FAIL with `calculate-daily-pnl-correlation` undefined.

**Step 3: Write minimal implementation**

Update `src/lisp/school/school-dalio.lisp` with new helpers:

```lisp
(defun %strategy-name (s)
  (cond
    ((stringp s) s)
    ((and (fboundp 'strategy-name) (ignore-errors (strategy-name s))) (strategy-name s))
    (t (format nil "~a" s))))

(defun %daily-pnl-map (strategy &key (days 30))
  (let* ((name (%strategy-name strategy))
         (rows (fetch-strategy-daily-pnl name :limit days))
         (table (make-hash-table :test 'equal)))
    (dolist (row rows)
      (setf (gethash (first row) table) (second row)))
    (values table (length rows))))

(defun aligned-daily-pnl-vectors (s1 s2 &key (days 30))
  (multiple-value-bind (map1 count1) (%daily-pnl-map s1 :days days)
    (multiple-value-bind (map2 count2) (%daily-pnl-map s2 :days days)
      (when (and (>= count1 days) (>= count2 days))
        (let ((v1 nil)
              (v2 nil))
          (maphash (lambda (k v)
                     (let ((v2val (gethash k map2 :missing)))
                       (unless (eq v2val :missing)
                         (push v v1)
                         (push v2val v2))))
                   map1)
          (when (>= (length v1) days)
            (values (coerce (nreverse v1) 'vector)
                    (coerce (nreverse v2) 'vector))))))))

(defun calculate-daily-pnl-correlation (s1 s2 &key (days 30))
  "Pearson correlation on aligned daily pnl vectors. Returns NIL if insufficient data."
  (multiple-value-bind (v1 v2) (aligned-daily-pnl-vectors s1 s2 :days days)
    (when (and v1 v2)
      (pearson-correlation v1 v2))))

(defun calculate-noncorrelation-score (strategies &key (days 30) (threshold *correlation-threshold*))
  "Return (values score reason). Score = uncorrelated_pairs / total_pairs."
  (let ((list (if (listp strategies) strategies (list strategies))))
    (when (< (length list) 2)
      (return-from calculate-noncorrelation-score (values nil :insufficient-strategies)))
    (let ((count 0)
          (uncorrelated 0))
      (dotimes (i (length list))
        (dotimes (j i)
          (let* ((s1 (nth i list))
                 (s2 (nth j list))
                 (corr (calculate-daily-pnl-correlation s1 s2 :days days)))
            (unless corr
              (return-from calculate-noncorrelation-score (values nil :insufficient-data)))
            (incf count)
            (when (< (abs corr) threshold)
              (incf uncorrelated)))))
      (if (zerop count)
          (values nil :insufficient-strategies)
          (values (/ (float uncorrelated) count) nil)))))
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --script tests/test_runner.lisp
```
Expected: PASS, `test-daily-pnl-correlation` succeeds.

**Step 5: Commit**

```bash
git add src/lisp/school/school-dalio.lisp src/lisp/tests.lisp
git commit -m "feat: add daily pnl correlation utilities"
```

---

### Task 3: Daily Batch Scheduling (00:10 JST)

**Files:**
- Modify: `src/lisp/core/globals.lisp`
- Modify: `src/lisp/core/scheduler.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add to `src/lisp/tests.lisp`:

```lisp
(defun mock-refresh-daily-pnl ()
  (push :daily-pnl *test-results*))

(deftest test-daily-pnl-aggregation-scheduler
  "Daily PnL aggregation should trigger at 00:10"
  (let ((swimmy.globals:*daily-pnl-aggregation-sent-today* nil)
        (orig (and (fboundp 'swimmy.school::refresh-strategy-daily-pnl)
                   (symbol-function 'swimmy.school::refresh-strategy-daily-pnl))))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::refresh-strategy-daily-pnl) #'mock-refresh-daily-pnl)
          (let ((time-0009 (encode-universal-time 0 9 0 1 2 2026)))
            (swimmy.main:check-scheduled-tasks time-0009)
            (assert-false swimmy.globals:*daily-pnl-aggregation-sent-today* "Should not trigger before 00:10"))
          (let ((time-0010 (encode-universal-time 0 10 0 1 2 2026)))
            (swimmy.main:check-scheduled-tasks time-0010)
            (assert-true swimmy.globals:*daily-pnl-aggregation-sent-today* "Should trigger at 00:10")))
      (when orig
        (setf (symbol-function 'swimmy.school::refresh-strategy-daily-pnl) orig)))))
```

Add to run list in `run-all-tests`:

```lisp
                  test-daily-pnl-aggregation-scheduler
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --script tests/test_runner.lisp
```
Expected: FAIL with missing flag or no trigger.

**Step 3: Write minimal implementation**

Update `src/lisp/core/globals.lisp`:

```lisp
(defparameter *daily-pnl-aggregation-sent-today* nil)
```

Update `src/lisp/core/scheduler.lisp`:

1) Add flag alongside other scheduler flags:
```lisp
(defvar *daily-pnl-aggregation-sent-today* nil "Prevents duplicate daily pnl aggregation")
```

2) Reset on new day inside `check-scheduled-tasks`:
```lisp
      (setf *daily-pnl-aggregation-sent-today* nil)
```

3) Trigger at 00:10:
```lisp
    (when (and (= h 0) (>= m 10) (not *daily-pnl-aggregation-sent-today*))
      (setf *daily-pnl-aggregation-sent-today* t)
      (format t "[SCHEDULER] ⏰ 00:10 Aggregating daily PnL...~%")
      (when (fboundp 'swimmy.school::refresh-strategy-daily-pnl)
        (funcall 'swimmy.school::refresh-strategy-daily-pnl)))
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --script tests/test_runner.lisp
```
Expected: PASS, `test-daily-pnl-aggregation-scheduler` succeeds.

**Step 5: Commit**

```bash
git add src/lisp/core/globals.lisp src/lisp/core/scheduler.lisp src/lisp/tests.lisp
git commit -m "feat: schedule daily pnl aggregation"
```

---

### Task 4: Promotion Notification Hook

**Files:**
- Modify: `src/lisp/school/school-dalio.lisp`
- Modify: `src/lisp/school/school-rank-system.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

Add to `src/lisp/tests.lisp`:

```lisp
(deftest test-promotion-triggers-noncorrelation-notification
  "Ensure A/S promotions fire noncorrelation notification once"
  (let* ((tmp-db (format nil "/tmp/swimmy-promo-~a.db" (get-universal-time)))
         (strat (make-strategy :name "PROMO" :symbol "USDJPY" :rank :B))
         (called 0)
         (orig (and (fboundp 'swimmy.school::notify-noncorrelated-promotion)
                    (symbol-function 'swimmy.school::notify-noncorrelated-promotion))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school::notify-noncorrelated-promotion)
                  (lambda (&rest args) (declare (ignore args)) (incf called)))
            (swimmy.school::ensure-rank strat :A "test")
            (assert-equal 1 called "A promotion should notify once")
            (swimmy.school::ensure-rank strat :S "test")
            (assert-equal 2 called "S promotion should notify once"))
        (when orig
          (setf (symbol-function 'swimmy.school::notify-noncorrelated-promotion) orig))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))
```

Add to run list in `run-all-tests`:

```lisp
                  test-promotion-triggers-noncorrelation-notification
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --script tests/test_runner.lisp
```
Expected: FAIL with missing `notify-noncorrelated-promotion` or no hook.

**Step 3: Write minimal implementation**

Update `src/lisp/school/school-dalio.lisp`:

```lisp
(defun %promotion-p (old-rank new-rank)
  (or (and (eq new-rank :A)
           (not (member old-rank '(:A :S :LEGEND) :test #'eq)))
      (and (eq new-rank :S) (eq old-rank :A))))

(defun %current-portfolio-strategies ()
  (remove-if-not
   (lambda (s)
     (member (strategy-rank s) '(:A :S :LEGEND) :test #'eq))
   *strategy-knowledge-base*))

(defun notify-noncorrelated-promotion (strategy new-rank &key (days 30))
  "Compute noncorrelation score and send Discord notification."
  (let* ((portfolio (%current-portfolio-strategies))
         (score nil)
         (reason nil))
    (multiple-value-setq (score reason)
      (calculate-noncorrelation-score portfolio :days days :threshold *correlation-threshold*))
    (let* ((title "⚖️ 非相関 昇格通知")
           (threshold *correlation-threshold*)
           (portfolio-size (length portfolio))
           (ts (swimmy.core::get-jst-timestamp))
           (score-text (if score (format nil "~,2f" score) "N/A"))
           (reason-text (case reason
                          (:insufficient-data "(データ不足)")
                          (:insufficient-strategies "(戦略数不足)")
                          (t "")))
           (msg (format nil "戦略: **~a**~%昇格: **~a**~%非相関スコア: **~a** ~a~%閾値: |corr| < ~,2f~%ポートフォリオ: ~d 戦略~%時刻: ~a"
                        (strategy-name strategy)
                        new-rank
                        score-text
                        reason-text
                        threshold
                        portfolio-size
                        ts))
           (webhook (or swimmy.core:*discord-daily-webhook* swimmy.globals:*discord-webhook-url*)))
      (when webhook
        (swimmy.core:queue-discord-notification webhook msg :color swimmy.core:+color-info+ :title title)))))
```

Update `src/lisp/school/school-rank-system.lisp` to hook in `%ensure-rank-no-lock`:

```lisp
      (let ((promotion-p (swimmy.school::%promotion-p old-rank new-rank)))
        (when promotion-p
          (handler-case
              (swimmy.school::notify-noncorrelated-promotion strategy new-rank)
            (error (e)
              (format t "[RANK] ⚠️ Noncorrelation notify failed: ~a~%" e)))))
```

Place the snippet right after `(upsert-strategy strategy)` to ensure rank is persisted before notification.

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --script tests/test_runner.lisp
```
Expected: PASS, `test-promotion-triggers-noncorrelation-notification` succeeds.

**Step 5: Commit**

```bash
git add src/lisp/school/school-dalio.lisp src/lisp/school/school-rank-system.lisp src/lisp/tests.lisp
git commit -m "feat: notify noncorrelated score on A/S promotion"
```

---

### Final Verification

Run full test suite:
```bash
sbcl --script tests/test_runner.lisp
```
Expected: All tests pass.

---

### Rollout Notes
- Daily batch runs at 00:10 JST; check `logs/brain.out` for `[SCHEDULER]` log.
- First run will backfill daily aggregates from existing `trade_logs`.
