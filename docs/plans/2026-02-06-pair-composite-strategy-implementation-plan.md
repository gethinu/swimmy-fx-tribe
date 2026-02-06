# Pair-Composite Strategy Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 既存単一戦略を壊さず、Pair-Composite の選定・評価・実行キャップを追加し、backtest trade_list を永続化する。

**Architecture:** S式 BACKTEST_RESULT に含まれる `trade_list` を `backtest_trade_logs` に保存し、A/S/Legend から非相関ペアを選定して DB に記録。実行時はペア枠 0.05 lot の上限のみをオーバーレイで適用する（方向/ロジックは不変）。

**Tech Stack:** Common Lisp (SBCL), SQLite, S-expression (alist)

### Task 1: trade_logs に pair_id を追加

**Files:**
- Modify: `src/lisp/school/school-state.lisp`
- Modify: `src/lisp/school/school-db.lisp`
- Modify: `src/lisp/school/school-learning.lisp`
- Modify: `src/lisp/core/executor.lisp`
- Test: `src/lisp/tests/backtest-db-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-trade-logs-supports-pair-id
  "trade_logs should accept pair_id and persist it"
  (let* ((tmp-db (format nil "/tmp/swimmy-pair-~a.db" (get-universal-time)))
         (pair-id "PAIR-ABC"))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.school::record-trade-to-db
             (swimmy.school::make-trade-record
              :timestamp 1
              :strategy-name "TEST"
              :symbol "USDJPY"
              :direction :buy
              :category :trend
              :regime :trend
              :pnl 1.0
              :hold-time 10
              :pair-id pair-id))
            (let ((row (swimmy.school::execute-single
                        "SELECT pair_id FROM trade_logs WHERE strategy_name = ?" "TEST")))
              (assert-equal pair-id row "pair_id should persist")))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-trade-logs-supports-pair-id) (sb-ext:quit))'
```
Expected: FAIL with missing `pair-id` slot or SQL column.

**Step 3: Write minimal implementation**

```lisp
;; school-state.lisp
(defstruct trade-record
  ...
  pair-id)

;; school-db.lisp (CREATE TABLE + migration)
"CREATE TABLE IF NOT EXISTS trade_logs (..., pair_id TEXT)"
(handler-case (execute-non-query "ALTER TABLE trade_logs ADD COLUMN pair_id TEXT") (error () nil))

;; record-trade-to-db
"INSERT INTO trade_logs (..., pair_id) VALUES (..., ?)"

;; school-learning.lisp
(defun record-trade-outcome (... &key pair-id)
  (make-trade-record ... :pair-id pair-id))

;; executor.lisp (capture pair-id if available and pass)
(swimmy.school:record-trade-outcome symbol dir category strategy-name pnl :pair-id pair-id)
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-trade-logs-supports-pair-id) (sb-ext:quit))'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/school/school-state.lisp src/lisp/school/school-db.lisp src/lisp/school/school-learning.lisp src/lisp/core/executor.lisp src/lisp/tests/backtest-db-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add pair_id to trade logs"
```

### Task 2: backtest_trade_logs を追加

**Files:**
- Modify: `src/lisp/school/school-db.lisp`
- Modify: `src/lisp/packages-school.lisp`
- Test: `src/lisp/tests/backtest-db-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-backtest-trade-logs-insert
  "backtest_trade_logs should accept trade_list rows"
  (let* ((tmp-db (format nil "/tmp/swimmy-bt-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (swimmy.school:record-backtest-trades
             "RID-1" "STRAT-A" "BACKTEST"
             (list (list :timestamp 1 :pnl 1.0 :symbol "USDJPY" :direction "BUY" :volume 0.01)))
            (let ((count (swimmy.school::execute-single
                          "SELECT count(*) FROM backtest_trade_logs")))
              (assert-true (= count 1) "Expected 1 backtest trade row")))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-backtest-trade-logs-insert) (sb-ext:quit))'
```
Expected: FAIL (missing table/function).

**Step 3: Write minimal implementation**

```lisp
;; school-db.lisp
"CREATE TABLE IF NOT EXISTS backtest_trade_logs (... oos_kind TEXT)"

(defun record-backtest-trades (request-id strategy-name oos-kind trade-list)
  (dolist (t trade-list)
    (execute-non-query "INSERT INTO backtest_trade_logs (...) VALUES (?,?,?,?,...)" ...))

(defun fetch-backtest-trades (strategy-name &key oos-kind)
  (execute-to-list "SELECT ... FROM backtest_trade_logs WHERE strategy_name = ?" strategy-name))

;; packages-school.lisp
(export 'record-backtest-trades 'fetch-backtest-trades)
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-backtest-trade-logs-insert) (sb-ext:quit))'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/school/school-db.lisp src/lisp/packages-school.lisp src/lisp/tests/backtest-db-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add backtest_trade_logs persistence"
```

### Task 3: BACKTEST_RESULT の trade_list を保存

**Files:**
- Modify: `src/lisp/core/message-dispatcher.lisp`
- Test: `src/lisp/tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-backtest-result-persists-trade-list
  "internal-process-msg should persist trade_list with oos_kind mapping"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (called nil)
         (orig (symbol-function 'swimmy.school:record-backtest-trades))
         (msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"AAA-OOS\") (sharpe . 0.1) (trades . 1) (request_id . \"RID-1\") (trade_list . (((timestamp . 1) (pnl . 1.0)))))))") )
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:record-backtest-trades)
                (lambda (rid name kind trades)
                  (setf called (list rid name kind trades))
                  nil))
          (funcall fn msg)
          (assert-true called "record-backtest-trades should be called")
          (assert-equal "OOS" (third called) "oos_kind should be OOS"))
      (setf (symbol-function 'swimmy.school:record-backtest-trades) orig))))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-backtest-result-persists-trade-list) (sb-ext:quit))'
```
Expected: FAIL (no trade_list handling).

**Step 3: Write minimal implementation**

```lisp
;; message-dispatcher.lisp
(let* ((trade-list (%alist-val result '(trade_list trade-list) nil))
       (request-id (%alist-val result '(request_id request-id) nil))
       (oos-kind (cond (is-oos "OOS")
                       ((or is-qual is-rr) "BACKTEST")
                       (t "BACKTEST"))))
  (when (and trade-list request-id name)
    (swimmy.school:record-backtest-trades request-id name oos-kind trade-list)))
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-backtest-result-persists-trade-list) (sb-ext:quit))'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/core/message-dispatcher.lisp src/lisp/tests.lisp
git commit -m "feat: persist backtest trade_list from results"
```

### Task 4: Pair-Composite 選定ロジックを追加

**Files:**
- Create: `src/lisp/school/school-pair-composite.lisp`
- Modify: `swimmy.asd`
- Modify: `src/lisp/tests/pair-composite-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-pair-id-stable
  "pair-id should be order-independent"
  (let ((a (swimmy.school::pair-id "A" "B"))
        (b (swimmy.school::pair-id "B" "A")))
    (assert-equal a b "pair-id should be stable")))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-id-stable) (sb-ext:quit))'
```
Expected: FAIL (pair-id not implemented).

**Step 3: Write minimal implementation**

```lisp
;; school-pair-composite.lisp (FNV-1a 64bit)
(defun %fnv1a-64 (s)
  (let ((hash #xCBF29CE484222325))
    (loop for ch across s
          do (setf hash (logxor hash (char-code ch))
                   hash (ldb (byte 64 0) (* hash #x100000001B3))))
    hash))

(defun pair-id (a b)
  (let* ((names (sort (list a b) #'string<))
         (raw (format nil "~a|~a" (first names) (second names))))
    (format nil "~16,'0x" (%fnv1a-64 raw))))
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-id-stable) (sb-ext:quit))'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/school/school-pair-composite.lisp swimmy.asd src/lisp/tests/pair-composite-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add pair-composite core utilities"
```

### Task 5: Execution Overlay と pair_id 伝播

**Files:**
- Modify: `src/lisp/school/school-execution.lisp`
- Modify: `src/lisp/school/school-allocation.lisp`
- Modify: `src/lisp/core/executor.lisp`
- Modify: `src/lisp/school/school-learning.lisp`
- Modify: `src/lisp/school/school-db.lisp`
- Test: `src/lisp/tests/pair-composite-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-pair-overlay-caps-lot
  "pair overlay should cap lot to 0.05 when enabled"
  (let* ((swimmy.school::*pair-strategy-enabled* t)
         (swimmy.school::*pair-active-defs* (list (list :pair-id "P1" :a "A" :b "B" :weight-a 0.5 :weight-b 0.5)))
         (lot (swimmy.school::apply-pair-overlay "A" :buy "USDJPY" 0.2)))
    (assert-true (<= (first lot) 0.05) "lot should be capped")))
```

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-overlay-caps-lot) (sb-ext:quit))'
```
Expected: FAIL (overlay not implemented).

**Step 3: Write minimal implementation**

```lisp
;; school-execution.lisp
(let* ((lot (calc-execution-lot ...))
       (overlay (swimmy.school::apply-pair-overlay lead-name direction symbol lot))
       (final-lot (first overlay))
       (pair-id (second overlay)))
  (execute-order-sequence ... final-lot ... pair-id))

;; school-allocation.lisp
(defun try-reserve-warrior-slot (... &key pair-id lot)
  (register-pending-order magic strategy-name symbol category direction :pair-id pair-id :lot lot))

(defun register-pending-order (... &key pair-id lot)
  (setf (gethash magic *pending-orders*) (list ... :pair-id pair-id :lot lot)))

;; executor.lisp
(let ((pair-id (swimmy.school::lookup-pair-id-by-magic magic-from-json)))
  (swimmy.school:record-trade-outcome ... :pair-id pair-id))
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-overlay-caps-lot) (sb-ext:quit))'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/school/school-execution.lisp src/lisp/school/school-allocation.lisp src/lisp/core/executor.lisp src/lisp/school/school-learning.lisp src/lisp/school/school-db.lisp src/lisp/tests/pair-composite-tests.lisp src/lisp/tests.lisp
git commit -m "feat: apply pair overlay and propagate pair_id"
```

### Final Verification

Run:
```bash
./tools/sbcl_sanity_check.sh
```
Expected: PASS (no load errors).

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy-tests:run-all-tests) (sb-ext:quit))'
```
Expected: PASS (all tests).
