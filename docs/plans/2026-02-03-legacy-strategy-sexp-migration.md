# Legacy Strategy SEXP Migration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 旧 plist 形式の `strategies.data_sexp` を #S(STRATEGY ...) へ完全移行し、DB内の互換性エラーと誤検知ログを解消する。

**Architecture:** オフライン一括移行。旧DBをバックアップ後、新DBへ全テーブルを移送し、`strategies.data_sexp` のみ再シリアライズして差し替える。変換不能行は隔離し件数を報告する。

**Tech Stack:** Common Lisp (SBCL), cl-sqlite, Swimmy Lisp system, SQLite WAL.

---

### Task 0: Worktree準備（生成ファイルの同期）

**Files:**
- Copy: `src/lisp/school/school-optimized-params.lisp`

**Step 1: 生成ファイルを同期**

```bash
cp /home/swimmy/swimmy/src/lisp/school/school-optimized-params.lisp \
  /home/swimmy/swimmy/.worktrees/legacy-sexp-migration/src/lisp/school/school-optimized-params.lisp
```

**Expected:** ファイルが存在し、SBCLロード時の missing file エラーが消える。

---

### Task 1: 変換ロジックのテスト追加（RED）

**Files:**
- Create: `src/lisp/tests/strategy-migration-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: 失敗するテストを作成**

```lisp
;;; src/lisp/tests/strategy-migration-tests.lisp
(in-package :swimmy.tests)

(deftest test-normalize-legacy-plist->strategy
  "plist形式を strategy に変換して #S に再シリアライズする"
  (let* ((sexp "(:NAME \"Legacy-1\" :TIMEFRAME \"M5\" :DIRECTION :BOTH :SYMBOL \"USDJPY\" :SL 0.1 :TP 0.2 :SHARPE 0.3 :PROFIT-FACTOR 1.1 :WIN-RATE 45.0 :MAX-DD 0.1)")
         (result (swimmy.school::normalize-strategy-sexp sexp :indicators "(SMA-10)" :entry "(> 1 0)" :exit "(< 1 0)"))
         (strat (getf result :strategy))
         (out   (getf result :sexp)))
    (assert-true (and strat (strategy-p strat)) "Expected strategy instance")
    (assert-equal "Legacy-1" (strategy-name strat))
    (assert-equal "M5" (strategy-timeframe strat))
    (assert-true (and (stringp out) (search "#S(STRATEGY" out))) "Expected #S serialization")))

(deftest test-normalize-struct-roundtrip
  "#S形式はそのまま strategy として読み直せる"
  (let* ((s (swimmy.school:make-strategy :name "S-1" :symbol "USDJPY" :timeframe "H1"))
         (sexp (format nil "~s" s))
         (result (swimmy.school::normalize-strategy-sexp sexp))
         (out (getf result :sexp)))
    (assert-true (and (stringp out) (search "#S(STRATEGY" out))) "Expected #S serialization")))
```

**Step 2: run-all-tests に追加**

`src/lisp/tests.lisp` の `run-all-tests` リストに以下を追加:
- `test-normalize-legacy-plist->strategy`
- `test-normalize-struct-roundtrip`

**Step 3: テストを実行して失敗を確認**

```bash
sbcl --noinform --disable-debugger \
  --eval '(require :asdf)' \
  --eval '(push (uiop:getcwd) asdf:*central-registry*)' \
  --eval '(asdf:load-system :swimmy)' \
  --eval '(swimmy.tests::test-normalize-legacy-plist->strategy)' \
  --eval '(swimmy.tests::test-normalize-struct-roundtrip)' \
  --quit
```

**Expected:** `normalize-strategy-sexp` 未実装で FAIL。

---

### Task 2: 変換ロジックの実装（GREEN）

**Files:**
- Create: `src/lisp/school/school-sexp-migration.lisp`
- Modify: `swimmy.asd`

**Step 1: 最小実装を追加**

```lisp
;;; src/lisp/school/school-sexp-migration.lisp
(in-package :swimmy.school)

(defun %legacy-plist-p (obj)
  (and (listp obj) (keywordp (first obj))))

(defun %parse-maybe-form (form-str)
  (when (and form-str (stringp form-str) (> (length form-str) 0))
    (swimmy.core:safe-read-sexp form-str :package :swimmy.school)))

(defun %plist->strategy (plist &key indicators entry exit)
  (let ((name (or (getf plist :name) (getf plist :NAME))))
    (when name
      (make-strategy
       :name name
       :indicators (or indicators '())
       :entry entry
       :exit exit
       :sl (or (getf plist :sl) (getf plist :SL) 0.0)
       :tp (or (getf plist :tp) (getf plist :TP) 0.0)
       :sharpe (or (getf plist :sharpe) (getf plist :SHARPE) 0.0)
       :profit-factor (or (getf plist :profit-factor) (getf plist :PROFIT-FACTOR) 0.0)
       :win-rate (or (getf plist :win-rate) (getf plist :WIN-RATE) 0.0)
       :max-dd (or (getf plist :max-dd) (getf plist :MAX-DD) 0.0)
       :timeframe (or (getf plist :timeframe) (getf plist :TIMEFRAME) "M1")
       :direction (or (getf plist :direction) (getf plist :DIRECTION) :BOTH)
       :symbol (or (getf plist :symbol) (getf plist :SYMBOL) "USDJPY")))))

(defun normalize-strategy-sexp (sexp-str &key indicators entry exit)
  "Return plist (:status :ok/:quarantine :strategy strat :sexp serialized :reason reason)."
  (let* ((obj (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school))
         (strat (cond
                  ((strategy-p obj) obj)
                  ((%legacy-plist-p obj) (%plist->strategy obj :indicators indicators :entry entry :exit exit))
                  (t nil))))
    (if strat
        (list :status :ok :strategy strat :sexp (format nil "~s" strat))
        (list :status :quarantine :strategy nil :sexp nil :reason :invalid))))
```

**Step 2: ASDF に追加**

`swimmy.asd` の school セクションに `school-sexp-migration` を追加。

**Step 3: テストを再実行**

```bash
sbcl --noinform --disable-debugger \
  --eval '(require :asdf)' \
  --eval '(push (uiop:getcwd) asdf:*central-registry*)' \
  --eval '(asdf:load-system :swimmy)' \
  --eval '(swimmy.tests::test-normalize-legacy-plist->strategy)' \
  --eval '(swimmy.tests::test-normalize-struct-roundtrip)' \
  --quit
```

**Expected:** PASS。

**Step 4: Commit**

```bash
git add src/lisp/school/school-sexp-migration.lisp src/lisp/tests/strategy-migration-tests.lisp src/lisp/tests.lisp swimmy.asd
git commit -m "feat: add legacy strategy sexp normalization"
```

---

### Task 3: オフライン移行スクリプトの実装

**Files:**
- Create: `tools/migrate_strategy_sexp.lisp`

**Step 1: 移行スクリプト追加**

```lisp
;;; tools/migrate_strategy_sexp.lisp
(require :asdf)
(load "swimmy.asd")
(asdf:load-system :swimmy)

(in-package :cl-user)

(defparameter *source-db* "data/memory/swimmy.db")
(defparameter *dest-db* "data/memory/swimmy.db.migrated")
(defparameter *quarantine-dir* "data/memory/quarantine/")

(setf swimmy.school::*disable-auto-migration* t)

(defun %ensure-quarantine ()
  (ensure-directories-exist *quarantine-dir*))

(defun %copy-table (conn table)
  (sqlite:execute-non-query conn (format nil "INSERT INTO ~a SELECT * FROM old.~a" table table)))

(defun %migrate-strategies (conn)
  (let ((rows (sqlite:execute-to-list conn "SELECT name, indicators, entry, exit, sl, tp, volume, sharpe, profit_factor, win_rate, trades, max_dd, category, timeframe, generation, rank, symbol, direction, last_bt_time, hash, oos_sharpe, cpcv_median, cpcv_pass_rate, data_sexp, updated_at FROM old.strategies"))
        (ok 0)
        (bad 0)
        (qfile (merge-pathnames (format nil "strategies_quarantine_~d.sexp" (get-universal-time)) *quarantine-dir*)))
    (%ensure-quarantine)
    (with-open-file (q qfile :direction :output :if-exists :supersede)
      (dolist (row rows)
        (destructuring-bind (name indicators entry exit sl tp volume sharpe profit-factor win-rate trades max-dd category timeframe generation rank symbol direction last-bt-time hash oos-sharpe cpcv-median cpcv-pass-rate data-sexp updated-at) row
          (let* ((ind (swimmy.school::%parse-maybe-form indicators))
                 (ent (swimmy.school::%parse-maybe-form entry))
                 (exi (swimmy.school::%parse-maybe-form exit))
                 (result (swimmy.school::normalize-strategy-sexp data-sexp :indicators ind :entry ent :exit exi)))
            (if (eq (getf result :status) :ok)
                (progn
                  (incf ok)
                  (sqlite:execute-non-query conn
                    "INSERT OR REPLACE INTO strategies (name, indicators, entry, exit, sl, tp, volume, sharpe, profit_factor, win_rate, trades, max_dd, category, timeframe, generation, rank, symbol, direction, last_bt_time, hash, oos_sharpe, cpcv_median, cpcv_pass_rate, data_sexp, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                    name indicators entry exit sl tp volume sharpe profit-factor win-rate trades max-dd category timeframe generation rank symbol direction last-bt-time hash oos-sharpe cpcv-median cpcv-pass-rate (getf result :sexp) updated-at))
                (progn
                  (incf bad)
                  (format q "~s~%" (list :name name :reason (getf result :reason) :data_sexp data-sexp)))))))
      (format t "[MIGRATE] ok=~d bad=~d quarantine=~a~%" ok bad qfile))))

(defun run-migration ()
  ;; Prepare new DB
  (when (probe-file *dest-db*) (delete-file *dest-db*))
  (setf swimmy.core:*sqlite-conn* nil)
  (setf swimmy.core:*db-path-default* *dest-db*)
  (swimmy.school:init-db)
  (let ((conn (swimmy.core:get-db-connection)))
    (sqlite:execute-non-query conn (format nil "ATTACH DATABASE '~a' AS old" *source-db*))
    (%copy-table conn "trade_logs")
    (%copy-table conn "swap_history")
    (%copy-table conn "oos_queue")
    (%migrate-strategies conn)
    (sqlite:execute-non-query conn "DETACH DATABASE old"))
  (swimmy.core:close-db-connection)
  (format t "[MIGRATE] done -> ~a~%" *dest-db*))

(run-migration)
(sb-ext:exit)
```

**Step 2: 実行コマンドを用意**

```bash
sbcl --noinform --disable-debugger \
  --load tools/migrate_strategy_sexp.lisp
```

**Expected:** `ok`/`bad`/`quarantine` 件数が表示され、新DBが生成される。

**Step 3: Commit**

```bash
git add tools/migrate_strategy_sexp.lisp
git commit -m "feat: add offline strategy sexp migration tool"
```

---

### Task 4: 運用手順の追記と検証コマンド

**Files:**
- Modify: `doc/runbook.md`

**Step 1: 追加内容（例）**

- 停止手順（`swimmy-brain`, `swimmy-backtest`）
- DBバックアップ手順
- 移行実行コマンド
- 件数検証（旧/新）
- `data_sexp LIKE '#S(%'` が100%の検証
- 旧DBへのロールバック方法

**Step 2: 実行例を記載**

```bash
sudo systemctl stop swimmy-brain swimmy-backtest
cp data/memory/swimmy.db data/memory/backup/swimmy.db.$(date +%Y%m%d%H%M%S)
sbcl --noinform --disable-debugger --load tools/migrate_strategy_sexp.lisp
mv data/memory/swimmy.db.migrated data/memory/swimmy.db
sudo systemctl start swimmy-brain swimmy-backtest
```

**Step 3: Commit**

```bash
git add doc/runbook.md
git commit -m "docs: add offline sexp migration runbook"
```

---

### Task 5: 最終検証（手動）

**Files:**
- None (runtime verification)

**Step 1: 件数と #S 率の検証**

```bash
python3 - <<'PY'
import sqlite3
conn = sqlite3.connect('data/memory/swimmy.db')
cur = conn.cursor()
print('total', cur.execute('SELECT count(*) FROM strategies').fetchone()[0])
print('#S', cur.execute("SELECT count(*) FROM strategies WHERE data_sexp LIKE '#S(%'").fetchone()[0])
conn.close()
PY
```

**Expected:** `total == #S`。

**Step 2: レポートとログ確認**
- `data/reports/evolution_factory_report.txt` で A/S 集計が戻っている
- `logs/swimmy.log` に `Corrupted Strategy SEXP` が出ない

---

## Notes
- 現状のテストは `school-optimized-params.lisp` が無いと失敗するため、Task 0 を必須とする。
- 既存 DB に `indicators/entry/exit` が null の場合は空として再生成される。重篤な欠損は quarantine に隔離する。
