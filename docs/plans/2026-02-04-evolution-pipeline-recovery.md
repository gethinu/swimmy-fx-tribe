# Evolution Pipeline Recovery Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** BACKTEST_RESULT が Brain/DB に確実に届き、バックテストの処理速度とキュー制御が釣り合う状態にして、進化パイプラインの Sharpe が回復する。

**Architecture:** 既存の ZMQ S式パイプライン（Brain ⇄ Backtest Service ⇄ Guardian）を維持し、Brain 側にバックプレッシャーとスループット制御を追加する。データ範囲の縮小は既存オプション（`candles_file`, `start_time`, `end_time`）の範囲内で行う。

**Tech Stack:** Common Lisp (SBCL, pzmq), Python (Backtest Service), SQLite, systemd.

## Progress
- 2026-02-04: Task 1-6 完了（バックプレッシャー/送信レート/受信計測/Deferred Flush/CSV Override/STATE更新）。

## 前提と正本
- 仕様正本: `docs/llm/SPEC.md`, `docs/llm/ARCHITECTURE.md`, `docs/llm/INTERFACES.md`, `docs/llm/STATE.md`
- 診断: `doc/knowledge/evolution_pipeline_diagnosis_2026-02-04.md`
- テスト: `./ci-test.sh`（`swimmy.tests:run-all-tests`）

## 未確定事項（要回答）
1. スループット改善の主軸はどれを優先しますか。
A. CSV を物理的に軽量化して差し替え
B. `start_time`/`end_time` を使った範囲指定を徹底
C. Backtest Service の並列化（複数 Guardian ワーカー）
2. `docs/llm/STATE.md` にある `SWIMMY_DEFERRED_FLUSH_BATCH` と `SWIMMY_DEFERRED_FLUSH_INTERVAL_SEC` は、実装でコードに反映させる方針で合っていますか。
3. 低負荷 CSV の対象は USDJPY のみで良いですか（EURUSD/GBPUSD も同様に作成しますか）。

---

### Task 1: バックテスト送信のバックプレッシャー基盤を追加

**Files:**
- Modify: `src/lisp/core/config.lisp`
- Modify: `src/lisp/core/globals.lisp`

**Step 1: Write the failing test**
```lisp
(deftest test-backtest-pending-counters-defaults
  "backtest pending counters should initialize to sane defaults"
  (assert-true (boundp 'swimmy.globals::*backtest-submit-count*) "submit counter exists")
  (assert-true (boundp 'swimmy.globals::*backtest-max-pending*) "max pending exists")
  (assert-true (numberp swimmy.globals::*backtest-max-pending*) "max pending numeric"))
```

**Step 2: Run test to verify it fails**
Run: `./ci-test.sh`
Expected: FAIL because the new globals are missing.

**Step 3: Write minimal implementation**
```lisp
;; src/lisp/core/config.lisp
(defparameter *backtest-max-pending* (env-int-or "SWIMMY_BACKTEST_MAX_PENDING" 500)
  "Max pending backtest requests before throttling.")
(defparameter *backtest-rate-limit-per-sec* (env-int-or "SWIMMY_BACKTEST_RATE_LIMIT" 5)
  "Max backtest sends per second.")

;; src/lisp/core/globals.lisp
(defparameter *backtest-submit-count* 0 "Total backtest requests submitted.")
(defparameter *backtest-max-pending* swimmy.core:*backtest-max-pending*)
(defparameter *backtest-rate-limit-per-sec* swimmy.core:*backtest-rate-limit-per-sec*)
(defparameter *backtest-last-send-ts* 0 "Last send timestamp (unix seconds).")
```

**Step 4: Run test to verify it passes**
Run: `./ci-test.sh`
Expected: PASS for the new test (note: existing failures may remain).

**Step 5: Commit**
```bash
git add src/lisp/core/config.lisp src/lisp/core/globals.lisp src/lisp/tests.lisp
git commit -m "feat(backtest): add pending/backtest rate config globals"
```

---

### Task 2: 送信レート制御と pending 上限の実装

**Files:**
- Modify: `src/lisp/school/school-backtest-utils.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**
```lisp
(deftest test-backtest-send-throttles-when-pending-high
  "send-zmq-msg should refuse backtest send when pending exceeds max"
  (let* ((orig-send (symbol-function 'pzmq:send))
         (sent nil))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-submit-count* 10)
          (setf swimmy.globals::*backtest-recv-count* 0)
          (setf swimmy.globals::*backtest-max-pending* 1)
          (setf (symbol-function 'pzmq:send) (lambda (&rest _) (setf sent t)))
          (swimmy.school:send-zmq-msg "(dummy)" :target :backtest)
          (assert-true (null sent) "send should be blocked"))
      (setf (symbol-function 'pzmq:send) orig-send))))
```

**Step 2: Run test to verify it fails**
Run: `./ci-test.sh`
Expected: FAIL because send is not blocked yet.

**Step 3: Write minimal implementation**
```lisp
;; src/lisp/school/school-backtest-utils.lisp
(defun backtest-pending-count ()
  (max 0 (- swimmy.globals::*backtest-submit-count*
            swimmy.main::*backtest-recv-count*)))

(defun backtest-send-allowed-p ()
  (let* ((now (get-universal-time))
         (pending (backtest-pending-count))
         (max-pending swimmy.globals::*backtest-max-pending*)
         (rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (interval (if (and rate (> rate 0)) (/ 1.0 rate) 0.0))
         (elapsed (- now swimmy.globals::*backtest-last-send-ts*)))
    (and (< pending max-pending)
         (or (<= interval 0.0) (>= elapsed interval)))))

(defun send-zmq-msg (msg &key (target :cmd))
  "Helper to send ZMQ message with throttling.
   TARGET: :backtest routes to Backtest Service; :cmd routes to main Guardian."
  (when (eq target :backtest)
    (unless (backtest-send-allowed-p)
      (format t "[BACKTEST] ⏳ Throttled send (pending=~d max=~d)~%"
              (backtest-pending-count) swimmy.globals::*backtest-max-pending*)
      (return-from send-zmq-msg nil))
    (incf swimmy.globals::*backtest-submit-count*)
    (setf swimmy.globals::*backtest-last-send-ts* (get-universal-time)))
  (sleep 0.005)
  (cond
    ((and (eq target :backtest)
          (boundp 'swimmy.globals:*backtest-requester*)
          swimmy.globals:*backtest-requester*)
     (pzmq:send swimmy.globals:*backtest-requester* msg))
    ((and (boundp 'swimmy.globals:*cmd-publisher*)
          swimmy.globals:*cmd-publisher*)
     (when (eq target :backtest)
       (format t "[ZMQ] ⚠️ Backtest requester missing. Falling back to CMD publisher.~%"))
     (pzmq:send swimmy.globals:*cmd-publisher* msg))
    (t
     (format t "[ZMQ] ❌ No publisher bound for target ~a. Msg dropped.~%" target))))
```

**Step 4: Run test to verify it passes**
Run: `./ci-test.sh`
Expected: PASS for new throttling test.

**Step 5: Commit**
```bash
git add src/lisp/school/school-backtest-utils.lisp src/lisp/tests.lisp
git commit -m "feat(backtest): throttle sends when pending is high"
```

---

### Task 3: 受信側で pending を可視化

**Files:**
- Modify: `src/lisp/core/message-dispatcher.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**
```lisp
(deftest test-backtest-pending-count-decrements-on-recv
  "pending count should drop when a BACKTEST_RESULT is processed"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-PENDING\") (sharpe . 0.1) (trades . 1))))"))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (setf swimmy.globals::*backtest-submit-count* 5)
    (setf swimmy.main::*backtest-recv-count* 0)
    (funcall fn msg)
    (assert-true (> swimmy.main::*backtest-recv-count* 0) "recv count increments")))
```

**Step 2: Run test to verify it fails**
Run: `./ci-test.sh`
Expected: FAIL if recv count is not incremented as expected in this test setup.

**Step 3: Write minimal implementation**
```lisp
;; src/lisp/core/message-dispatcher.lisp
;; (No new logic needed if *backtest-recv-count* already increments,
;; but ensure the increment happens before any early-return paths.)
```

**Step 4: Run test to verify it passes**
Run: `./ci-test.sh`
Expected: PASS for the new test.

**Step 5: Commit**
```bash
git add src/lisp/core/message-dispatcher.lisp src/lisp/tests.lisp
git commit -m "test(backtest): assert recv increments pending metrics"
```

---

### Task 4: Deferred Flush のレート制御を STATE に合わせる

**Files:**
- Modify: `src/lisp/core/config.lisp`
- Modify: `src/lisp/school/school-founders.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**
```lisp
(deftest test-deferred-flush-respects-batch
  "flush-deferred-founders should only request up to batch size"
  (let* ((orig-request (symbol-function 'swimmy.school:request-backtest))
         (count 0))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "S1")
                      (swimmy.school:make-strategy :name "S2")
                      (swimmy.school:make-strategy :name "S3")))
          (setf swimmy.school::*deferred-flush-batch* 1)
          (setf (symbol-function 'swimmy.school:request-backtest)
                (lambda (&rest _) (incf count)))
          (swimmy.school:flush-deferred-founders)
          (assert-equal 1 count "batch=1 should send exactly one"))
      (setf (symbol-function 'swimmy.school:request-backtest) orig-request))))
```

**Step 2: Run test to verify it fails**
Run: `./ci-test.sh`
Expected: FAIL because flush currently sends all.

**Step 3: Write minimal implementation**
```lisp
;; src/lisp/core/config.lisp
(defparameter *deferred-flush-batch* (env-int-or "SWIMMY_DEFERRED_FLUSH_BATCH" 0)
  "Max deferred backtests per flush. 0 means unlimited.")
(defparameter *deferred-flush-interval-sec* (env-int-or "SWIMMY_DEFERRED_FLUSH_INTERVAL_SEC" 0)
  "Min seconds between deferred flushes. 0 means no interval.")

;; src/lisp/school/school-founders.lisp
(defparameter *last-deferred-flush* 0)
(defparameter *deferred-flush-batch* swimmy.core:*deferred-flush-batch*)
(defparameter *deferred-flush-interval-sec* swimmy.core:*deferred-flush-interval-sec*)

(defun flush-deferred-founders ()
  (format t "[HEADHUNTER] 🚽 Flushing deferred backtests...~%")
  (let ((now (get-universal-time)))
    (when (and (> *deferred-flush-interval-sec* 0)
               (< (- now *last-deferred-flush*) *deferred-flush-interval-sec*))
      (format t "[HEADHUNTER] ⏳ Deferred flush cooldown (~ds).~%" *deferred-flush-interval-sec*)
      (return-from flush-deferred-founders 0))
    (setf *last-deferred-flush* now))
  (let ((count 0)
        (limit *deferred-flush-batch*))
    (dolist (s *strategy-knowledge-base*)
      (when (or (null (strategy-rank s))
                (and (stringp (strategy-rank s)) (string= (strategy-rank s) "NIL"))
                (eq (strategy-rank s) :nil))
        (when (and (> limit 0) (>= count limit))
          (return))
        (format t "[HEADHUNTER] 🚀 Requesting deferred BT for ~a...~%" (strategy-name s))
        (handler-case
            (request-backtest s)
          (error (e) (format t "[HEADHUNTER] ⚠️ BT Request failed: ~a~%" e)))
        (incf count)))
    (format t "[HEADHUNTER] ✅ Flushed ~d deferred strategies.~%" count)
    count))
```

**Step 4: Run test to verify it passes**
Run: `./ci-test.sh`
Expected: PASS for the new batch test.

**Step 5: Commit**
```bash
git add src/lisp/core/config.lisp src/lisp/school/school-founders.lisp src/lisp/tests.lisp
git commit -m "feat(backtest): rate-limit deferred flush by batch/interval"
```

---

### Task 5: 低負荷 CSV の選択を明示化（スループット改善）

**Files:**
- Modify: `src/lisp/core/config.lisp`
- Modify: `src/lisp/school/school-backtest.lisp`
- Modify: `src/lisp/school/school-backtest-v2.lisp`
- Modify: `docs/llm/STATE.md`

**Step 1: Write the failing test**
```lisp
(deftest test-backtest-uses-csv-override
  "request-backtest should honor SWIMMY_BACKTEST_CSV_OVERRIDE when set"
  (let* ((orig (uiop:getenv "SWIMMY_BACKTEST_CSV_OVERRIDE"))
         (path "/tmp/swimmy-test.csv")
         (captured nil)
         (orig-send (symbol-function 'swimmy.school:send-zmq-msg)))
    (unwind-protect
        (progn
          (sb-posix:setenv "SWIMMY_BACKTEST_CSV_OVERRIDE" path 1)
          (setf (symbol-function 'swimmy.school:send-zmq-msg)
                (lambda (msg &key target) (declare (ignore target)) (setf captured msg)))
          (swimmy.school:request-backtest (swimmy.school:make-strategy :name "T" :symbol "USDJPY"))
          (assert-true (and captured (search path captured)) "payload should include override path"))
      (when orig (sb-posix:setenv "SWIMMY_BACKTEST_CSV_OVERRIDE" orig 1))
      (setf (symbol-function 'swimmy.school:send-zmq-msg) orig-send))))
```

**Step 2: Run test to verify it fails**
Run: `./ci-test.sh`
Expected: FAIL because override is not used yet.

**Step 3: Write minimal implementation**
```lisp
;; src/lisp/core/config.lisp
(defparameter *backtest-csv-override* (getenv-or-dotenv "SWIMMY_BACKTEST_CSV_OVERRIDE")
  "Optional override path for backtest CSV.")

;; src/lisp/school/school-backtest.lisp
(let* ((override swimmy.core:*backtest-csv-override*)
       (data-file (if (and override (> (length override) 0))
                      override
                      (format nil "~a" (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" actual-symbol))))))
  ...)

;; src/lisp/school/school-backtest-v2.lisp
(let* ((override swimmy.core:*backtest-csv-override*)
       (data-file (if (and override (> (length override) 0))
                      override
                      (format nil "~a" (swimmy.core::swimmy-path (format nil "data/historical/~a_M1.csv" actual-symbol))))))
  ...)
```

**Step 4: Run test to verify it passes**
Run: `./ci-test.sh`
Expected: PASS for override test.

**Step 5: Commit**
```bash
git add src/lisp/core/config.lisp src/lisp/school/school-backtest.lisp src/lisp/school/school-backtest-v2.lisp src/lisp/tests.lisp docs/llm/STATE.md
git commit -m "feat(backtest): allow CSV override for lighter datasets"
```

---

### Task 6: 運用手順の明文化と状態更新

**Files:**
- Modify: `docs/llm/STATE.md`
- Modify: `docs/llm/INTERFACES.md` (必要な場合のみ)

**Step 1: Write the failing test**
```text
(No automated test. This task is documentation-only.)
```

**Step 2: Run test to verify it fails**
Run: `./ci-test.sh`
Expected: SKIP (documentation-only).

**Step 3: Write minimal implementation**
```text
STATE.md に以下を追記:
- `SWIMMY_BACKTEST_MAX_PENDING`, `SWIMMY_BACKTEST_RATE_LIMIT` の説明
- `SWIMMY_DEFERRED_FLUSH_BATCH`, `SWIMMY_DEFERRED_FLUSH_INTERVAL_SEC` の実装状況
- `SWIMMY_BACKTEST_CSV_OVERRIDE` の運用例
```

**Step 4: Run test to verify it passes**
Run: `./ci-test.sh`
Expected: unchanged from baseline.

**Step 5: Commit**
```bash
git add docs/llm/STATE.md docs/llm/INTERFACES.md
git commit -m "docs(state): document backtest throttling and csv override"
```

---

## 実行後の確認チェック
- `data/reports/backtest_status.txt` の `count` が増加している
- `*backtest-recv-count*` が増加し、`pending = submit - recv` が落ち着く
- `S-Rank` / `A-Rank` が 0 から増加し始める
