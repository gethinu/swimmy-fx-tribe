# OOS Status Reporting Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** OOS自動審査の可視化を「イベント駆動 + 定期再生成」で実装し、Evolution ReportにOOS行を追加する。

**Architecture:** OOSメトリクス（メモリ）と `oos_queue`（DB）を統合して1行サマリを生成し、`data/reports/oos_status.txt` に書き出す。送信/受信イベントで即時更新し、1時間ごとの報告タイミングで再生成する。Evolution ReportにOOS行を埋め込む。

**Tech Stack:** Common Lisp, SQLite (oos_queue), file-based reports

---

### Task 1: OOS Status 更新テストを追加

**Files:**
- Modify: `src/lisp/tests/school-split-tests.lisp`
- Test: `test_runner.lisp`

**Step 1: Write the failing test (dispatch updates status)**

```lisp
(deftest test-oos-status-updated-on-dispatch
  "OOS dispatch should update oos_status.txt via writer."
  (let* ((data-path (swimmy.core::swimmy-path "data/historical/USDJPY_M1.csv"))
         (created-file nil)
         (tmp-db (format nil "data/memory/test-oos-status-~a.db" (get-universal-time))))
    (unless (probe-file data-path)
      (ensure-directories-exist data-path)
      (with-open-file (s data-path :direction :output :if-does-not-exist :create :if-exists :supersede)
        (write-line "timestamp,open,high,low,close,volume" s)
        (write-line "1700000000,145.1,145.2,145.0,145.15,1000" s))
      (setf created-file t))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil))
      (unwind-protect
           (progn
             (swimmy.school::init-db)
             (let ((write-count 0)
                   (orig-write (symbol-function 'swimmy.school::write-oos-status-file))
                   (orig-request (symbol-function 'swimmy.school::request-backtest)))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'swimmy.school::write-oos-status-file)
                            (lambda (&rest _args) (declare (ignore _args)) (incf write-count) t))
                      (setf (symbol-function 'swimmy.school::request-backtest)
                            (lambda (&rest _args) (declare (ignore _args)) t))
                      (let ((strat (cl-user::make-strategy :name "UnitTest-OOS-Status"
                                                           :symbol "USDJPY"
                                                           :oos-sharpe nil)))
                        (swimmy.school::run-oos-validation strat)
                        (assert-true (> write-count 0) "OOS status should update on dispatch")))
                 (setf (symbol-function 'swimmy.school::write-oos-status-file) orig-write)
                 (setf (symbol-function 'swimmy.school::request-backtest) orig-request))))
        (swimmy.core:close-db-connection)
        (when created-file (delete-file data-path))
        (when (probe-file tmp-db) (delete-file tmp-db))))))
```

**Step 2: Write the failing test (report includes OOS line)**

```lisp
(deftest test-evolution-report-includes-oos-status
  "Evolution report should include OOS status line."
  (let ((orig-refresh (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)))
    (unwind-protect
         (progn
           (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db)
                 (lambda (&rest _args) (declare (ignore _args)) nil))
           (let ((report (swimmy.school::generate-evolution-report)))
             (assert-true (search "OOS" report) "Report should contain OOS status")))
      (setf (symbol-function 'swimmy.school::refresh-strategy-metrics-from-db) orig-refresh))))
```

**Step 3: Run test to verify it fails**

Run: `sbcl --script test_runner.lisp`
Expected: FAIL with missing `write-oos-status-file` and/or missing OOS line in report.

---

### Task 2: OOS Queue 統計 + Status Writer を実装

**Files:**
- Modify: `src/lisp/school/school-db.lisp`
- Modify: `src/lisp/school/school-validation.lisp`

**Step 1: Implement minimal queue stats helper**

```lisp
(defun fetch-oos-queue-stats ()
  "Return plist: :pending :errors :oldest-age :oldest-requested-at."
  (handler-case
      (progn
        (ignore-errors (init-db))
        (let* ((pending (or (execute-single "SELECT count(*) FROM oos_queue WHERE status != 'error'") 0))
               (errors (or (execute-single "SELECT count(*) FROM oos_queue WHERE status = 'error'") 0))
               (oldest (execute-single "SELECT MIN(requested_at) FROM oos_queue WHERE status != 'error'"))
               (age (and oldest (- (get-universal-time) oldest))))
          (list :pending pending :errors errors :oldest-requested-at oldest :oldest-age age)))
    (error (e)
      (list :error (format nil "~a" e)))))
```

**Step 2: Add status line + writer**

```lisp
(defparameter *oos-status-path* (swimmy.core::swimmy-path "data/reports/oos_status.txt"))

(defun build-oos-status-line ()
  (let* ((base (oos-metrics-summary-line))
         (q (fetch-oos-queue-stats))
         (pending (getf q :pending 0))
         (errors (getf q :errors 0))
         (age (getf q :oldest-age))
         (age-text (if age (format nil "~ds" age) "-")))
    (format nil "~a | queue pending: ~d error: ~d oldest: ~a"
            base pending errors age-text)))

(defun write-oos-status-file (&key (reason "event"))
  (handler-case
      (let* ((line (build-oos-status-line))
             (stamp (format-timestamp (get-universal-time)))
             (content (format nil "~a~%updated: ~a reason: ~a" line stamp reason)))
        (ensure-directories-exist *oos-status-path*)
        (with-open-file (out *oos-status-path* :direction :output :if-exists :supersede :if-does-not-exist :create)
          (write-string content out))
        t)
    (error (e)
      (format t "[OOS] ⚠️ Failed to write oos_status.txt: ~a~%" e)
      nil)))
```

**Step 3: Run tests to verify green**

Run: `sbcl --script test_runner.lisp`
Expected: PASS for the two new tests.

---

### Task 3: Event/Periodic Hook 追加 + Evolution Report 統合

**Files:**
- Modify: `src/lisp/school/school-validation.lisp`
- Modify: `src/lisp/school/school-connector.lisp`
- Modify: `src/lisp/school/school-narrative.lisp`

**Step 1: Event-driven updates**

- `maybe-request-oos-backtest` の成功パスで `write-oos-status-file` を呼ぶ
- `handle-oos-backtest-result` の末尾で `write-oos-status-file` を呼ぶ

**Step 2: Periodic recovery**

- `phase-7-report` で `notify-evolution-report` の後に `write-oos-status-file :reason "scheduled"` を呼ぶ

**Step 3: Evolution Report に OOS 行を追加**

```lisp
(let ((top-snippet (build-top-candidates-snippet all))
      (cpcv-snippet (build-cpcv-status-snippet))
      (oos-snippet (oos-metrics-summary-line)))
  ...
  (format nil "...~%~a~%~a~%~a~%..." cpcv-snippet oos-snippet top-snippet ...))
```

**Step 4: Run tests**

Run: `sbcl --script test_runner.lisp`
Expected: PASS

---

### Task 4: Owner’s Guide 更新

**Files:**
- Modify: `doc/owners_guide.md`

**Step 1: Update docs**

- 「確認方法」に `data/reports/oos_status.txt` を追加
- 「OOS自動審査の確認は Evolution Report + oos_status.txt」 を明記

**Step 2: Run tests (optional)**

Run: `sbcl --script test_runner.lisp`
Expected: PASS

---

### Task 5: Commit

**Step 1: Commit baseline fix + OOS status changes**

```bash
git add src/lisp/tests.lisp src/lisp/tests/school-split-tests.lisp \
  src/lisp/school/school-db.lisp src/lisp/school/school-validation.lisp \
  src/lisp/school/school-connector.lisp src/lisp/school/school-narrative.lisp \
  doc/owners_guide.md docs/plans/2026-02-03-oos-status-reporting.md

git commit -m "feat: add OOS status reporting and integrate into reports"
```
