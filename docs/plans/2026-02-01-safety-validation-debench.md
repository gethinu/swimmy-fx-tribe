# Safety & Validation & Debench Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 入力安全・Backtest V2整合・ベンチ廃止を同時に実装し、運用を単純化する。  
**Architecture:** 外部入力は安全リーダ＋スキーマで隔離、Backtest V2は `strategy-alist` を唯一の真実に統一。ベンチは評価フェーズでは墓場/killへ置換、実行フェーズは Thompson Sampling とリスク管理に委ねる。  
**Tech Stack:** Common Lisp (SBCL), ASDF, ZMQ, Python (監視スクリプト), systemd

---

### Task 1: 安全リーダの基盤を追加する

**Files:**
- Create: `src/lisp/core/safe-read.lisp`
- Modify: `swimmy.asd`
- Modify: `src/lisp/packages.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**
`src/lisp/tests.lisp` に以下を追加し、`run-all-tests` リストに追加する。

```lisp
(deftest test-safe-read-rejects-read-eval
  "safe-read-sexp rejects read-time eval (#.)"
  (let ((form (swimmy.core:safe-read-sexp "#.(+ 1 2)")))
    (assert-true (null form) "Expected nil for read-time eval")))

(deftest test-safe-read-allows-simple-alist
  "safe-read-sexp allows simple alist"
  (let ((form (swimmy.core:safe-read-sexp "((action . \"PING\"))")))
    (assert-true (and (listp form) (assoc 'action form)) "Expected alist")))
```

**Step 2: Run test to verify it fails**
Run:
```bash
./ci-test.sh
```
Expected: `test-safe-read-*` が **FAIL**（関数未定義）。

**Step 3: Write minimal implementation**
`src/lisp/core/safe-read.lisp` を作成:

```lisp
(in-package :swimmy.core)

(defun safe-read-sexp (input &key (package :swimmy.main))
  "Safely read a single S-expression string. Returns form or NIL on error."
  (when (and input (stringp input))
    (handler-case
        (let ((*read-eval* nil)
              (*package* (find-package package)))
          (multiple-value-bind (form _) (read-from-string input nil nil)
            (declare (ignore _))
            form))
      (error () nil))))
```

`swimmy.asd` に追加（core の読み込み順に挿入）:
```lisp
(:file "src/lisp/core/safe-read")
```

`src/lisp/packages.lisp` に export:
```lisp
#:safe-read-sexp
```

**Step 4: Run test to verify it passes**
Run:
```bash
./ci-test.sh
```
Expected: `test-safe-read-*` が **PASS**。

**Step 5: Commit**
```bash
git add src/lisp/core/safe-read.lisp swimmy.asd src/lisp/packages.lisp src/lisp/tests.lisp
git commit -m "feat: add safe read utility"
```

---

### Task 2: ZMQ/LLM 入力を安全リーダへ切替

**Files:**
- Modify: `src/lisp/core/message-dispatcher.lisp`
- Modify: `src/lisp/school/school-evolution.lisp`
- Modify: `src/lisp/dsl.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**
`src/lisp/tests.lisp` に追加:

```lisp
(deftest test-internal-process-msg-rejects-read-eval
  "internal-process-msg should ignore unsafe sexp"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.core)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (assert-true (null (funcall fn "#.(+ 1 2)")) "Should ignore unsafe input")))
```

**Step 2: Run test to verify it fails**
Run:
```bash
./ci-test.sh
```
Expected: 追加テストが **FAIL**（read-from-string が実行される）。

**Step 3: Write minimal implementation**
`src/lisp/core/message-dispatcher.lisp` の sexp 経路:
```lisp
(let* ((sexp (swimmy.core:safe-read-sexp msg :package :swimmy.main))
       (type (and sexp (cdr (assoc 'type sexp))))
       ...)
  (unless sexp
    (format t "[DISPATCH] ⚠️ Unsafe/invalid SEXP ignored~%")
    (return-from internal-process-msg nil))
  ...)
```

`src/lisp/dsl.lisp` に安全DSL読取関数追加:
```lisp
(defun safe-read-dsl-form (code)
  (let ((form (swimmy.core:safe-read-sexp code :package :swimmy.school)))
    (when (and form (validate form)) form)))
```

`src/lisp/school/school-evolution.lisp`:
- `read-from-string` を `safe-read-sexp` に置換
- `entry/exit` は `safe-read-dsl-form` で検証
- 不正ならスキップ＋ログ

**Step 4: Run test to verify it passes**
Run:
```bash
./ci-test.sh
```
Expected: テスト **PASS**。

**Step 5: Commit**
```bash
git add src/lisp/core/message-dispatcher.lisp src/lisp/school/school-evolution.lisp src/lisp/dsl.lisp src/lisp/tests.lisp
git commit -m "feat: guard zmq/llm input with safe reader"
```

---

### Task 3: 永続化ロードを安全リーダへ切替

**Files:**
- Modify: `src/lisp/school/school-db.lisp`
- Modify: `src/lisp/school/school-kb.lisp`
- Modify: `src/lisp/school/school-cemetery.lisp`
- Modify: `src/lisp/school/school-rank-system.lisp`

**Step 1: Write the failing test**
`src/lisp/tests.lisp` に追加:

```lisp
(deftest test-safe-read-used-for-db-rank
  "rank parsing should ignore read-time eval"
  (let ((orig (symbol-function 'swimmy.core:safe-read-sexp)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:safe-read-sexp)
                (lambda (_ &key package) (declare (ignore _ package)) nil))
          (let ((rank (swimmy.school::%parse-rank-safe "#.(+ 1 2)")))
            (assert-true (null rank) "Expected nil rank")))
      (setf (symbol-function 'swimmy.core:safe-read-sexp) orig))))
```

**Step 2: Run test to verify it fails**
Run:
```bash
./ci-test.sh
```
Expected: **FAIL**（`%parse-rank-safe` 未実装）。

**Step 3: Write minimal implementation**
`src/lisp/school/school-db.lisp` に安全パーサを追加し、`read-from-string` を `safe-read-sexp` に置換。例:
```lisp
(defun %parse-rank-safe (rank-str)
  (let ((obj (swimmy.core:safe-read-sexp rank-str :package :swimmy.school)))
    (and (symbolp obj) obj)))

(let ((obj (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school)))
  (when obj ...))
```

**Step 4: Run test to verify it passes**
Run:
```bash
./ci-test.sh
```
Expected: PASS。

**Step 5: Commit**
```bash
git add src/lisp/school/school-db.lisp src/lisp/school/school-kb.lisp src/lisp/school/school-cemetery.lisp src/lisp/school/school-rank-system.lisp
git commit -m "refactor: use safe reader for persistence loads"
```

---

### Task 4: Backtest V2 payload 修正 & Phase2 昇格

**Files:**
- Modify: `src/lisp/school/school-backtest-v2.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing tests**
`src/lisp/tests.lisp` に追加:

```lisp
(deftest test-backtest-v2-uses-alist
  "request-backtest-v2 should send alist strategy payload"
  (let ((captured nil)
        (orig (symbol-function 'pzmq:send))
        (orig-fetch (symbol-function 'swimmy.school::fetch-swap-history)))
    (unwind-protect
        (progn
          (setf (symbol-function 'pzmq:send)
                (lambda (_ msg) (declare (ignore _)) (setf captured msg)))
          (setf (symbol-function 'swimmy.school::fetch-swap-history)
                (lambda (&rest _) (declare (ignore _)) nil))
          (let ((swimmy.globals:*cmd-publisher* t)
                (s (swimmy.school:make-strategy :name "T" :symbol "USDJPY")))
            (swimmy.school::request-backtest-v2 s :start-date "2020.01.01" :end-date "2020.12.31")))
      (setf (symbol-function 'pzmq:send) orig)
      (setf (symbol-function 'swimmy.school::fetch-swap-history) orig-fetch))
    (assert-not-nil captured)
    (let ((payload (car (read-from-string captured))))
      (assert-true (consp (cdr (assoc 'strategy payload))) "Strategy should be alist"))))

(deftest test-backtest-v2-phase2-promotes-to-a
  "Phase2 result should promote to A when sharpe meets threshold"
  (let* ((s (swimmy.school:make-strategy :name "Phase2" :symbol "USDJPY"))
         (swimmy.school::*strategy-knowledge-base* (list s))
         (called nil))
    (let ((orig (symbol-function 'swimmy.school:ensure-rank)))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:ensure-rank)
                  (lambda (strat rank &optional reason)
                    (declare (ignore reason))
                    (setf called (list strat rank))
                    rank))
            (swimmy.school::handle-v2-result "Phase2_2021" (list :sharpe 1.0)))
        (setf (symbol-function 'swimmy.school:ensure-rank) orig)))
    (assert-equal :A (second called) "Expected A-RANK promotion")))
```

**Step 2: Run test to verify it fails**
Run:
```bash
./ci-test.sh
```
Expected: Phase2 test **FAIL**（昇格ロジック未実装）。

**Step 3: Write minimal implementation**
`src/lisp/school/school-backtest-v2.lisp`:
- `strategy-alist` を payload の `strategy` に設定
- Phase2 合格時に `ensure-rank strat :A` を呼ぶ
- Phase2 失敗時は `send-to-graveyard strat` を呼ぶ

**Step 4: Run test to verify it passes**
Run:
```bash
./ci-test.sh
```
Expected: PASS。

**Step 5: Commit**
```bash
git add src/lisp/school/school-backtest-v2.lisp src/lisp/tests.lisp
git commit -m "fix: backtest v2 payload and phase2 promotion"
```

---

### Task 5: ベンチ廃止（評価/実行/表示）

**Files:**
- Modify: `src/lisp/strategies/strategies.lisp`
- Modify: `src/lisp/school/school-lifecycle.lisp`
- Modify: `src/lisp/school/school-evaluation.lisp`
- Modify: `src/lisp/school/school-rank-system.lisp`
- Modify: `src/lisp/engine/positions.lisp`
- Modify: `src/lisp/engine/portfolio.lisp`
- Modify: `src/lisp/engine/heartbeat.lisp`
- Modify: `src/lisp/core/config.lisp`
- Modify: `src/lisp/core/globals.lisp`
- Modify: `src/lisp/packages.lisp`
- Modify: `src/lisp/packages-school.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing tests**
`src/lisp/tests.lisp` に追加:

```lisp
(deftest test-evaluate-performance-sends-to-graveyard
  "Low sharpe should send strategy to graveyard (no bench)"
  (let* ((s (swimmy.school:make-strategy :name "LowSharpe" :symbol "USDJPY"))
         (swimmy.school::*strategy-knowledge-base* (list s))
         (orig (symbol-function 'swimmy.school:send-to-graveyard))
         (called nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (_ reason) (declare (ignore reason)) (setf called t)))
          (swimmy.school:evaluate-strategy-performance s -1.0 10 0.3 1.0))
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig))
    (assert-true called "Expected send-to-graveyard")))

(deftest test-heartbeat-no-bench-string
  "Heartbeat summary should not mention benched"
  (let ((summary (swimmy.engine::get-system-summary)))
    (assert-false (search "benched" summary) "No benched in heartbeat")))
```

**Step 2: Run test to verify it fails**
Run:
```bash
./ci-test.sh
```
Expected: 追加テストが **FAIL**。

**Step 3: Write minimal implementation**
- `strategies.lisp`: `bench-strategy` 呼び出しを `send-to-graveyard` へ置換、`strategy-benched-p` 等のベンチ関連関数を削除
- `school-lifecycle.lisp`: 連敗ベンチ処理と unbench 関数群を削除
- `school-evaluation.lisp`: `benched` フィルタ分岐を削除
- `school-rank-system.lisp`: `:benched` を条件から排除
- `engine/positions.lisp` / `engine/portfolio.lisp`: `*benched-arms*` と `arm-benched-p` / `bench-arm` を削除
- `engine/heartbeat.lisp`: benched カウント表示を削除
- `core/config.lisp` / `core/globals.lisp` / `packages*.lisp`: `*benched-arms*` の重複定義/export を削除

**Step 4: Run test to verify it passes**
Run:
```bash
./ci-test.sh
```
Expected: PASS。

**Step 5: Commit**
```bash
git add src/lisp/strategies/strategies.lisp src/lisp/school/school-lifecycle.lisp src/lisp/school/school-evaluation.lisp src/lisp/school/school-rank-system.lisp \
  src/lisp/engine/positions.lisp src/lisp/engine/portfolio.lisp src/lisp/engine/heartbeat.lisp \
  src/lisp/core/config.lisp src/lisp/core/globals.lisp src/lisp/packages.lisp src/lisp/packages-school.lisp src/lisp/tests.lisp
git commit -m "refactor: remove benching from evaluation and runtime"
```

---

### Task 6: 運用の正直さ（systemd/監視）

**Files:**
- Modify: `tools/install_services.sh`
- Modify: `tools/report_system_status.py`
- Create: `tools/test_install_services.sh`
- Create: `tools/test_report_system_status.sh`

**Step 1: Write the failing tests**
`tools/test_install_services.sh`:
```bash
#!/usr/bin/env bash
set -euo pipefail
TMP_DIR=$(mktemp -d)
SWIMMY_SYSTEMD_DIR="$TMP_DIR" SWIMMY_SYSTEMD_DRY_RUN=1 bash tools/install_services.sh && exit 1
```

`tools/test_report_system_status.sh`:
```bash
#!/usr/bin/env bash
set -euo pipefail
python3 -c "import tools.report_system_status" >/dev/null
```

**Step 2: Run test to verify it fails**
Run:
```bash
bash tools/test_install_services.sh
bash tools/test_report_system_status.sh
```
Expected: `install_services` が fail-fast せず **FAIL**、`report_system_status` は `re` 未importで **FAIL**。

**Step 3: Write minimal implementation**
`tools/install_services.sh`:
- `set -euo pipefail`
- `SOURCE_DIR=${SWIMMY_SYSTEMD_DIR:-systemd}`
- `.service/.timer` が存在しない場合は即 `exit 1`
- `SWIMMY_SYSTEMD_DRY_RUN=1` なら `systemctl` をスキップ

`tools/report_system_status.py`:
- `import re` を追加

**Step 4: Run test to verify it passes**
Run:
```bash
bash tools/test_install_services.sh
bash tools/test_report_system_status.sh
```
Expected: PASS。

**Step 5: Commit**
```bash
git add tools/install_services.sh tools/report_system_status.py tools/test_install_services.sh tools/test_report_system_status.sh
git commit -m "fix: fail-fast systemd install and report script"
```

---

## Execution Handoff
Plan complete and saved to `docs/plans/2026-02-01-safety-validation-debench.md`.

Two execution options:
1. **Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration
2. **Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

Which approach?
