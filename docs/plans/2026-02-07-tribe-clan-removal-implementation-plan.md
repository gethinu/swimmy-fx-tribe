# Tribe/Clan Removal Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 部族/クラン概念を完全撤去し、カテゴリのみを上位概念として統一する。live_status は schema_version=2 に更新し、Swarm合意のみを残す。

**Architecture:** tribe/clan の状態・表示・語彙を削除し、カテゴリベースの制御（間隔・枠）だけ維持する。危険時ゲートは swarm-consensus のみで判定。通知/永続化/REPL/儀式/DSL/ドキュメントまで一貫して更新する。

**Tech Stack:** Common Lisp (SBCL), Swimmy test harness (`src/lisp/tests.lisp`), S-expression outputs (live_status, ledger state).

> **Baseline note:** 既存テストに失敗があるため、各タスクは **対象テストのみ** 実行する（フル `scripts/ci-test.sh` は最後に任意実行）。

---

### Task 1: カテゴリ別トレード間隔の改名 + テスト

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/school/school-execution.lisp`

**Step 1: Write the failing test**
Add this test near other execution/risk tests and remove the old “CLAN TESTS” block.

```lisp
(deftest test-category-trade-interval
  "category trade interval should allow/deny by elapsed seconds"
  (let* ((cat '("M5" :BUY "USDJPY"))
         (orig-table swimmy.school::*last-category-trade-time*)
         (orig-interval swimmy.school::*min-trade-interval*))
    (unwind-protect
        (progn
          (setf swimmy.school::*min-trade-interval* 10)
          (setf swimmy.school::*last-category-trade-time*
                (make-hash-table :test 'equal))
          (setf (gethash cat swimmy.school::*last-category-trade-time*)
                (- (get-universal-time) 20))
          (assert-true (swimmy.school::can-category-trade-p cat))
          (setf (gethash cat swimmy.school::*last-category-trade-time*)
                (get-universal-time))
          (assert-false (swimmy.school::can-category-trade-p cat)))
      (setf swimmy.school::*last-category-trade-time* orig-table)
      (setf swimmy.school::*min-trade-interval* orig-interval))))
```

**Step 2: Run test to verify it fails**
Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-category-trade-interval)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL (undefined `can-category-trade-p` and missing var).

**Step 3: Write minimal implementation**
In `src/lisp/school/school-execution.lisp`, rename and update call sites:

```lisp
(defparameter *last-category-trade-time* (make-hash-table :test 'equal))

(defun record-category-trade-time (category)
  (setf (gethash category *last-category-trade-time*) (get-universal-time)))

(defun can-category-trade-p (category)
  (let ((last-time (gethash category *last-category-trade-time* 0)))
    (> (- (get-universal-time) last-time) *min-trade-interval*)))
```

Replace calls:
- `record-clan-trade-time` -> `record-category-trade-time`
- `can-clan-trade-p` -> `can-category-trade-p`
- `close-opposing-clan-positions` -> `close-opposing-category-positions`

**Step 4: Run test to verify it passes**
Run the same command as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/tests.lisp src/lisp/school/school-execution.lisp
git commit -m "feat: rename clan trade interval to category"
```

---

### Task 2: High CouncilのDanger Lv2判定をSwarmのみへ + テスト

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/school/school-voting.lisp`

**Step 1: Write the failing test**
```lisp
(deftest test-high-council-danger-lv2-uses-swarm-consensus
  (let ((orig-danger swimmy.globals::*danger-level*)
        (orig-swarm swimmy.globals::*last-swarm-consensus*)
        (orig-vol swimmy.globals::*current-volatility-state*))
    (unwind-protect
        (progn
          (setf swimmy.globals::*danger-level* 2)
          (setf swimmy.globals::*current-volatility-state* :normal)
          (setf swimmy.globals::*last-swarm-consensus* 0.8)
          (assert-true (swimmy.school::convene-high-council
                        '(:symbol "USDJPY" :direction :buy) :trend))
          (setf swimmy.globals::*last-swarm-consensus* 0.6)
          (assert-false (swimmy.school::convene-high-council
                         '(:symbol "USDJPY" :direction :buy) :trend)))
      (setf swimmy.globals::*danger-level* orig-danger)
      (setf swimmy.globals::*last-swarm-consensus* orig-swarm)
      (setf swimmy.globals::*current-volatility-state* orig-vol))))
```

**Step 2: Run test to verify it fails**
Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-high-council-danger-lv2-uses-swarm-consensus)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL (still uses tribe-consensus).

**Step 3: Write minimal implementation**
In `src/lisp/school/school-voting.lisp`, remove `tribe-consensus` and gate on swarm only:

```lisp
(let* (...
       (swarm-consensus (if (boundp '*last-swarm-consensus*) *last-swarm-consensus* 0.0))
       ...)
  (cond
    ((>= danger-level 2)
     (if (> swarm-consensus 0.7)
         (setf approval t reason "⚠️ APPROVED: Swarm consensus in Danger Lv2")
         (setf approval nil reason "🛡️ REJECTED: Danger Lv2 requires 70%+ swarm consensus")))
    ...))
```

**Step 4: Run test to verify it passes**
Run the same command as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/tests.lisp src/lisp/school/school-voting.lisp
git commit -m "feat: use swarm-only consensus in high council"
```

---

### Task 3: live_status schema v2 + tribe削除 + テスト

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/shell/notifications.lisp`

**Step 1: Write the failing test**
```lisp
(deftest test-live-status-schema-v2-no-tribe
  (let ((captured nil)
        (orig (symbol-function 'swimmy.core:write-sexp-atomic)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:write-sexp-atomic)
                (lambda (path payload)
                  (declare (ignore path))
                  (setf captured payload)))
          (let ((swimmy.shell::*live-status-interval* 0)
                (swimmy.shell::*last-status-write* 0))
            (swimmy.shell::save-live-status))
          (assert-equal 2 (cdr (assoc 'swimmy.shell::schema_version captured)))
          (assert-false (assoc 'swimmy.shell::tribes captured))
          (assert-false (assoc 'swimmy.shell::tribe_consensus captured)))
      (setf (symbol-function 'swimmy.core:write-sexp-atomic) orig))))
```

**Step 2: Run test to verify it fails**
Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-live-status-schema-v2-no-tribe)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL (schema_version=1 and tribe fields exist).

**Step 3: Write minimal implementation**
In `src/lisp/shell/notifications.lisp`:
- Change `(schema_version . 1)` to `(schema_version . 2)`.
- Remove `tribes` and `tribe_consensus` payload blocks.
- Delete `hunter-sig/shaman-sig/breaker-sig/raider-sig` locals.

**Step 4: Run test to verify it passes**
Run the same command as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/tests.lisp src/lisp/shell/notifications.lisp
git commit -m "feat: remove tribe fields from live status schema"
```

---

### Task 4: 日次レポートのtribe除去 + rename + テスト

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/core/narrative.lisp`
- Modify: `src/lisp/core/scheduler.lisp`
- Modify: `src/lisp/tests/scheduler-tests.lisp`

**Step 1: Write the failing test**
```lisp
(deftest test-daily-report-omits-tribe
  (let ((captured nil)
        (orig (symbol-function 'swimmy.shell:notify-discord-daily)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.shell:notify-discord-daily)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf captured msg)))
          (swimmy.core::send-daily-status-report)
          (assert-true (null (search "Tribe" captured)))
          (assert-true (null (search "部族" captured))))
      (setf (symbol-function 'swimmy.shell:notify-discord-daily) orig))))
```

**Step 2: Run test to verify it fails**
Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-daily-report-omits-tribe)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL (function name or message still includes tribe).

**Step 3: Write minimal implementation**
In `src/lisp/core/narrative.lisp`:
- Rename `send-daily-tribal-narrative` -> `send-daily-status-report`.
- Remove `tribe-dir` / `tribe-consensus` vars and use `last-prediction` only.
- Replace header text and “合意率: Tribe …” line with Swarm only.

Example diff inside the function:
```lisp
(let* (...
       (last-prediction (safe-symbol-value 'swimmy.globals::*last-prediction*))
       (swarm-consensus (safe-symbol-value 'swimmy.globals::*last-swarm-consensus*))
       (direction (or last-prediction :hold))
       ...)
  (notify-discord-daily (format nil "\n📜 **日刊・システムレポート**\n...\n📊 合意率: Swarm ~a\n..."
                               ...
                               (format-percent swarm-consensus)
                               ...)))
```

Update call sites:
- `src/lisp/core/scheduler.lisp`: call `send-daily-status-report`.
- `src/lisp/tests/scheduler-tests.lisp`: replace mock binding targets to new function name.

**Step 4: Run test to verify it passes**
Run the same command as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/tests.lisp src/lisp/core/narrative.lisp src/lisp/core/scheduler.lisp src/lisp/tests/scheduler-tests.lisp
git commit -m "feat: remove tribe wording from daily report"
```

---

### Task 5: Ledgerからtribeフィールド除去 + globals/packages整理 + テスト

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/engine/ledger.lisp`
- Modify: `src/lisp/core/globals.lisp`
- Modify: `src/lisp/packages.lisp`

**Step 1: Write the failing test**
```lisp
(deftest test-ledger-omits-tribe-fields
  (let* ((tmp-path (merge-pathnames (format nil "/tmp/swimmy-state-~a.sexp" (get-universal-time))))
         (orig-path swimmy.engine::*state-file-path*))
    (unwind-protect
        (progn
          (setf swimmy.engine::*state-file-path* tmp-path)
          (swimmy.engine:save-state)
          (with-open-file (in tmp-path :direction :input)
            (let ((obj (read in nil nil)))
              (assert-false (member :tribe-consensus obj))
              (assert-false (member :tribe-direction obj)))))
      (setf swimmy.engine::*state-file-path* orig-path)
      (when (probe-file tmp-path) (delete-file tmp-path)))))
```

**Step 2: Run test to verify it fails**
Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-ledger-omits-tribe-fields)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL (tribe fields still present).

**Step 3: Write minimal implementation**
- In `src/lisp/engine/ledger.lisp`, remove `:tribe-consensus` and `:tribe-direction` from save/load.
- In `src/lisp/core/globals.lisp`, remove `*tribe-direction*`, `*tribe-consensus*`, `*tribe-status*`, `*tribal-dialect*`, and `*clans*`.
- In `src/lisp/packages.lisp`, remove those exports.

**Step 4: Run test to verify it passes**
Run the same command as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/tests.lisp src/lisp/engine/ledger.lisp src/lisp/core/globals.lisp src/lisp/packages.lisp
git commit -m "feat: remove tribe fields from ledger and globals"
```

---

### Task 6: High Council / Governance から clan語彙撤去

**Files:**
- Modify: `src/lisp/core/governance.lisp`
- Modify: `src/lisp/tests.lisp` (optional small test)

**Step 1: Write the failing test**
```lisp
(deftest test-category-vote-list
  (let ((votes (swimmy.core::gather-category-votes "proposal" :trend)))
    (assert-true (and (listp votes) (> (length votes) 0)))))
```

**Step 2: Run test to verify it fails**
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-category-vote-list)' \
  --eval '(sb-ext:quit)'
```
Expected: FAIL (function not defined).

**Step 3: Write minimal implementation**
In `src/lisp/core/governance.lisp`:
- Rename functions: `gather-clan-votes` -> `gather-category-votes`, `simulate-clan-vote` -> `simulate-category-vote`.
- Replace `*clans*` iteration with category IDs:

```lisp
(defun list-category-ids ()
  (if (boundp 'swimmy.school::*category-allocation*)
      (mapcar #'car swimmy.school::*category-allocation*)
      '(:trend :reversion :breakout :scalp)))
```

- Update logs to use `(string-upcase (symbol-name category-id))` instead of `get-clan-display`.
- Update `convene-policy-council` parameters from `proposer-clan` to `proposer-category` and call sites inside the file.

**Step 4: Run test to verify it passes**
Run the same command as Step 2. Expected: PASS.

**Step 5: Commit**
```bash
git add src/lisp/core/governance.lisp src/lisp/tests.lisp
git commit -m "refactor: remove clan vocabulary from governance"
```

---

### Task 7: Clan構造体/財務/ヘッジの撤去 + narrative整理

**Files:**
- Modify: `src/lisp/school/school-strategy.lisp`
- Modify: `src/lisp/school/school-state.lisp`
- Modify: `src/lisp/school/school-narrative.lisp`

**Step 1: Write the failing test**
No new test here. This is structural removal; rely on existing/new tests and manual check.

**Step 2: Implement removal**
In `src/lisp/school/school-strategy.lisp` remove:
- `defstruct clan` and `*clans*` list
- `get-clan`, `get-clan-display`, `get-clan-battle-cry`, `generate-clan-narrative`
- clan treasury + mutual aid + hedge functions

Add minimal category display helper if needed by narrative/logs:
```lisp
(defun get-category-display (category-id)
  (string-upcase (symbol-name category-id)))
```

In `src/lisp/school/school-state.lisp`, remove clan treasury globals:
```lisp
(defparameter *clan-treasury* ...)
(defparameter *mutual-aid-history* ...)
```

In `src/lisp/school/school-narrative.lisp`:
- Replace clan emoji/name with category display:
  - `get-clan` usage → `get-category-display`
- Remove/replace `get-clan-positions-summary` call (omit the section or rename to category summary).
- Replace “部族” wording in narrative output.

**Step 3: Manual check**
Run:
```bash
rg -n "clan" src/lisp/school/school-strategy.lisp src/lisp/school/school-narrative.lisp
```
Expected: no remaining clan vocabulary.

**Step 4: Commit**
```bash
git add src/lisp/school/school-strategy.lisp src/lisp/school/school-state.lisp src/lisp/school/school-narrative.lisp
git commit -m "refactor: remove clan structures and narrative"
```

---

### Task 8: Founders/Recruitment の clan語彙撤去

**Files:**
- Modify: `src/lisp/school/school-founders.lisp`

**Step 1: Implement renames**
Rename functions and output text:
- `get-clan-counts` -> `get-category-counts`
- `get-clan-performance` -> `get-category-performance`
- `trigger-autohunt (clan ...)` -> `trigger-autohunt (category ...)`
- Logs: “clan” → “category”

Update internal references and calls within the file.

**Step 2: Manual check**
```bash
rg -n "clan" src/lisp/school/school-founders.lisp
```
Expected: no clan vocabulary.

**Step 3: Commit**
```bash
git add src/lisp/school/school-founders.lisp
git commit -m "refactor: rename clan terminology in founders"
```

---

### Task 9: DSL/儀式/REPL/Runner の tribe/clan撤去

**Files:**
- Modify: `src/lisp/dsl.lisp`
- Modify: `src/lisp/core/rituals.lisp`
- Modify: `src/lisp/repl.lisp`
- Modify: `src/lisp/system/runner.lisp`
- Modify: `src/lisp/school/school-fortress.lisp`

**Step 1: Implement removals**
- `src/lisp/dsl.lisp`: remove `with-tribe-context` macro and any references.
- `src/lisp/core/rituals.lisp`: remove clan gather block and “tribe” wording (e.g., “tribes celebrate”).
- `src/lisp/repl.lisp`: remove `:clans` and `:clan` commands + handlers + help text.
- `src/lisp/system/runner.lisp`: remove clan banner lines and `initialize-clan-treasury` call.
- `src/lisp/school/school-fortress.lisp`: drop `tribe-cons` parameter and its log line; update call sites (search `log-why-trade`).

**Step 2: Manual check**
```bash
rg -n "tribe|clan" src/lisp/dsl.lisp src/lisp/core/rituals.lisp src/lisp/repl.lisp src/lisp/system/runner.lisp src/lisp/school/school-fortress.lisp
```
Expected: no remaining tribe/clan vocabulary.

**Step 3: Commit**
```bash
git add src/lisp/dsl.lisp src/lisp/core/rituals.lisp src/lisp/repl.lisp src/lisp/system/runner.lisp src/lisp/school/school-fortress.lisp
git commit -m "refactor: remove tribe/clan from DSL, rituals, REPL, runner"
```

---

### Task 10: Docs更新 + 最終クリーンアップ

**Files:**
- Modify: `doc/SYSTEM_ARCHITECTURE.md`

**Step 1: Update docs**
- Remove clan/tribe terminology and the “4大氏族” section.
- Replace withカテゴリベースの説明（TF×Direction×Symbol）。

**Step 2: Final scan**
```bash
rg -n "tribe|clan" src/lisp doc | cat
```
Fix any remaining references (comments/strings included).

**Step 3: Targeted tests**
Run the new tests added in Tasks 1–6:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(load "swimmy.asd")' \
  --eval '(ql:quickload :swimmy :silent t)' \
  --eval '(swimmy.tests::test-category-trade-interval)' \
  --eval '(swimmy.tests::test-high-council-danger-lv2-uses-swarm-consensus)' \
  --eval '(swimmy.tests::test-live-status-schema-v2-no-tribe)' \
  --eval '(swimmy.tests::test-daily-report-omits-tribe)' \
  --eval '(swimmy.tests::test-ledger-omits-tribe-fields)' \
  --eval '(swimmy.tests::test-category-vote-list)' \
  --eval '(sb-ext:quit)'
```
Expected: PASS.

**Step 4: Commit**
```bash
git add doc/SYSTEM_ARCHITECTURE.md
git commit -m "docs: remove tribe/clan terminology"
```

---

## Execution Handoff

Plan complete and saved to `docs/plans/2026-02-07-tribe-clan-removal-implementation-plan.md`.

Two execution options:

1. **Subagent-Driven (this session)** – I dispatch a fresh subagent per task, review between tasks.
2. **Parallel Session (separate)** – Open new session with executing-plans, batch execution with checkpoints.

Which approach?
