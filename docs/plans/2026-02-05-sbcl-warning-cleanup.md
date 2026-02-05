# SBCL Warning Cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove all SBCL WARNING/STYLE-WARNING during `asdf:load-system :swimmy :force t`.

**Architecture:** Reorder ASDF components so globals/structs load before use, add forward declarations, export correct symbols, remove obsolete hooks, and apply targeted ignore/bug fixes without changing external behavior.

**Tech Stack:** Common Lisp (SBCL/ASDF), Bash

### Task 1: Add Warning Gate Test

**Files:**
- Create: `tools/sbcl_no_warnings_check.sh`
- Test: `tools/sbcl_no_warnings_check.sh`

**Step 1: Write the failing test**

```bash
#!/usr/bin/env bash
set -euo pipefail

SWIMMY_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$SWIMMY_HOME"

if [ -f tools/sbcl_env.sh ]; then
  # shellcheck disable=SC1091
  source tools/sbcl_env.sh
fi

SBCL_BASE=("sbcl" "--dynamic-space-size" "${SWIMMY_SBCL_DYNAMIC_SPACE_MB:-1024}" "--noinform")
log="$(mktemp)"

"${SBCL_BASE[@]}" --eval '(load "swimmy.asd")' --eval '(asdf:load-system :swimmy :force t)' --eval '(quit)' 2>&1 | tee "$log"

if rg -q "WARNING:|STYLE-WARNING:" "$log"; then
  echo "[FAIL] SBCL warnings detected" >&2
  exit 1
fi

echo "[PASS] No SBCL warnings."
```

**Step 2: Run test to verify it fails**

Run: `tools/sbcl_no_warnings_check.sh`

Expected: exit 1 with `[FAIL] SBCL warnings detected`.

**Step 3: Commit**

```bash
git add tools/sbcl_no_warnings_check.sh
git commit -m "test: add SBCL no-warnings gate"
```

### Task 2: Update STATE (Pre-Implementation)

**Files:**
- Modify: `docs/llm/STATE.md`

**Step 1: Add change log entry**

Add to “直近の変更履歴”:

```markdown
- **2026-02-05**: SBCLロード時WARNING/STYLE-WARNINGの全解消（ロード順、export、未使用変数、廃止フックの整理）。
```

**Step 2: Commit**

```bash
git add docs/llm/STATE.md
git commit -m "docs: note SBCL warning cleanup plan"
```

### Task 3: Load Order + Early Declarations

**Files:**
- Modify: `swimmy.asd`
- Modify: `src/lisp/school/school-state.lisp`
- Modify: `src/lisp/school/school-strategy.lisp`

**Step 1: Reorder ASDF components**

Update the top of `swimmy.asd` to load globals and DSL earlier, and move `core/message-dispatcher`, `school-voting`, `core/governance`, and `prediction` to positions that satisfy struct/variable dependencies:

```lisp
:components ((:file "src/lisp/packages")
             (:file "src/lisp/core/globals")
             (:file "src/lisp/core/config")
             (:file "src/lisp/packages-school")
             (:file "src/lisp/dsl")
             (:file "src/lisp/school/school-state")
             (:file "src/lisp/school/school-agent")
             (:file "src/lisp/school/school-integrity")
             (:file "src/lisp/school/school-scribe")
             (:file "src/lisp/school/school-watchdog")
             ...
             (:file "src/lisp/core/discord")
             (:file "src/lisp/core/message-dispatcher")
             ...
             (:file "src/lisp/school/school-voting")
             ...
             (:file "src/lisp/school/school-strategy")
             (:file "src/lisp/core/governance")
             ...
             (:file "src/lisp/school/prediction")
             (:file "src/lisp/school/school-learning")
             ...)
```

Keep the relative order of other files not shown.

**Step 2: Add forward declarations + move `strategy-rank` defstruct**

In `src/lisp/school/school-state.lisp`, add near the top (after `trade-record`):

```lisp
(defstruct strategy-rank
  name
  rank
  trades
  wins
  total-pnl
  promotion-date
  last-trade)

(defvar *predicted-regime* nil)
(defvar *predicted-volatility* nil)
(defvar *hall-of-fame* nil)
(defvar *startup-mode* t)
```

Also add ignore declarations:

```lisp
(defun check-correlation-risk (symbol direction)
  (declare (ignore direction))
  ...)

(maphash (lambda (k v)
           (declare (ignore k))
           ...)
         *strategy-ranks*)
```

**Step 3: Remove `strategy-rank` defstruct + unused vars in school-strategy**

In `src/lisp/school/school-strategy.lisp`, remove the `defstruct strategy-rank` block entirely.

Clean unused bindings:

```lisp
(let* (...
       (dir-emoji ...))
  ...)

(let ((name ...)
      (tp (strategy-tp strat)))
  ...)
```

**Step 4: Run warning test**

Run: `tools/sbcl_no_warnings_check.sh`

Expected: still FAIL, but fewer warnings.

**Step 5: Commit**

```bash
git add swimmy.asd src/lisp/school/school-state.lisp src/lisp/school/school-strategy.lisp
git commit -m "refactor: fix load order and early structs"
```

### Task 4: Exports + Obsolete Hook Removal

**Files:**
- Modify: `src/lisp/packages.lisp`
- Modify: `src/lisp/packages-school.lisp`
- Modify: `src/lisp/engine/positions.lisp`
- Modify: `src/lisp/strategies/legends.lisp`

**Step 1: Update exports**

In `src/lisp/packages.lisp`:

```lisp
;; swimmy.globals exports
#:arm-state-size
#:arm-state-symbol

;; swimmy.core exports
#:*constitution-version*
#:*council-decision-threshold*
#:*notify-chieftain-threshold*
#:*philosophy-log-max*
#:*trading-days-in-month*

;; swimmy.engine exports (remove)
;; #:train-neural
```

In `src/lisp/packages-school.lisp` export `save-knowledge-base`:

```lisp
(defpackage :swimmy.main
  ...
  (:export
   ...
   #:save-knowledge-base))
```

**Step 2: Remove obsolete hooks in positions**

In `src/lisp/engine/positions.lisp`, remove the `train-neural` block and the `check-hard-constraints` gate, and drop unused outer `sl-p`/`tp-p` while ignoring `conf`:

```lisp
(let ((type (first signal))
      (conf (second signal))
      (sl-p (nth 2 params))
      (tp-p (nth 3 params)))
  (declare (ignore conf))
  (let ((dynamic-vol ...))
    (cond
      ((eql type :BUY) ...)
      ((eql type :SELL) ...))))
```

**Step 3: Qualify save-knowledge-base**

In `src/lisp/strategies/legends.lisp`:

```lisp
(when (fboundp 'swimmy.main:save-knowledge-base)
  (swimmy.main:save-knowledge-base))
```

**Step 4: Run warning test**

Run: `tools/sbcl_no_warnings_check.sh`

Expected: still FAIL, but fewer warnings.

**Step 5: Commit**

```bash
git add src/lisp/packages.lisp src/lisp/packages-school.lisp \
  src/lisp/engine/positions.lisp src/lisp/strategies/legends.lisp
git commit -m "fix: exports and remove obsolete engine hooks"
```

### Task 5: Fix Real Bug + Scoping

**Files:**
- Modify: `src/lisp/engine/metrics.lisp`
- Modify: `src/lisp/shell/handoff.lisp`
- Modify: `src/lisp/core/research-algorithms.lisp`

**Step 1: Bind `pnl` in metrics**

```lisp
(dolist (trade all-trades)
  (let ((pnl (or (swimmy.school::trade-record-pnl trade) 0.0)))
    (incf total-pnl pnl)
    (push pnl pnls)
    (if (> pnl 0) (incf wins) (incf losses))
    ...))
```

**Step 2: Fix `watchers` scope in handoff**

```lisp
(let* ((regime ...)
       (vol ...)
       (danger ...)
       (pred :hold)
       (conf 0.0)
       (watchers (let ((results nil) ...)
                   ...
                   (if results (nreverse results) '("  (Sランク待機なし)")))))
  ... (subseq watchers 0 (min (length watchers) 10)) ...)
```

**Step 3: Qualify strategy accessors + ignore unused vars**

```lisp
(when (and (swimmy.school:strategy-sl strategy)
           (swimmy.school:strategy-tp strategy))
  ...)

(defun hmm-regime-probability (history current-regime)
  (declare (ignore current-regime))
  ...)

(defun apply-zero-shot-caution (prediction confidence source)
  (declare (ignore prediction))
  ...)
```

**Step 4: Run warning test**

Run: `tools/sbcl_no_warnings_check.sh`

Expected: still FAIL, but warnings further reduced.

**Step 5: Commit**

```bash
git add src/lisp/engine/metrics.lisp src/lisp/shell/handoff.lisp \
  src/lisp/core/research-algorithms.lisp
git commit -m "fix: metrics pnl binding and warning cleanup"
```

### Task 6: Clear Remaining Style Warnings

**Files:**
- Modify: `src/lisp/risk-manager.lisp`
- Modify: `src/lisp/core/governance.lisp`
- Modify: `src/lisp/system/opus.lisp`
- Modify: `src/lisp/shell/notifications.lisp`
- Modify: `src/lisp/shell/narrative.lisp`
- Modify: `src/lisp/dsl.lisp`
- Modify: `src/lisp/engine/portfolio.lisp`
- Modify: `src/lisp/school/prediction.lisp`
- Modify: `src/lisp/school/school-founders-dalio.lisp`
- Modify: `src/lisp/logger.lisp`
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/tests/school-split-tests.lisp`

**Step 1: Apply targeted ignores/removals**

```lisp
;; risk-manager.lisp
(maphash (lambda (idx state)
           (declare (ignore idx))
           ...)
         ...)
(defun risk-check-all (symbol direction lot category)
  (declare (ignore category))
  ...)

;; core/governance.lisp
:check-fn (lambda (ctx) (declare (ignore ctx)) 1.0)
(error (e) (declare (ignore e)) nil)

;; system/opus.lisp
(error (e) (declare (ignore e)) nil)

;; shell/notifications.lisp
(defun on-trade-opened (idx symbol direction warmup-p conf)
  (declare (ignore symbol))
  ...)
(defun on-trade-closed (idx symbol won pnl)
  (declare (ignore symbol))
  ...)

;; shell/narrative.lisp
(defun detect-patterns (history)
  (declare (ignore history))
  nil)
(defun display-elder-vote-message (elder-name wisdom vote-type context-type)
  (declare (ignore context-type))
  ...)

;; dsl.lisp
(defun ind-stoch (k-n d-n history)
  (declare (ignore d-n))
  ...)

;; engine/portfolio.lisp
(defun get-sharpe-boost (arm-idx)
  (declare (ignore arm-idx))
  ...)

;; school/prediction.lisp
(let ((now (get-universal-time)))
  (declare (ignore now))
  ...)

;; school-founders-dalio.lisp
(add-to-kb vix-hunter "Dalio-Recruiter")
(add-to-kb counter-punch "Dalio-Recruiter")
(add-to-kb time-bandit "Dalio-Recruiter")

;; logger.lisp
(with-open-file (out target ...)
  (write-string "" out))

;; tests.lisp
(setf (symbol-function 'pzmq:send)
      (lambda (&rest _)
        (declare (ignore _))
        (setf sent t)))

;; school-split-tests.lisp
(multiple-value-bind (rid3 _time3 _status3) ...
  (declare (ignore _time3 _status3))
  ...)
```

**Step 2: Run warning test**

Run: `tools/sbcl_no_warnings_check.sh`

Expected: PASS (no warnings).

**Step 3: Commit**

```bash
git add src/lisp/risk-manager.lisp src/lisp/core/governance.lisp \
  src/lisp/system/opus.lisp src/lisp/shell/notifications.lisp \
  src/lisp/shell/narrative.lisp src/lisp/dsl.lisp \
  src/lisp/engine/portfolio.lisp src/lisp/school/prediction.lisp \
  src/lisp/school/school-founders-dalio.lisp src/lisp/logger.lisp \
  src/lisp/tests.lisp src/lisp/tests/school-split-tests.lisp
git commit -m "chore: clear remaining SBCL style warnings"
```

### Task 7: Final Verification

**Files:**
- Test: `tools/sbcl_no_warnings_check.sh`
- Test: `tools/sbcl_sanity_check.sh`

**Step 1: Run warning gate**

Run: `tools/sbcl_no_warnings_check.sh`

Expected: PASS.

**Step 2: Run sanity check**

Run: `tools/sbcl_sanity_check.sh`

Expected: PASS with no WARNING/STYLE-WARNING output.

**Step 3: Commit (if needed)**

If any last edits were made after Task 6, commit them with a final message.
