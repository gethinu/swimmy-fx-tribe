# Tribe/Clan Category Unification Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove clan/tribe vocabulary from user-visible output and align risk-gate category matching to the canonical category keys.

**Architecture:** Keep the current behavior, but normalize category checks in risk gates and replace clan/tribe terms in logs, notifications, templates, and docs. No schema or payload changes; only wording and category-key matching.

**Tech Stack:** Common Lisp (SBCL), shell scripts, Markdown docs.

---

### Task 1: Recruit Notification Uses Category Vocabulary

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/school/school-kb.lisp`

**Step 1: Write the failing test**

Add to `src/lisp/tests.lisp`:

```lisp
(deftest test-recruit-notification-uses-category
  "recruit notification should say Category and omit Clan/tribe"
  (let ((captured nil)
        (orig (symbol-function 'swimmy.core:notify-discord-recruit)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:notify-discord-recruit)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf captured msg)))
          (let ((strat (swimmy.school:make-strategy
                        :name "UT-RECRUIT"
                        :symbol "USDJPY"
                        :timeframe 5
                        :direction :BUY
                        :category :trend)))
            (swimmy.school::notify-recruit-unified strat :founder))
          (assert-true (and captured (> (length captured) 0)))
          (assert-true (search "Category" captured))
          (assert-false (search "Clan" captured))
          (assert-false (search "Tribe" captured))
          (assert-false (search "部族" captured)))
      (setf (symbol-function 'swimmy.core:notify-discord-recruit) orig))))
```

Add the test to the run-all list near existing “no tribe” tests.

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' --eval '(load "swimmy.asd")' --eval '(ql:quickload :swimmy :silent t)' --eval '(swimmy.tests::test-recruit-notification-uses-category)' --eval '(sb-ext:quit)'
```
Expected: FAIL because the message still contains “Clan”.

**Step 3: Write minimal implementation**

Update `src/lisp/school/school-kb.lisp` in `notify-recruit-unified`:

```lisp
(format nil "📣 **Strategy Recruitment**
━━━━━━━━━━━━━━━━━━━━━━
🏷️ Source: ~a
📊 Name: `~a`
🏷️ Category: ~a
📈 Sharpe: ~,2f"
        source name cat sharpe)
```

**Step 4: Run test to verify it passes**

Run the same command as Step 2. Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/tests.lisp src/lisp/school/school-kb.lisp
git commit -m "test: enforce category wording in recruit notification"
```

---

### Task 2: Remove Clan Terms from User-Facing Sources (Strings/Templates)

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/core/rituals.lisp`
- Modify: `src/lisp/mixseek.lisp`
- Modify: `src/lisp/quality.lisp`
- Modify: `src/lisp/school/school-execution.lisp`
- Modify: `src/lisp/school/school-founders.lisp`
- Modify: `src/lisp/templates/founder.lisp.template`

**Step 1: Write the failing tests**

Add to `src/lisp/tests.lisp`:

```lisp
(deftest test-category-vocabulary-omits-clan-terms-in-sources
  "key source files should not contain clan/tribe vocabulary"
  (dolist (path '("src/lisp/core/rituals.lisp"
                  "src/lisp/mixseek.lisp"
                  "src/lisp/quality.lisp"
                  "src/lisp/school/school-execution.lisp"
                  "src/lisp/school/school-founders.lisp"))
    (let ((content (uiop:read-file-string path)))
      (assert-false (search "Clan" content))
      (assert-false (search "Clans" content))
      (assert-false (search "Hunters" content))
      (assert-false (search "Shamans" content))
      (assert-false (search "Breakers" content))
      (assert-false (search "Raiders" content)))))

(deftest test-founder-template-uses-category-placeholder
  "founder template should use CATEGORY placeholder and omit CLAN"
  (let ((content (uiop:read-file-string "src/lisp/templates/founder.lisp.template")))
    (assert-true (search ":{{CATEGORY}}" content))
    (assert-false (search ":{{CLAN}}" content))))
```

Add both tests to the run-all list near existing “no tribe” tests.

**Step 2: Run tests to verify they fail**

Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' --eval '(load "swimmy.asd")' --eval '(ql:quickload :swimmy :silent t)' --eval '(swimmy.tests::test-category-vocabulary-omits-clan-terms-in-sources)' --eval '(swimmy.tests::test-founder-template-uses-category-placeholder)' --eval '(sb-ext:quit)'
```
Expected: FAIL because the files still contain clan/tribe terms.

**Step 3: Write minimal implementation**

Update these strings/words:
- `src/lisp/core/rituals.lisp`: change “The Clans Gather” to a category-based phrase (e.g., “Categories Assemble”).
- `src/lisp/mixseek.lisp`: replace the startup “4 Clans ready…” line and any clan-name comments with category terms.
- `src/lisp/quality.lisp`: replace “4 Great Clans” with “4 Core Categories”.
- `src/lisp/school/school-execution.lisp`: change allocation log “Clan ~a Full” → “Category ~a Full”.
- `src/lisp/school/school-founders.lisp`: replace comment “Clan: …” lines with “Category: …”.
- `src/lisp/templates/founder.lisp.template`: replace `:{{CLAN}}` with `:{{CATEGORY}}`.

**Step 4: Run tests to verify they pass**

Run the same command as Step 2. Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/tests.lisp src/lisp/core/rituals.lisp src/lisp/mixseek.lisp src/lisp/quality.lisp \
  src/lisp/school/school-execution.lisp src/lisp/school/school-founders.lisp \
  src/lisp/templates/founder.lisp.template
git commit -m "refactor: remove clan terms from user-facing sources"
```

---

### Task 3: Risk Gate Category Mapping (Logic + Tests)

**Files:**
- Modify: `src/lisp/tests.lisp`
- Modify: `src/lisp/school/school-fortress.lisp`
- Modify: `src/lisp/school/school-voting.lisp`

**Step 1: Write the failing tests**

Add to `src/lisp/tests.lisp`:

```lisp
(deftest test-verify-parallel-scenarios-uses-category-keys
  "parallel verification should map canonical categories"
  (let ((orig-regime swimmy.school::*market-regime*)
        (orig-vol swimmy.school::*current-volatility-state*))
    (unwind-protect
        (progn
          (setf swimmy.school::*market-regime* :trending)
          (setf swimmy.school::*current-volatility-state* :normal)
          (assert-true (swimmy.school::verify-parallel-scenarios "USDJPY" :buy :trend))
          (assert-true (swimmy.school::verify-parallel-scenarios "USDJPY" :buy :scalp))
          (setf swimmy.school::*market-regime* :ranging)
          (assert-true (swimmy.school::verify-parallel-scenarios "USDJPY" :buy :reversion))
          (setf swimmy.school::*market-regime* :volatile)
          (assert-true (swimmy.school::verify-parallel-scenarios "USDJPY" :buy :breakout)))
      (setf swimmy.school::*market-regime* orig-regime)
      (setf swimmy.school::*current-volatility-state* orig-vol))))

(deftest test-high-council-extreme-volatility-uses-category-keys
  "extreme volatility approvals should use canonical categories"
  (let ((orig-danger swimmy.globals::*danger-level*)
        (orig-swarm swimmy.globals::*last-swarm-consensus*)
        (orig-vol swimmy.globals::*current-volatility-state*)
        (orig-notify (symbol-function 'swimmy.core:notify-discord-symbol)))
    (unwind-protect
        (progn
          (setf swimmy.globals::*danger-level* 0)
          (setf swimmy.globals::*last-swarm-consensus* 0.0)
          (setf swimmy.globals::*current-volatility-state* :extreme)
          (setf (symbol-function 'swimmy.core:notify-discord-symbol)
                (lambda (&rest args) (declare (ignore args)) nil))
          (assert-true (swimmy.school::convene-high-council
                        '(:symbol "USDJPY" :direction :buy) :breakout))
          (assert-false (swimmy.school::convene-high-council
                         '(:symbol "USDJPY" :direction :buy) :trend)))
      (setf swimmy.globals::*danger-level* orig-danger)
      (setf swimmy.globals::*last-swarm-consensus* orig-swarm)
      (setf swimmy.globals::*current-volatility-state* orig-vol)
      (setf (symbol-function 'swimmy.core:notify-discord-symbol) orig-notify))))
```

Add both tests to the run-all list near existing High Council tests.

**Step 2: Run tests to verify they fail**

Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' --eval '(load "swimmy.asd")' --eval '(ql:quickload :swimmy :silent t)' --eval '(swimmy.tests::test-verify-parallel-scenarios-uses-category-keys)' --eval '(swimmy.tests::test-high-council-extreme-volatility-uses-category-keys)' --eval '(sb-ext:quit)'
```
Expected: FAIL due to old clan categories.

**Step 3: Write minimal implementation**

Update `src/lisp/school/school-fortress.lisp`:

```lisp
(cond ((member category '(:trend :scalp)) (eq regime :trending))
      ((eq category :breakout) (eq regime :volatile))
      ((eq category :reversion) (eq regime :ranging))
      (t t))
```

Update `src/lisp/school/school-voting.lisp`:

```lisp
((eq volatility-state :extreme)
 (if (member category '(:breakout :reversion))
     (setf approval t reason "🌊 APPROVED: Extreme volatility fits Category")
     (setf approval nil reason "⛔ REJECTED: Too volatile for Category")))
```

**Step 4: Run tests to verify they pass**

Run the same command as Step 2. Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/tests.lisp src/lisp/school/school-fortress.lisp src/lisp/school/school-voting.lisp
git commit -m "fix: align risk gates with category keys"
```

---

### Task 4: Documentation Alignment (Schema + Vocabulary)

**Files:**
- Modify: `doc/owners_guide.md`
- Modify: `doc/SYSTEM_ARCHITECTURE.md`
- Modify: `src/lisp/core/narrative.lisp`
- Modify: `src/lisp/school/school-strategy.lisp`
- Modify: `src/lisp/school/school-state.lisp`
- Modify: `src/lisp/main.lisp`

**Step 1: Update docs**
- In `doc/owners_guide.md`, set `live_status.sexp` schema version to `2` (match implementation).
- In `doc/SYSTEM_ARCHITECTURE.md`, remove “氏族システム” references and align language to categories.

**Step 2: Clean comments to match vocabulary**
- `src/lisp/core/narrative.lisp`: replace “tribal narrative” comment with “category narrative”.
- `src/lisp/school/school-strategy.lisp`: replace “Clan mapping” comment with “Category allocation”.
- `src/lisp/school/school-state.lisp`: remove “Tribe signal integration” comment.
- `src/lisp/main.lisp`: replace “Clan & Pool Init” comment with “Category & Pool Init”.

**Step 3: Commit**

```bash
git add doc/owners_guide.md doc/SYSTEM_ARCHITECTURE.md src/lisp/core/narrative.lisp \
  src/lisp/school/school-strategy.lisp src/lisp/school/school-state.lisp src/lisp/main.lisp
git commit -m "docs: align schema and category vocabulary"
```

---

### Task 5: Full Verification

**Files:**
- No code changes.

**Step 1: Run full test suite**

Run:
```bash
scripts/ci-test.sh
```
Expected: PASS with “172 tests” or updated count.

**Step 2: Sanity scan for leftover clan/tribe tokens**

Run:
```bash
rg -n -i "clan|tribe|hunters|shamans|breakers|raiders" src/lisp doc
```
Expected: No hits in source/docs except test names (if any). Investigate any unexpected hits.

**Step 3: Commit (if any fixes were needed)**

```bash
git add -u
git commit -m "chore: finalize category vocabulary cleanup"
```

---

## Execution Options

Plan complete and saved to `docs/plans/2026-02-09-tribe-clan-category-unification-implementation-plan.md`.

Two execution options:
1. **Subagent-Driven (this session)** — I dispatch a fresh subagent per task, review between tasks.
2. **Parallel Session (separate)** — Open a new session and run `superpowers:executing-plans`.

Which approach?
