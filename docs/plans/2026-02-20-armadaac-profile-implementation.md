# ArmadaAC Core Profile Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** ArmadaAC上位群の共通挙動（PF/DD/取引密度）をSwimmyのゲート・キルスイッチ・評価レポートに実装する。

**Architecture:** 既存ランクシステムへ「Armada Core Profile」ゲートを追加し、実行ガードは `school-danger` で fail-close 判定を一元化する。検証は既存テスト群へTDDで追加し、最後にArmadaデータ評価レポートを生成して戦略有用性と実力を定量化する。

**Tech Stack:** Common Lisp (SBCL), Swimmy school modules, existing `tests.lisp` harness.

---

### Task 1: Armadaプロファイル要件の失敗テストを追加（RED）

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: Write failing test**
- `test-armada-core-profile-gate-requires-pf-dd-trades`
- `test-armada-core-profile-gate-requires-hold-time-band`

**Step 2: Run test to verify it fails**
Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' --eval '(load "swimmy.asd")' --eval '(ql:quickload :swimmy :silent t)' --eval '(swimmy.tests::test-armada-core-profile-gate-requires-pf-dd-trades)' --eval '(swimmy.tests::test-armada-core-profile-gate-requires-hold-time-band)' --eval '(sb-ext:quit)'
```
Expected: FAIL (関数未実装または期待値不一致)

### Task 2: Armadaゲートをランク判定へ実装（GREEN）

**Files:**
- Modify: `src/lisp/school/school-rank-system.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Implement minimal code**
- Armadaゲート設定値（PF/DD/Trades/保有時間レンジ）追加
- `armada-core-profile-passed-p` 追加
- `check-rank-criteria` の A/S 判定に Armadaゲートを条件追加（フラグ有効時）

**Step 2: Run tests**
Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' --eval '(load "swimmy.asd")' --eval '(ql:quickload :swimmy :silent t)' --eval '(swimmy.tests::test-armada-core-profile-gate-requires-pf-dd-trades)' --eval '(swimmy.tests::test-armada-core-profile-gate-requires-hold-time-band)' --eval '(swimmy.tests::test-check-rank-criteria-a-requires-min-trade-evidence)' --eval '(sb-ext:quit)'
```
Expected: PASS

### Task 3: Armadaキルスイッチを実行ガードへ実装（RED→GREEN）

**Files:**
- Modify: `src/lisp/school/school-danger.lisp`
- Modify: `src/lisp/school/school-execution.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write failing test**
- `test-armada-kill-switch-pauses-on-hard-dd`
- `test-guard-execution-status-blocks-when-armada-kill-switch-trips`

**Step 2: Run tests to verify fail**
Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' --eval '(load "swimmy.asd")' --eval '(ql:quickload :swimmy :silent t)' --eval '(swimmy.tests::test-armada-kill-switch-pauses-on-hard-dd)' --eval '(swimmy.tests::test-guard-execution-status-blocks-when-armada-kill-switch-trips)' --eval '(sb-ext:quit)'
```
Expected: FAIL

**Step 3: Implement minimal code**
- `armada-kill-switch-triggered-p` 追加（ハードDD・週次DD・30日PF低下・60日取引低下）
- `guard-execution-status` から fail-close 参照

**Step 4: Run tests again**
Expected: PASS

### Task 4: 戦略有用性/実力評価レポート作成

**Files:**
- Create: `doc/knowledge/armadaac_strategy_assessment_20260220.md`

**Step 1: Generate quantified assessment**
- Armada Core 5口座 + Aggressive 2口座の比較
- 有用性判定（採用可否）
- 実力スコア（収益効率/安定性/再現容易性）

**Step 2: Validation command**
Run:
```bash
rg -n "Verdict|Core|Aggressive|PF|DD|Trades|Score" doc/knowledge/armadaac_strategy_assessment_20260220.md
```
Expected: 評価指標と結論が出力される

### Task 5: 回帰テストと最終検証

**Files:**
- Modify: `src/lisp/tests.lisp` (test list append if needed)

**Step 1: Targeted suite**
Run:
```bash
sbcl --non-interactive --eval '(require :asdf)' --eval '(load "swimmy.asd")' --eval '(ql:quickload :swimmy :silent t)' --eval '(swimmy.tests::test-armada-core-profile-gate-requires-pf-dd-trades)' --eval '(swimmy.tests::test-armada-core-profile-gate-requires-hold-time-band)' --eval '(swimmy.tests::test-armada-kill-switch-pauses-on-hard-dd)' --eval '(swimmy.tests::test-guard-execution-status-blocks-when-armada-kill-switch-trips)' --eval '(sb-ext:quit)'
```

**Step 2: Full suite**
Run:
```bash
SWIMMY_DISABLE_DISCORD=1 sbcl --script tests/test_runner.lisp
```

