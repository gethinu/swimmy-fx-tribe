# CPCV Median + Stagnant C-Rank Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Stagnant C-Rankの「日次cull＋1時間バッチ通知」をdocで明記し、CPCV中央値（PF/WR/MaxDD）をGuardian→Lispに通して、投票重みをSharpe偏重から複合指標へ修正する。

**Architecture:** CPCV_RESULTにmedian指標を追加してS式で送信→Lispのmessage-dispatcherで正規化・保存→rank gateはCPCV中央値で評価。投票重みはSharpe + PF/WRボーナス − MaxDDペナルティの合成。

**Tech Stack:** Common Lisp (SBCL), Rust (guardian), Markdown

---

### Task 1: Stagnant C-Rank 日次cullの仕様明記

**Files:**
- Modify: `doc/owners_guide.md`

**Step 1: ドキュメント更新**
- 既存のLifecycle/運用セクション付近に以下を追記:
  - 「Stagnant C‑Rank のcullは **日次（day-keyガードで1日1回）**」
  - 「通知は **1時間バッチでサマリ送信**（個別通知抑制）」

**Step 2: Doc-only変更のためテストなし**

**Step 3: Commit**
```bash
git add doc/owners_guide.md
git commit -m "docs: document stagnant c-rank daily cull"
```

---

### Task 2: Guardian CPCV_RESULTに中央値(PF/WR/MaxDD)を含める

**Files:**
- Modify: `guardian/src/main.rs`
- Test: `guardian/src/main.rs` (unit tests)

**Step 1: 先に failing test を追加**
```rust
#[test]
fn test_cpcv_payload_includes_medians() {
    let agg = cpcv::CpcvAggregateResult {
        median_sharpe: 0.75,
        median_pf: 1.4,
        median_wr: 0.52,
        median_maxdd: 0.12,
        std_sharpe: 0.2,
        path_count: 10,
        passed_count: 6,
    };
    let payload = cpcv_payload_from_aggregate("UT-CPCV-MED", None, &agg);
    assert!((payload.median_pf - 1.4).abs() < 1e-9);
    assert!((payload.median_wr - 0.52).abs() < 1e-9);
    assert!((payload.median_maxdd - 0.12).abs() < 1e-9);
}
```

**Step 2: テスト実行（失敗確認）**
```bash
cd guardian
cargo test test_cpcv_payload_includes_medians
```
Expected: コンパイル失敗 or assertion failure（medianフィールド未実装のため）

**Step 3: 最小実装**
- `CpcvResultPayload` に `median_pf/median_wr/median_maxdd` を追加
- `cpcv_result_to_sexp` に3項目を出力
- `cpcv_payload_from_aggregate` で `agg` から埋める
- エラー時の payload では 0.0 を設定

**Step 4: テスト再実行（成功確認）**
```bash
cargo test test_cpcv_payload_includes_medians
```
Expected: PASS

**Step 5: Commit**
```bash
git add guardian/src/main.rs
git commit -m "feat: include cpcv median pf/wr/maxdd in result"
```

---

### Task 3: 投票重みの複合化（Sharpe偏重是正）

**Files:**
- Modify: `src/lisp/school/school-voting.lisp`
- Modify: `src/lisp/school/school-rank-system.lisp` (コメント明記)
- Test: `src/lisp/tests.lisp`

**Step 1: failing test を追加**
```lisp
(deftest test-calculate-strategy-weight-composite
  "PF/WRが高くMaxDDが低いほど重みが増え、MaxDDが高いと減る"
  (let* ((base (make-strategy :name "BASE" :sharpe 0.2 :profit-factor 1.0 :win-rate 0.3 :max-dd 0.10))
         (boost (make-strategy :name "BOOST" :sharpe 0.2 :profit-factor 1.6 :win-rate 0.50 :max-dd 0.10))
         (penalty (make-strategy :name "PEN" :sharpe 0.2 :profit-factor 1.6 :win-rate 0.50 :max-dd 0.30))
         (w-base (swimmy.school::calculate-strategy-weight base))
         (w-boost (swimmy.school::calculate-strategy-weight boost))
         (w-pen (swimmy.school::calculate-strategy-weight penalty)))
    (assert-true (> w-boost w-base) "PF/WR should raise weight")
    (assert-true (< w-pen w-boost) "MaxDD should penalize weight")))
```

**Step 2: テスト実行（失敗確認）**
```bash
sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :run-all-tests) (sb-ext:quit))'
```
Expected: `test-calculate-strategy-weight-composite` が失敗

**Step 3: 最小実装（school-voting.lisp）**
- 既存の Sharpe ベース重みに以下を加算/減算し、最終値をクランプ:
```lisp
(let* ((pf (or (strategy-profit-factor strat) 0.0))
       (wr (or (strategy-win-rate strat) 0.0))
       (maxdd (or (strategy-max-dd strat) 0.0))
       (pf-bonus (cond ((>= pf 1.5) 0.2) ((>= pf 1.2) 0.1) (t 0.0)))
       (wr-bonus (cond ((>= wr 0.5) 0.2) ((>= wr 0.45) 0.1) (t 0.0)))
       (dd-penalty (cond ((>= maxdd 0.30) 0.3) ((>= maxdd 0.20) 0.2) ((>= maxdd 0.15) 0.1) (t 0.0)))
       (weight (+ sharpe-weight pf-bonus wr-bonus (- dd-penalty)))
       (weight (min 2.0 (max 0.3 weight))))
  weight)
```

**Step 4: rank-system のコメント補足**
- `*rank-criteria*` または `check-rank-criteria` 付近に
  - 「S判定のPF/WR/MaxDDはCPCV中央値を使用」
  を短いコメントで明記。

**Step 5: テスト再実行（成功確認）**
```bash
sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :run-all-tests) (sb-ext:quit))'
```
Expected: PASS

**Step 6: Commit**
```bash
git add src/lisp/school/school-voting.lisp src/lisp/school/school-rank-system.lisp src/lisp/tests.lisp
git commit -m "feat: weight votes with pf/wr/maxdd"
```

---

### Final Verification
```bash
cd guardian && cargo test
cd .. && sbcl --non-interactive --eval '(progn (asdf:load-asd "./swimmy.asd") (asdf:load-system :swimmy) (uiop:symbol-call :swimmy.tests :run-all-tests) (sb-ext:quit))'
```

---

Plan complete and saved to `docs/plans/2026-02-09-cpcv-stagnant-implementation-plan.md`.

Two execution options:
1. **Subagent-Driven (this session)** - I dispatch a fresh subagent per task, review between tasks
2. **Parallel Session (separate)** - Open new session with executing-plans

Which approach?
