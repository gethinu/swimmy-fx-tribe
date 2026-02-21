# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-20  
**Leader:** Elon Musk  
**Mode:** consult  
**Trigger:** OOS は実装済みだが「検証期間固定（3-6か月）」「フォワード期間」の正本契約と実装方針をどう設計するか。  
**Purpose:** 進化速度を落とさず、実運用投入の破滅リスクを下げる検証ゲート設計を決める。  
**Constraints:** `docs/llm/SPEC.md` / `docs/llm/ARCHITECTURE.md` / `docs/llm/INTERFACES.md` / `docs/llm/STATE.md` を正本とし、矛盾時は正本優先。  
**Success Criteria:**  
1. A/S ランクゲートと「実運用Go/No-Go」を混同しない。  
2. OOS 窓・フォワード窓が再現可能な契約として定義される。  
3. 既存の Tier レガシー経路が Rank 正本を汚染しない。  
**Assumptions:** `data/historical/<SYMBOL>_M1.csv` が継続的に更新され、時刻は単調。  
**Uncertainties:** フォワード期間を「A昇格必須」にするか「実運用投入必須」にするかは未合意。

## 🏛️ 常設顧問の意見
### Taleb
- いまの最大脆弱性は「長さ可変のOOS窓」。`*oos-test-ratio*=0.3` で全履歴を切る方式は、データ長に応じて検証難易度が勝手に変わる。`src/lisp/school/school-validation.lisp:12` `src/lisp/school/school-validation.lisp:565`
- 正本は A/S 共通 Stage2 を必須と書いている一方、A では DryRun 必須が既定OFF。`docs/llm/STATE.md:97` `src/lisp/school/school-validation.lisp:64`
- **選択肢**
1. A でも DryRun 必須化（最も安全、昇格速度は低下）
2. 現状維持 + 「実運用Go/No-Go」でのみ DryRun 必須（速度と安全の折衷、二段管理が必要）
3. A を `A-provisional` と `A-live-ready` に分割（明確だが複雑）

### Graham
- 検証経路が二重化している。Rank正本系（OOS/CPCV）とは別に WFV レガシーがループ内で常時実行される。`src/lisp/school/school-connector.lisp:230` `src/lisp/core/config.lisp:281`
- **選択肢**
1. WFV を既定OFFにして Rank 正本へ一本化（最も単純）
2. WFV を残すが「分析専用（昇格に不関与）」へ格下げ（移行が滑らか）
3. 両方維持（保守コスト増、事故面積が広い）

### Naval
- 自動化すべきは「意思決定」そのもの。今はゲート判定が関数呼び出しに分散し、運用から状態遷移が見えない。`src/lisp/school/school-validation.lisp:827` `src/lisp/school/school-rank-system.lisp:981`
- **提案**: `deployment_gate_status` をDBで持つ（`research_passed` / `oos_passed` / `forward_paper_passed` / `live_ready`）。

### Jim Simons
- OOS を Sharpe 単独で見るのは弱い。`check-rank-criteria` の A OOS 条件は Sharpe のみ。`src/lisp/school/school-rank-system.lisp:317`
- **選択肢**
1. OOS でも PF/MaxDD/TradeEvidence 下限を導入（頑健）
2. Sharpe単独は維持し、実運用Go/No-Goで PF/DD を強制（現行互換）
3. シンボル別に閾値可変（柔軟だが過剰最適化リスク）

## 💻 技術パネルの意見
### Fowler
- Rank正本なのに WFV 成功時は `:battlefield` へ移動するレガシー遷移が残る。`src/lisp/school/school-backtest.lisp:570`  
- しかも persistence 側の保存ランク集合に `:battlefield` はなく、未知ランクは `INCUBATOR` へフォールバックする。`src/lisp/core/persistence.lisp:54` `src/lisp/core/persistence.lisp:55`  
- これは設計として危険な「暗黙変換」。

### Hickey
- 正本は Rank一本化を宣言している。`docs/llm/STATE.md:13` `docs/llm/STATE.md:86`
- 一方で実行パスは Tier語彙を運び続ける。データモデルが一つでない時点で、不整合は時間問題。`src/lisp/school/school-backtest.lisp:570`

### Uncle Bob
- 既存テストは DryRun bootstrap 許容を明示的に肯定している。`src/lisp/tests.lisp:8706`
- 仕様が「必須」と言うなら、テストは fail-fast へ寄せるべき。仕様とテストの同時改訂が必要。

## 🚀 ビジョナリーの意見
### Ng
- モデル改善より先に評価プロトコル固定。窓長可変のまま比較しても、改善/悪化を識別できない。`src/lisp/school/school-validation.lisp:565`

### López de Prado
- 「研究ゲート」と「資金投入ゲート」を分けるのは妥当。  
- 推奨は **二層**:  
  - 層1: 進化用 Rank（現行維持）  
  - 層2: 実運用Go/No-Go（固定期間OOS + フォワード）

### Gene Kim
- 運用に必要なのは状態可観測性。`oos_status.txt` はあるが forward 状態の正本が無い。`src/lisp/school/school-validation.lisp:33`
- 「forward未実施」「実施中」「合格/不合格」を機械判定可能にすべき。

## 対抗意見（反証）
- 反証: 「ゲートを増やすとA/Sが枯渇し、探索が止まる」  
- 反証への回答: ランク昇格基準は据え置き、実運用Go/No-Goのみ追加すれば、探索速度と安全性を分離できる。

## 🚀 Musk's Decision (Final)
> 「ランクは研究の速度計、実運用ゲートはブレーキだ。速度計をブレーキ代わりに使うな。二層化して、レガシーWFVの昇格関与を止める。」

**Do**
1. Rank（B/A/S）の昇格ロジックは当面維持。`docs/llm/SPEC.md:25` `docs/llm/SPEC.md:26` `docs/llm/SPEC.md:27`
2. 実運用Go/No-Goを別契約として `STATE` に追加（固定OOS期間 + フォワード期間）。
3. WFV は分析モードへ隔離し、昇格/ランク変更を禁止。

**Don’t**
1. memo4基準をそのまま Rank 閾値へ上書きしない（探索停止リスク）。
2. `:battlefield` のような Tier 由来遷移を正本ランク経路に残さない。

## Actionable Items
1. **契約追加（先に文書）**: `docs/llm/STATE.md` に「実運用Go/No-Go契約」を新設。  
   - 例: OOS固定窓 `180日`、Forward `30日（paper）` を最低条件として明文化。  
2. **契約整合**: `docs/llm/SPEC.md` の Rank定義には手を入れず、`STATE` の運用章として分離。  
3. **WFV隔離**: `phase-3-qualify` を既定無効化し、WFV結果で `move-strategy` を呼ばない。`src/lisp/school/school-connector.lisp:230` `src/lisp/school/school-backtest.lisp:570`  
4. **OOS窓固定化**: `%derive-oos-dispatch-range` を比率分割から期間分割（例: `end_ts - 180d`）へ変更可能な設計にする。`src/lisp/school/school-validation.lisp:565`  
5. **Aゲート明確化**: `*a-rank-require-dryrun*` の既定を仕様どおりにするか、仕様文を「Aはブートストラップ許容」に更新して矛盾を解消。`src/lisp/school/school-validation.lisp:64` `docs/llm/STATE.md:97`  
6. **新テーブル**: `deployment_gate_status`（strategy_name, oos_window, forward_start, forward_days, decision, reason, updated_at）を追加し、運用判定の正本をDB化。  
7. **テスト追加**:  
   - WFVがランクを変更しないこと  
   - 固定期間OOSレンジ算出が再現可能なこと  
   - forward未実施戦略は `live_ready=false` になること  
8. **可観測性**: `data/reports/forward_status.txt` を追加し、Evolution Reportから参照。  

