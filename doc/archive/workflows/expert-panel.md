---
description: Elon Muskをリーダーとする専門家パネルに意地悪く批評/相談をもらう
---

# /expert-panel - 専門家パネル諮問

このワークフローは、Elon Muskをリーダーとする10人の専門家＋3人の常設顧問に、現在のシステムに対する**意地悪な（nasty）批評**または**相談**を求めます。

## モード

- `critique`（批評）: 意地悪な品質改善意見を引き出す（従来どおり）
- `consult`（相談）: 意地悪さは保ちつつ、意思決定のための選択肢・前提・不確実性を明確化

## パネル構成

### 🏛️ 常設顧問 (Permanent Advisors)
1. **Nassim Taleb** - リスク/Ruin回避
2. **Paul Graham** - Startup/Scaleしないこと
3. **Naval Ravikant** - レバレッジ/自動化
4. **Jim Simons** - 数学/金融工学/パターン

### 💻 技術パネル
4. **Martin Fowler** - リファクタリング/クリーンアーキテクチャ
5. **Rich Hickey** - シンプルさ/Lisp
6. **Robert C. Martin (Uncle Bob)** - クリーンコード/テスト

### 🚀 ビジョナリーパネル
7. **Andrew Ng** - AI/ML
8. **Marcos López de Prado** - クオンツ/オーバーフィット
9. **Gene Kim** - DevOps/運用
10. **Elon Musk** - **リーダー** / 最終判断

## 実行手順

1. **モードを決める**
   - `critique` or `consult` を明示
   - `consult` の場合は「目的・制約・成功条件・前提・不確実性」を入力として集める

2. **現在のシステム状態を分析する**
   - 主要ファイル（brain.lisp, engine/*.lisp, shell/*.lisp）を確認
   - 最近の変更点を把握

3. **各専門家から意地悪く意見を聞く**
   - 「何が壊れているか？」
   - 「何が足りないか？」
   - 「何が過剰か？」
   - 日本語で、批判的に、具体的に
   - `consult` の場合:
     - 各専門家の意見に**2-3の選択肢**と簡単なトレードオフを含める
     - レポート全体で**対抗意見（反証/リスク）**を必ず含める

4. **Elon Muskがリーダーとして議論をまとめる**
   - 全員の意見を聞いた上で
   - 「やるべきこと」と「やらなくていいこと」を決定
   - 最終的なActionable Adviceを提示

5. **結果をアーティファクトに保存**
   - `expert_panel_[日付].md` として保存
   - task.mdに次のステップを追記（必要な場合）

## 出力フォーマット

```markdown
# 🦅 Expert Panel Report (Critique)

**Date:** YYYY-MM-DD
**Leader:** Elon Musk
**Mode:** critique
**Trigger:** [ユーザーの依頼内容]

## 🏛️ 常設顧問の意見
### Taleb: ...
### Graham: ...
### Naval: ...

## 💻 技術パネルの意見
### Fowler: ...
### Hickey: ...
### Uncle Bob: ...

## 🚀 ビジョナリーの意見
### Ng: ...
### López de Prado: ...
### Gene Kim: ...

## 🚀 Musk's Decision (Final)
> 「...」

## Actionable Items
1. ...
2. ...
```

```markdown
# 🦅 Expert Panel Report (Consult)

**Date:** YYYY-MM-DD
**Leader:** Elon Musk
**Mode:** consult
**Trigger:** [ユーザーの依頼内容]
**Purpose:** [相談の目的]
**Constraints:** [制約]
**Success Criteria:** [成功条件]
**Assumptions:** [前提]
**Uncertainties:** [不確実性]

## 🏛️ 常設顧問の意見
### Taleb: ...
### Graham: ...
### Naval: ...

## 💻 技術パネルの意見
### Fowler: ...
### Hickey: ...
### Uncle Bob: ...

## 🚀 ビジョナリーの意見
### Ng: ...
### López de Prado: ...
### Gene Kim: ...

## 🚀 Musk's Decision (Final)
> 「...」

## Actionable Items
1. ...
2. ...
```

## 注意事項

- **意地悪く**：専門家は批判的であるべき。褒めるだけでは意味がない。
- **具体的に**：抽象的な指摘ではなく、ファイル名や行番号を含める。
- **最終決定はMusk**：全員の意見を聞いた上で、Muskが「やる/やらない」を決める。
- **consult の必須要件**：
  - 2-3の選択肢とトレードオフを含める
  - 反証/対抗意見を最低1つ入れる
  - 目的・前提・不確実性を明記する
