---
description: Elon Muskをリーダーとする専門家パネルに意地悪く品質改善意見をもらう
---

# /expert-panel - 専門家パネル諮問

このワークフローは、Elon Muskをリーダーとする10人の専門家＋3人の常設顧問に、現在のシステムに対する**意地悪な（nasty）品質改善意見**を求めます。

## パネル構成

### 🏛️ 常設顧問 (Permanent Advisors)
1. **Nassim Taleb** - リスク/Ruin回避
2. **Benjamin Graham** - 安全域/Value投資
3. **Naval Ravikant** - レバレッジ/自動化

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

1. **現在のシステム状態を分析する**
   - 主要ファイル（brain.lisp, engine/*.lisp, shell/*.lisp）を確認
   - 最近の変更点を把握

2. **各専門家から意地悪く意見を聞く**
   - 「何が壊れているか？」
   - 「何が足りないか？」
   - 「何が過剰か？」
   - 日本語で、批判的に、具体的に

3. **Elon Muskがリーダーとして議論をまとめる**
   - 全員の意見を聞いた上で
   - 「やるべきこと」と「やらなくていいこと」を決定
   - 最終的なActionable Adviceを提示

4. **結果をアーティファクトに保存**
   - `expert_panel_[日付].md` として保存
   - task.mdに次のステップを追記（必要な場合）

## 出力フォーマット

```markdown
# 🦅 Expert Panel Report

**Date:** YYYY-MM-DD
**Leader:** Elon Musk
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

## 注意事項

- **意地悪く**：専門家は批判的であるべき。褒めるだけでは意味がない。
- **具体的に**：抽象的な指摘ではなく、ファイル名や行番号を含める。
- **最終決定はMusk**：全員の意見を聞いた上で、Muskが「やる/やらない」を決める。
