# 🦅 Expert Panel Report (Critique)

**Date:** 2026-02-09
**Leader:** Elon Musk
**Mode:** critique
**Trigger:** 「dual-trend-signalは雑魚すぎない？」

## 🏛️ 常設顧問の意見
### Taleb: 「固定窓・固定閾値は脆弱性の温床」
- 短期/長期の固定窓 (`*short-trend-period*`/`*long-trend-period*`) と固定閾値0.1は、レジームが変わった瞬間に壊れる設計。`dual-trend-signal` は**“頑健”を名乗る根拠がない**。`src/lisp/core/research-algorithms.lisp:93-136`
- さらにモデルゲートが実運用に影響するなら、**尾部リスクの説明責任**が必要だが、設計から欠落している。`src/lisp/school/school-execution.lisp:318-383`

### Graham: 「プロダクト価値ゼロなら切れ」
- `get-model-prediction` は**効果検証が弱いままゲートに直結**。この段階で運用を抑制するのは“未検証のアラート”と同じ。`src/lisp/core/research-algorithms.lisp:190-214`, `src/lisp/school/school-execution.lisp:318-383`
- 価値が出ないなら削除、価値が出るなら最小構成で証明。それ以外は技術負債。

### Naval: 「自動化しても勝てないならノイズ」
- `research-enhanced-analysis` はログ中心で実益が薄い。**ゲートに繋ぐなら、観測ではなく“勝率向上”が必要**。`src/lisp/core/research-algorithms.lisp:468-499`

### Jim Simons: 「“弱い”は統計で潰せ」
- `dual-trend-signal` は符号一致だけで**情報量が薄い**。`agreement` 判定はあまりに単純。`src/lisp/core/research-algorithms.lisp:133-136`
- さらにSharpe/PFの一貫性が欠けるなら撤去対象。**短期検証でISが負けるなら“雑魚”認定で良い**。

## 💻 技術パネルの意見
### Fowler: 「研究と運用の境界が不明」
- `select-optimal-model`/`get-model-prediction` が運用ゲートに直結したことで、**研究コードが本番ロジックになっている**。責務分離が弱い。`src/lisp/core/research-algorithms.lisp:171-214`, `src/lisp/school/school-execution.lisp:318-383`

### Hickey: 「余計な抽象化。削るべき」
- “研究”の名で導入された関数がシンプルなIFになっている。**抽象化コストに見合わない**。`src/lisp/core/research-algorithms.lisp:171-214`

### Uncle Bob: 「テストが薄い。仕様がない」
- あるのは“通常ボラは :ensemble”の最小テストだけ。`dual-trend-signal` 自体の挙動や `get-model-prediction` の品質を保証していない。`src/lisp/tests.lisp:1176-1195`

## 🚀 ビジョナリーの意見
### Ng: 「MLの皮を被ったルール。検証しないなら削除」
- 現状は“研究っぽい”だけで、一般化性能の根拠がない。`dual-trend-signal` を使うなら、OOS/WFVの検証が必須。`src/lisp/core/research-algorithms.lisp:115-136`

### López de Prado: 「過学習リスクが高い」
- 固定窓は最悪の過学習源。**Purged CV/WFV無しでは“効果的”とは言えない**。`src/lisp/core/research-algorithms.lisp:93-136`

### Gene Kim: 「運用に入れるなら可観測性を増やせ」
- `MODEL GATE` で抑制した数を報告に出さないのは運用事故の温床。`src/lisp/school/school-execution.lisp:374-383`

## 🚀 Musk's Decision (Final)
> 「“雑魚”なら切る。残すなら**OOS/WFV＋コスト込み**で明確に勝て。勝てないならゲートから外せ。」

## Actionable Items
1. `dual-trend-signal` を **OOS/WFV（コスト込み）で短期検証**し、IS負けが出るなら撤去: `src/lisp/core/research-algorithms.lisp:115-136`
2. ゲート抑制数をテレメトリ/レポートに出す（運用可観測性）: `src/lisp/school/school-execution.lisp:374-383`
3. `dual-trend-signal` の挙動テストを追加し、仕様を固定する: `src/lisp/tests.lisp:1176-1195`
