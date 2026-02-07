# Expert Panel Report (Consult)

**Date:** 2026-02-07
**Leader:** Elon Musk
**Mode:** consult
**Trigger:** 非相関ペア戦略を単一戦略と併存させ、同列でランク競争させる是非の相談。
**Purpose:** 非相関ペアを「独立戦略としてランク付けする」か「overlayとして補助に留める」か、または併存させる設計判断を行う。
**Constraints:** 既存のランク昇格フローと実運用を壊さないこと。非相関評価は実績データに基づくこと。過度な計算負荷と監視コストを増やさないこと。
**Success Criteria:** 併存後にポートフォリオ全体のMaxDDが低下し、Sharpe/PFが改善すること。ランク枠の公平性と再現性が維持されること。非相関評価が説明可能で監視可能であること。
**Assumptions:** 日次PnL集計は00:10で実行され、DBに蓄積される。非相関スコアは日次PnL相関で算出される。ペア合成は既存のoverlayユーティリティで評価可能。
**Uncertainties:** ペア戦略を単一戦略と同じ枠に置くか、別枠にするか。ペアの評価期間と最小トレード数の適正値。ペア戦略の永続化方式。

## 常設顧問の意見
### Taleb:
現在の非相関評価は日次PnL相関に依存し、データ不足時はN/Aで通知止まりです。これは尾部リスク耐性の証明にはならない。`src/lisp/school/school-dalio.lisp:137` と `src/lisp/school/school-dalio.lisp:176` が示す通り、昇格通知はあるが昇格判断や淘汰に使っていない。さらに集計は1日1回で、破局的相関崩壊の即時検知ではない。`src/lisp/core/scheduler.lisp:154` と `src/lisp/school/school-db.lisp:237`。
Option 1: 併存は認めるが、ペアは別枠で「防御専用」扱い。Tradeoffは収益機会が減る。
Option 2: ペアも同列ランクに置くが、CPCV相当の検証を必須にする。Tradeoffは開発と計算負荷増。
Option 3: overlayのみ維持し、非相関スコアは通知だけに留める。Tradeoffは学習効果が限定。
反証: 日次PnL相関はポートフォリオの尾部リスクを反映しにくく、同列ランク競争は「見かけの分散」に騙されやすい。

### Graham:
現状のpair関連はユーティリティ層で、永続化やランキング統合がない。`src/lisp/school/school-pair-composite.lisp:5` と `src/lisp/school/school-execution.lisp:247` が示すのは「lot上限のoverlay」止まり。ここに全体ランキングを重ねると規模が膨らみやすい。
Option 1: 併存はするが、ペアは「別商品」として上限枠を固定する。Tradeoffは横断最適の自由度が下がる。
Option 2: 単一と同列の競争にするが、枠のサイズを明示的に制限。Tradeoffは上位の選別がより厳しくなる。
Option 3: overlayのみ維持し、単一ランキングの簡潔性を守る。Tradeoffはペアの価値が評価されにくい。

### Naval:
自動化の観点では、候補抽出・評価・適用が一連で自動化されないと負債になる。候補抽出や評価はあるが、常時更新・保存の仕組みが薄い。`src/lisp/school/school-pair-composite.lisp:84` と `src/lisp/school/school-db.lisp:222`。
Option 1: 日次バッチでペア候補を再計算し、自動で有効ペアを更新する。Tradeoffは計算量増。
Option 2: 手動または半自動でペア選定し、運用負荷を抑える。Tradeoffは最適化速度が低下。
Option 3: 併存は先送りし、overlayだけ運用して学習を溜める。Tradeoffは機会損失。

### Simons:
ペア合成のスコアはSharpe/PFに依存しており、相関が低いだけではない。`src/lisp/school/school-pair-composite.lisp:226` と `src/lisp/school/school-pair-composite.lisp:233`。しかし評価は短期トレード系列に寄るため、多重検定リスクが高い。
Option 1: 併存するならペア合成にもOOS/CPCV相当の検証を義務化。Tradeoffは遅延。
Option 2: ペアはリスク調整係数としてのみ利用。Tradeoffは「戦略」としての学習が弱い。
Option 3: ペア候補は毎週の監査対象として手動承認。Tradeoffは運用負担。
反証: ペアの見かけの改善は同じ市場要因の遅延相関であり、独立性は誤認されやすい。

## 技術パネルの意見
### Fowler:
実装上、pairの概念が「実行時overlay」に埋め込まれ、ランキングと分離されている。`src/lisp/school/school-execution.lisp:247` と `src/lisp/school/school-pair-composite.lisp:35`。このまま同列ランキングを導入すると境界がさらに曖昧になる。
Option 1: Pairを独立エンティティ化し、評価・保存・実行を分離。Tradeoffは設計工数増。
Option 2: 既存のランキングには触れず、portfolio層にペア評価レイヤを作る。Tradeoffは二重基準。
Option 3: overlayを残し、ランキングは単一のみ。Tradeoffはペアの存在感が小さい。

### Hickey:
グローバル状態が多く、pair定義がplistで保持されるのは壊れやすい。`src/lisp/school/school-pair-composite.lisp:5` と `src/lisp/school/school-pair-composite.lisp:8`。名前依存のpair-idもリネームに弱い。`src/lisp/school/school-pair-composite.lisp:22`。
Option 1: ペア定義をDBに永続化し、idはstrategy-hash由来にする。Tradeoffは移行コスト。
Option 2: 今のまま使うが、明示的に「実験機能」とラベルする。Tradeoffは運用品質低下。
Option 3: 併存はやめ、単一戦略の純度を高める。Tradeoffは多様性損失。

### Uncle Bob:
ユニットテストはあるが統合テストがない。`src/lisp/tests/pair-composite-tests.lisp:9` と `src/lisp/tests.lisp:1084` が示す通り、通知や評価はテスト済みだが、ランキング統合や永続化の保証がない。
Option 1: 併存するなら、rank昇格フローとペア評価のE2Eテストを追加。Tradeoffはテスト整備コスト。
Option 2: overlay維持で、テスト範囲を現状に留める。Tradeoffは品質の天井。
Option 3: 併存は段階的にし、まずはDB永続化まで。Tradeoffは段階遅延。

## ビジョナリーの意見
### Ng:
非相関はラベルではなく特徴量。日次PnL相関だけで判定するのは粗い。`src/lisp/school/school-dalio.lisp:143`。レジーム別に分解しないと学習が歪む。
Option 1: レジームごとの相関スコアを導入してペア選定に使う。Tradeoffは特徴量増。
Option 2: 現行の相関スコアは「通知」に限定し、意思決定には使わない。Tradeoffは改善速度低下。
Option 3: 併存するが、スコアは補助指標として重み付ける。Tradeoffは運用の説明が難化。

### López de Prado:
同列ランキングは多重検定の地獄。候補数を増やすほど偽陽性が増える。`src/lisp/school/school-pair-composite.lisp:253` が示す候補生成は最大5ペアに抑えているが、基準はまだ緩い。
Option 1: 併存はするが、ペアはCPCV相当の検証を通過したものだけに限定。Tradeoffはスループット低下。
Option 2: 同列競争は避け、別枠の長期評価を作る。Tradeoffは意思決定が遅くなる。
Option 3: overlayのみで、統計検定は単一戦略に集中。Tradeoffは多様性低下。
反証: 見かけの相関低下はサンプルの短さによるノイズの可能性が高い。

### Gene Kim:
運用視点では、pair定義と実績の可観測性が弱い。trade_logsにpair_idは保存されるが監視がない。`src/lisp/school/school-db.lisp:60` と `src/lisp/school/school-db.lisp:222`。通知も昇格時のみで、日常のペア健全性が見えない。`src/lisp/school/school-dalio.lisp:176`。
Option 1: 併存するならペアごとの日次レポートと異常検知を追加。Tradeoffは運用負担。
Option 2: overlay維持で、最小限のダッシュボードだけ追加。Tradeoffは洞察不足。
Option 3: 併存を延期し、まず監視基盤を整える。Tradeoffは機会損失。

## Musk's Decision (Final)
> 「併存はする。ただし“同列競争”は段階的だ。まずはペアを独立エンティティとして保存し、別枠で検証・監査する。CPCV相当の検証を通ったペアのみ、限定枠で単一戦略と競争させる。見かけの相関低下に騙される設計は捨てる。」

## Actionable Items
1. ペア戦略の永続化方式を決める。少なくともペアIDと構成戦略をDBに保存できる設計にする。根拠: `src/lisp/school/school-pair-composite.lisp:8`。
2. 同列競争をする場合は「別枠」または「限定枠」を導入し、枠数を明示する。根拠: `src/lisp/school/school-rank-system.lisp:106`。
3. ペア評価にOOS/CPCV相当の検証ゲートを追加する。根拠: `src/lisp/school/school-db.lisp:237`。
4. 併存フェーズのE2Eテストを追加し、昇格通知とペア適用が分離されることを検証する。根拠: `src/lisp/tests.lisp:1084`。

