# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-07
**Leader:** Elon Musk
**Mode:** consult
**Trigger:** ペア戦略の限定枠（シンボル×TF）と単一戦略との競争設計をどうするべきか。
**Purpose:** ペア戦略を「独立エンティティとして評価・昇格」しつつ、単一戦略と競わせる枠設計（同一枠/別枠/ハイブリッド）を決定する。
**Constraints:** 既存のB/A/S昇格フローとOOS/CPCVゲートを壊さないこと。パフォーマンス劣化を避けること。実行時のoverlayは維持すること。`trade_logs` の `pair_id` 既存利用を前提とすること。`src/lisp/school/school-rank-system.lisp:31-33` `src/lisp/school/school-validation.lisp:300-421` `src/lisp/school/school-execution.lisp:247-265` `src/lisp/school/school-db.lisp:58-74`。
**Success Criteria:** ペア導入後もA/S選抜の再現性が維持され、ポートフォリオ全体のMaxDD悪化を招かず、かつOOS/CPCVゲートを通過したペアだけが運用されること。枠数と競争ルールが明示的で、テストで保証されること。
**Assumptions:** ペア候補はシンボル×TF単位のプールから抽出される（`src/lisp/school/school-pair-composite.lisp:84-99`）。ペアの上限数は定数で管理される（`src/lisp/school/school-pair-composite.lisp:233-235`）。A昇格の候補選抜はTF×方向×シンボル単位で `*a-rank-slots-per-tf*` によって絞り、OOSへ投入する（`src/lisp/school/school-rank-system.lisp:303-325`）。
**Uncertainties:** CPCVのtrade_listが永続化されておらず、ペア合成CPCVの検証基盤が不足している（`src/lisp/core/message-dispatcher.lisp:296-318`）。ペアIDが戦略名ハッシュであり、リネームに弱い（`src/lisp/school/school-pair-composite.lisp:22-26`）。ペアがS-rankドラフトに入る際の公平性定義（`src/lisp/school/school-portfolio.lisp:9-35`）。

## 🏛️ 常設顧問の意見
### Taleb:
ペアは「低相関」によって短期的に魅力的に見えるが、同時期の尾部で同方向に崩れる可能性がある。候補生成は相関閾値と短期トレード系列に依存し、破局的相関を検知しない（`src/lisp/school/school-pair-composite.lisp:129-180` `src/lisp/school/school-pair-composite.lisp:253-295`）。
1. **同一枠で競争**。Tradeoff: 破局リスクを単一戦略と同等に扱うため、誤判定のコストが上がる。
2. **別枠で隔離**。Tradeoff: パフォーマンス向上が遅くなる。
3. **ハイブリッド（同一ランキング＋ペア上限）**。Tradeoff: 実装が増えるが、破局の上限を明示できる。
反証: 同一枠競争が「見かけの分散」を増やすだけで、尾部リスクを減らさない可能性がある。

### Graham:
現行のA枠選抜は `*a-rank-slots-per-tf*` で厳しく制限されている（`src/lisp/school/school-rank-system.lisp:31-33` `src/lisp/school/school-rank-system.lisp:317-319`）。ここにペアを混ぜると、単一戦略の“最強”が押し出されるリスクがある。
1. **別枠**。Tradeoff: ペアの存在感が薄い。
2. **同一枠＋上限**。Tradeoff: 選抜の説明が難化する。
3. **段階導入（まず別枠→実績で同一枠）**。Tradeoff: 判断の遅れ。

### Naval:
ペアは現在「overlayの実行パス」にだけ存在し、定義がメモリ上のplistに留まっている（`src/lisp/school/school-pair-composite.lisp:8-46` `src/lisp/school/school-execution.lisp:247-265`）。同一枠競争をするなら、永続化と更新が自動化されない限り運用負債になる。
1. **DB永続化＋日次再評価を自動化**。Tradeoff: 計算負荷増。
2. **手動選別＋限定枠**。Tradeoff: 運用負荷増。
3. **overlayのまま維持**。Tradeoff: 価値検証が進まない。

### Simons:
ペアのスコアはSharpe/PFに依存するが、相関低下＝独立性ではない（`src/lisp/school/school-pair-composite.lisp:226-231`）。多重検定リスクが強いので、CPCV相当の検証ゲートが必須。
1. **同一枠競争 + CPCV合格のみ採用**。Tradeoff: スループット低下。
2. **別枠で長期評価**。Tradeoff: 機会損失。
3. **ハイブリッド（同一枠＋ペア上限＋CPCV）**。Tradeoff: 実装複雑化。
反証: ペアの改善は短期ノイズで、長期で消える可能性が高い。

## 💻 技術パネルの意見
### Fowler:
現在の境界は「実行時overlay」と「ランク判定」が分離されている。混在させると責務が曖昧になる（`src/lisp/school/school-execution.lisp:247-265` `src/lisp/school/school-rank-system.lisp:303-325`）。
1. **ペアを独立エンティティ化し、ランク評価は別パイプライン**。Tradeoff: 実装増。
2. **同一枠競争だが、評価・保存層は分離**。Tradeoff: 二重基準の説明が必要。
3. **overlay維持**。Tradeoff: 競争設計が進まない。

### Hickey:
`*pair-active-defs*` のようなグローバルplistは壊れやすい（`src/lisp/school/school-pair-composite.lisp:8-9`）。さらに `pair-id` は戦略名から計算しており、リネームで壊れる（`src/lisp/school/school-pair-composite.lisp:22-26`）。
1. **DB永続化＋strategy-hashベースID**。Tradeoff: 既存データ移行。
2. **現状維持だが“実験”と明示**。Tradeoff: 品質低下。
3. **ペア導入を止める**。Tradeoff: 多様性喪失。

### Uncle Bob:
テストはpairユーティリティ単体に限られ、昇格ゲートや枠競争のE2E保証がない（`src/lisp/tests/pair-composite-tests.lisp:1-110`）。
1. **TDDでランク昇格・枠競争の統合テストを追加**。Tradeoff: 初期コスト。
2. **overlay維持＋テスト据え置き**。Tradeoff: 回帰リスク。
3. **段階導入（DB保存→昇格ゲート→枠競争）**。Tradeoff: リリース遅延。

## 🚀 ビジョナリーの意見
### Ng:
相関はラベルではなく特徴量。ペアの評価を単一の相関指標に寄せると学習が歪む。既存の相関利用はtrade_logsベースで、ペアID視点の集計設計がない（`src/lisp/school/school-portfolio.lisp:56-72` `src/lisp/school/school-db.lisp:58-74`）。
1. **相関を補助指標として扱い、昇格はOOS/CPCVで担保**。Tradeoff: 計算増。
2. **相関は通知に限定**。Tradeoff: 競争設計が弱い。
3. **段階導入**。Tradeoff: 完成が遅い。

### López de Prado:
多重検定の地獄。候補生成はシンボル×TFで限定されるが、上限5ペアの基準は緩い（`src/lisp/school/school-pair-composite.lisp:84-99` `src/lisp/school/school-pair-composite.lisp:233-235`）。
1. **同一枠競争＋CPCV合格のみ**。Tradeoff: スループット低下。
2. **別枠で長期評価**。Tradeoff: 意思決定遅延。
3. **ハイブリッド（同一ランキング＋ペア上限）**。Tradeoff: ルール説明が必要。
反証: 見かけの相関低下はサンプル不足で発生する。

### Gene Kim:
運用面では、pair_idは保存されているが、日常の健全性が見えない（`src/lisp/school/school-db.lisp:58-74`）。これで同一枠競争を始めるのは監視不足。
1. **ペアの可観測性（集計・監視）を先に整備**。Tradeoff: 速度低下。
2. **最小限の監視で先に実装**。Tradeoff: 事故時の説明困難。
3. **overlay維持**。Tradeoff: 学習速度低下。

## 🚀 Musk's Decision (Final)
> 「**ハイブリッド**で行く。ペアはシンボル×TFごとに上限枠を持ち、同一ランキングで単一戦略と競争させる。ただし、ペアの占有率は枠で制限し、OOS/CPCVを通過したペアだけが入る。これで“競争”と“限定枠”を両立する。」

## Actionable Items
1. ペア枠ルールを明文化する（シンボル×TFごとのペア上限 + 同一ランキング競争 + ペア占有率上限）。
2. ペアの独立エンティティ永続化（DB）とID設計（strategy-hash由来）を決める。
3. ペアのOOS/CPCV合成評価を通すため、CPCV trade_list 永続化の方針を確定する。
4. 「枠競争＋昇格ゲート」を保証する統合テストを追加する。
