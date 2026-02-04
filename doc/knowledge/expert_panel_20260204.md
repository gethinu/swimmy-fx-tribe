# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-04
**Leader:** Elon Musk
**Mode:** consult
**Trigger:** "BACKTEST V2の`timeframe`表現がテストとズレている。INTERFACES優先かテスト優先か、どう決めるべきか？"
**Purpose:** Backtest V2の`Option`表現を仕様・実装・テストで一致させ、Guardian互換と進化パイプラインの信頼性を守る。
**Constraints:** 内部ZMQはS式のみ、Backtest Serviceはserde_lexprの`Option<T>`表現（空/1要素リスト）を前提（`docs/llm/INTERFACES.md#L193`）。
**Success Criteria:** `test-backtest-v2-uses-alist`が仕様に沿ってパスし、Backtest Service/Guardian側のパース互換を壊さない。
**Assumptions:** `timeframe`はBacktest V2で`Option<i64>`扱いである（`docs/llm/INTERFACES.md#L217`）。
**Uncertainties:** Guardian側の受信実装が`(timeframe . 1)`と`(timeframe 1)`の両方を受理するかは未検証。

## 🏛️ 常設顧問の意見
### Taleb:
- **批判**: テストが仕様より「偶然の内部表現」に依存している。仕様は`Option`を「空/1要素リスト」と明記しているのに、テストはドットペア前提で壊れている（`docs/llm/INTERFACES.md#L193`, `src/lisp/tests.lisp#L742`）。
- **選択肢**:
  1) **仕様優先**: 実装は`(timeframe 1)`のまま、テストを`(cadr ...)`/`(first (cdr ...))`に修正。
  2) **互換拡張**: 受信側で`(timeframe . 1)`も`(timeframe 1)`も許容（破壊的変更の回避）。
  3) **仕様破り**: `timeframe`をドットペアに統一（ただしOption規約違反）。
- **トレードオフ**: 1が最小リスク。2は防御的だが複雑化。3は将来の破綻リスク。
- **具体根拠**: `request-backtest-v2`は`(list 'timeframe timeframe)`でリスト化している（`src/lisp/school/school-backtest-v2.lisp#L58`）。
- **反証**: Guardianが「ドットペアのみ受理」なら仕様が嘘になる。だが仕様を変えるなら`INTERFACES`を修正してからやれ。

### Graham:
- **批判**: テストは「契約」じゃなく「実装詳細」を検証している。仕様の整合性より、テストの都合が勝っているのは最悪（`src/lisp/tests.lisp#L742`）。
- **選択肢**:
  1) **最小変更**: テストだけ修正し、仕様と実装を一致させる。
  2) **仕様を変える**: `INTERFACES`を更新してテストに合わせる（ただし全依存箇所へ波及）。
  3) **抽象化**: `Option`を生成するヘルパーを導入し、テストはヘルパーに追従。
- **トレードオフ**: 1が最短。2は全域修正が必要。3は将来の負債削減だが今は余計。
- **具体根拠**: `INTERFACES`に明記済みの`Option`表現（`docs/llm/INTERFACES.md#L193`）。

### Naval:
- **批判**: 仕様とテストの二重管理が「レバレッジ」を殺している。単一の真実（INTERFACES）から機械的にテストを生成するべき。
- **選択肢**:
  1) 仕様優先 + テスト修正。
  2) 仕様優先 + `Option`整合のユーティリティを作る。
  3) テスト優先だが、その場合は仕様更新を強制。
- **トレードオフ**: 2は長期的に強いが短期工数。
- **具体根拠**: `timeframe`の表現はBacktest V2 payloadで明示的にリスト化されている（`src/lisp/school/school-backtest-v2.lisp#L58`）。

### Simons:
- **批判**: 数値フィールドの型契約が曖昧なままテストに反映されている。`Option<i64>`を表す構文を「一貫」させよ。
- **選択肢**:
  1) `Option`表現を仕様通りに維持し、テストを合わせる。
  2) 受信側で2種類の表現を許可して、送信側は仕様通りのまま。
  3) `Option`ヘルパーを導入して全送信を正規化。
- **トレードオフ**: 3が最も統計的に安全だが、今は小さな不整合の修正が先。
- **具体根拠**: `Option`規約の明記（`docs/llm/INTERFACES.md#L193`）。

## 💻 技術パネルの意見
### Fowler:
- **批判**: `Option`のシリアライズ規約が「仕様」と「実装」「テスト」で分散している。`timeframe`がlistなのは仕様通りだが、テストが契約を破っている（`src/lisp/tests.lisp#L742`）。
- **選択肢**:
  1) 仕様優先でテストを修正。
  2) 仕様を更新し、テストも実装も同時に合わせる（大きい）。
  3) `Option`構築/検証のヘルパーを導入し、テストはそれを使う。
- **トレードオフ**: 1が最小。3は長期の技術負債に効く。
- **具体根拠**: `request-backtest-v2`のpayload構築（`src/lisp/school/school-backtest-v2.lisp#L51`）。

### Hickey:
- **批判**: データが複雑化しているのに、テストが「表現」に縛られている。S式はデータだ。表現の揺れを許容して本質を検証せよ。
- **選択肢**:
  1) `Option`はlistで統一し、テストは`(cadr ...)`で検証。
  2) parser側で両方許容。
- **トレードオフ**: 2は設計が重くなる。1で十分。
- **具体根拠**: `Option`仕様に基づくpayload（`docs/llm/INTERFACES.md#L193`, `src/lisp/school/school-backtest-v2.lisp#L58`）。

### Uncle Bob:
- **批判**: テストが間違った期待をしているのは、設計の「契約」がコードに反映されていないから。テストは契約を検証しろ。
- **選択肢**:
  1) テスト修正（`(numberp (cadr ...))`）で仕様に整合。
  2) 実装変更＋仕様更新のセットで対応。
- **トレードオフ**: 1が安全。2は変更範囲が広すぎる。
- **具体根拠**: テスト側の`(cdr (assoc 'timeframe ...))`がリストを返す（`src/lisp/tests.lisp#L742`）。

## 🚀 ビジョナリーの意見
### Ng:
- **批判**: Backtest結果が止まると進化が止まる。テストの誤仕様でパイプラインの可観測性が下がるのは致命的。
- **選択肢**:
  1) 仕様に合わせてテストを直す。
  2) 受信側の柔軟性も追加して「取りこぼし」を減らす。
- **トレードオフ**: 2は安全だが実装コストと複雑性が増える。
- **具体根拠**: Backtest V2 payloadとOption規約の不一致が原因（`docs/llm/INTERFACES.md#L193`, `src/lisp/tests.lisp#L742`）。

### López de Prado:
- **批判**: 進化パイプラインのデータ品質が落ちると、統計的に意味のない進化になる。仕様とテストのズレは「ノイズ源」だ。
- **選択肢**:
  1) 仕様通りにテストを修正。
  2) 送受信の両方でOptionのスキーマ検証を追加。
- **トレードオフ**: 2は本質的に良いが、まずは1でノイズ源を消せ。
- **具体根拠**: Option規約は明示されているのにテストが破っている（`docs/llm/INTERFACES.md#L193`, `src/lisp/tests.lisp#L742`）。

### Gene Kim:
- **批判**: テストが赤いままでは運用判断ができない。仕様ドリフトは運用リスク。
- **選択肢**:
  1) 仕様優先でテストを修正し、STATEに明記。
  2) 実装を変えるなら、影響範囲を洗ってから。
- **トレードオフ**: 1が即効性。2はリードタイムが伸びる。
- **具体根拠**: `docs/llm/INTERFACES.md`の`Option`表現とテストの不一致（`docs/llm/INTERFACES.md#L193`, `src/lisp/tests.lisp#L742`）。

## 🚀 Musk's Decision (Final)
> 「仕様が正義。`Option`表現は`(timeframe 1)`のまま維持し、テストを仕様に合わせる。受信側の柔軟化は“必要になったら”でいい。まず動くパイプラインを取り戻せ。」

## Actionable Items
1. `test-backtest-v2-uses-alist`を仕様準拠に修正（`(numberp (cadr ...))`等）: `src/lisp/tests.lisp#L742`
2. `INTERFACES`の`Option`記述が実装と一致していることを再確認・必要なら追記: `docs/llm/INTERFACES.md#L193`
3. `STATE`に「Backtest V2の`Option`表現はリスト形式が正本」と明記: `docs/llm/STATE.md`
