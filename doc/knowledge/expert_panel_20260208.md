# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-08
**Leader:** Elon Musk
**Mode:** consult
**Trigger:** `oos_queue` の `Strategy not found for OOS result` をどう扱うか。OOS待ち中にプルーニングで削除される競合の解消。
**Purpose:** OOS結果が失われず、Aランク昇格の検証が正しく進むように、OOS待ちとプルーニングの整合性を取る。
**Constraints:** DBが正本でKBはキャッシュという原則を崩さない（`doc/owners_guide.md:194-196`）。OOS検証フローと昇格ゲートを維持する（`src/lisp/school/school-validation.lisp:112-229`）。即時削除のプルーニング方針は維持する（`src/lisp/school/school-backtest.lisp:517-541` `src/lisp/school/school-rank-system.lisp:137-153`）。
**Success Criteria:** OOS結果が到着しても「戦略が見つからない」エラーがほぼゼロになる。A昇格のOOS検証が欠損せず進む。`oos_queue` の `error` が積み上がらない。
**Assumptions:** `handle-oos-backtest-result` は `find-strategy` でKBを探すため、メモリ削除済みの戦略には適用できない（`src/lisp/school/school-validation.lisp:180-199` `src/lisp/strategies/strategies.lisp:260-269`）。プルーニングはKBから即時削除する（`src/lisp/school/school-backtest.lisp:517-541`）。
**Uncertainties:** OOS結果の平均/最大レイテンシとプルーニング頻度の相対関係。OOS待ちの戦略数が多い時のメモリコスト。DB再取得での復元を許すべきかどうか。

## 🏛️ 常設顧問の意見
### Taleb:
OOS結果は破滅回避の「検証ゲート」なのに、KB削除で結果を捨てているのは危険（`src/lisp/school/school-validation.lisp:180-199` `src/lisp/strategies/strategies.lisp:260-269`）。
1. **OOS待ち中はプルーニングしない**。Tradeoff: 弱戦略の滞留でメモリ負荷が増える。
2. **プルーニング時に `oos_queue` を掃除**。Tradeoff: 結果は捨てるので検証機会が減る。
3. **`Strategy not found` を info 扱い**。Tradeoff: 痛みが見えなくなり再発が続く。
反証: OOS待ち保護は「弱戦略の延命」になり、尾部での損失確率を増やす可能性がある。

### Graham:
OOS検証はA昇格の要所だが、結果が欠損すれば昇格フロー自体が歪む（`src/lisp/school/school-validation.lisp:224-228`）。
1. **OOS待ち中は削除しない**。Tradeoff: 即時淘汰の哲学と一部衝突する。
2. **削除するが `oos_queue` を即削除**。Tradeoff: 速度は保てるが検証の投資が無駄になる。
3. **info化で観測だけ維持**。Tradeoff: 意思決定の質が改善しない。

### Naval:
「DBが正本」なのにKB不在で結果を捨てるのは設計負債（`doc/owners_guide.md:194-196` `src/lisp/strategies/strategies.lisp:260-269`）。
1. **OOS待ち中は削除しない**。Tradeoff: 短期的なメモリ負荷。
2. **削除時に `oos_queue` を消して“キャンセル”扱い**。Tradeoff: OOS結果の価値を捨てる。
3. **info化**。Tradeoff: レバレッジは生まれない。

### Simons:
検証データを欠損させるのは統計的に最悪。結果が返る設計なら必ず受け取れ（`src/lisp/school/school-validation.lisp:180-229`）。
1. **OOS待ち中は削除しない**。Tradeoff: スループットが若干落ちる。
2. **削除時に `oos_queue` を掃除**。Tradeoff: 検証が未完のまま破棄される。
3. **info化**。Tradeoff: 失敗を「黙殺」するだけ。
反証: 低Sharpeで既に死刑判定の戦略なら、OOS結果を捨てても統計的損失は小さい可能性がある。

## 💻 技術パネルの意見
### Fowler:
`handle-oos-backtest-result` がメモリKB依存で、DB正本設計と責務がズレている（`src/lisp/school/school-validation.lisp:180-199` `doc/owners_guide.md:194-196`）。
1. **OOS待ちは削除禁止**。Tradeoff: 既存の即時削除ルールと一時的に矛盾。
2. **削除時に `oos_queue` を掃除**。Tradeoff: “処理した”のに結果を使わない不整合。
3. **info化**。Tradeoff: 設計負債が残る。

### Hickey:
「正本はDB」「KBはキャッシュ」と言うなら、結果処理はDB側に落とすべき（`doc/owners_guide.md:194-196`）。`find-strategy` がKBにしか触れないのは単純性の欠如（`src/lisp/strategies/strategies.lisp:260-269`）。
1. **OOS待ち中は削除しない**。Tradeoff: キャッシュの寿命が延びる。
2. **削除時に `oos_queue` を掃除**。Tradeoff: OOS結果の意味が消える。
3. **info化**。Tradeoff: 問題を先送りするだけ。

### Uncle Bob:
OOS結果が来る前に削除するレースはテストで再現できるのに放置されている（`src/lisp/school/school-validation.lisp:180-199` `src/lisp/school/school-backtest.lisp:517-541`）。
1. **OOS待ちは削除禁止 + テスト追加**。Tradeoff: 実装とテストの初期コスト。
2. **削除時に `oos_queue` を掃除 + テスト追加**。Tradeoff: 結果破棄を前提にする。
3. **info化だけ**。Tradeoff: テストなしで回帰が増える。

## 🚀 ビジョナリーの意見
### Ng:
OOS結果は学習信号の一部。結果欠損は学習の歪みになる（`src/lisp/school/school-validation.lisp:201-210`）。
1. **OOS待ちは削除禁止**。Tradeoff: メモリ負荷。
2. **削除時に `oos_queue` を掃除**。Tradeoff: データ欠損が増える。
3. **info化**。Tradeoff: 監視だけで改善しない。

### López de Prado:
OOSは過学習を防ぐ唯一のゲート。結果欠損は「検証を捨てる」ことに等しい（`src/lisp/school/school-validation.lisp:224-228`）。
1. **OOS待ちは削除禁止**。Tradeoff: 回転が少し遅くなる。
2. **削除時に `oos_queue` を掃除**。Tradeoff: ゲートの信頼性が下がる。
3. **info化**。Tradeoff: 破綻の兆候を黙殺する。
反証: 既にSharpeが低い戦略は、OOS結果を待たずとも削除してもリスクが小さい場合がある。

### Gene Kim:
`oos_queue` はDBにあるのに、KB削除が前提で結果処理が失敗するのは運用上のSLO違反（`src/lisp/school/school-db-oos.lisp:7-39` `src/lisp/school/school-validation.lisp:180-199`）。
1. **OOS待ちは削除禁止**。Tradeoff: プルーニング遅延の監視が必要。
2. **削除時に `oos_queue` を掃除**。Tradeoff: 監視指標としてのOOS成功率が歪む。
3. **info化**。Tradeoff: 原因が見えなくなる。

## 🚀 Musk's Decision (Final)
> 「**1を採用**。OOS待ち中は削除しない。OOSは昇格ゲートであり、結果を捨てるのは愚かだ。**ただし補助として2を追加**。例外的に削除する場合は `oos_queue` を“キャンセル”扱いで掃除する。**3はやらない**。問題を隠すだけだ。」

## Actionable Items
1. OOS待ち中の戦略をプルーニング対象から除外するガードを追加する（`src/lisp/school/school-backtest.lisp:517-541` `src/lisp/school/school-rank-system.lisp:137-153` `src/lisp/school/school-db-oos.lisp:17-39`）。
2. 例外的に削除する場合は `oos_queue` をキャンセル扱いで削除/更新し、結果が来たら「stale」として無害に捨てる（`src/lisp/school/school-validation.lisp:185-194` `src/lisp/school/school-db-oos.lisp:26-39`）。
3. `Strategy not found for OOS result` を発生させた回数と対象名を週次で可視化し、OOSレイテンシと相関を取る（`src/lisp/school/school-validation.lisp:197-209`）。
