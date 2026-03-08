# Legend MQ5 Batch 3

日付: 2026-03-07 JST

第3弾では、次の 3 本を追加します。

- `MACD-Signal-Cross`
- `Legend-London-Breakout-V1`
- `Legend-RSI-Reversion-V1`

このバッチは、現行 LEGEND 本流の MACD 系 1 本と、library 上に残っている external Legend 2 本を先に整理するためのものです。

## source divergence の扱い

`Legend-London-Breakout-V1` と `Legend-RSI-Reversion-V1` は、以下のソース間で定義が揺れています。

- `src/lisp/strategies/strategies-legendary.lisp`
- `src/lisp/strategies/legends.lisp`
- `data/library/LEGEND/*.lisp`
- `data/memory/swimmy.db` の `strategies` 行

この batch-3 では次の方針を採ります。

- ロジック正本は「現行 library / DB row」
- ただし MQ5 実装では、時間足や意味論が崩れないよう旧 source の意図も参照する
- どこに揺れがあるかは各戦略の注記で明示する

freeze 後の監査結果は `doc/knowledge/legend_external_source_audit_20260307.md` に固定した。

## 1. MACD-Signal-Cross

### 自然言語版

- シンボル: `USDJPY`
- 時間足: `H4`
- 指標: `MACD(12,26,9)`
- ロングエントリー:
  - MACD 本線が signal 線を上抜いたら買う
  - 厳密条件: `MACD-LINE` が `SIGNAL-LINE` を上抜く
- ロングイグジット:
  - `MACD-LINE` が `SIGNAL-LINE` を下抜いたら手仕舞う
- ショート側の対称推論:
  - `MACD-LINE` が `SIGNAL-LINE` を下抜いたら売る
  - `MACD-LINE` が `SIGNAL-LINE` を上抜いたら手仕舞う
- リスク設定:
  - `SL = 0.25`
  - `TP = 0.50`
- 現在メトリクス:
  - Sharpe `0.164`
  - PF `1.097`
  - Trades `2351`
  - MaxDD `0.00177`
- 市場アイデア:
  - 非常に素直な MACD クロス戦略です。ゼロライン条件を付けず、モメンタム転換そのものをシグナルとして扱います。

### MQ5版

- ファイル: `src/mt5/legend_batch3/Legend_MACDSignalCross.mq5`
- 実装メモ:
  - デフォルトは `H4`
  - MACD main / signal の closed-bar cross を使う
  - ゼロライン条件は入れない

## 2. Legend-London-Breakout-V1

### 自然言語版

- シンボル: `USDJPY`
- 運用時間足: `H1`
- 現行 library 表現:
  - 指標: `TIME-RANGE 08:00-09:00`
  - エントリー: `BREAK-HIGH-LOW 08:00-09:00`
  - イグジット: `TIME-CLOSE 16:00`
- ロングエントリー:
  - 08:00-09:00 のレンジ高値を、その後の時間帯で上抜いたら買う
- ロングイグジット:
  - 16:00 到達で手仕舞う
- ショート側の対称推論:
  - 08:00-09:00 のレンジ安値を下抜いたら売る
  - 16:00 到達で手仕舞う
- リスク設定:
  - `SL = 0.20`
  - `TP = 0.60`
- 現在メトリクス:
  - Sharpe `0.339`
  - PF `1.053`
  - Trades `194`
  - MaxDD `0.00155`
- 市場アイデア:
  - ロンドン時間の初動レンジを作り、その外側へ抜けた方向についていく時刻依存のブレイクアウトです。

### 注記

- `strategies-legendary.lisp` では別のセッション表現と `SL/TP` が見えます。
- 現行 library / DB では `08:00-09:00` レンジ、`16:00` クローズとして保持されています。
- MQ5 版は現行 library / DB を正本にし、時間足は運用上自然な `H1` として実装しています。

### MQ5版

- ファイル: `src/mt5/legend_batch3/Legend_LondonBreakoutV1.mq5`
- 実装メモ:
  - 1 日 1 回レンジ更新
  - レンジ確定後に 1 回だけ breakout を狙う
  - 16:00 で時間決済

## 3. Legend-RSI-Reversion-V1

### 自然言語版

- シンボル: `USDJPY`
- 運用時間足: `M5`
- 現行 library 表現:
  - 指標: `RSI(2)`
  - エントリー: `RSI-BELOW 10`
  - イグジット: `RSI-ABOVE 90`
- ロングエントリー:
  - RSI(2) が 10 未満まで売られたら買う
- ロングイグジット:
  - RSI(2) が 90 を上回ったら手仕舞う
- ショート側の対称推論:
  - RSI(2) が 90 を上回ったら売る
  - RSI(2) が 10 未満へ戻ったら手仕舞う
- リスク設定:
  - `SL = 0.10`
  - `TP = 0.10`
- 現在メトリクス:
  - Sharpe `-0.168`
  - PF `0.974`
  - Trades `3216`
  - MaxDD `0.00336`
- 市場アイデア:
  - 超短期の行き過ぎを RSI(2) で取る極端な平均回帰型です。エントリー頻度は高いですが、現時点の実測成績は強くありません。

### 注記

- 古い source では `RSI < 5` や `close > sma-5` での exit も見えます。
- ただし現行 library / DB では `RSI-BELOW 10` と `RSI-ABOVE 90` が保存されているため、こちらを正本として採用します。
- MQ5 版のショートは原定義ではなく対称推論です。

### MQ5版

- ファイル: `src/mt5/legend_batch3/Legend_RSIReversionV1.mq5`
- 実装メモ:
  - デフォルトは `M5`
  - long は library 定義に忠実
  - short は対称推論として追加

## 次候補

この次は、

1. `doc/knowledge/legend_mt5_compile_report_20260307.md` に沿って Strategy Tester 実測
2. `MACD-Zero-Cross-Long`
3. `MACD-Expansion`
4. `Crossover-Plus-MACD`

の順で進めるのが妥当です。
