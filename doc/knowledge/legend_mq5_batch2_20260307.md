# Legend MQ5 Batch 2

日付: 2026-03-07 JST

第2弾では、LEGEND の中でも

- トレンド押し目型
- 単純クロス型
- MACD 確認型

の 3 本を追加します。すべて自然言語版は日本語で記述し、MQ5 側は単体 EA として独立させます。

## 1. Trend-Pullback-Entry

### 自然言語版

- シンボル: `USDJPY`
- 時間足: `M15`
- 指標: `SMA(200)`, `RSI(14)`
- ロングエントリー:
  - 大きな上昇トレンドの中で、一時的に売られすぎた押し目を買う
  - 厳密条件: `Close > SMA200` かつ `RSI14 < 40`
- ロングイグジット:
  - 反発が十分進んだら手仕舞う
  - 厳密条件: `RSI14 > 60`
- ショート側の対称推論:
  - `Close < SMA200` かつ `RSI14 > 60` で売る
  - `RSI14 < 40` で手仕舞う
- リスク設定:
  - `SL = 0.50`
  - `TP = 0.80`
- 現在メトリクス:
  - Sharpe `0.151`
  - PF `1.071`
  - Trades `4871`
  - MaxDD `0.00209`
- 市場アイデア:
  - 方向は SMA200 で固定し、エントリーは逆張り気味に取る設計です。順張りの土台に対して短期の行き過ぎだけを拾います。

### MQ5版

- ファイル: `src/mt5/legend_batch2/Legend_TrendPullbackEntry.mq5`
- 実装メモ:
  - デフォルトは `M15`
  - トレンド方向は SMA200
  - エントリー / イグジットは RSI 閾値

## 2. Sweet-Chariot-SMA-40

### 自然言語版

- シンボル: `USDJPY`
- 時間足: `M30`
- 指標: `SMA(40)`
- ロングエントリー:
  - 終値が `SMA40` を上抜いたら買う
- ロングイグジット:
  - 終値が `SMA40` を下抜いたら手仕舞う
- ショート側の対称推論:
  - 終値が `SMA40` を下抜いたら売る
  - 終値が `SMA40` を上抜いたら手仕舞う
- リスク設定:
  - `SL = 0.30`
  - `TP = 0.90`
- 現在メトリクス:
  - Sharpe `0.231`
  - PF `1.093`
  - Trades `6710`
  - MaxDD `0.00246`
- 市場アイデア:
  - 非常に単純なトレンド切り替え型です。価格が基準線のどちら側にあるかだけを見るので、EA 化や追加検証がしやすいです。

### MQ5版

- ファイル: `src/mt5/legend_batch2/Legend_SweetChariotSMA40.mq5`
- 実装メモ:
  - デフォルトは `M30`
  - Close と SMA40 のクロスのみで判断
  - long / short の対称性が高い

## 3. MACD-Above-Zero-Cross

### 自然言語版

- シンボル: `USDJPY`
- 時間足: `H4`
- 指標: `MACD(12,26,9)`
- ロングエントリー:
  - MACD 本線がゼロラインより上にあり、さらに signal を上抜いたら買う
  - 厳密条件: `MACD-LINE > 0` かつ `MACD-LINE` が `SIGNAL-LINE` を上抜く
- ロングイグジット:
  - `MACD-LINE` が `SIGNAL-LINE` を下抜いたら手仕舞う
- ショート側の対称推論:
  - `MACD-LINE < 0` かつ `MACD-LINE` が `SIGNAL-LINE` を下抜いたら売る
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
  - 単なる MACD クロスではなく、ゼロライン上だけをロング対象にすることで、上昇側の地合いがある場面に限定しています。

### MQ5版

- ファイル: `src/mt5/legend_batch2/Legend_MACDAboveZeroCross.mq5`
- 実装メモ:
  - デフォルトは `H4`
  - MACD main / signal の closed-bar cross を利用
  - ショート側はゼロライン下で対称化

## 次候補

次に切るならこの順が自然です。

1. `MACD-Signal-Cross`
2. `Legend-London-Breakout-V1`
3. `Legend-RSI-Reversion-V1`
4. 過去 S ランク候補の `data_sexp` 再抽出
