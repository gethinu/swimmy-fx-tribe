# Historical S MQ5 Batch 3

日付: 2026-03-07 JST

このバッチは、過去S候補の残り 7 本をまとめて整理する最終バッチです。

対象:

1. `Bred-Bred--187-Gen23-N3980038264-261`
2. `Bred-Bred--436-Gen32-N3980040463-744`
3. `Bred-Bred--940-Gen31-N3980039835-605`
4. `Bred-Bred--586-Gen29-N3980038495-317`
5. `Bred-Bred--458-Gen32-N3980040289-704`
6. `Bred-Bred--139-Gen11-N3979972567-6`
7. `Bred-Bred--139-Gen11-N3979972610-6`

これで historical S の 13 件はすべて言語化と MQ5 化が揃いました。

## 同一シグナルコア

このバッチの 7 本も、backup DB の `data_sexp` を見ると batch-1 / batch-2 の bred 群と同じ実行コアです。

- ロング:
  - `Close > SMA20 > SMA50`
  - `RSI > 55`
- ショート:
  - `Close < SMA20 < SMA50`
  - `RSI < 45`
- イグジット:
  - `PNL > TP` または `PNL < -SL`

つまり、historical S の bred 12 本すべてが同一シグナルコアで、

- 保存された `indicators` は個体ごとにズレる
- 実際の差分は `SL/TP` と validation 成績
- 例外は recruit 系の `RECRUIT-RND-1768781166-12` だけ

という構造でした。

## 実行正本

このバッチでも `data_sexp` を実行正本として扱います。

理由:

- 保存上の `indicators` は `SMA 89` や `EMA 53` など各個体で違います
- しかし `entry` / `exit` は 7 本とも完全一致です
- MQ5 は保存指標の見た目ではなく、実行式を忠実に再構成します

## 時間足の扱い

- 7 本とも `timeframe=3600`
- これは MT5 標準足ではありません
- MQ5 版は `PERIOD_CURRENT` で実装しているため、厳密再現には custom timeframe 前提です

## 1. Bred-Bred--187-Gen23-N3980038264-261

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
  - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット: `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 0.7912`
  - `TP = 0.6591`
- 主要メトリクス:
  - Sharpe `0.386`
  - PF `9.620`
  - WR `88.6%`
  - Trades `35`
  - OOS Sharpe `17.750`
  - CPCV pass_rate `0.378`
- 市場アイデア:
  - 低い現行 Sharpe に対して OOS がかなり強い、後期 validation 寄りの execution 個体です。
- 注記:
  - 保存指標は `SMA 89 / 29 / 90 / 29` ですが、実行式はやはり `SMA20 / SMA50 / RSI` です。

### MQ5版

- ファイル: `src/mt5/historical_s_batch3/HistS_Bred187TrendCore.mq5`
- 実装メモ:
  - bred trend core にこの個体の `SL/TP` を載せた移植です

## 2. Bred-Bred--436-Gen32-N3980040463-744

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
  - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット: `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 0.7966`
  - `TP = 0.7308`
- 主要メトリクス:
  - Sharpe `16.180`
  - PF `8.137`
  - WR `88.6%`
  - Trades `35`
  - OOS Sharpe `16.180`
  - CPCV pass_rate `0.378`
- 市場アイデア:
  - 同一コアの中では 187番より均整の取れた risk envelope で、現行/OOS の数字が揃っています。
- 注記:
  - 保存指標 `SMA 101 / 28 / 89 / 89` は entry の実行式と一致しません。

### MQ5版

- ファイル: `src/mt5/historical_s_batch3/HistS_Bred436TrendCore.mq5`
- 実装メモ:
  - batch-2 bred 群と同じ entry/exit を維持

## 3. Bred-Bred--940-Gen31-N3980039835-605

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
  - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット: `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 1.7118`
  - `TP = 0.8760`
- 主要メトリクス:
  - Sharpe `12.483`
  - PF `4.992`
  - WR `88.6%`
  - Trades `35`
  - OOS Sharpe `12.483`
  - CPCV pass_rate `0.311`
- 市場アイデア:
  - 同一コアの中では最も stop が深く、耐久寄りの execution です。
- 注記:
  - 保存指標は `SMA 88 / 109 / 93 / 29` ですが、エントリー条件には反映されていません。

### MQ5版

- ファイル: `src/mt5/historical_s_batch3/HistS_Bred940TrendCore.mq5`
- 実装メモ:
  - 深い stop とやや広い target を個体差として反映

## 4. Bred-Bred--586-Gen29-N3980038495-317

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
  - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット: `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 1.2383`
  - `TP = 0.6734`
- 主要メトリクス:
  - Sharpe `12.287`
  - PF `4.817`
  - WR `85.3%`
  - Trades `34`
  - OOS Sharpe `12.287`
  - CPCV pass_rate `0.400`
- 市場アイデア:
  - 940番ほどではないが stop を深めに取り、勝率維持で押し切るタイプです。
- 注記:
  - 保存指標は `SMA 116 / 28 / 92 / 102` で、実行式と乖離しています。

### MQ5版

- ファイル: `src/mt5/historical_s_batch3/HistS_Bred586TrendCore.mq5`
- 実装メモ:
  - 同一コアの risk envelope 差分だけを実装

## 5. Bred-Bred--458-Gen32-N3980040289-704

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
  - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット: `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 1.0312`
  - `TP = 0.9322`
- 主要メトリクス:
  - Sharpe `11.751`
  - PF `4.280`
  - WR `80.0%`
  - Trades `35`
  - OOS Sharpe `11.751`
  - CPCV pass_rate `0.511`
- 市場アイデア:
  - 勝率はやや落ちる一方、CPCV pass_rate はこのクラスターでは相対的に高い個体です。
- 注記:
  - 保存指標 `SMA 109 / 31 / 87 / 139` は entry と独立です。

### MQ5版

- ファイル: `src/mt5/historical_s_batch3/HistS_Bred458TrendCore.mq5`
- 実装メモ:
  - bred trend core の中でも比較的バランス型の `SL/TP`

## 6. Bred-Bred--139-Gen11-N3979972567-6

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
  - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット: `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 0.8031`
  - `TP = 0.6844`
- 主要メトリクス:
  - Sharpe `0.214`
  - PF `1.625`
  - WR `60.0%`
  - Trades `160`
  - OOS Sharpe `0.000`
  - CPCV pass_rate `0.000`
- 市場アイデア:
  - 後年の bred S よりかなり初期の個体で、同じコアでも validation では弱い部類です。
- 注記:
  - 保存指標は `EMA 53 / SMA 113 / EMA 54` ですが、entry はやはり `SMA20 / SMA50 / RSI` です。

### MQ5版

- ファイル: `src/mt5/historical_s_batch3/HistS_Bred139TrendCore_3979972567.mq5`
- 実装メモ:
  - duplicate pair の片方としてそのまま分離

## 7. Bred-Bred--139-Gen11-N3979972610-6

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
  - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット: `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 0.8031`
  - `TP = 0.6844`
- 主要メトリクス:
  - Sharpe `0.214`
  - PF `1.625`
  - WR `60.0%`
  - Trades `160`
  - OOS Sharpe `0.000`
  - CPCV pass_rate `0.000`
- 市場アイデア:
  - 2567個体と同じ内容で、保存タイムスタンプだけ違う duplicate pair です。
- 注記:
  - hash も `9727D39B5FCDFD1` で一致しており、実体としては同一個体と見てよいです。

### MQ5版

- ファイル: `src/mt5/historical_s_batch3/HistS_Bred139TrendCore_3979972610.mq5`
- 実装メモ:
  - duplicate pair のもう片方としてファイルを分け、名前だけ保持

## duplicate pair について

`Bred-Bred--139-Gen11-N3979972567-6` と `Bred-Bred--139-Gen11-N3979972610-6` は、

- `entry`
- `exit`
- `SL/TP`
- `indicators`
- `hash`

がすべて一致しています。運用上は同一個体の二重保存と見なすのが自然です。

## 完了メモ

これで historical S は以下の形で完了です。

- batch-1: bred 2 本 + recruit 1 本
- batch-2: bred 3 本
- batch-3: bred 7 本

結論として、

1. historical S の bred 12 本は同一シグナルコア
2. 個体差の本体は `SL/TP` と validation 指標
3. `RECRUIT-RND-1768781166-12` だけが別ロジック

という整理で扱うのが最も実務的です。
