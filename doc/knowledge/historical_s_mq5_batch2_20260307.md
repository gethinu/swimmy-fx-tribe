# Historical S MQ5 Batch 2

日付: 2026-03-07 JST

このバッチは、過去S候補の bred 系から、同じシグナルコアを共有する 3 個体をまとめて整理する第2弾です。

対象:

1. `Bred-Bred--508-Gen32-N3980040829-808`
2. `Bred-Bred--794-Gen32-N3980040593-767`
3. `Bred-Bred--128-Gen28-N3980038170-239`

## 同一シグナルコア

この 3 本は、保存上の `indicators` は違っていても、`data_sexp` の実行式としては同一シグナルコアです。

- ロング:
  - `Close > SMA20 > SMA50`
  - `RSI > 55`
- ショート:
  - `Close < SMA20 < SMA50`
  - `RSI < 45`
- イグジット:
  - `PNL > TP` または `PNL < -SL`

つまり、3 本は「別戦略」というより、

- 同じ bred trend core
- 異なる `SL/TP`
- 異なる保存済み `indicators` の残骸

という関係です。

## 実行正本

このバッチでも `data_sexp` を実行正本として扱います。

理由:

- `indicators` 列の SMA / PSAR 群は、実際の `entry` 式に使われていません
- 保存系と実行系がずれているので、MQ5 は `entry` / `exit` 式を優先して再構成します

## 時間足の扱い

- 3 本とも `timeframe=3600`
- これは MT5 の標準足ではありません
- MQ5 版は `PERIOD_CURRENT` を使うため、厳密再現したい場合は custom timeframe 前提です

## 1. Bred-Bred--508-Gen32-N3980040829-808

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
  - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット: `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 0.7934`
  - `TP = 0.7634`
- 主要メトリクス:
  - Sharpe `0.390`
  - PF `9.769`
  - WR `88.6%`
  - Trades `35`
  - OOS Sharpe `18.142`
  - CPCV pass_rate `0.378`
- 市場アイデア:
  - 同一コアの中では最も PF が高く、短期の強い順張りに寄った execution です。
- 注記:
  - `indicators` は `SMA 101 / 98 / 89 / 29` ですが、これはエントリー式の `SMA20 / SMA50 / RSI` と一致しません。

### MQ5版

- ファイル: `src/mt5/historical_s_batch2/HistS_Bred508TrendCore.mq5`
- 実装メモ:
  - batch-1 bred テンプレートを踏襲
  - 個体固有なのは `SL/TP` と識別子だけ

## 2. Bred-Bred--794-Gen32-N3980040593-767

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
  - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット: `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 0.5577`
  - `TP = 0.6757`
- 主要メトリクス:
  - Sharpe `16.089`
  - PF `8.010`
  - WR `85.7%`
  - Trades `35`
  - OOS Sharpe `16.089`
  - CPCV pass_rate `0.422`
- 市場アイデア:
  - シグナルコア自体は 508番と同じですが、リスク幅がややタイトで、validation 指標は少しだけ良いです。
- 注記:
  - `indicators` に `PSAR 0.02 0.2` が残っていますが、`entry` 式では PSAR を参照していません。MQ5 では使用しません。

### MQ5版

- ファイル: `src/mt5/historical_s_batch2/HistS_Bred794TrendCore.mq5`
- 実装メモ:
  - PSAR は保存残骸として扱い、実装対象外
  - シグナル判定は 508番と同一

## 3. Bred-Bred--128-Gen28-N3980038170-239

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
  - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット: `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 1.3997`
  - `TP = 0.4927`
- 主要メトリクス:
  - Sharpe `13.421`
  - PF `5.904`
  - WR `88.2%`
  - Trades `34`
  - OOS Sharpe `13.421`
  - CPCV pass_rate `0.333`
- 市場アイデア:
  - シグナルコアは同じでも、かなり深めの許容損失を持つ耐久型です。3 本の中では最も stop が大きく、target が浅い非対称設計です。
- 注記:
  - `indicators` は `SMA 110 / 28 / 92 / 28` を持っていますが、実行正本はやはり `entry` 式です。

### MQ5版

- ファイル: `src/mt5/historical_s_batch2/HistS_Bred128TrendCore.mq5`
- 実装メモ:
  - 同じシグナルコアに対して、risk envelope だけがかなり異なる個体

## 見方

この batch-2 で重要なのは、過去Sの bred 系には

- シグナルコアとしては同一
- ただし保存済み補助指標はばらつく
- 実質的な違いは `SL/TP` と validation 成績

というクラスターが存在することです。

次は、

1. `同一ロジック重複の統合表`
2. `Bred-Bred--139...` の duplicate pair 扱い
3. `残り候補の別クラスター抽出`

に進むのが自然です。
