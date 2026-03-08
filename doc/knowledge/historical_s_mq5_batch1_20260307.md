# Historical S MQ5 Batch 1

日付: 2026-03-07 JST

このバッチは、過去に `:S` だった候補の中から、まず 3 本だけを正本抽出して日本語化し、MQ5 版まで切り出す第1弾です。

対象:

1. `Bred-Bred--222-Gen30-N3980040329-718`
2. `Bred-Bred--723-Gen29-N3980038311-278`
3. `RECRUIT-RND-1768781166-12`

## 実行正本の扱い

このバッチでは、`strategies` テーブルの通常カラムよりも `data_sexp` を実行正本として扱います。

理由:

- bred 系の 2 本は、保存されている `indicators` と、実際の `entry` 式が一致していません
- たとえば `indicators` は `SMA 89 / 94 / 29` のような値を持つのに、`entry` 式は `SMA-20 / SMA-50 / RSI` を参照しています
- よって MQ5 化では `indicators` 列をそのまま信じず、`entry` / `exit` 式の意味を優先して実装します

## 時間足の扱い

- `Bred-Bred--222...` と `Bred-Bred--723...` は `timeframe=3600`
- これは MT5 の標準時間足ではありません
- MQ5 版は `PERIOD_CURRENT` を使うため、厳密再現したい場合は custom timeframe チャートにアタッチする前提です

## 1. Bred-Bred--222-Gen30-N3980040329-718

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - エントリー:
    - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
    - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット:
    - `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 0.6525`
  - `TP = 0.7479`
- 主要メトリクス:
  - Sharpe `17.113`
  - PF `8.926`
  - WR `85.7%`
  - Trades `35`
  - OOS Sharpe `17.113`
  - CPCV pass_rate `0.378`
- 市場アイデア:
  - bred 系の順張りトレンドフィルタです。価格と短中期平均が同方向に整列し、RSI で勢いを確認したときだけ入ります。
- 注記:
  - `indicators` 列は `SMA 89 / 94 / 29` を持っていますが、エントリー式とは整合しません。よって MQ5 では `entry` 式を採用します。

### MQ5版

- ファイル: `src/mt5/historical_s_batch1/HistS_Bred222TrendCore.mq5`
- 実装メモ:
  - custom timeframe 前提
  - エントリーは `SMA20 / SMA50 / RSI14`
  - イグジットは broker-side SL / TP

## 2. Bred-Bred--723-Gen29-N3980038311-278

### 自然言語版

- シンボル: `USDJPY`
- 元時間足: `3600-minute custom`
- 実行正本:
  - エントリー:
    - ロング: `Close > SMA20 > SMA50` かつ `RSI > 55`
    - ショート: `Close < SMA20 < SMA50` かつ `RSI < 45`
  - イグジット:
    - `PNL > TP` または `PNL < -SL`
- リスク設定:
  - `SL = 0.7619`
  - `TP = 0.8436`
- 主要メトリクス:
  - Sharpe `13.475`
  - PF `5.329`
  - WR `80.0%`
  - Trades `35`
  - OOS Sharpe `13.475`
- 市場アイデア:
  - 222番と同じ bred トレンドコアですが、利確損切り幅が大きく、やや耐える型です。
- 注記:
  - `indicators` 列は `SMA 90 / 30 / 97` ですが、これもエントリー式と整合しません。こちらも `entry` 式を優先して再構成します。

### MQ5版

- ファイル: `src/mt5/historical_s_batch1/HistS_Bred723TrendCore.mq5`
- 実装メモ:
  - custom timeframe 前提
  - ロジックは 222番と同系
  - リスク幅だけ個体固有値を採用

## 3. RECRUIT-RND-1768781166-12

### 自然言語版

- シンボル: `USDJPY`
- 時間足: `D1`
- 実行正本:
  - エントリー:
    - `CROSS SMA 5 20`
  - イグジット:
    - `CROSS SMA 13 120`
- リスク設定:
  - `SL = 0.034`
  - `TP = 0.043`
- 主要メトリクス:
  - Sharpe `-0.080`
  - PF `0.986`
  - WR `46.4%`
  - Trades `317`
- 市場アイデア:
  - bred 系よりずっと素直な移動平均クロスです。エントリーは短期と中期のクロス、イグジットは別の長めのクロスで切り替える構造です。
- 注記:
  - `ENTRY` / `EXIT` が文字列表現なので、MQ5 では次の推論で実装します
  - エントリー:
    - `SMA5` が `SMA20` を上抜いたら買い
    - `SMA5` が `SMA20` を下抜いたら売り
  - イグジット:
    - `SMA13` と `SMA120` がどちら向きでもクロスしたら手仕舞い

### MQ5版

- ファイル: `src/mt5/historical_s_batch1/HistS_RecruitRndTrendCross.mq5`
- 実装メモ:
  - デフォルトは `D1`
  - entry / exit とも closed-bar cross
  - exit 方向は文字列が曖昧なので「任意のクロス」で解釈

## 次の見方

この batch-1 で分かるのは次の 2 点です。

1. 過去S候補の bred 系は、保存上の `indicators` と実行式がずれている可能性が高い
2. そのため、機械移植の前に `data_sexp` を正本として読む工程が必須

次は、残りの過去S候補から

- bred 系の別クラスター
- 同一ロジックの重複個体
- 本当に別戦略と呼べるもの

を分けていくのが自然です。
