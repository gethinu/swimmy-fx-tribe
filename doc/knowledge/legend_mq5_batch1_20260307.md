# Legend MQ5 Batch 1

日付: 2026-03-07 JST

現行 LEGEND 在庫から、段階的に

1. 運用者向けの自然言語版
2. 単体で動かせる MQ5 EA 版

を切り出していく第1弾です。

Batch-1 では、現在の DB `data_sexp` と整合している、名前付きの分かりやすい LEGEND 3本だけを対象にしています。

同日中に後続バッチとして切り出し済み:

- `doc/knowledge/legend_mq5_batch2_20260307.md`
- `doc/knowledge/legend_mq5_batch3_20260307.md`
- `doc/knowledge/historical_s_mq5_batch1_20260307.md`
- `doc/knowledge/historical_s_mq5_batch2_20260307.md`
- `doc/knowledge/historical_s_mq5_batch3_20260307.md`

このため、この文書は「最初の 3 本をどう切り出したか」の記録として残し、以後の残タスクは compile / tester / source audit に寄せます。

関連する完了/監査メモ:

- `doc/knowledge/legend_mt5_compile_report_20260307.md`
- `doc/knowledge/legend_external_source_audit_20260307.md`
- `doc/knowledge/legend_mq5_batch4_20260307.md`

除外:

- `TEST-REFRESH-ACTIVE-SEXP-SYNC`
  - DB 上は active ですが、テスト・同期確認用の匂いが強く、裁量や自動売買のエッジとして扱うのは不適切

## 正本

- ロジック正本: `strategies.data_sexp`
- LEGEND 正本ガード: `src/lisp/strategies/legend-61.lisp`
- メトリクス参照: `data/memory/swimmy.db` の `strategies`

## 翻訳ポリシー

- MQ5 実装は確定足ベースで判定する
- `SL` / `TP` は strategy 定義の値をそのまま価格距離として持ち込む
- Lisp 側がロング方向だけを明示している場合、MQ5 のショート側は対称ロジックとして補完する
- このショート側は「原作者の意図の確定」ではなく、「EA 化のための実装上の推論」として扱う

## 1. Perfect-Order-SMA

### 自然言語版

- シンボル: `USDJPY`
- 時間足: `M30`
- 指標: `SMA(20)`, `SMA(50)`, `SMA(100)`
- ロングエントリー:
  - 短期・中期・長期の移動平均が強気順に綺麗に並んだら買う
  - 厳密条件: `SMA20 > SMA50 > SMA100`
- ロングイグジット:
  - `SMA20` が `SMA50` を下抜いたら手仕舞う
- ショート側の対称推論:
  - `SMA20 < SMA50 < SMA100` で売る
  - `SMA20` が `SMA50` を上抜いたら手仕舞う
- リスク設定:
  - `SL = 0.30`
  - `TP = 0.90`
- 現在メトリクス:
  - Sharpe `0.333`
  - PF `1.150`
  - Trades `5415`
  - MaxDD `0.00208`
- 市場アイデア:
  - これはトレンド整列型です。クロス発生そのものを待つのではなく、短中長のトレンド層が既に同方向に並んでいる局面を取りにいきます。

### MQ5版

- ファイル: `src/mt5/legend_batch1/Legend_PerfectOrderSMA.mq5`
- 実装メモ:
  - デフォルトは `M30`
  - ロング / ショート両対応
  - `magic` 単位で 1ポジション管理
  - SL / TP は broker 側に置く

## 2. Simple-Momentum-Sync

### 自然言語版

- シンボル: `USDJPY`
- 時間足: `M30`
- 指標: `EMA(50)`, `RSI(14)`
- ロングエントリー:
  - 価格が中期トレンドの上にあり、なおかつモメンタムが十分強いときに買う
  - 厳密条件: `Close > EMA50` かつ `RSI14 > 55`
- ロングイグジット:
  - モメンタムが崩れたら手仕舞う
  - 厳密条件: `RSI14 < 45`
- ショート側の対称推論:
  - `Close < EMA50` かつ `RSI14 < 45` で売る
  - `RSI14 > 55` で手仕舞う
- リスク設定:
  - `SL = 0.30`
  - `TP = 0.60`
- 現在メトリクス:
  - Sharpe `0.265`
  - PF `1.106`
  - Trades `5864`
  - MaxDD `0.00151`
- 市場アイデア:
  - EMA でトレンド方向を限定し、RSI で勢いがある場面だけを通す、単純なトレンド＋モメンタム同期型です。

### MQ5版

- ファイル: `src/mt5/legend_batch1/Legend_SimpleMomentumSync.mq5`
- 実装メモ:
  - デフォルトは `M30`
  - ロング / ショートは対称実装
  - イグジットはクロスではなく閾値判定

## 3. Pullback-Breakout

### 自然言語版

- シンボル: `USDJPY`
- 時間足: `H1`
- 指標: `EMA(20)`, `RSI(14)`
- ロングエントリー:
  - 押し目後に価格が EMA20 を奪い返し、勢いもプラスなら買う
  - 厳密条件: `Close` が `EMA20` を上抜き、かつ `RSI14 > 50`
- ロングイグジット:
  - 価格が再び EMA20 を下抜いたら手仕舞う
- ショート側の対称推論:
  - `Close` が `EMA20` を下抜き、かつ `RSI14 < 50` で売る
  - `Close` が `EMA20` を上抜いたら手仕舞う
- リスク設定:
  - `SL = 0.30`
  - `TP = 0.60`
- 現在メトリクス:
  - Sharpe `0.230`
  - PF `1.084`
  - Trades `7374`
  - MaxDD `0.00279`
- 市場アイデア:
  - これは押し目・戻りからの再加速を取る型です。EMA を回復しただけでは入らず、RSI の最小限の追認を要求して、弱い戻しを避けます。

### MQ5版

- ファイル: `src/mt5/legend_batch1/Legend_PullbackBreakout.mq5`
- 実装メモ:
  - デフォルトは `H1`
  - クロス判定は直近 2 本の確定足を使う
  - ショート側は対称推論

## freeze 後の残タスク

すでに batch-2 / batch-3 / historical S batch-1..3 まで揃っており、compile と external Legend 監査も完了したので、残件は次です。なお、batch-4 は smoke 後に dropped 扱いとし、source も削除済みです。

1. `tools/mt5_inventory_tester.py` で isolated Strategy Tester 実測を回す。
2. active shortlist を中心に比較を進め、archive へ落とした batch-4 は必要時だけ参照する。
