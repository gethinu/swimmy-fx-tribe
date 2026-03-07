# Legend MQ5 Batch 4 Drop Memo

日付: 2026-03-07 JST

この文書は drop 記録です。batch-4 の 3 本は一度 MQ5 化と smoke 実測まで行いましたが、PF が弱く active candidate から外し、その後 source / tester artifact を repo と MT5 環境の両方から削除しました。

- `MACD-Zero-Cross-Long`
- `MACD-Expansion`
- `Crossover-Plus-MACD`

したがって、`tools/mt5_inventory_tester.py` の active `legend` job set には含めていません。

## 正本

- ロジック正本:
  - `data/library/LEGEND/*.lisp`
  - `data/memory/swimmy.db` の `strategies`
- パラメータ参照:
  - `src/lisp/school/school-optimized-params.lisp`

## 1. MACD-Zero-Cross-Long

### 自然言語版

- シンボル: `USDJPY`
- 時間足: `H4`
- 指標: `MACD(12,26,9)`
- ロングエントリー:
  - MACD 本線がゼロラインを上抜いたら買う
  - 厳密条件: `MACD-LINE` が `0` を上抜く
- ロングイグジット:
  - `MACD-LINE` がゼロラインを下抜いたら手仕舞う
- ショート側の対称推論:
  - `MACD-LINE` がゼロラインを下抜いたら売る
  - `MACD-LINE` がゼロラインを上抜いたら手仕舞う
- リスク設定:
  - `SL = 0.30`
  - `TP = 0.60`
- 現在メトリクス:
  - Sharpe `0.149`
  - PF `1.088`
  - Trades `2351`
  - MaxDD `0.00152`
- 市場アイデア:
  - signal 線クロスより大きいモメンタム転換だけを使う、粗めの MACD トレンド判定です。

### MQ5版

- ファイル: drop 済み
- 実装メモ:
  - デフォルトは `H4`
  - ゼロラインの closed-bar cross を使う
  - name は `Long` ですが、EA 化ではショートを対称推論で補う

## 2. MACD-Expansion

### 自然言語版

- シンボル: `USDJPY`
- 時間足: `H4`
- 指標: `MACD(12,26,9)`
- ロングエントリー:
  - MACD 本線がゼロラインより上にあり、なおかつ signal 線より上にあれば買う
  - 厳密条件: `MACD-LINE > 0` かつ `MACD-LINE > SIGNAL-LINE`
- ロングイグジット:
  - `MACD-LINE` が `SIGNAL-LINE` を下抜いたら手仕舞う
- ショート側の対称推論:
  - `MACD-LINE < 0` かつ `MACD-LINE < SIGNAL-LINE` で売る
  - `MACD-LINE` が `SIGNAL-LINE` を上抜いたら手仕舞う
- リスク設定:
  - `SL = 0.30`
  - `TP = 0.70`
- 現在メトリクス:
  - Sharpe `0.141`
  - PF `1.085`
  - Trades `2350`
  - MaxDD `0.00184`
- 市場アイデア:
  - クロスそのものより、MACD が優位状態に滞在している区間を取りにいく continuation 系です。

### MQ5版

- ファイル: drop 済み
- 実装メモ:
  - デフォルトは `H4`
  - entry は cross ではなく状態条件
  - exit は signal 線との逆クロス

## 3. Crossover-Plus-MACD

### 自然言語版

- シンボル: `USDJPY`
- 時間足: `D1`
- 指標: `EMA(10)`, `EMA(20)`, `MACD(12,26,9)`
- ロングエントリー:
  - EMA10 が EMA20 を上抜き、同時に MACD 本線が signal 線より上なら買う
  - 厳密条件: `EMA10` が `EMA20` を上抜き、`MACD-LINE > SIGNAL-LINE`
- ロングイグジット:
  - `EMA10` が `EMA20` を下抜いたら手仕舞う
- ショート側の対称推論:
  - `EMA10` が `EMA20` を下抜き、`MACD-LINE < SIGNAL-LINE` で売る
  - `EMA10` が `EMA20` を上抜いたら手仕舞う
- リスク設定:
  - `SL = 0.25`
  - `TP = 0.50`
- 現在メトリクス:
  - Sharpe `0.148`
  - PF `1.150`
  - Trades `957`
  - MaxDD `0.00142`
- 市場アイデア:
  - 価格系クロスだけでなく、MACD で勢いを追認してダマシを減らす複合トレンド型です。

### MQ5版

- ファイル: drop 済み
- 実装メモ:
  - デフォルトは `D1`
  - EMA10 / EMA20 の closed-bar cross を使う
  - MACD は entry filter のみで、exit 条件には使わない

## smoke 結果

`tools/mt5_inventory_tester.py` を使い、`2025.01.01` から `2025.03.01` の isolated smoke を実施した。

| Job | Net | PF | Trades |
| --- | ---: | ---: | ---: |
| `legend-macd-zero-cross-long` | `-8.57` | `0.58` | `13` |
| `legend-macd-expansion` | `-1.23` | `0.99` | `74` |
| `legend-crossover-plus-macd` | `-1.66` | `0.00` | `1` |

短期 smoke では 3 本とも強い勝ち残りには見えない。`MACD-Expansion` が最も観測量を確保できたが、PF はまだ `1.0` を割っている。

## 次の論点

1. top-10 named Legend を同一 window で比較し、安定性順の再ランキングを作る。
2. batch-4 の 3 本は dropped と見なし、明確な再実装理由が出るまで復活させない。
3. named Legend の次を切るなら、二軍候補を trade count と安定性で再選定する。
