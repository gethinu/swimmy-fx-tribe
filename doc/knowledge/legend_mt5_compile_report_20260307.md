# Legend MT5 Compile Report

日付: 2026-03-07 JST

対象:

- `src/mt5/legend_batch1`
- `src/mt5/legend_batch2`
- `src/mt5/legend_batch3`
- `src/mt5/historical_s_batch1`
- `src/mt5/historical_s_batch2`
- `src/mt5/historical_s_batch3`

## 実行コマンド

WSL から既存の汎用 wrapper を使って一括コンパイルした。

```bash
scripts/compile_swimmybridge_mt5.sh --src <ea.mq5>
```

使用 wrapper:

- `scripts/compile_swimmybridge_mt5.sh`

前提:

- `powershell.exe` 利用可
- `/mnt/c/Program Files/MetaTrader 5/MetaEditor64.exe` 利用可
- `C:\\Users\\stair\\AppData\\Roaming\\MetaQuotes\\Terminal\\D0E8209F77C8CF37AD8BF550E51FF075\\MQL5\\Experts` 利用可

## 結果

- compile 成功: `22 / 22`
- compile 失敗: `0`

### compiled files

`historical_s_batch1`

- `HistS_Bred222TrendCore.mq5`
- `HistS_Bred723TrendCore.mq5`
- `HistS_RecruitRndTrendCross.mq5`

`historical_s_batch2`

- `HistS_Bred128TrendCore.mq5`
- `HistS_Bred508TrendCore.mq5`
- `HistS_Bred794TrendCore.mq5`

`historical_s_batch3`

- `HistS_Bred139TrendCore_3979972567.mq5`
- `HistS_Bred139TrendCore_3979972610.mq5`
- `HistS_Bred187TrendCore.mq5`
- `HistS_Bred436TrendCore.mq5`
- `HistS_Bred458TrendCore.mq5`
- `HistS_Bred586TrendCore.mq5`
- `HistS_Bred940TrendCore.mq5`

`legend_batch1`

- `Legend_PerfectOrderSMA.mq5`
- `Legend_PullbackBreakout.mq5`
- `Legend_SimpleMomentumSync.mq5`

`legend_batch2`

- `Legend_MACDAboveZeroCross.mq5`
- `Legend_SweetChariotSMA40.mq5`
- `Legend_TrendPullbackEntry.mq5`

`legend_batch3`

- `Legend_LondonBreakoutV1.mq5`
- `Legend_MACDSignalCross.mq5`
- `Legend_RSIReversionV1.mq5`

## ここで完了したこと

- offline 側の MQ5 artifact 生成準備は完了
- source file から MetaEditor compile までの疎通は確認済み
- batch1-3 / historical_s_batch1-3 の EA は、少なくとも MetaEditor compile では全件 clean pass

## まだ残るもの

compile までは完了し、その後 `tools/mt5_inventory_tester.py` により Strategy Tester 実測の自動経路も追加した。

### 自動 runner

- tool: `tools/mt5_inventory_tester.py`
- spec: `doc/knowledge/legend_mt5_tester_automation_20260307.md`
- 方式: live terminal を触らず、portable root の isolated terminal で tester を回す

`Legend_PerfectOrderSMA` の isolated probe では、portable terminal から HTML / PNG report が取得できることを確認した。

### tester 優先順

1. `Legend_PerfectOrderSMA.mq5`
2. `Legend_SimpleMomentumSync.mq5`
3. `Legend_PullbackBreakout.mq5`
4. `Legend_LondonBreakoutV1.mq5`
5. `Legend_RSIReversionV1.mq5`
6. `HistS_Bred222TrendCore.mq5`

### tester 最低チェック

- EA が新規注文と決済を発生させること
- docs に書いた時間足 / entry / exit と挙動が一致すること
- `Legend-London-Breakout-V1` は時刻ロジックが `08:00-09:00` / `16:00` で期待どおりに動くこと
- `Legend-RSI-Reversion-V1` は `RSI(2)` の 10/90 閾値と short 対称実装を確認すること
- historical S 群は `timeframe=3600` の再現条件を明示して backtest すること

## freeze 時点の扱い

- Swimmy 本体 runtime は停止済みでも、MQ5 compile は独立に完了できた。
- さらに、Strategy Tester 実測は手動前提ではなくなり、isolated portable runner で自動実行できる。
- freeze 後に残る実機依存論点は、`manual 操作` ではなく `local tester 資源待ち` のみ。
