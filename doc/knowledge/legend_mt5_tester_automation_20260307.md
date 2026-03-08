# Legend MT5 Tester Automation

日付: 2026-03-07 JST

## 目的

`Legend` / `historical S` の MQ5 移植物に対して、live で使っている MT5 terminal を触らずに Strategy Tester 実測を自動化する。

## なぜ既存 terminal を使わないか

- 既存の `terminal64.exe` は live / 手動 tester と同居しており、同一 data dir に対する `/config` 起動は安全ではない。
- 既存 script もその前提で、`terminal64` / `metatester64` が 0 件になるまで launch を待つ実装になっている。
- よって、自動実行は `portable` な隔離 terminal を別 root で立ち上げる必要がある。

## 採用した方式

- tool: `tools/mt5_inventory_tester.py`
- portable root:
  - 既定: `C:\Users\stair\AppData\Local\SwimmyMT5Portable\inventory_tester`
- repo output:
  - 既定: `data/reports/mt5/inventory_tester/<run-id>/`

runner は次を自動で行う。

1. `C:\Program Files\MetaTrader 5` から `terminal64.exe` / `metatester64.exe` / `MetaEditor64.exe` / `Terminal.ico` を portable root に配置
2. 現行 terminal data dir から `config/*` を portable root に同期
3. 実行対象 `.ex5` を portable root `MQL5/Experts` に同期
4. job ごとの tester `.ini` を生成
5. `terminal64.exe /portable /config:<ini>` を PowerShell 経由で起動
6. HTML / PNG report を repo 側へ copy
7. UTF-16 HTML を読み、summary JSON を出力

## Job Set

- `legend`
  - 9 本
- `historical_s`
  - 13 本
- `all`
  - 22 本

`historical S` の `timeframe=3600` 個体は、移植コードが `PERIOD_CURRENT` を使っている。自動 runner では tester `Period=H1` を与えて再現する。この契約は `run_20260307_064816` の `historical_s` 13 件で実測済み。

## 使い方

job 一覧:

```bash
python3 tools/mt5_inventory_tester.py --job-set all --list-jobs
```

Legend 1 本の isolated smoke:

```bash
python3 tools/mt5_inventory_tester.py \
  --job-set legend \
  --job legend-perfect-order-sma \
  --from-date 2025.01.01 \
  --to-date 2025.01.15
```

22 本まとめて実行:

```bash
python3 tools/mt5_inventory_tester.py \
  --job-set all \
  --from-date 2025.01.01 \
  --to-date 2025.03.01
```

注記:

- `src/mt5/archive/legend_batch4/` の 3 本は smoke 後に archive 扱いへ落としたため、active `legend` job set には含めていない。

## 実機プローブ結果

2026-03-07 JST に portable probe を実施し、次を確認した。

- live の `C:\Program Files\MetaTrader 5\terminal64.exe` と別に、
  `C:\Users\stair\AppData\Local\SwimmyMT5Portable\probe_20260307_152928\terminal64.exe` が並行起動できる
- `config` と対象 `.ex5` だけを同期すれば backtest を開始できる
- `USDJPY` の M1 history は portable root 側で自動取得される
- report `.htm` / `.png` が portable root に出力される

同 probe の `Legend_PerfectOrderSMA` では、以下の summary を抽出できた。

- `initial_deposit = 10 000.00`
- `total_net_profit = 0.70`
- `profit_factor = 1.03`
- `total_trades = 20`

## 制約

- local `metatester64` が他の run で塞がっている間は、portable terminal 側の test 開始が待たされることがある。
- compile 不足の `.ex5` は自動生成しない。事前に `scripts/compile_swimmybridge_mt5.sh --src <ea.mq5>` で compile しておく。
- 現在は sequential 実行のみ。portfolio 並列化や multi-agent 分散は入れていない。
