# XAU AutoBot Quickstart (MT5, Separate System)

このボットは既存 Swimmy 本体とは独立した軽量プロトタイプです。  
`XAUUSD` を対象に、`EMAトレンド + ATRプルバック` でエントリーします。

## 1. テスト

```bash
python3 -m unittest tools/tests/test_xau_autobot.py -v
```

## 2. 設定ファイル作成

```bash
cp tools/configs/xau_autobot.example.json tools/configs/xau_autobot.local.json
```

必要に応じて:
- `symbol` をブローカー名に合わせる（例: `XAUUSD.a`）
- `max_spread_points` を広げる/狭める
- `lot` を調整
- `session_start_hour_utc` / `session_end_hour_utc` で時間帯制御
- `min_atr_ratio_to_median` / `max_atr_ratio_to_median` でボラティリティ帯制御

チューニング済みサンプル:

```bash
cp tools/configs/xau_autobot.tuned_gc_m5.json tools/configs/xau_autobot.local.json
```

この tuned サンプルは `EMA24/EMA140` + `ATR pullback 0.2` + 時間帯/ボラ帯フィルタを使います。

## 3. Dry-run 実行（注文送信なし）

```bash
python3 tools/xau_autobot.py --config tools/configs/xau_autobot.local.json
```

出力は JSON 1 行で、`HOLD` / `BLOCKED` / `ORDER` を返します。  
`dry_run: true` の場合、`ORDER` でも実注文は出ません。

## 4. ループ監視（新バー単位）

```bash
python3 tools/xau_autobot.py \
  --config tools/configs/xau_autobot.local.json \
  --loop \
  --poll-seconds 10
```

## 5. ライブ実行（実注文あり）

```bash
python3 tools/xau_autobot.py \
  --config tools/configs/xau_autobot.local.json \
  --live \
  --loop
```

## 前提

- MT5端末が起動済みでログイン済み
- Pythonパッケージ `MetaTrader5` が導入済み

```bash
pip install MetaTrader5
```

## 最低限の安全設定

- 既定は `dry_run: true`
- `max_positions: 1`
- `max_spread_points` で高コスト時間帯を回避
- `sl_atr` / `tp_atr` で固定損益幅を保持

## 6. バックテスト比較（base vs tuned）

`GC=F`（金先物5分足）を使って、base/tuned を同条件比較します。

```bash
./.venv/bin/python tools/xau_autobot_backtest.py --mode both
```

出力は JSON Lines で、`segment=all` と `split_*`（IS/OOS）を返します。

## 7. 自動最適化（tuned候補の再探索）

`GC=F` 5分足で候補探索し、最良候補を設定ファイルに出力します。

```bash
./.venv/bin/python tools/xau_autobot_optimize.py \
  --top-k 5 \
  --write-config tools/configs/xau_autobot.tuned_auto_gc_m5.json
```

出力された `tools/configs/xau_autobot.tuned_auto_gc_m5.json` をそのまま dry-run に使えます。

## 8. 運用可否判定（Readiness）

想定コスト（片道）を与えて、損益分岐コストと GO/CAUTION/NO_GO を出します。

```bash
./.venv/bin/python tools/xau_autobot_readiness.py \
  --config tools/configs/xau_autobot.tuned_auto_gc_m5.json \
  --assumed-cost-side 0.0002 \
  --write-report data/reports/xau_autobot_readiness.json
```

`break_even_roundtrip_cost` が実口座の実効往復コストより高いほど有利です。

## 9. コストガード（実スプレッド入力判定）

Readiness結果に対して、現在スプレッド/手数料/スリッページで GO 判定を出します。

```bash
./.venv/bin/python tools/xau_autobot_cost_guard.py \
  --readiness-report data/reports/xau_autobot_readiness.json \
  --spread-points 25 \
  --spread-grid 10,25,50,75,100,125,140,160,180,200,220,240 \
  --commission-roundtrip-pct 0.02 \
  --slippage-roundtrip-pct 0.01 \
  --write-report data/reports/xau_autobot_cost_guard.json
```

注:
- `commission_roundtrip_pct` / `slippage_roundtrip_pct` は `%` 単位です（例: `0.02` = 0.02%）
- `max_spread_points_safe` 以下なら、設定した safety margin 内で運用可能です
- `max_spread_points_go` 以下なら、GO 判定域（コスト余裕あり）です

## 10. Windows + MT5 実測プローブ

MT5 接続できる環境では、スプレッドを複数サンプル取得して自動判定できます。

```bash
python tools/xau_autobot_windows_probe.py \
  --readiness-report data/reports/xau_autobot_readiness.json \
  --symbol XAUUSD \
  --samples 120 \
  --interval-ms 500 \
  --commission-roundtrip-pct 0.02 \
  --slippage-roundtrip-pct 0.01 \
  --write-report data/reports/xau_autobot_windows_probe.json
```

Linux 等で MT5 がない場合は `--spread-points --price --point` を指定して同じ形式で判定できます。

## 11. 1コマンド実行（Linux）

90日・5分足でも自動で分割ダウンロードして実行できます（Yahooの60日制限を回避）。

```bash
./.venv/bin/python tools/xau_autobot_cycle.py \
  --python-exe ./.venv/bin/python \
  --period 90d \
  --interval 5m \
  --spread-points 80 \
  --commission-roundtrip-pct 0.02 \
  --slippage-roundtrip-pct 0.01 \
  --write-config tools/configs/xau_autobot.tuned_auto_gc_m5_90d.json \
  --write-summary data/reports/xau_autobot_cycle_summary_90d.json
```

生成物:
- `data/reports/xau_autobot_backtest_*_90d.jsonl`
- `data/reports/xau_autobot_optimize_*_90d.jsonl`
- `data/reports/xau_autobot_readiness_90d.json`
- `data/reports/xau_autobot_cost_guard_90d.json`
- `data/reports/xau_autobot_cycle_summary_90d.json`

## 12. 1コマンド実行（Windows）

PowerShell:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File tools\windows\xau_autobot_run_all.ps1
```

Batch:

```bat
tools\windows\xau_autobot_run_all.bat
```

補足:
- 既定で `xau_autobot_cycle.py` 実行後に `xau_autobot_windows_probe.py` まで実行します
- MT5プローブを飛ばす場合は `-SkipProbe` を付けます

## 13. 定期実行（cron / systemd）

### cron（15分ごと）

```bash
*/15 * * * * cd /home/swimmy/swimmy && ./.venv/bin/python tools/xau_autobot_cycle.py --python-exe ./.venv/bin/python --period 90d --interval 5m --write-config tools/configs/xau_autobot.tuned_auto_gc_m5_90d.json --write-summary data/reports/xau_autobot_cycle_summary_90d.json >> data/reports/xau_autobot_cycle_cron.log 2>&1
```

### systemd（例）

同梱済み:
- `systemd/xau-autobot-cycle.service`
- `systemd/xau-autobot-cycle.timer`

導入:

```bash
sudo cp systemd/xau-autobot-cycle.service /etc/systemd/system/
sudo cp systemd/xau-autobot-cycle.timer /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now xau-autobot-cycle.timer
sudo systemctl status xau-autobot-cycle.timer --no-pager
```

通知:
- `tools/xau_autobot_cycle_runner.sh` が `.env` を読み込み、`SWIMMY_DISCORD_REPORTS` があれば `xau_autobot_cycle.py --discord-webhook` を自動付与します。
- `SWIMMY_DISCORD_REPORTS` が失敗した場合は `SWIMMY_DISCORD_SYSTEM_LOGS` → `SWIMMY_DISCORD_ALERTS` → `SWIMMY_DISCORD_APEX` の順でフォールバックします。
- systemd定期実行は `--market-hours-only` を有効化しており、市場クローズ時は `SKIP` で正常終了します。
