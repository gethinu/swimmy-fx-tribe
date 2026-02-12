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
