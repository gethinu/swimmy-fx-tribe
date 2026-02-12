# OpenClaw + Polymarket Contrarian Bot (Dry-Run)

## 目的
- OpenClawの確率シグナルとPolymarket市場価格の乖離を抽出する。
- スポーツ市場では人気側過熱を想定し、`contrarian fade` で逆張り候補を優先する。
- 実行は **dry-run**（注文プラン生成）に限定し、まず期待値を検証する。

## 1. サンプル設定
- `tools/configs/polymarket_openclaw.contrarian.example.json`

主要パラメータ:
- `max_daily_loss_pct`: 1日の最大損失上限（資金比率）
- `max_trade_risk_pct`: 1トレード上限（資金比率）
- `max_open_positions`: 未決着ポジション上限
- `max_daily_entries`: 当日新規エントリー上限
- `max_daily_loss_streak`: 当日連敗上限（決着ベース）
- `max_daily_realized_loss_usd`: 当日実現損失上限（USD）
- `fee_bps_per_side` / `slippage_bps_per_side`: コスト控除
- `min_price_sum` / `max_price_sum`: `yes+no`価格帯フィルタ
- `min_market_price` / `max_market_price`: 極端価格の市場除外
- `enable_contrarian_fade`: 人気側逆張り
- `contrarian_favorite_threshold`: 人気側判定価格
- `question_keywords`: 市場質問フィルタ（例: リーグ名、チーム名）

## 2. 実行例（signals file）
```bash
python3 tools/polymarket_openclaw_bot.py \
  --config-file tools/configs/polymarket_openclaw.contrarian.example.json \
  --signals-file data/openclaw/signals.jsonl \
  --limit 250 \
  --journal-file data/reports/polymarket_openclaw_journal.jsonl \
  --write-plan data/reports/polymarket_openclaw_plan.json
```

## 3. 実行例（openclaw command）
```bash
python3 tools/polymarket_openclaw_bot.py \
  --config-file tools/configs/polymarket_openclaw.contrarian.example.json \
  --openclaw-cmd "./bin/openclaw signals --format jsonl" \
  --limit 250 \
  --journal-file data/reports/polymarket_openclaw_journal.jsonl \
  --write-plan data/reports/polymarket_openclaw_plan.json
```

`openclaw`バイナリが無い場合の代替:
```bash
python3 tools/polymarket_openclaw_bot.py \
  --config-file tools/configs/polymarket_openclaw.contrarian.example.json \
  --openclaw-cmd "python3 tools/openclaw_signal_heuristic.py --limit 250 --question-keyword nba --question-keyword nfl" \
  --limit 250 \
  --journal-file data/reports/polymarket_openclaw_journal.jsonl \
  --write-plan data/reports/polymarket_openclaw_plan.json
```

## 4. ワンショット実行（bot + report）
```bash
python3 tools/polymarket_openclaw_cycle.py \
  --config-file tools/configs/polymarket_openclaw.contrarian.example.json \
  --signals-file data/openclaw/signals.jsonl \
  --output-dir data/reports/polymarket_openclaw \
  --run-id cycle_20260212_01 \
  --auto-fetch-settlements \
  --autotune
```

自動採用まで有効化する例（慎重運用推奨）:
```bash
python3 tools/polymarket_openclaw_cycle.py \
  --config-file tools/configs/polymarket_openclaw.contrarian.example.json \
  --signals-file data/openclaw/signals.jsonl \
  --output-dir data/reports/polymarket_openclaw \
  --auto-fetch-settlements \
  --autotune \
  --autotune-apply-best \
  --autotune-apply-min-trades 20 \
  --autotune-apply-min-realized-pnl-usd 1.0
```

生成物:
- `plan_<run_id>.json`（今回の注文プラン）
- `journal.jsonl`（累積ジャーナル）
- `report_<date>_<run_id>.json`（当日サマリー）

## 5. 出力確認
- 標準出力にJSONを表示。
- `summary.entries > 0` かつ `summary.total_stake_usd > 0` なら候補あり。
- `entries[].expected_value_usd` はコスト控除後の期待値。

## 6. 日次サマリー
```bash
python3 tools/polymarket_openclaw_report.py \
  --journal-file data/reports/polymarket_openclaw_journal.jsonl \
  --date 2026-02-12 \
  --write-report data/reports/polymarket_openclaw_daily_2026-02-12.json
```

主な出力:
- `runs`: 当日の実行回数
- `entries`: 候補トレード件数
- `total_stake_usd`: 想定投下額
- `total_expected_value_usd`: 期待値合計（コスト控除後）
- `expected_return_on_stake`: `total_expected_value_usd / total_stake_usd`

### 決着反映（実現PnL）
`settlements.json` 例:
```json
{
  "market_id_1": "YES",
  "market_id_2": "NO"
}
```

```bash
python3 tools/polymarket_openclaw_report.py \
  --journal-file data/reports/polymarket_openclaw_journal.jsonl \
  --date 2026-02-12 \
  --settlements-file data/reports/polymarket_settlements_2026-02-12.json \
  --fee-bps-per-side 20 \
  --slippage-bps-per-side 30 \
  --write-report data/reports/polymarket_openclaw_realized_2026-02-12.json
```

追加される主な指標:
- `realized.resolved_entries`: 決着照合できた件数
- `realized.realized_pnl_usd`: 実現損益（コスト控除後）
- `realized.realized_minus_expected_usd`: 実績と期待値の乖離
- `realized.brier_score`: 予測確率の校正品質（小さいほど良い）

### 決着自動取得（Gamma API）
```bash
python3 tools/polymarket_fetch_settlements.py \
  --journal-file data/reports/polymarket_openclaw_journal.jsonl \
  --date 2026-02-12 \
  --existing-settlements data/reports/polymarket_settlements_2026-02-12.json \
  --write-settlements data/reports/polymarket_settlements_2026-02-12.json
```

### 自動チューニング（min_edge / kelly_scale）
```bash
python3 tools/polymarket_openclaw_autotune.py \
  --journal-file data/reports/polymarket_openclaw_journal.jsonl \
  --settlements-file data/reports/polymarket_settlements_2026-02-12.json \
  --base-config-file tools/configs/polymarket_openclaw.contrarian.example.json \
  --date 2026-02-12 \
  --write-report data/reports/polymarket_openclaw_autotune_2026-02-12.json \
  --write-candidate-config data/reports/polymarket_openclaw_candidate_2026-02-12.json
```

## 7. 運用上の注意
- まずはpaper-onlyで継続記録し、実損益と乖離を測る。
- 約定品質が悪い時間帯は`question_keywords`や`min_edge`を厳格化する。
- 連敗時は `max_daily_loss_pct` 到達前でも手動停止を推奨。
- 既定では`journal`上の未決着marketは再エントリーを抑止する（重複建て防止）。

## 8. 自動運用（systemd）
追加済みユニット:
- `systemd/swimmy-polymarket-openclaw.service`
- `systemd/swimmy-polymarket-openclaw.timer`
- `systemd/swimmy-openclaw-signal-sync.service`
- `systemd/swimmy-openclaw-signal-sync.timer`

環境変数テンプレート:
- `tools/configs/polymarket_openclaw.env.example`

最低限の設定:
1. `.env` に `POLYCLAW_SIGNALS_FILE` を設定（`POLYCLAW_OPENCLAW_CMD` 未設定でもヒューリスティックで補完可）
2. 必要ならコストや出力先を上書き

主要な自動採用変数:
- `POLYCLAW_AUTOTUNE_APPLY_BEST`
- `POLYCLAW_AUTOTUNE_APPLY_MIN_TRADES`
- `POLYCLAW_AUTOTUNE_APPLY_MIN_REALIZED_PNL_USD`
- `POLYCLAW_AUTOTUNE_APPLY_TARGET_CONFIG`

シグナル同期関連:
- `POLYCLAW_OPENCLAW_CMD`
- `POLYCLAW_SYNC_SIGNALS_BEFORE_RUN`
- `POLYCLAW_USE_HEURISTIC_IF_NO_OPENCLAW_CMD`
- `POLYCLAW_HEURISTIC_LIMIT`
- `POLYCLAW_HEURISTIC_QUESTION_KEYWORDS`
- `POLYCLAW_SIGNALS_META_FILE`
- `POLYCLAW_SIGNAL_SYNC_MIN_SIGNALS`
- `POLYCLAW_SIGNAL_SYNC_TIMEOUT_SECONDS`
- `POLYCLAW_REQUIRE_FRESH_SIGNALS`
- `POLYCLAW_MIN_SIGNAL_COUNT`
- `POLYCLAW_MAX_SIGNAL_AGE_SECONDS`
- `POLYCLAW_SKIP_ON_BAD_SIGNALS`

重複エントリー制御:
- `POLYCLAW_ALLOW_DUPLICATE_OPEN_MARKETS`（既定0。1で重複抑止を無効化）
- `POLYCLAW_MAX_OPEN_POSITIONS`（未決着market総数の上限。到達時は新規エントリー停止）
- `POLYCLAW_MAX_DAILY_ENTRIES`（1日の新規エントリー上限。到達時は当日分を停止）
- `POLYCLAW_MAX_DAILY_LOSS_STREAK`（当日の決着ベース連敗数上限。到達時はクールダウン停止）
- `POLYCLAW_MAX_DAILY_REALIZED_LOSS_USD`（当日の実現PnLが`-X USD`を下回ったら停止）

有効化:
```bash
sudo install -m 0644 systemd/swimmy-polymarket-openclaw.service /etc/systemd/system/
sudo install -m 0644 systemd/swimmy-polymarket-openclaw.timer /etc/systemd/system/
sudo install -m 0644 systemd/swimmy-openclaw-signal-sync.service /etc/systemd/system/
sudo install -m 0644 systemd/swimmy-openclaw-signal-sync.timer /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now swimmy-openclaw-signal-sync.timer
sudo systemctl enable --now swimmy-polymarket-openclaw.timer
```

確認:
```bash
systemctl status swimmy-openclaw-signal-sync.timer --no-pager
systemctl status swimmy-polymarket-openclaw.timer --no-pager
journalctl -u swimmy-openclaw-signal-sync.service -n 100 --no-pager
journalctl -u swimmy-polymarket-openclaw.service -n 100 --no-pager
```
