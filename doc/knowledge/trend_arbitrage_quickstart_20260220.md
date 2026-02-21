# Trend Arbitrage Engine Quickstart

作成日: 2026-02-20

## 1. 何を自動化するか

`tools/trend_arbitrage_engine.py` は以下を自動実行します。

1. Google Trends/サジェストで需要検知
2. 検索結果の供給不足をスコア化
3. 商用意図を判定して機会スコア算出
4. 上位候補を X / 自サイト / メルマガへ配信

生成物:

- `data/trend_arbitrage/runs/run_*.json`
- `data/trend_arbitrage/site/posts/*.md`
- `data/trend_arbitrage/site/index.json`
- `data/trend_arbitrage/site/rss.xml`
- `data/trend_arbitrage/state.json`

## 2. 手動実行

```bash
python3 tools/trend_arbitrage_engine.py --dry-run --scan-only
python3 tools/trend_arbitrage_engine.py --dry-run
python3 tools/trend_arbitrage_engine.py
```

ラッパー:

```bash
./tools/trend_arbitrage_runner.sh
```

## 3. 必須/推奨環境変数

### 共通

- `TREND_ARB_SITE_BASE_URL` 例: `https://your-domain.com/trend-arbitrage`
- `TREND_ARB_MAX_CANDIDATES` 例: `12`
- `TREND_ARB_TOP_N` 例: `3`
- `TREND_ARB_GEO` 既定 `JP`
- `TREND_ARB_LANG` 既定 `ja`
- `TREND_ARB_MIN_INTENT` 既定 `0.55`
- `TREND_ARB_MIN_OPPORTUNITY` 既定 `0.50`
- `TREND_ARB_SEED_QUERIES` 例: `副業 AI,AI ツール 比較,業務自動化 テンプレ`
- `TREND_ARB_SERP_PROVIDER` 既定 `auto`（`duckduckgo` 固定にも変更可）
- `TREND_ARB_SERP_TIMEOUT_SEC` 既定 `8`
- `TREND_ARB_PUBLISH_SITE` 既定 `1`
- `TREND_ARB_PUBLISH_X` 既定 `1`
- `TREND_ARB_PUBLISH_NEWSLETTER` 既定 `1`
- `TREND_ARB_REPUBLISH_HOURS` 既定 `72`（同一候補の再掲クールダウン）

### X 投稿（どちらか）

Webhook 経由:

- `TREND_ARB_X_WEBHOOK_URL`

X API 直結（OAuth1）:

- `TREND_ARB_X_CONSUMER_KEY`
- `TREND_ARB_X_CONSUMER_SECRET`
- `TREND_ARB_X_ACCESS_TOKEN`
- `TREND_ARB_X_ACCESS_TOKEN_SECRET`

### メルマガ（SMTP）

- `TREND_ARB_NEWSLETTER_TO` 例: `a@example.com,b@example.com`
- `TREND_ARB_SMTP_HOST`
- `TREND_ARB_SMTP_PORT` 既定 `587`
- `TREND_ARB_SMTP_USER`
- `TREND_ARB_SMTP_PASS`
- `TREND_ARB_SMTP_FROM`
- `TREND_ARB_SMTP_TLS` 既定 `1`

注:
- X/SMTP が未設定でも、既定ではジョブは失敗せず `skipped` として継続します。

## 4. systemd で常駐自動化（2時間ごと）

同梱:

- `systemd/swimmy-trend-arbitrage.service`
- `systemd/swimmy-trend-arbitrage.timer`

導入:

```bash
sudo install -m 0644 systemd/swimmy-trend-arbitrage.service /etc/systemd/system/
sudo install -m 0644 systemd/swimmy-trend-arbitrage.timer /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now swimmy-trend-arbitrage.timer
sudo systemctl status swimmy-trend-arbitrage.timer --no-pager
```

即時実行:

```bash
sudo systemctl start swimmy-trend-arbitrage.service
sudo journalctl -u swimmy-trend-arbitrage.service -n 100 --no-pager
```

補助スクリプト（同等操作）:

```bash
sudo bash tools/install_trend_arbitrage_service.sh
```

監視:

```bash
python3 tools/trend_arbitrage_status.py --fail-on-problem
```

### sudo なし環境（user systemd）

```bash
mkdir -p ~/.config/systemd/user
cp systemd/swimmy-trend-arbitrage.service ~/.config/systemd/user/
cp systemd/swimmy-trend-arbitrage.timer ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now swimmy-trend-arbitrage.timer
systemctl --user start swimmy-trend-arbitrage.service
```

## 5. 安全運用の順序

1. `--dry-run --scan-only` で候補品質確認
2. `--dry-run` で配信本文確認
3. 実配信に切替
4. timer 有効化
