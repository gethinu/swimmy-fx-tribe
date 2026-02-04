# Swimmy Runbook (運用手順書)

**正本**: systemd(system) を正本とし、user unit は診断用途のみ。

## 1. 起動手順

### 1.1 通常起動 (systemd)
```bash
# 全サービスを起動
sudo systemctl start swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper \
  swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog

# 状態確認
sudo systemctl status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper \
  swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog
```

### 1.2 開発モード起動
```bash
cd /home/swimmy/swimmy
make run  # Brain + Guardian を tmux で起動
```

### 1.3 起動確認チェックリスト
- [ ] `swimmy-brain` が active (running)
- [ ] `swimmy-guardian` が active (running)
- [ ] `swimmy-school` が active (running)
- [ ] `swimmy-data-keeper` が active (running)
- [ ] `swimmy-backtest` が active (running)
- [ ] `swimmy-risk` が active (running)
- [ ] `swimmy-notifier` が active (running)
- [ ] `swimmy-evolution` が active (running)
- [ ] `swimmy-watchdog` が active (running)
- [ ] Discord heartbeat が受信されている
- [ ] MT5 EA が接続されている (heartbeat)
- [ ] MT5 EA の `InpWSL_IP` が設定済み（空だと初期化失敗）

---

## 2. 停止手順

### 2.1 通常停止
```bash
sudo systemctl stop swimmy-guardian swimmy-brain swimmy-school swimmy-data-keeper \
  swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog
```

### 2.2 緊急停止 (EMERGENCY_CLOSE_ALL)
```bash
# MT5 に直接コマンド送信
echo '((type . "CLOSE") (close_all . t) (symbol . "ALL"))' | zmq-send tcp://localhost:5560

# または Guardian 経由
echo 'EMERGENCY_CLOSE_ALL' | zmq-send tcp://localhost:5559
```

### 2.3 強制停止 (プロセス Kill)
```bash
pkill -9 sbcl
pkill -9 guardian
```

### 2.4 旧戦略データ S式移行（オフライン）
**目的**: `strategies.data_sexp` の plist 形式を `#S(STRATEGY ...)` に完全移行する。

**手順**:
```bash
# 1) 停止
sudo systemctl stop swimmy-brain swimmy-backtest

# 2) バックアップ
mkdir -p data/memory/backup
cp data/memory/swimmy.db data/memory/backup/swimmy.db.$(date +%Y%m%d%H%M%S)

# 3) 移行実行
sbcl --noinform --disable-debugger --load tools/migrate_strategy_sexp.lisp

# 4) 検証（件数と #S 率）
python3 - <<'PY'
import sqlite3
conn = sqlite3.connect('data/memory/swimmy.db.migrated')
cur = conn.cursor()
total = cur.execute('SELECT count(*) FROM strategies').fetchone()[0]
valid = cur.execute("SELECT count(*) FROM strategies WHERE data_sexp LIKE '#S(%'").fetchone()[0]
print('total', total)
print('#S', valid)
conn.close()
PY

# 5) スワップ
mv data/memory/swimmy.db data/memory/swimmy.db.pre_migration
mv data/memory/swimmy.db.migrated data/memory/swimmy.db

# 6) 再起動
sudo systemctl start swimmy-brain swimmy-backtest
```

**ロールバック**:
```bash
sudo systemctl stop swimmy-brain swimmy-backtest
mv data/memory/swimmy.db data/memory/swimmy.db.failed
cp data/memory/backup/swimmy.db.<timestamp> data/memory/swimmy.db
sudo systemctl start swimmy-brain swimmy-backtest
```

---

## 3. 緊急対応

### 3.1 Brain 無応答 (Heartbeat Timeout)
**症状**: Guardian が `🔴 BRAIN DISCONNECT` を表示

**対応**:
1. Guardian が自動で `CLOSE_SHORT_TF` を発行 (120秒後)
2. Brain 再起動: `sudo systemctl restart swimmy-brain`
3. ログ確認: `journalctl -u swimmy-brain -f`

### 3.2 Guardian Crash
**症状**: Brain が `GUARDIAN_TIMEOUT` をログ

**対応**:
1. `sudo systemctl restart swimmy-guardian`
2. ポジション確認: MT5 で手動確認

### 3.3 MT5 接続断
**症状**: Tick データが来ない

**対応**:
1. MT5 再起動
2. EA のアタッチ確認
3. ブローカー接続確認

---

## 4. 日次運用

### 4.1 朝の確認 (09:00 JST)
- [ ] 前日の PnL 確認 (Discord)
- [ ] ログにエラーがないか確認
- [ ] オープンポジション確認

### 4.2 週末処理 (金曜 23:00 JST)
- システムは自動でポジションクローズ (週末リスク回避)
- 月曜朝まで新規エントリー停止

---

## 5. 障害時連絡先

- **Discord**: #swimmy-alerts チャンネル
- **ログ場所**: `/home/swimmy/swimmy/brain.log`
- **監査ログ**: `/home/swimmy/swimmy/guardian_audit.jsonl`

---

## 6. テレメトリ / 監視

- **JSONLログ**: `/home/swimmy/swimmy/logs/swimmy.json.log`（`log_type="telemetry"` がイベント）
- **ローテーション**: `swimmy.json.log.1`（サイズ上限超過時、既定10MB）
- **システムメトリクス**: `data/system_metrics.json`
- **ライブステータス**: `.opus/live_status.json`
- **スキーマ**: `schema_version` は破壊的変更時にインクリメント

---

## 6.1 MCP Gateway

- 起動: `sudo systemctl start swimmy-mcp-gateway`
- 停止: `sudo systemctl stop swimmy-mcp-gateway`
- 状態: `sudo systemctl status swimmy-mcp-gateway`
- 監視: `journalctl -u swimmy-mcp-gateway -f`

---

## 6.2 MCP stdio (JSON-RPC)

- 手動起動: `SWIMMY_MCP_API_KEY=... python3 tools/mcp_stdio_server.py`
- 停止: 標準入力を閉じる（MCPホスト停止）
- ログ: `logs/mcp_gateway.jsonl`

---

## 7. コマンドリファレンス

| コマンド | 説明 |
|:---|:---|
| `make run` | 開発モード起動 |
| `./tools/quality_gate.sh` | テスト＆整合性チェック |
| `/deploy` | 本番デプロイ (systemd restart) |
| `CLOSE_ALL` | 全ポジション決済 |
| `CLOSE_SHORT_TF` | H4以下のポジション決済 |
| `CANCEL_ALL` | ペンディング注文キャンセル |
