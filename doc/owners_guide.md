# 🐟 Swimmy Ver 8.6 オーナーズガイド

**最終版:** 2026-01-10 (V8.6 - Systemd & ACCOUNT_INFO Monitoring)
**リーダー判断:** Elon Musk (Deploy & Iterate)

---

## 🚀 起動 / 停止 (Systemd)

```bash
# 全サービス起動
systemctl --user start swimmy-brain swimmy-guardian

# 全サービス停止
systemctl --user stop swimmy-brain swimmy-guardian

# 状態確認
systemctl --user status 'swimmy-*'

# ログ確認 (リアルタイム)
journalctl --user -u swimmy-brain -f

# 全サービス再起動
systemctl --user restart swimmy-brain swimmy-guardian
```

> ⚠️ `make run` は開発用です。本番では systemd を使用してください。

---

## 📋 システム構成 (7サービス)

```
┌─────────────────────────────────────────────────────┐
│  BRAIN (Lisp)           Port 5555/5556              │
│  - シグナル生成、学習、Heartbeat                     │
├─────────────────────────────────────────────────────┤
│  GUARDIAN (Rust)        Port 5557/5559/5560         │
│  - MT5通信、注文執行、バックテスト                   │
├──────────────────┬──────────────────────────────────┤
│  DATA KEEPER     │  NOTIFIER      │  RISK GATEWAY  │
│  (Python)        │  (Python)      │  (Python)      │
│  Port 5561       │  Port 5562     │  Port 5563     │
├──────────────────┴──────────────────────────────────┤
│  BACKTEST SERVICE (Python)  Port 5564              │
│  WATCHDOG (Bash)  - ログ監視・Discord通知           │
└─────────────────────────────────────────────────────┘
```

| サービス | systemd name | 役割 |
|----------|--------------|------|
| Brain | `swimmy-brain` | シグナル生成、学習 |
| Guardian | `swimmy-guardian` | MT5通信、注文執行 |
| Data Keeper | `swimmy-keeper` | ヒストリカルデータ |
| Notifier | `swimmy-notifier` | Discord通知 |
| Risk Gateway | `swimmy-risk` | リスクチェック |
| Backtest | `swimmy-backtest` | バックテスト |
| Watchdog | `swimmy-watchdog` | ログ監視 |

詳細: [doc/port_map.md](file:///home/swimmy/swimmy/doc/port_map.md)

---

## 🛡️ ACCOUNT_INFO 監視 (V8.5+)

MT5 EA が30秒ごとに口座情報を送信します。

- **60秒間データが来ない場合**: Discord に警告通知
- **復旧時**: 回復通知

> ⚠️ MT5 EA (`SwimmyBridge.mq5`) を最新版に更新してください。

---

## 🏁 Launch Checklist

```bash
# 1. Quality Gate 確認
cd /home/swimmy/swimmy && make quality-gate

# 2. サービス状態確認
systemctl --user status 'swimmy-*'

# 3. ポート確認
ss -tlnp | grep -E "555|556"

# 4. 最新ログ確認
tail -20 /home/swimmy/swimmy/logs/swimmy.log
```

---

## 🚨 緊急時対応

```bash
# ゾンビプロセス発見時のみ使用
pkill -9 -f "sbcl.*brain.lisp"
pkill -9 guardian

# その後、正常に再起動
systemctl --user restart swimmy-brain swimmy-guardian
```

> ⚠️ `pkill -9` は状態保存なしで強制終了します。緊急時のみ使用。

---

## 📊 戦略パフォーマンス確認

```bash
# 最新のBacktest結果を確認
journalctl --user -u swimmy-brain | grep "🏆 Top strategies" | tail -5

# Heartbeat 手動送信
# (Lisp REPL から)
(swimmy.engine:heartbeat-now)
```

---

**"Don't overthink. Ship it."** — Elon Musk
