# 🐟 Swimmy Ver 15.2 オーナーズガイド

**最終版:** 2026-01-13 (V15.2 - Multi-TF History & SRP Refactoring)
**リーダー判断:** Elon Musk (Deploy & Iterate)

---

## 🚀 起動 / 停止 (Systemd)

```bash
# 全サービス起動 (Zombie Processの一掃を含む)
make run  # 推奨 (kill-zombies を自動実行)

# または個別起動
systemctl --user start swimmy-brain swimmy-guardian swimmy-data-keeper

# 全サービス停止
systemctl --user stop swimmy-brain swimmy-guardian swimmy-data-keeper

# ステータス確認
make status

# リアルタイムログ監視
make logs
```

---

## 📋 システム構成

```
┌─────────────────────────────────────────────────────┐
│  MT5 EA (SwimmyBridge Ver 15.2)                     │
│  - 3通貨ペア (USDJPY, EURUSD, GBPUSD)              │
│  - Multi-TF History Support                         │
│  - BUY/SELL/CLOSE/REQ_HISTORY                       │
├─────────────────────────────────────────────────────┤
│  GUARDIAN (Rust)        Port 5557/5559/5560         │
│  - MT5通信、注文執行、バックテスト、RiskGate        │
├─────────────────────────────────────────────────────┤
│  BRAIN (Lisp)           Port 5555/5556 (PUB)        │
│  - シグナル生成、学習、Evolution、Heartbeat         │
├─────────────────────────────────────────────────────┤
│  DATA KEEPER (Python)   Port 5561                   │
│  - 10M Candle Buffer (Async Save)                   │
└─────────────────────────────────────────────────────┘
```

| サービス | systemd name | 役割 |
|----------|--------------|------|
| Brain | `swimmy-brain` | シグナル生成、学習、進化 |
| Guardian | `swimmy-guardian` | MT5通信、注文執行、バックテスト |
| Data Keeper | `swimmy-data-keeper` | ヒストリカルデータ永続化 |

---

## 🧬 SRP モジュール構成 (V15.2)

`school.lisp` は1,226行から**33行**に分割されました（God Class解消）。

```
src/lisp/
├── school.lisp                    (33行) オーケストレーター
├── school/
│   ├── school-execution.lisp     (570行) 売買ロジック
│   ├── school-market.lisp        (216行) 市場分析
│   ├── school-risk.lisp          (159行) リスク管理
│   ├── school-ecosystem.lisp     (136行) 生態系シミュレーション
│   ├── school-memory.lisp        (227行) 記憶・学習
│   └── school-narrative.lisp      (41行) Discord文言
├── school-evolution.lisp         (568行) 進化・突然変異
├── school-hunter.lisp           (1090行) 戦略レジストリ
├── school-learning.lisp          (451行) 失敗学習
├── strategies.lisp               (177行) 戦略ベース
├── strategies-trend.lisp          (99行) トレンド戦略
├── strategies-reversion.lisp      (65行) リバージョン戦略
├── strategies-breakout.lisp       (13行) ブレイクアウト戦略
└── strategies-scalp.lisp         (196行) スキャルプ戦略
```

---

## 🛡️ EA (SwimmyBridge) Ver 15.2

MT5のExpertsフォルダにコピーし、コンパイルして適用してください。

**新機能:**
- ✅ マルチタイムフレーム履歴対応 (`REQ_HISTORY` + `tf` パラメータ)
- ✅ W1, D1, H4, H1, M30, M15, M5, M1 対応
- ✅ BUY/SELL/CLOSE ロジック完備

**ログで確認:**
```
🚀 Swimmy Bridge Ver 15.2 - Multi-TF Support
📊 This EA handles: USDJPY only
✅ PUB connected to Guardian
✅ SUB connected to Guardian
```

> ⚠️ Ver 15.1 以前は `REQ_HISTORY` のタイムフレーム対応がありません。必ず更新してください。

---

## 🏁 Launch Checklist

```bash
# 1. Quality Gate 確認
make quality-gate

# 2. サービス状態確認
make status

# 3. MT5 EA バージョン確認
# → EA ログで "Ver 15.2" を確認

# 4. ログ確認
tail -30 logs/swimmy.log
tail -10 logs/guardian.log
```

---

## 🚨 緊急時対応

```bash
# ゾンビプロセス一掃（サービス再起動で自動実行）
make run

# 手動でゾンビを殺す場合
make kill-zombies

# その後、正常に再起動
systemctl --user restart swimmy-brain swimmy-guardian swimmy-data-keeper
```

> ⚠️ Systemd の `ExecStartPre` が自動でゾンビを処理します。通常は `make run` で十分です。

---

## 📊 戦略パフォーマンス確認

```bash
# Heartbeat 確認
grep "💓" logs/swimmy.log | tail -3

# Backtest 結果確認
grep "�" logs/swimmy.log | tail -10

# Evolution 確認
grep "🧬" logs/swimmy.log | tail -10
```

---

## 🧭 Swimmy Philosophy

### 1. Speed = Survival
Rustのバックテスト速度（50,000本/ms）で「失敗を高速に捨てる」。

### 2. Hot Reloading = Adaptation
Lispでシステム停止なしにロジック修正可能。

### 3. SRP = Antifragility
28モジュールに分割することで、1箇所の障害が全体を殺さない。

> **"Don't fix what isn't broken. Ship it."** — Elon Musk

---

## ❓ トラブルシューティング

### Q. EA が Ver 15.1 のまま
**A.** `src/mt5/SwimmyBridge.mq5` をMT5にコピーし、MetaEditorでコンパイル。

### Q. トレードがエントリーされない
1. EA Ver 15.2 が適用されているか確認
2. 市場がオープンしているか確認
3. ログに `🧪 Batch testing` が出ているか確認

### Q. HISTORY データが来ない
1. EA ログに `📊 Sending ... candles` が出ているか確認
2. Brain ログに `[DATA-CLIENT] Data Keeper is ONLINE` があるか確認
3. Protocol: Brain は `REQ_HISTORY` を送信する必要あり（`GET_HISTORY` は非対応）

### Q. メモリ不足 (OOM)
Data Keeper は 10M Candle Buffer を使用。M1 は無効化されています。
