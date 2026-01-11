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

# ステータス確認 (CLI Dashboard)
make dashboard

# 日々のログ確認 (Systemd Status)
make status

# リアルタイムログ監視
make logs
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
| サービス | systemd name | 役割 |
|----------|--------------|------|
| Brain | `swimmy-brain` | シグナル生成、学習 |
| Guardian | `swimmy-guardian` | MT5通信、注文執行 |
| Data Keeper | `swimmy-data-keeper` | ヒストリカルデータ |
| Notifier | `swimmy-notifier` | Discord通知 |
| Backtest | `swimmy-backtest` | バックテスト |
| Watchdog | `swimmy-watchdog` | ログ監視 (エラー検知) |

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

# リスクリミット（ドローダウン等）の即時リセット
systemctl --user restart swimmy-risk
```

> [!NOTE]
> **最大ドローダウン (MAX_DRAWDOWN) について**
> `swimmy-risk` サービスが起動した時点の口座残高を基準に計算されます。
> 今日取引をしていなくても、過去の含み損や前日以前の確定損が基準（デフォルト5%）を超えている場合、新規エントリーが自動的にブロックされます。
> 必要に応じてサービスを再起動することで、現在の残高を基準に再設定（リセット）できます。

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

---

## 🧬 戦略の進化と学習 (Strategy Evolution)

### 1. ネーミングルール (Naming Convention)
戦略名は、その出自と性質を表しています。

`[RootName]-Gen[N]-mut-[Param][Value]`

- **例**: `Wisdom-USDJPY-Trend-3977-Gen1-mut-RSI23`
- **RootName**: 家系図の祖となる戦略名
- **Gen[N]**: 世代数 (Generation)。`Gen1` は初代変異、`Gen2` はその子供。
- **mut**: 変異 (Mutation) したことを示す
- **Param**: 変更されたパラメータ (例: `RSI`, `SMA`)
- **Value**: 新しい値 (例: `23`)

### 2. 進化のサイクル (Metabolism)
システムは24時間365日、以下のサイクルで進化を続けます。

1.  **Metabolism (代謝)**: 定期的に（主に週末や市場が静かな時）、既存の戦略から「子孫」を生成します。
2.  **Mutation (変異)**: 親戦略のパラメータをわずかに変更し、新しい可能性を探ります。
3.  **Backtest (検証)**: 生成された子孫は、過去のデータを用いてバックテストされます。
    - **Overfitting対策**: 常に「新しい未知の戦略」を「既知のデータ」でテストする形式をとっています。同じ戦略を同じデータで何度も最適化することはありません。また、バックテストはTICKが動いていない週末でも、過去データを用いて「新入生（変異体）の入団テスト」として実行されます。
4.  **Recruitment (採用)**: 優秀な成績を収めた変異体のみが、正式な「知識ベース」に採用され、実弾運用の候補となります。
    - **Clan Balance**: 特定の部族（Category）が不足している場合、システムは優先的にその部族の候補生を育成・採用しようとします（Diversity Injection）。

### 3. 学習の範囲 (Learning Scope)
システムは2つの異なるレベルで学習を行います。

- **Trade Outcome (個別トレード学習)**:
    - **Live/Paper**: 実弾およびフォワードテストの結果は `school-learning.lisp` に記録され、即座に次の判断にフィードバックされます。
    - エントリー時の相場環境（Context）と結果（Win/Loss）をセットで記憶し、似た環境での失敗を避けます（損切りの結果も当然学習されます）。
- **Evolution (進化論的学習)**:
    - **Backtest**: バックテストの結果は、戦略の「生存競争」に使われます。
    - 弱い戦略は淘汰され、強い戦略が子孫を残すことで、システム全体が市場に適応していきます。
