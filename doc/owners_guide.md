# 🐟 Swimmy Ver 9.2 オーナーズガイド

**最終版:** 2026-01-11 (V9.2 - Strategy Immigration & Hunter Service)
**リーダー判断:** Elon Musk (Deploy & Iterate)

---

## 🚀 起動 / 停止 (Systemd)

```bash
# 全サービス起動
systemctl --user start swimmy-brain swimmy-guardian strategy_hunter

# 全サービス停止
systemctl --user stop swimmy-brain swimmy-guardian strategy_hunter

# ステータス確認 (CLI Dashboard)
make dashboard

# 日々のログ確認 (Systemd Status)
make status

# リアルタイムログ監視
make logs
```

---

## 📋 システム構成 (8サービス)

```
┌─────────────────────────────────────────────────────┐
│  BRAIN (Lisp)           Port 5555/5556 (PUB)        │
│  - シグナル生成、学習、Heartbeat                     │
├─────────────────────────────────────────────────────┤
│  GUARDIAN (Rust)        Port 5557/5559/5560         │
│  - MT5通信、注文執行、バックテスト                   │
├──────────────────┬──────────────────────────────────┤
│  DATA KEEPER     │  NOTIFIER      │  RISK GATEWAY  │
│  (Python)        │  (Python)      │  (Python)      │
│  Port 5561       │  Port 5562     │  Port 5563     │
├──────────────────┴──────────────────────────────────┤
│  HUNTER SERVICE (Python)    Port 5556 (SUB)        │
│  - 不足検知・自動補充 (Immigration Bureau)           │
├─────────────────────────────────────────────────────┤
│  BACKTEST SERVICE (Python)  Port 5564              │
│  WATCHDOG (Bash)  - ログ監視・Discord通知           │
└─────────────────────────────────────────────────────┘
```

| サービス | systemd name | 役割 |
|----------|--------------|------|
| Brain | `swimmy-brain` | シグナル生成、学習 |
| Guardian | `swimmy-guardian` | MT5通信、注文執行 |
| Hunter | `strategy_hunter` | **自動狩猟・不足検知 (V9.2)** |
| Data Keeper | `swimmy-data-keeper` | ヒストリカルデータ |
| Notifier | `swimmy-notifier` | Discord通知 |
| Backtest | `swimmy-backtest` | バックテスト |
| Watchdog | `swimmy-watchdog` | ログ監視 (エラー検知) |

詳細: [doc/port_map.md](file:///home/swimmy/swimmy/doc/port_map.md)

---

## 🏹 Strategy Immigration (戦略移民局) V9.2

システムは多様性を維持するために、自律的に外部から戦略を取り込みます。

### 1. 監視と検知 (Active Census)
- **Hunter Service** (`strategy_hunter.py`) が `IMMIGRATION_SHORTAGE` イベント（ZMQ Port 5556）を常時監視しています。
- 特定の Clan (Trend, Scalp, etc) が不足すると、自動的に補填プロセスが回ります。

### 2. 手動トリガー (Agentic Hunt)
エージェントによる自動生成を手動で試す場合：

```bash
# クラン名を指定して狩猟 (scalp, trend, reversion, breakout)
python3 tools/trigger_hunt.py scalp
```

> **Safety Gate**: 生成された戦略は、登録前に自動的に「簡易バックテスト」を受けます。品質基準（Sharpe > 0 等）を満たさない場合は破棄されます。

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
systemctl --user status 'swimmy-*' strategy_hunter

# 3. ポート確認
ss -tlnp | grep -E "555|556"

# 4. 最新ログ確認
tail -20 /home/swimmy/swimmy/logs/swimmy.log
tail -20 /home/swimmy/swimmy/logs/strategy_hunter.log
```

---

## 🚨 緊急時対応

```bash
# ゾンビプロセス発見時のみ使用
pkill -9 -f "sbcl.*brain.lisp"
pkill -9 guardian
pkill -9 -f "strategy_hunter.py"

# その後、正常に再起動
systemctl --user restart swimmy-brain swimmy-guardian strategy_hunter

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

---

## 🧭 Swimmy Philosophy (技術的信条)

ユーザーからの問い「なぜ Lisp と Rust なのか？」に対する回答。

### 1. Speed = Survival (速度は生存能力)
Rustのバックテスト速度（50,000本/ms）は、高頻度取引で勝つためではなく、**「失敗を高速に捨てる」** ために使われます。
市場環境は刻一刻と変化します。Pythonで数日かかる検証を数分で終えることで、Swimmyは「今の市場に合わない戦略」を即座に見限り、次の仮説へ移行できます。

### 2. Hot Reloading = Adaptation (柔軟性は適応能力)
Lispの真価は、システムを停止せずに脳外科手術（関数の書き換え・進化）ができる点にあります。
市場が急変したその瞬間に、システムを止めずにロジックを修正・適応させることができる。これが不確実性への最大の防御です。

### 3. "Fail Fast, Evolve Faster"
Swimmyは完璧な戦略を一つ作るのではなく、**「大量の失敗（死）を高速に繰り返す」** ことで、生き残る種を見つけ出します。
M1スキャルピングの失敗も、高速な検証があったからこそ即座に結論が出せました。

> **"It is not the strongest of the species that survives, nor the most intelligent; it is the one most adaptable to change."** — Charles Darwin

---

## 🛠️ Stress Testing & Validation (V10.0+)

システムの堅牢性と戦略の品質を検証するためのツール群です。

### 1. Deep Validation Campaign (ストレス耐性テスト)
4年分以上のティックデータを再現し、メモリリークや長時間運用の安定性をテストします。

```bash
# クイック実行 (USDJPY M5)
python3 tools/stress_test.py --symbol USDJPY --timeframe M5 --strategy stress_test_strategy.json

# キャンペーン実行 (全通貨ペア・長時間)
./run_validation_campaign.sh
```

### 2. Data Persistence (データ永続化)
ライブデータは自動的に `data/historical/*.csv` に追記保存されます。

- **スペック (V9.3 Optimized)**:
    - **Buffer**: 10,000,000 Candles (per symbol/timeframe)
    - **Async Save**: バックグラウンド保存（メイン処理をブロックしません）
    - **Article 5 Compliant**: 上書き禁止 & 自動復旧機能付き

これにより、システムを稼働させるだけで、バックテスト用の「生きた20年分のデータ」が自然に蓄積されていきます。

---

## ❓ トラブルシューティング

### Q. Discordで "Brain Silence Detected" (Dead Man's Switch) が出た
**A. 高負荷時の正常な挙動の可能性があります。**
`stress_test.py` や `Backtest` などの重い処理を実行中、詳細なログ出力やCPU負荷により、Brainのハートビート（生存信号）が一時的に遅延することがあります。
`make status` で `swimmy-brain` が `active (running)` であれば、システムは生きています。

### Q. トレードがエントリーされない
1. 市場がオープンしているか確認 (`is-safe-trading-time-p`)
2. 十分なヒストリカルデータがあるか確認 (ログに `REJECTED: History empty/short` がないか)
3. **[Resolved 2026-01-12]** 過去に `BINDINGS unbound` エラーがありましたが、修正済みです。

### Q. メモリ不足 (OOM) でクラッシュする
最新の Guardian は、巨大なCSV (3GB+) をディスクから直接ストリーミング読み込み (`Stream loading`) するよう最適化されています。
それでも落ちる場合は、WSLの割当メモリを確認してください。

