## 🆕 V50.6 新機能 (2026-02-03) - Structured Telemetry

### 1. JSONLイベント統合
- **イベントログ**: `/home/swimmy/swimmy/logs/swimmy.json.log` に統一 (`log_type="telemetry"`)
- **対象**: OOS / WFV / Heartbeat / System metrics / Live status
- **ローテーション**: `swimmy.json.log.1`（既定10MB上限）

### 2. 監視ファイルの原子書き込み
- **system_metrics.sexp / live_status.sexp** を tmp→rename で破損防止（`schema_version=2`）

---

## 🆕 V50.5 新機能 (2026-01-28) - Phase 24/25: System Hardening II

### 1. Symbolic Hashing / Logic Integrity (Jim Simons)
- **Symbolic Hashing**: 戦略の「論理的指紋」を計算し、名前が違っても中身が同じ戦略を自動排除。
- **Jaccard Similarity Check**: 95%以上類似した戦略（変数微差など）を「冗長」と見なす。
- **The Highlander Rule**: 類似戦略が見つかった場合、より強い方だけを残して弱い方を即時抹殺 (Survival of the Fittest)。

### 2. Service Isolation & Watchdog (Vogels/Hamilton)
- **Scribe Service**: ディスクI/Oを専門の非同期ワーカーに委譲。トレード判断ループの遅延をゼロ化。
- **Broken Arrow Watchdog**: 100ms以上のフリーズを検知する独立監視システム。
- **DNA Verification**: 起動時に重要ファイルのSHA256ハッシュをチェックし、コード改ざんを防止。

### 3. AI Auditor (Andrew Ng)
- **Toxic Feature Detection**: XGBoostにより「Graveyard（墓場）」行きになる確率が高い「毒入り特徴量（パラメータの組み合わせ）」を事前に特定。

---

## 🆕 V50.2 新機能 (2026-01-28) - Phase 21: The Evolution Factory

### 1. Engine of Life (進化エンジン)
- **DNA Upgrade**: 戦略構造体に `age` (寿命) と `immortal` (不死属性) を追加。
- **Deathmatch (死闘)**: 「親と子」が競い合い、子が親のSharpe Ratioを超えた場合、親を殺害して入れ替わる (`compete-parent-child`)。
- **Culling (間引き)**: 毎日1回、成果の出ない古い戦略 (Rank C/D かつ 5日以上経過) を自動処刑。
- **Stagnant C-Rank**: 日次cull（day-keyガードで1日1回, Sharpe<0.6 & Age>10）。通知は個別抑制し、**1時間バッチ**で合計件数＋上位5件を送信。
- **Immortality (不死)**: `Legend` ランクの戦略は、老衰や間引きの影響を受けない不老不死となる。

> [!WARNING]
> **構造体の変更による互換性注意**
> 戦略のＤＮＡ構造 (`defstruct strategy`) を変更したため、既存のメモリ上のインスタンスと互換性がありません。
> **アップデート後は必ず Lisp プロセス (`swimmy-school`, `swimmy-brain`) を再起動してください。**

---

## 🆕 V49.8 新機能 (2026-01-24) - Phase 12: SQL Persistence & Scaling

### 1. SQLite Migration (Jim Simons / Taleb)
- **永続化層の刷新**: 戦略ナレッジベース（KB）とトレードログをフラットファイルから SQLite (`swimmy.db`) へ移行。
- **Swarm Draft (SQL版)**: 数万件のBランク戦略をメモリに保持せず、必要な時にSQLクエリで高速抽出。RAM消費を劇的に削減。
- **データ規模**: 8.5万戦略、43万墓場パターンを安定して管理可能に。

### 2. Genetic Persistence (Rich Hickey)
- **トランザクション整合性**: 戦略の追加(`add-to-kb`)やトレード記録が、クラッシュ時もデータ消失しないトランザクション下で実行されます。
- **S-expression 互換**: DB内にもS式を保存し、既存のLispロジックと100%の互換性を維持。

---

## 🆕 V49.5 新機能 (2026-01-24) - Phase 11: Project Haystack

### 1. The Haystack Paradigm (Jim Simons)
- **Mass Weak Ensemble**: 「一本の針」ではなく「干し草の山（全データ）」を集め、無数の「弱い予測シグナル（Weak Learners）」の多数決で取引します。
- **Swarm Strategy**: 内部に数千の `Predictor` を抱えた巨大なコンテナ戦略。既存の「精鋭戦略」と共存し、競争します。
- **Predictor Factory**: RSIや移動平均、価格アクションの組み合わせをプログラムが自動生成し、過去データで有効なものだけを抽出します。

### 2. Ghost Notification Fix
- **安全装置**: システム停止後やバックテスト中に「損切り通知」がDiscordに飛ぶバグを修正しました。

---

## 🆕 V49.4 新機能 (2026-01-24) - Phase 11: System Hardening

### 1. Hot Reload (Hot Reloading) (Gene Kim)
- **ゼロダウンタイム**: プロセスを停止せずにコード更新が可能。
- **操作**: `./tools/reload.sh` を実行すると、SIGHUPシグナルが送信され、ASDFシステムがリロードされます。
- **効果**: リードタイムの大幅短縮。

### 2. Regime Hard Lock (Musk)
- **物理的ロック**: レジームと戦略の不整合（例: レンジ相場でトレンド戦略）を物理的に排除。
- **検証**: `test-regime-lock.lisp` により、対象リストから除外されていることを数学的に証明。

---

## 🆕 V49.2 新機能 (2026-01-24) - Phase 10: Strategic Evolution

### 1. Data-Driven Tactical Mapping (Regime-Aware) (Fowler/Musk)
- **タクティカル・ウィズダム**: 市場レジーム（`trend-mature`, `range-expansion` 等）ごとに最適なインジケータ設定と「知恵」を定義。
- **実装**: `*regime-tactics*` (school-strategy.lisp)
- **効果**: LLMが生成する戦略が、現在の市場環境により適合するように誘導される。

### 2. LLM Tactical Injection (Uncle Bob)
- **プロンプト注入**: 戦略生成プロンプトに `[TACTICAL WISDOM]` セクションを自動挿入。
- **自動テスト**: `(test-llm-tactical-injection)` で注入プロセスを保証。

### 3. Breeding Jackpot Safety Check (Taleb)
- **相関チェック**: 「Breeding Jackpot」（S-Rank同士の優先交配）実行時に、親同士の遺伝的距離をチェック。
- **クローン防止**: 距離が近すぎる（< 0.2）場合は交配をスキップし、システムの脆弱性（多様性の欠如）を防ぐ。
- **メタデータ**: 生成された戦略に `regime-intent` を付与し、どのレジームを意図して生まれたかを追跡可能に。

## 🆕 V48.2 新機能 (2026-01-22) - Phase 8: Robustness

### 1. Atomic KB 操作 (Rich Hickey)
- **KBロック**: `*kb-lock*` を導入し、戦略の追加・削除・ランク変更をスレッッド安全に実行
- **データ不整合防止**: マルチスレッド環境での競合を排除

### 2. S-RANK カテゴリ枠制限 (López de Prado)
- **多様性維持**: 54カテゴリごとに最大3枠のS-RANK枠を設定
- **エリート選抜**: 枠が埋まっている場合、既存の最弱戦略よりも高性能な戦略が現れると自動的に入れ替え（旧最弱はAへ降格）

### 3. Safe Graveyard Save (Taleb)
- **フォールバック保存**: メインの墓場保存に失敗した場合、緊急用ファイル (`graveyard.emergency.sexp`) に書き込みを試行

---

## 🆕 V48.1 新機能 (2026-01-22) - Phase 7: Efficiency

### 1. Graveyard 即時削除 (Taleb)
- 失敗戦略をKBから即座に排除し、マシンの「脆弱性」を排除。パターンは保存済み。

### 2. CPCV バッチサイズ増加
- 5件 → **20件** に拡大。検証サイクルを4倍速化。

### 3. ensure-rank 統一
- ランク変更ロジックを一点集中化し、不整合な `setf` を廃止。

---

## 🆕 V47.10 新機能 (2026-01-22)

### 1. P10 KB Optimization
- **戦略プルーニング**: 18,349戦略 → 目標5,000
  - 低Sharpe削除 (< 0.08)
  - 90日非活性削除
  - 類似戦略削除 (distance < 0.1)
- **school-pruning.lisp**: `run-kb-pruning` でワンコマンド実行
- **自動実行**: 6時間ごと (`phase-8-weekly-prune`) ← V48.0で変更

> [!TIP]
> **PostgreSQLサーバー稼働中** ✅
> 基準値DB: `swimmy_db.rank_criteria` テーブル使用可能
> スキーマ場所: `db/schema/rank_criteria.sql`

### 2. P12 True CPCV Integration
- **Guardian CPCV_VALIDATE action**: 真のCPCV検証
- **request-cpcv-validation**: Lisp→Rust ZMQ呼び出し
- **validate-for-s-rank-promotion**: S-RANK昇格時自動CPCV検証
- **基準**: Sharpe≥0.5, PF≥1.5, WR≥45%, MaxDD<15%, CPCV PASS

### 3. P9 OOS Validation (CPCV Lite)
- **A-RANK昇格検証**: 70/30分割でOOS検証
- **school-validation.lisp**: `validate-for-a-rank-promotion`
- **基準**: Sharpe≥0.3, PF≥1.2, WR≥40%, MaxDD<20%

---

## 📜 P8 Strategy Pipeline (V47.8)

### 入口統一 (2入口)
- **Founder**: 外部検証済み戦略 (Web/論文/Hunted/Legend)
- **Breeder**: 既存高性能戦略の交配

### add-to-kb関数
戦略追加の単一エントリーポイント。BT検証必須 (Sharpe ≥ 0.1)。

---

## 🎯 ランク体系 (B/A/S)

**B-RANK (Selection)**  
- 条件: Sharpe ≥ 0.1 / PF ≥ 1.0 / WR ≥ 30% / MaxDD < 30%  
- 判定: Phase 1 Backtest合格でBへ昇格  

**A-RANK (Pro)**  
- 条件: Sharpe ≥ 0.3 / PF ≥ 1.2 / WR ≥ 40% / MaxDD < 20% / OOS Sharpe ≥ 0.3  
- 判定: OOS検証 (CPCV Lite) 合格でAへ昇格  

**S-RANK (Verified Elite)**  
- 条件: Sharpe ≥ 0.5 / PF ≥ 1.5 / WR ≥ 45% / MaxDD < 15% / CPCV Median Sharpe ≥ 0.5  
- 判定: CPCV合格でSへ昇格 (ライブ実行許可)

**昇格フロー**  
`B → (OOS) → A → (CPCV) → S`

**関連コード**  
- `src/lisp/school/school-rank-system.lisp`  
- `src/lisp/school/school-validation.lisp`  

**確認方法**  
- `data/reports/evolution_factory_report.txt`  
- `data/reports/oos_status.txt`  
- Discord Reports チャンネル

OOS自動審査の稼働状況は Evolution Report と oos_status.txt で確認。

> [!IMPORTANT]
> **真実のソースはSQLite（data/memory/swimmy.db）**。  
> Libraryは派生スナップショット、In-memory KBはキャッシュとして扱う。

## 🧭 OOS Monitor (Ops)
- 監視ログ: `logs/oos_monitor.log`（JSONL）
- 直近サマリ: `data/reports/oos_monitor_status.txt`
- 定期実行: `swimmy-oos-monitor.timer`（10分毎）
- systemd user で動かす場合は `~/.config/systemd/user/` にユニットを配置して `systemctl --user enable --now swimmy-oos-monitor.timer`
- systemd system に移す場合は `systemd/swimmy-oos-monitor.service` と `systemd/swimmy-oos-monitor.timer` を `/etc/systemd/system/` に配置して `sudo systemctl daemon-reload && sudo systemctl enable --now swimmy-oos-monitor.timer`

## 🔗 ペア戦略 (Pair Strategy)

- **独立エンティティ**: `pair_strategies` テーブルで `pair_id/strategy_a/strategy_b/weight/評価指標/rank/last_updated` を保持。
- **選抜方式**: `*pair-slots-per-tf*` でシンボル×TFのペア上限、`*pair-competition-top-n*` で単一戦略と同列競争。
- **昇格ゲート**: A=OOS合成評価、S=CPCV合成評価。trade_list不足は昇格不可。
- **更新タイミング**: 毎日 00:10 の日次PnL集計後に `refresh-pair-strategies` → `refresh-pair-active-defs`。
- **実行反映**: `*pair-active-defs*` のみ overlay 適用（`*pair-strategy-enabled*` が有効時）。




## 🚀 起動 / 停止 (Systemd)

```bash
# 全サービス再起動 (推奨)
sudo systemctl restart swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog

# 停止
sudo systemctl stop swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog

# ステータス確認
sudo systemctl status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog

# リアルタイムログ監視
journalctl -f -u swimmy-brain -u swimmy-guardian -u swimmy-notifier -u swimmy-school -u swimmy-watchdog

# 🧬 進化状況モニター (Multi-Currency Visualizer)
./tools/monitor_evolution.sh
```

> [!IMPORTANT]
> **Watchdog自動復旧の注記 (2026-02-10)**  
> `swimmy-watchdog` は systemd(system) のサービスですが `User=swimmy` で動くため、権限がない環境では `systemctl restart swimmy-...` が `Interactive authentication required` で失敗します。  
> この場合でも systemd の `Restart=` を活かして復旧できるので、下の「PID kill で復旧」を使ってください（watchdogも同様）。
>
> ```bash
> # PID kill で復旧（sudo不要, systemdのRestart=で自動再起動させる）
> pid=$(systemctl show -p MainPID --value swimmy-brain);    [ "${pid:-0}" -gt 0 ] && kill -TERM "$pid"
> pid=$(systemctl show -p MainPID --value swimmy-guardian); [ "${pid:-0}" -gt 0 ] && kill -TERM "$pid"
> pid=$(systemctl show -p MainPID --value swimmy-watchdog); [ "${pid:-0}" -gt 0 ] && kill -TERM "$pid"
> 
> # 反映/復旧確認
> systemctl status swimmy-brain swimmy-guardian swimmy-watchdog --no-pager
> ```

> [!IMPORTANT]
> **運用注記（systemd状態と実稼働の不一致）**  
> この環境では **systemd --user が inactive でもプロセスが稼働している**ケースがあります。  
> そのまま再起動すると二重起動でポート競合が起きるため、**systemd状態と実稼働を必ず突き合わせてから操作**してください。
> 
> **正本**: systemd(system) を正本とし、`systemctl --user` は診断用途のみ。  
> **user unit 常用禁止**: 誤って起動した場合は停止・無効化してから system を再起動する。
> 
> 例（user unit の復旧）:
> ```bash
> systemctl --user stop swimmy-brain
> systemctl --user disable swimmy-brain
> sudo systemctl restart swimmy-brain
> ss -tulnp | rg "5555|5556|5581"
> ```
> 
> 判定フロー:
> 1. systemd状態を確認（system / user 両方）
> 2. プロセス稼働を確認
> 3. ポートlistenを確認
> 4. ログで稼働確認
> 
> 例:
> ```bash
> # 1) systemd状態（system / user 両方）
> sudo systemctl status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog
> systemctl --user status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog
> 
> # 2) プロセス実体
> ps aux | rg -i "sbcl|guardian|data_keeper|notifier|risk_gateway|backtest_service"
> 
> # 3) ポートlisten
> ss -tulnp | rg "5555|5556|5557|5559|5560|5561|5562|5563|5580|5581"
> 
> # 4) 直近ログ（system / user）
> sudo journalctl -n 50 -u swimmy-brain -u swimmy-guardian -u swimmy-school -u swimmy-data-keeper -u swimmy-notifier -u swimmy-backtest --no-pager
> journalctl --user -n 50 -u swimmy-brain -u swimmy-guardian -u swimmy-school -u swimmy-data-keeper -u swimmy-notifier -u swimmy-backtest --no-pager
> ```

---

## 🌊 Lisp Native Implementation (V46.0)

2026-01-19、システムは「完全なるLisp化」を達成しました。Pythonによるオーケストレーションは廃止されました。

### 1. Pure Lisp Daemon (swimmy-school)
進化・学習・最適化のループは、Systemd管理下のSBCLプロセス (`swimmy-school`) によって直接制御されます。
- **高速化**: Pythonインタプリタの起動オーバーヘッドがゼロになりました。
- **堅牢性**: Systemdがプロセスの死活監視を行い、クラッシュ時に自動復旧します。

### 2. Sharded File Persistence (The Great Library)
戦略データを単一の巨大ファイルではなく、戦略ごとに個別のファイル (`data/library/<rank>/<name>.lisp`) として管理します。
- **Incubator**: 生まれたて/テスト中
- **B**: Selection
- **A**: Pro
- **S**: Verified Elite
- **Legend**: 外部から招喚された英雄
- **Graveyard**: 敗者
- **Retired**: Max Age 退役アーカイブ（低ウェイト学習 / `data/memory/retired.sexp`）

### 2. 7つの市場状態 (Softmax Regime Detection V45.0)

単純な「トレンド/レンジ」ではなく、**確率的な市場認識**を行います。

**レジーム一覧:**
| レジーム | 説明 | 取引対象 |
|---------|------|---------|
| `trend-early` | トレンド初期 | trend, breakout |
| `trend-mature` | トレンド成熟 | trend, breakout |
| `trend-exhausted` | トレンド枯渇 | mean-reversion |
| `range-expansion` | レンジ拡大 | range, mean-reversion |
| `range-compression` | レンジ縮小 | breakout, trend |
| `volatile-spike` | ボラ急上昇 | ⚠️ LEGEND/scalp のみ |
| `illiquid` | 流動性枯渇 | ⚠️ LEGEND/scalp のみ |

**Softmax確率:**
- `detect-market-regime` は `(TrendMature: 0.8, RangeExpansion: 0.2)` のような確率ベクトルを返す
- 最高確率のレジームが `*current-regime*` にセットされる
- 複数レジームが近い確率の場合、コンフィデンスが下がる

**シンボル別レジーム (V45.0):**
- 各通貨ペアごとに独立したレジーム判定を実施
- `(gethash symbol *candle-histories*)` で通貨別履歴を使用
- 例: USDJPYがトレンド中でもEURUSDはレンジの可能性あり

**リスク対応:**
- `volatile-spike` / `illiquid` 時は全戦略スキャンではなく、LEGEND/scalp 戦略のみに制限
- 完全停止はせず、防御的トレードを許可（V45.0で修正）

### 3. ケリー基準による資金管理 (Kelly Criterion)
ロット数は固定ではなく、戦略の「優位性（Edge）」に基づいて動的に計算されます。
- `f* = p - q/b` （勝率とリスクリワード比から算出）
- **Safety**: ハーフケリーを採用し、最大リスクを口座資金の10%に制限。

### 4. ベイズ適応 (Bayesian Adaptation)
戦略の信頼度は、ベイズ統計（Beta分布）を用いて更新されます。
少ないトレード数でも統計的に正しい信頼区間を推定し、"使えない戦略"を早期に見切ります。

---

## 🔒 アトミック予約システム (Atomic Allocation)

V44.2で「椅子取りゲーム (Musical Chairs)」バグを修正しました。
戦略は、トレード判断の **計算前** にスロットを物理的に確保します。

**フロー:**
1. `try-reserve-warrior-slot` が空きスロットを探す。
2. 空いていれば **即時予約** (Pending登録)。
3. 確保できた場合のみ、AI予測などの計算に進む。
4. 最終的にトレードしなかった場合、予約は即時解除される。

これにより、Magic Numberの衝突は物理的に発生しません。

---

## 🛑 段階的リスク冷却 (Tiered Cooldown)

以前の「Circuit Breaker」「Resignation」は統合されました。
損失が出るたびに、冷却期間（休止時間）が段階的に長くなります。

**ティア構成:**
`3m → 5m → 10m → 15m → 30m → 45m → 1h → 2h → 3h → 4h → EOD (強制終了)`

- **ペナルティ**: 負けるたびにティアが 1 上がり、冷却時間が伸びる。
- **回復**: 勝つとティアが 1 下がる。
- **EOD**: 最大ティアに達すると、その日は強制終了（辞表提出）。

---

## 📋 システム構成

```
┌─────────────────────────────────────────────────────┐
│  MT5 EA (SwimmyBridge Ver 15.2)                     │
│  - 3通貨ペア (USDJPY, EURUSD, GBPUSD)              │
│  - Multi-TF History Support                         │
│  - Strategy Transparency (Comment Field)            │
├─────────────────────────────────────────────────────┤
│  GUARDIAN (Rust)        Port 5557/5559/5560         │
│  - MT5通信、注文執行、バックテスト、RiskGate        │
├─────────────────────────────────────────────────────┤
│  BRAIN (Lisp)           Port 5555/5556 (PUB)        │
│  - シグナル生成、学習、State Management             │
├─────────────────────────────────────────────────────┤
│  SCHOOL (Lisp)          Systemd Managed             │
│  - 進化、淘汰、リクルート (Daemon Native)           │
├─────────────────────────────────────────────────────┤
│  DATA KEEPER (Python)   Port 5561                   │
│  - 500k Candle Buffer / symbol / TF (Async Save)    │
└─────────────────────────────────────────────────────┘
```

| サービス | systemd name | 役割 |
|----------|--------------|------|
| Brain | `swimmy-brain` | シグナル生成、学習、進化 |
| Guardian | `swimmy-guardian` | MT5通信、注文執行、バックテスト |
| School | `swimmy-school` | **Hyper-Time Evolution (無限進化ループ)**, Purge, Wisdom |
| Data Keeper | `swimmy-data-keeper` | ヒストリカルデータ永続化 |
| Backtest | `swimmy-backtest` | BACKTEST専用ZMQ (5580/5581) |
| Risk Gateway | `swimmy-risk` | リスクチェック (5563) |
| Notifier | `swimmy-notifier` | Discord通知 (5562) |
| Evolution | `swimmy-evolution` | 進化デーモン (SBCL runner) |
| Watchdog | `swimmy-watchdog` | Brain/Guardianの生存監視 |

---

## 🧬 SRP モジュール構成 (V46.0)

`school-execution.lisp` もリファクタリングされ、全ファイルがSRP準拠（600行以下）です。

```
src/lisp/
├── school.lisp                    (Orchestrator - 33行)
├── school/
│   ├── school-daemon.lisp        (Systemd Entry Point)
│   ├── school-connector.lisp     (Evolution Loop)
│   ├── school-scout.lisp         (Recruitment)
│   ├── school-breeder.lisp       (Breeding/Wisdom)
│   ├── school-allocation.lisp    (Atomic Allocation)
│   ├── school-execution.lisp     (Atomic Execution)
│   ├── school-danger.lisp        (Tiered Cooldown)
│   └── ...
```

---

## 🛡️ EA (SwimmyBridge) Ver 15.2

MT5のExpertsフォルダにコピーし、コンパイルして適用してください。

**新機能:**
- ✅ マルチタイムフレーム履歴対応 (`REQ_HISTORY` + `tf` パラメータ)
- ✅ W1, D1, H4, H1, M30, M15, M5, M1 対応
- ✅ 戦略名 (Comment) 対応
- ✅ マルチカレンシー対応 (USDJPY, EURUSD, GBPUSD)
- ✅ コアZMQはS式（alist）を正本（ORDER_OPENは `instrument` + `side`）
- ✅ 補助サービス境界（Data Keeper 5561 / Notifier 5562 / Risk Gateway 5563）はS式（schema_version=1）

**重要:**
- `InpWSL_IP` は **空だと起動失敗**。MT5 EA の入力で必ず設定する。

**ログで確認:**
```
🚀 Swimmy Bridge Ver 15.2 - Multi-TF Support
📊 This EA handles: USDJPY only
✅ PUB connected to Guardian
✅ SUB connected to Guardian
```

---

## 🏁 Launch Checklist (Lisp Native)

```bash
# 1. Quality Gate 確認
./tools/quality_gate.sh

# 2. サービス状態確認
sudo systemctl status swimmy-school swimmy-brain

# 3. MT5 EA バージョン確認
# → EA ログで "Ver 15.2" を確認

# 4. ログ確認
journalctl --user -u swimmy-school -f
journalctl --user -u swimmy-brain -f
```

---

## 🚨 緊急時対応

```bash
# 1. ゾンビプロセス一掃
./tools/kill_zombies.sh

# 2. 正常に再起動
sudo systemctl restart swimmy-brain swimmy-guardian swimmy-data-keeper swimmy-school
```

---

## 🧭 Swimmy Philosophy

### 1. Speed = Survival
Rustのバックテスト速度（50,000本/ms）で「失敗を高速に捨てる」。

### 2. Hot Reloading = Adaptation
Lispでシステム停止なしにロジック修正可能。

### 3. SRP = Antifragility
細かいモジュール分割により、1箇所の障害が全体を殺さない。
## 🆕 V50.7 運用メモ (2026-02-10) - CPCV可視化と単一運用

### 1. CPCV status の見方
- `data/reports/cpcv_status.txt` 形式  
  `queued | sent | received | failed (send X / result Y: runtime R / criteria C) | inflight Z`
- **send** = 送信失敗（そもそも動いていない）
- **runtime** = 実行時エラー（実行はされたが処理失敗）
- **criteria** = 実行成功だが基準不合格（期待どおりの不合格）
- **inflight** = 送信済みで結果未達（処理中）

### 2. CPCV通知のFAIL区別
- DiscordのCPCV通知で `ERROR (runtime)` と `FAILED (criteria)` を明示

### 3. 進化ループの単一運用
- **推奨**: systemd `swimmy-school.service` を唯一の進化ループにする
- `tools/evolution_daemon.py` を同時起動しない（多重起動の原因）
- `tools/evolution_daemon.py` は `school-daemon.lisp` 稼働中に自動待機する（V50.7.1）
- 例外運用が必要な場合のみ `SWIMMY_ALLOW_PARALLEL_EVOLUTION=1` を設定
- 停止/再起動は `sudo systemctl start/stop swimmy-school` が必要
- 1行チェック: `ps aux | rg -i 'school-daemon\\.lisp|evolution_daemon\\.py|run_lisp_evolution\\.lisp'`
