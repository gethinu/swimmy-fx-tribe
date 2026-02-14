# Swimmy System Specification

## 1. 概要
MT5 (Execution), Rust (Middleware/Guardian), Lisp (Brain/Evolution) から構成される複合アルゴリズム取引システム。「進化する自律型ヘッジファンド」を目指す。
V50.6 (Structured Telemetry) に到達し、SQL永続化、サービス分離、論理的整合性チェックに加え、補助サービスS式統一とStructured Telemetry(JSONL)／ローカル保存S式化を備える。

## 2. コンポーネント構成
| コンポーネント | 言語 | 役割 | 責務 |
| :--- | :--- | :--- | :--- |
| **SwimmyBridge** | MQL5 (V15.2) | Execution Node | Tick送信、注文執行、口座情報フィード、DeadManSwitch監視 |
| **Guardian** | Rust (V15.x) | Middleware | MT5-Lisp間の通信仲介、Risk Gate (拒否権)、高速バックテスト、ニューラルネット予測 |
| **School/Brain** | Common Lisp (V50.6) | Cognitive Engine | 戦略の遺伝的進化(Breeding)、ポートフォリオ構築、物語生成、Discord通知 |
| **Data Keeper** | Python | Persistence Layer | Port 5561（REQ/REP, S式 + schema_version=1）。ヒストリカルデータ保存（**M1は最大 10M candles / symbol**、その他TFは最大 500k / symbol / TF）。ティック履歴も保存（VAP用途、GET_TICKS/ADD_TICK）。 |
| **Pattern Similarity Service** | Python | Analytics/Gating | チャートパターン画像化・埋め込み・近傍検索・確率返却（REQ/REP 5565, S式 + schema_version=1）。 |

## 3. 取引前提
- **銘柄**: USDJPY, EURUSD, GBPUSD (Config可変)。マルチカレンシー対応。
- **時間足**: M1 (保存/基礎), M5/M15 (M1リサンプル), H1/H4/D1/W1/MN1 (分析/レジーム/ゲート)
- **取引時間**: 24時間 (土日除く)。Guardianが土曜朝(06:50 JST)に強制全決済(Weekend Close)を行う。

## 4. 戦略仕様
- **KB (Knowledge Base)**: 戦略はSQLite (`swimmy.db`) とフラットファイル (`data/library/`) で管理。
- **ランク体系**: 
  - **Stage 1（固定閾値）**
  - **B-RANK**: Sharpe >= 0.15 / PF >= 1.05 / WR >= 35% / MaxDD < 25%
  - **A-RANK**: Sharpe >= 0.45 / PF >= 1.30 / WR >= 43% / MaxDD < 16%
  - **S-RANK**: Sharpe >= 0.75 / PF >= 1.70 / WR >= 50% / MaxDD < 10%
  - **Stage 2（検証ゲート）**
  - **A昇格**: OOS Sharpe >= 0.35 かつ コスト控除後 Expectancy > 0
  - **S昇格**: CPCV pass_rate >= 70% かつ CPCV median MaxDD < 12%
  - **共通（A/S昇格時）**: Monte Carlo `prob_ruin <= 2%` を満たし、DryRun 実測スリッページが運用上限以下であること
  - **Incubator**: Stage 1 の B-RANK 条件未満
  - **Legend**: 不老不死（外部導入戦略）
  - **Graveyard**: 廃棄戦略（失敗パターン分析用）
  - **Retired**: Max Age 退役アーカイブ（低ウェイト学習、`data/library/RETIRED/`・`data/memory/retired.sexp`）
- **進化**:
  - **Symbolic Hashing**: 論理的に同一な戦略を自動排除 (Jaccard Similarity)。
  - **Highlander Rule**: 類似戦略は強い方だけが生き残る。
  - **Engine of Life**: 親子対決(Deathmatch)による世代交代。
  - **選抜スコア (Selection Score)**: Sharpe + PF + WR + (1-MaxDD) を合成（重み: 0.4 / 0.25 / 0.2 / 0.15）。
  - **投票ウェイト**: `1.0 + 0.6*score` を `0.3–2.0` にクランプ。

## 4.5. Pattern Similarity Gate (Regime/Gate)
- **目的**: 類似チャートパターンの集合意識をレジーム/ゲートとして利用（既存戦略の前段フィルタ）。
- **モデル**: Phase 1 は `vector-fallback-v1`（OHLCV派生ベクトル埋め込み）を正本として実装。CLIP ViT-B/32 相当は Phase 2 以降の差し替え候補とし、`model` フィールドで実装モデル名を返す。
- **特徴量**: ローソク足画像 + ティック出来高 + 価格帯別出来高（VAP）。
- **検索**: 近傍探索は **距離重み付き確率** を返す（k=30、閾値=0.60）。
- **ゲート**: 不一致時はロットを **0.7倍** に減衰（ソフトゲート）。**ライブ/OOS/CPCV/バックテストに適用**。
- **適用範囲**: **TF一致のみ**（H1以上の足確定時に評価）。
- **ラベル**: ATR基準の Up/Down/Flat。評価幅はTFグループ別固定。
  - M5/M15: 4時間
  - H1/H4: 1日
  - D1: 1週間
  - W1/MN1: 1か月
- **ウィンドウ本数（画像生成）**:
  - H1: 120（5日）
  - H4: 120（20日）
  - D1: 120（約6か月）
  - W1: 104（2年）
  - MN1: 120（10年）
  - M5: 120（10時間）
  - M15: 120（30時間）
- **ストライド（サンプル間隔）**:
  - M5: 30分ごと（6本）
  - M15: 1時間ごと（4本）
  - H1/H4/D1/W1/MN1: 1本ごと
- **保存**: `data/patterns/` に npz + FAISS インデックスを保存。SQLiteはメタ情報のみ保持。
- **VAP生成**: MT5ティック由来で生成（**ヒストリカルはData Keeperにティック履歴保存**。スキーマ/取得APIはTBD）。
- **EV一貫性**: Gate は単体で売買判断を置き換えず、**コスト込み期待値(EV)最大化**の前段フィルタとして扱う。
  - `EV_net` が閾値未満なら No-Trade。Gate は低信頼時に `EV_threshold` を引き上げる、またはロットを減衰。
  - 学習/検証は **Purged CV + Embargo** と WFV を前提にレジーム耐性を確認する。

## 5. リスク管理 (Guardian & Lisp)
- **Risk Gate (Rust)**:
  - Daily Loss Limit (-¥5000), Max Risk Per Trade, Liquidity Check, Concentration Limit。
  - Dead Man's Switch (300秒無通信で全決済)。
- **Atomic Allocation (Lisp)**:
  - トレード計算前にスロットを物理確保し、Magic Number衝突を防止。
- **Tiered Cooldown (Lisp)**:
  - 損失が続くと冷却期間が段階的に伸びる (3m → ... → EOD)。
- **Toxic Feature Auditor (Lisp/Python)**:
  - XGBoostにより「毒入りパラメータ」を事前に検知・排除。

## 6. インターフェース仕様
- **ZeroMQ**: 全コンポーネント間の通信バックボーン。
  - MT5 (PUB 5557) -> Rust
  - Rust (PUB 5560) -> MT5
  - Rust (PUSH 5555) -> Lisp
  - Lisp (PUB 5556) -> Rust
  - External Command (PUB 5559) -> Rust (MCP/Tools)
  - Data Keeper (REQ/REP 5561, S式 + schema_version=1) <-> Lisp/Tools
  - Notifier (PUSH 5562, S式 + schema_version=1) -> Notifier Service
  - Risk Gateway (REQ/REP 5563, S式 + schema_version=1) <-> Lisp
  - Backtest Service (PUSH 5580 / PULL 5581, S式) <-> Lisp
  - Pattern Similarity (REQ/REP 5565, S式 + schema_version=1) <-> Lisp
- **Encoding**: 内部ZMQ＋補助サービス境界はS-expression（alist形式）に統一。**ZMQはS式のみでJSONは受理しない**。外部API境界（Discord/HTTP/MCP stdio）はJSONを維持。
- **Persistence**: 
  - **SQLite**: メタデータ、ランク、トレードログ。
  - **Daily PnL Aggregation**: `strategy_daily_pnl`（日次損益の集計テーブル）を正本として使用。
  - **Sharded Files**: 戦略本体 (S式)。
- **Local Storage (方針)**: `data/backtest_cache.sexp` / `data/system_metrics.sexp` / `.opus/live_status.sexp` を **S式のみ**で保存・参照する（tmp→renameで原子書き込み）。`backtest_cache/system_metrics` は `schema_version=1`、`live_status` は `schema_version=2`。`data/` と `db/data/` のJSON/JSONLはレガシー維持だが、**Structured Telemetry** は `/home/swimmy/swimmy/logs/swimmy.json.log` にJSONL出力（`log_type="telemetry"`）。
- **Pattern DB**: `data/patterns/` に埋め込み（npz）＋インデックス（FAISS）を保存。SQLiteはメタ情報のみ保持。

## 7. 実行制約・環境
- **OS**: Windows (MT5) + WSL2 (Rust/Lisp/Python)
- **Systemd**: コア4サービス（`swimmy-brain`, `swimmy-guardian`, `swimmy-school`, `swimmy-data-keeper`）＋補助（`swimmy-backtest`, `swimmy-risk`, `swimmy-notifier`, `swimmy-evolution`, `swimmy-watchdog`）。
- **Hot Reload**: `./tools/reload.sh` でLispプロセスを停止せずにコード更新可能。

## 8. 非要件
- **超高頻度取引 (HFT)**: ミリ秒単位の競争はしない。
- **完全自動復旧**: 重度障害時は人間による介入を許容。

## 9. 禁止事項 (NG)
- **未来参照**: バックテスト時のLookahead Bias。
- **テスト汚染**: テスト用データと本番データの混同。
- **Regime Lock無視**: レジームと不整合な戦略の強制稼働。
- **ZMQでバイナリ送信**: 画像などのバイナリは送らず、OHLCVのS式のみで連携する。

## 10. 未確定事項
- `libzmq.dll` のバージョン管理。
- ローカル保存S式の **schema_version 運用**（互換の範囲と破壊的変更の手順）。

## 11. Metrics & Reporting Logic (Reference)

Evolution Factory Reportなどのレポートで表示される指標の算出ロジック定義。
（Source: `src/lisp/school/school-narrative.lisp`）

### 11.1. Status Counts
- **Knowledge Base (Active)**: `get-db-rank-counts` の `:active`（= `total - graveyard - retired`）
  - DBを正としたアクティブ戦略数（KBはキャッシュ）。ドリフトは `school-db-stats` が検出。
- **New Recruits (24h)**: `(count-if (lambda (s) (> (strategy-creation-time s) (- now 86400))) ...)`
  - 過去24時間以内に生成された戦略数。
- **Graveyard**: `get-db-rank-counts` の `:graveyard`
  - 公式レポートはDBを正本。Libraryファイル数はドリフト検知に使用。
- **Retired**: `get-db-rank-counts` の `:retired`
  - Max Age退役。Libraryファイル数はドリフト検知に使用。

### 11.2. Rank Definitions & Criteria
ランク判定は `school-rank-system.lisp` および `school-validation.lisp` に準拠。

| Rank | Label | Criteria (AND条件) | Validation Gate |
| :--- | :--- | :--- | :--- |
| **S-Rank** | Verified Elite | Sharpe >= 0.75<br>PF >= 1.70<br>WR >= 50%<br>MaxDD < 10% | **CPCV**<br>- pass_rate >= 70%<br>- median MaxDD < 12%<br>- 共通ゲート（MC/DryRun）合格 |
| **A-Rank** | Pro | Sharpe >= 0.45<br>PF >= 1.30<br>WR >= 43%<br>MaxDD < 16% | **OOS**<br>- OOS Sharpe >= 0.35<br>- コスト控除後 Expectancy > 0<br>- 共通ゲート（MC/DryRun）合格 |
| **B-Rank** | Selection | Sharpe >= 0.15<br>PF >= 1.05<br>WR >= 35%<br>MaxDD < 25% | **Phase 1 Screening**<br>- Backtest (IS) Passed |
| **Incubator** | - | B-Rank条件未満 | (None) |
| **Retired** | Archive | Max Age 退役 / 明示退役 | (None) |

### 11.3. Persistence Logic
- **Sync**: レポート生成時に `(refresh-strategy-metrics-from-db :force t)` を実行し、DB上の値を正とする。
- **Upsert**: 戦略のメトリクス（Sharpe等）、ランク、ステータス変更時は即座に `upsert-strategy` でDB保存される。
- **Retired Patterns**: `:retired` への移動時に `data/memory/retired.sexp` へ低ウェイト学習用パターンを保存する。

### 11.4. Noncorrelation Score (Promotion Notification)
- **目的**: A/S昇格時にポートフォリオ全体の「非相関スコア」を算出しDiscord通知する。
- **入力**: `strategy_daily_pnl` を用いた日次PnL相関（Pearson）。
- **判定**: `|corr| < 0.2` を非相関とみなし、`uncorrelated_pairs / total_pairs` をスコア化。
- **注意**: 非相関は**ランクではなく指標**。単一戦略の並び・ランク体系は変更しない。

## 12. Optional: Polymarket OpenClaw (Prediction Market Autotrader)
Swimmy の FX コアとは独立に、Polymarket（Polygon）の予測市場へ自動エントリーする補助サブシステム。
ZMQ バックボーンは使わず、**HTTP API + ローカルファイル + systemd timer** で完結する（Guardian の Risk Gate 対象外）。

### 12.1. ゴール / 非ゴール
- **ゴール**: 定期サイクルで `signals -> market universe -> plan -> (optional) live execution -> report -> status` を生成し、問題は Discord に通知する。
- **非ゴール**: HFT、利益保証、FX コア（MT5/Rust/Lisp）との統合リスクゲート。

### 12.2. コンポーネント
- **Signal Sync**: `tools/openclaw_signal_sync.py`
  - systemd: `~/.config/systemd/user/swimmy-openclaw-signal-sync.timer`（5分ごと）
  - 出力: `data/openclaw/signals.jsonl` / `data/openclaw/signals.meta.json`（+ `signals.last_good.*`）
  - シグナル源: `POLYCLAW_OPENCLAW_CMD`（OpenClaw CLI/Bridge）またはヒューリスティック生成（`tools/openclaw_signal_heuristic.py`）
- **Cycle（計画/実行/レポート）**: `tools/run_polymarket_openclaw_service.py` → `tools/polymarket_openclaw_cycle.py`
  - systemd: `~/.config/systemd/user/swimmy-polymarket-openclaw.timer`（30分ごと）
  - 出力: `data/reports/polymarket_openclaw_live/`（`plan_*.json`, `execution_*.json`, `report_*.json`, `journal.jsonl`, `latest_status.json`, `status_history.jsonl`）
- **Execution（任意）**: `tools/polymarket_openclaw_execute.py`
  - 有効化: `POLYCLAW_LIVE_EXECUTION=1`（デフォルトは OFF）
  - 事前条件: Polymarket 側で USDC の Approve/Deposit を済ませる（Allowance が 0 の間は注文が失敗する）
- **Status Monitor**: `tools/polymarket_openclaw_status.py`
  - systemd(system): `/etc/systemd/system/swimmy-polymarket-openclaw-status.timer`（10分ごと）
  - 契約: cycle timer が無効なら `Health: disabled`（stale 誤検知を抑止）
- **Discord Notifier**: `tools/notifier.py`
  - systemd(system): `/etc/systemd/system/swimmy-notifier.service`
  - シークレット分離: `config/.env.systemd` の webhook のみ使用（`.env` の取引秘密情報を読まない）

### 12.3. 主なファイル/成果物
- **Signals（入力）**:
  - `data/openclaw/signals.jsonl`: JSONL（例: `{"market_id":"553865","p_yes":0.14,"confidence":0.66}`）
  - `data/openclaw/signals.meta.json`: 更新時刻、件数、ソース内訳、openclaw 実行コマンドの記録（トレーサビリティ）
- **Cycle（出力）**: `data/reports/polymarket_openclaw_live/`
  - `plan_<RUN_ID>.json`: エントリー候補（market_id/side/price/stake/EV）
  - `execution_<DATE>_<RUN_ID>.json`: 注文送信結果（sent/failed/orderID 等）
  - `report_<DATE>_<RUN_ID>.json`: 日次集計（stake/EV/realized）
  - `journal.jsonl`: 監査用の追記ログ（run_summary/entry など）
  - `latest_status.json`, `status_history.jsonl`: status 監視用スナップショット
- **Logs**: `logs/openclaw_signal_sync.log`, `logs/polymarket_openclaw_cycle.log`, `logs/polymarket_openclaw_status.log`

### 12.4. リスク/セーフティ（ガード）
- **ポジション/回数上限**: `POLYCLAW_MAX_OPEN_POSITIONS`, `POLYCLAW_MAX_DAILY_ENTRIES`
- **損失ガード**: `POLYCLAW_MAX_DAILY_LOSS_STREAK`, `POLYCLAW_MAX_DAILY_REALIZED_LOSS_USD`
- **品質フィルタ**: `POLYCLAW_MIN_LIQUIDITY_USD`, `POLYCLAW_MIN_VOLUME_USD`
- **シグナル鮮度**: `POLYCLAW_REQUIRE_FRESH_SIGNALS=1`, `POLYCLAW_MAX_SIGNAL_AGE_SECONDS`
- **重複防止**: `POLYCLAW_ALLOW_DUPLICATE_OPEN_MARKETS=0`（未解決マーケットの重複エントリーをブロック）

### 12.5. ウォレットモデル（Polymarket）
- 実行は Polygon（`chain_id=137`）を前提。
- Polymarket は EOA（署名者）と Proxy/Smart Wallet（funder）が分離することがある。
  - この場合 `POLYCLAW_LIVE_FUNDER=0x...` と `POLYCLAW_LIVE_SIGNATURE_TYPE` を適切に設定する。
- 秘密鍵は `.env` 直書きではなく `POLYCLAW_LIVE_PRIVATE_KEY_FILE`（例: `~/.secrets/polyclaw_live.key`）の利用を推奨する。
