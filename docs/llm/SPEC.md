# Swimmy System Specification

## 1. 概要
MT5 (Execution), Rust (Middleware/Guardian), Lisp (Brain/Evolution) から構成される複合アルゴリズム取引システム。「進化する自律型ヘッジファンド」を目指す。
V50.5 (System Hardening II) に到達し、SQL永続化、サービス分離、論理的整合性チェックを備える。

## 2. コンポーネント構成
| コンポーネント | 言語 | 役割 | 責務 |
| :--- | :--- | :--- | :--- |
| **SwimmyBridge** | MQL5 (V15.2) | Execution Node | Tick送信、注文執行、口座情報フィード、DeadManSwitch監視 |
| **Guardian** | Rust (V15.x) | Middleware | MT5-Lisp間の通信仲介、Risk Gate (拒否権)、高速バックテスト、ニューラルネット予測 |
| **School/Brain** | Common Lisp (V50.5) | Cognitive Engine | 戦略の遺伝的進化(Breeding)、ポートフォリオ構築、物語生成、Discord通知 |
| **Data Keeper** | Python | Persistence Layer | Port 5561。非同期でのヒストリカルデータ保存（10M Candle Buffer）。 |

## 3. 取引前提
- **銘柄**: USDJPY, EURUSD, GBPUSD (Config可変)。マルチカレンシー対応。
- **時間足**: M1 (基本), H1/D1 (分析用)
- **取引時間**: 24時間 (土日除く)。Guardianが土曜朝(06:50 JST)に強制全決済(Weekend Close)を行う。

## 4. 戦略仕様
- **KB (Knowledge Base)**: 戦略はSQLite (`swimmy.db`) とフラットファイル (`data/library/`) で管理。
- **ランク体系**: 
  - **Incubator/B-RANK**: Sharpe ≥ 0.1
  - **A-RANK**: Sharpe ≥ 0.3 + OOS検証合格
  - **S-RANK**: Sharpe ≥ 0.5 + CPCV検証合格（実弾許可）
  - **Legend**: 不老不死（外部導入戦略）
  - **Graveyard**: 廃棄戦略（失敗パターン分析用）
- **進化**:
  - **Symbolic Hashing**: 論理的に同一な戦略を自動排除 (Jaccard Similarity)。
  - **Highlander Rule**: 類似戦略は強い方だけが生き残る。
  - **Engine of Life**: 親子対決(Deathmatch)による世代交代。

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
  - Data Keeper (PULL 5561) <- Rust/Lisp
- **Encoding**: 内部ZMQはS-expression（alist形式）に統一。**内部ZMQはS式のみでJSONは受理しない**。外部API境界はJSONを維持。
- **Persistence**: 
  - **SQLite**: メタデータ、ランク、トレードログ。
  - **Sharded Files**: 戦略本体 (S式)。
- **Local Storage (方針)**: `data/backtest_cache.sexp` / `data/system_metrics.sexp` / `.opus/live_status.sexp` を **S式のみ**で保存・参照する（JSONは移行後に読まない）。`data/` と `db/data/` のJSON/JSONLは当面維持。

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

## 10. 未確定事項
- `libzmq.dll` のバージョン管理。
- Data Keeperの具体的なデータスキーマ詳細。
- ローカル保存S式の **schema_version 運用**（互換の範囲と破壊的変更の手順）。

## 11. Metrics & Reporting Logic (Reference)

Evolution Factory Reportなどのレポートで表示される指標の算出ロジック定義。
（Source: `src/lisp/school/school-narrative.lisp`）

### 11.1. Status Counts
- **Knowledge Base (Active)**: `(length *strategy-knowledge-base*)`
  - メモリ上にロードされている全戦略数。
- **New Recruits (24h)**: `(count-if (lambda (s) (> (strategy-creation-time s) (- now 86400))) ...)`
  - 過去24時間以内に生成された戦略数。
- **Graveyard**: `(length (directory "data/library/GRAVEYARD/*.lisp"))`
  - 物理的に墓場フォルダに存在するファイル数。

### 11.2. Rank Definitions & Criteria
ランク判定は `school-rank-system.lisp` および `school-validation.lisp` に準拠。

| Rank | Label | Criteria (AND条件) | Validation Gate |
| :--- | :--- | :--- | :--- |
| **S-Rank** | Verified Elite | Sharpe ≥ 0.5<br>PF ≥ 1.5<br>WR ≥ 45%<br>MaxDD < 15% | **CPCV** (Combinatorial Purged Cross-Validation)<br>- Median Sharpe ≥ 0.5<br>- Pass Rate ≥ 50% |
| **A-Rank** | Pro | Sharpe ≥ 0.3<br>PF ≥ 1.2<br>WR ≥ 40%<br>MaxDD < 20% | **OOS** (Out-of-Sample)<br>- OOS Sharpe ≥ 0.3 |
| **B-Rank** | Selection | Sharpe ≥ 0.1<br>PF ≥ 1.0<br>WR ≥ 30%<br>MaxDD < 30% | **Phase 1 Screening**<br>- Backtest (IS) Passed |
| **Incubator** | - | Sharpe < 0.1 | (None) |

### 11.3. Persistence Logic
- **Sync**: レポート生成時に `(refresh-strategy-metrics-from-db :force t)` を実行し、DB上の値を正とする。
- **Upsert**: 戦略のメトリクス（Sharpe等）、ランク、ステータス変更時は即座に `upsert-strategy` でDB保存される。
