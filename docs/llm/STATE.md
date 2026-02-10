# Swimmy System State

## 現在の状態
- **稼働フェーズ**: V50.6 (Structured Telemetry) - 2026-02-03
- **Rust (Guardian)**: `V15.x` Full Duplex, Risk Gate実装済み。Daily Loss Limit: -¥5000。
- **MQL5 (Bridge)**: `V15.2` Multi-TF対応。
- **Lisp (Brain/School)**: `V50.6` Pure Lisp Daemon。SQLite移行完了。
- **MCP Gateway**: read-only/backtest-execのみ有効。trade-capableは封印（403固定）。
- **Backtest Service**: Python ZMQサービス（`SWIMMY_BACKTEST_SERVICE=1` 時に有効）。BrainからのBACKTESTを 5580 で受信し、結果を 5581 で返却。
- **Lifecycle**: Rank（Incubator/B/A/S/Legend/Graveyard/Retired）が正義。Tierロジックは廃止（保存はRankディレクトリ）。
- **Aux Services**: Data Keeper / Notifier / Risk Gateway は **S式 + schema_version=1** のみ受理（ZMQでJSONは受理しない）。
- **Structured Telemetry**: `/home/swimmy/swimmy/logs/swimmy.json.log` にJSONL統合（`log_type="telemetry"`、10MBローテ）。
- **Local Storage**: `data/backtest_cache.sexp` / `data/system_metrics.sexp` / `.opus/live_status.sexp` をS式で原子保存（tmp→rename）。`backtest_cache/system_metrics` は `schema_version=1`、`live_status` は `schema_version=2`。
- **Daily PnL Aggregation**: `strategy_daily_pnl` を日次集計（00:10 JST）、非相関スコア計算に使用。
- **Graveyard/Retired 指標**: Evolution Report は DB（`get-db-rank-counts`）を正本、Libraryファイル数はドリフト検知に使用。
- **Notifications**: Max Age Retirement と Stagnant C-Rank の `Strategy Soft-Killed (Cooldown)` は個別通知を抑制し、**1時間ごと**に「合計件数＋上位5件名」でサマリ送信。

## 決定事項
- **アーキテクチャ**: Rust Guardianを中心としたハブ＆スポーク。
- **永続化**: SQLite (`swimmy.db`) と Sharded Files (`data/library/`) のハイブリッド。
- **サービス管理**: Systemdによるコア4サービス＋補助サービス体制。
- **System Audit**: `tools/system_audit.sh` を正本とし、systemd(system) を監査・自動修復（daemon-reload + enable + restart）。`DRY_RUN=1` で修復をスキップ。`SUDO_CMD` で sudo 実行方法を上書き可能（例: `SUDO_CMD="sudo"` で対話式許可）。`.env` を自動読み込みして Discord 設定を拾う。
- **運用**: ログはDiscordに集約。`./tools/monitor_evolution.sh` で状況確認。
- **運用（Brain起動）**: `swimmy-brain` は systemd 経由のみで起動する。cron watchdog `tools/check_guardian_health.sh` が **systemd MainPID 以外**の `/home/swimmy/swimmy/run.sh` を自動停止する（MainPID が取得できない場合は `run.sh` を全停止）。
- **運用（systemd正本）**: systemd(system) を正本とし、systemctl --user は診断用途のみ。
- **Rank一本化**: ライフサイクル判断は Rank のみ。Tierは判断ロジックから除外（ディレクトリもRankへ移行）。
- **選抜/投票スコア**: Selection Score は Sharpe + PF + WR + (1-MaxDD) を合成（重み: 0.4 / 0.25 / 0.2 / 0.15）。投票ウェイトは `1.0 + 0.6*score` を `0.3–2.0` にクランプ。
- **S判定ルール**: **IS Sharpe ≥ 0.5** を必須とし、**PF/WR/MaxDDはCPCV中央値**（`median_pf/median_wr/median_maxdd`）で最終判定する。CPCV gateは `median_sharpe ≥ 0.5` と `pass_rate ≥ 50%` を含む。
- **Graveyard/Retiredの正**: Evolution ReportはDB、Libraryはドリフト検知の正本（`data/library/GRAVEYARD/*.lisp` / `RETIRED/*.lisp`）。
- **B案方針**: 内部ZMQ＋補助サービス境界をS式へ統一。**ZMQはS式のみでJSONは受理しない**。外部API境界はJSON維持。**ローカル保存はS式即時単独（backtest_cache/system_metrics/live_statusを .sexp に統一）**。Structured TelemetryはJSONLログに集約。
- **MT5プロトコル**: Brain→MT5 は S式を正本（ORDER_OPEN は `instrument` + `side`）。
- **Pattern Similarity Service**: 5564(REQ/REP) で **S式のみ**受理。QUERY入力はOHLCVのS式、画像生成はサービス側（バイナリ送信は禁止）。
- **Pattern DB**: `data/patterns/` に npz + FAISS を保存、SQLiteはメタ情報のみ。
- **時間足データ方針**: M1は **10M candles/シンボル** 保存。M5/M15はM1からリサンプル。H1/H4/D1/W1/MN1は直取得。
- **Pattern Gate**: H1以上の足確定時に評価、TF一致のみ適用。距離重み確率（k=30 / 閾値0.60）で **ロット0.7倍**のソフトゲート。**ライブ/OOS/CPCV/バックテストに適用**。
- **ラベル評価幅**: M5/M15=4時間、H1/H4=1日、D1=1週間、W1/MN1=1か月（ATR基準のUp/Down/Flat）。
- **サンプルストライド**: M5=30分(6本)、M15=1時間(4本)、H1/H4/D1/W1/MN1=1本。
- **画像ウィンドウ本数**: M5=120、M15=120、H1=120、H4=120、D1=120、W1=104、MN1=120。
- **VAP**: 価格帯別出来高はMT5ティック由来で生成（**ヒストリカルはData Keeperにティック履歴保存**）。
- **Tick API**: Data Keeper に `GET_TICKS` / `ADD_TICK` を追加し、VAP用のティック履歴を取得できるようにする。
- **Backtest Phase方針**: Phase1=2011-2020、Phase2=2021-CSV末尾(rolling end_time)。Backtest要求に `phase`/`range_id` と `start_time`/`end_time` を含める。Evolution Reportに Phase2 end_time を明記する。
- **Startup Liveness**: Brain起動時は「受信ループ（Backtest結果/Guardian stream）」へ先に入る。重い一括処理（Deferred Backtest Flush）は **後回し＋レート制限**で段階的に実行する（起動で詰まらせない）。`SWIMMY_DEFERRED_FLUSH_BATCH`（1回の送信上限、0で無効）と `SWIMMY_DEFERRED_FLUSH_INTERVAL_SEC`（実行間隔秒）で調整する。
- **OOS Queue Startup Cleanup**: 起動時に `oos_queue` を全クリアし、OOS再キューは通常の検証ループで再投入する（古い request_id を残さない）。
- **Brain Bootstrap**: `run.sh` は `brain.lisp` を優先し、欠落時はASDF直起動へフォールバックする。
- **Backtest Option表現**: `Option<T>` は空/1要素リストを正本（例: `(timeframe 1)` / `(timeframe)`）。
- **Backtest Result Contract**: `BACKTEST_RESULT` は常に `request_id` を含める（相関/キュー整合のため必須）。
- **Backtest Throttle**: `SWIMMY_BACKTEST_MAX_PENDING` と `SWIMMY_BACKTEST_RATE_LIMIT` で送信抑制。`pending = submit - recv` が上限超過で送信停止。
- **Deferred Flush 制御**: `SWIMMY_DEFERRED_FLUSH_BATCH` で1回のフラッシュ数を制限し、`SWIMMY_DEFERRED_FLUSH_INTERVAL_SEC` でフラッシュ間隔を制御（0は無制限/即時）。
- **Backtest CSV Override**: `SWIMMY_BACKTEST_CSV_OVERRIDE` が設定されている場合、Backtestの `candles_file` は指定パスを優先する。
- **Backtest CSV Override運用例**: `SWIMMY_BACKTEST_CSV_OVERRIDE=/path/to/USDJPY_M1_light.csv` で軽量CSVに差し替える。
- **非相関スコア通知**: A/S昇格時に日次PnL相関から「非相関スコア」を算出しDiscord通知。**非相関はランクではなくポートフォリオ指標**（単一戦略の並びは維持）。
- **Pair Strategy (実装済)**: ペアを**独立エンティティ**としてDB永続化し、選抜ペアのみ overlay に反映する。`pair_strategies` テーブルで `pair_id/構成A/B/weight/評価指標/rank/last_updated` を保持し、`trade_logs` の `pair_id` を引き続き利用する。選抜は **ハイブリッド枠**（シンボル×TFごとに `*pair-slots-per-tf*` 上限、同一ランキングで単一と競争）で実施。昇格ゲートは**OOS合成評価（A）**と**CPCV合成評価（S）**を必須とし、trade_list不足時は昇格不可。trade_listは OOS/CPCV/Backtest の履歴から合成し、CPCVは `CPCV_RESULT` の trade_list 永続化を前提にする。日次 00:10 の PnL 集計後に `refresh-pair-strategies` → `refresh-pair-active-defs` を実行し、DB選抜されたペアのみ `*pair-active-defs*` に反映する。

## 既知のバグ/課題
- **WSL IP**: MT5側の設定 (`InpWSL_IP`) は **空がデフォルト**。手動指定が必須。
- **Backtest Status**: `data/reports/backtest_status.txt` の `last_request_id` と `pending` を監視。
- **Backtest Debug**: `SWIMMY_BACKTEST_DEBUG_RECV=1` で受信状況を `data/reports/backtest_debug.log` に追記（内部ZMQはS式のみのため、Backtest Serviceの戻りもS式であることが前提）。
- **Backtest Option値**: Guardian `--backtest-only` の `Option<T>` は「空/1要素リスト」で表現（例: `(data_id "USDJPY_M1")` / `(data_id)` / `(start_time 1700000000)`）。
- **データ不整合**: MT5とLisp間のヒストリカルデータ差異。
- **再起動耐性**: Guardianのリスク状態 (`risk_state.json`) の永続化は実装済みだが、クラッシュ時の整合性は要監視。
- **メモリ**: `load-graveyard-cache` はデフォルトのSBCLヒープで枯渇する場合がある（診断時は `--dynamic-space-size 2048` 以上を推奨）。
- **レポート手動更新**: `tools/ops/finalize_rank_report.sh` は `tools/sbcl_env.sh` を読み込み、`SWIMMY_SBCL_DYNAMIC_SPACE_MB`（未指定時 4096MB）で `finalize_rank_report.lisp` を実行する。

## 直近の変更履歴
- **2026-02-06**: `strategy_daily_pnl` の日次集計と 00:10 JST スケジュールを追加。日次PnL相関（Pearson）と非相関スコア通知（A/S昇格時）を実装。
- **2026-02-06**: Retired Rank を追加（Max Age退役、`data/library/RETIRED/`・`data/memory/retired.sexp`）。Evolution Report/DB集計に Retired を追加。
- **2026-02-06**: Pair-Composite の設計を確定し、`trade_logs` に `pair_id`、`backtest_trade_logs` を追加。`BACKTEST_RESULT` の `trade_list` を永続化し、`trades` は件数のまま維持。PnL系列は OOS/CPCV/Backtest を結合、`oos_kind` は `-OOS`=OOS / `-QUAL/-RR`=BACKTEST(IS) とする方針を明記（ペア選定/スコアは未実装）。候補母集団は **シンボル×TFごとの上位N=50** とする方針を追加。
- **2026-02-07**: ペア戦略の永続化・OOS/CPCV合成ゲート・ハイブリッド枠選抜・日次 00:10 スケジュール連携を実装。`pair_strategies` 保存、`CPCV_RESULT` trade_list 永続化、`refresh-pair-strategies`/`refresh-pair-active-defs` による選抜更新が稼働。
- **2026-02-06**: 起動時に `oos_queue` を全クリアし、古い request_id が残らないようにする方針を明記。
- **2026-02-06**: `BACKTEST_RESULT` の `request_id` を必須とする契約を明記。
- **2026-02-05**: 補助サービス（Data Keeper / Risk Gateway / Notifier）ZMQをS式のみに統一（legacy JSON廃止、`schema_version=1` 必須）。
- **2026-02-05**: Notifier の `payload`（S式）を受理し、`payload_json` 互換をINTERFACESに明記。
- **2026-02-05**: SBCLロード時WARNING/STYLE-WARNINGの全解消（ロード順、export、未使用変数、廃止フックの整理）。
- **2026-02-05**: 補助サービス（Data Keeper / Risk Gateway / Notifier）のS式スキーマをINTERFACESに定義（`schema_version=1`）。
- **2026-02-03**: Structured Telemetry を導入（JSONLイベントログ統合、`log_type="telemetry"`）。`system_metrics.sexp` / `live_status.sexp` を原子書き込みに統一。
- **2026-02-04**: `run.sh` の Brain 起動は `brain.lisp` 優先、欠落時はASDF直起動フォールバックの方針を明記。
- **2026-02-04**: Backtestのpending/レート制御とDeferred Flush制御パラメータをSTATEに明記。
- **2026-02-04**: `SWIMMY_BACKTEST_CSV_OVERRIDE` でBacktestのCSV差し替えを許可する方針を明記。
- **2026-02-04**: Backtest V2の`Option`表現（`timeframe`等）は空/1要素リストが正本と明記。
- **2026-02-04**: `tools/system_audit.sh` と `tools/test_notifier_direct.py` で `.env` を自動読み込みする運用に統一。
- **2026-02-04**: `tools/system_audit.sh` に `SUDO_CMD` で sudo 実行方法を上書きできる運用を追加（非対話 `sudo -n` 既定、必要時は対話式 `sudo`）。
- **2026-02-04**: MT5コマンド送信をS式に統一（ORDER_OPENは `instrument`/`side`）。`InpWSL_IP` のデフォルトを空に変更。
- **2026-02-04**: `backtest_status.txt` に `last_request_id` を追加。
- **2026-02-04**: MCPの `request_id` を JSON-RPC id/明示指定から引き回すよう統一。
- **2026-02-04**: `tools/dashboard.py` は systemd(system) の状態を正として表示する方針に統一。
- **2026-02-03**: REQ_HISTORY の要求キーを `count` に統一（`volume` は廃止）。
- **2026-02-03**: MT5系メッセージ（HISTORY/POSITIONS/SWAP_DATA/ORDER_ACK/TRADE_CLOSED）と管理コマンド（GET_POSITIONS/GET_SWAP/CLOSE_SHORT_TF）をINTERFACESに明記。
- **2026-02-04**: 内部ZMQはS式のみ（JSON受理しない）に厳格化。ORDER_OPENは`instrument/side/lot`に固定。
- **2026-02-01**: systemdの `swimmy-guardian` をRust Guardian実行に修正し、evolution daemonのSBCLメモリ設定を `SWIMMY_SBCL_DYNAMIC_SPACE_MB` に統一。watchdogのsystemdユニットを追加。
- **2026-02-01**: owners guide / runbook のsystemd操作をsystemレベル（`sudo systemctl ...`）に一本化。`.env` の読み込み方法を修正してコメント行エラーを解消。
- **2026-02-01**: Backtest ServiceのS式戦略名抽出正規表現を修正（`re.error: unbalanced parenthesis` 対策）。
- **2026-02-01**: monolith `swimmy.service` をrepoから削除（systemdコア4サービス構成へ完全移行）。
- **2026-02-01**: systemdのsystemレベル正本ユニット（brain/guardian/school/data-keeper/backtest/risk/notifier/evolution/watchdog）をrepo側に整備し、`User=swimmy`/絶対パス指定で統一。
- **2026-02-01**: arXiv Scout の `last_sent` をリセットし、手動通知を実行。
- **2026-01-31**: SBCLロード時WARNING/STYLE-WARNINGの解消（未定義関数/変数・ロード順・重複定義の整理）。
- **2026-01-31**: SBCLロード時STYLE-WARNING追加整理（未使用変数の無視/削除、LLM圧縮コンテキスト適用、Founders docstring登録）。
- **2026-01-31**: SBCLロード時の重複定義WARNING整理（`calculate-slope`/`format-duration` の衝突回避）。
- **2026-01-31**: SBCLロード時STYLE-WARNING追加整理（school-* の未使用変数/ハンドラ整理）。
- **2026-02-01**: 依存ライブラリ更新（Quicklisp dist 2026-01-01）と警告対策（pzmq ASDF命名、cl-sqlite CFFI struct参照、ironclad/postmodernのdefgeneric再定義ガード）。
- **2026-02-01**: school-breeder の括弧不整合を修正（ロード時COMPILE-FILEエラー解消）。
- **2026-02-01**: Quicklisp dist側のパッチを撤回し、`local-projects` のパッチ版を優先利用する構成に整理。
- **2026-02-01**: SBCL起動オプションを `SWIMMY_SBCL_DYNAMIC_SPACE_MB` に統一（run.sh / tools / systemd）。
- **2026-02-01**: Quicklisp local-projects固定化スクリプトとパッチを追加（`tools/setup_quicklisp_local_projects.sh`）。
- **2026-02-01**: Quicklispパッチをunified diff形式に整形し、`patch` 適用エラーを解消。
- **2026-02-01**: SBCLサニティチェックを追加（`tools/sbcl_sanity_check.sh`）。
- **2026-02-01**: V3.0「61-STRATEGY SIGNAL SYSTEM」を `tools/restore_legend_61.lisp` で再登録（59本追加、2本は重複判定で除外）。弱体Legendは `archive-weak-legends` で LEGEND-ARCHIVE へ退避。
- **2026-02-01**: Legend保護テストスクリプト `tools/test_legend_protection.lisp` 追加（Fortress/ensure-rank を検証）。
- **2026-02-01**: Legend再検証フラグ `strategy-revalidation-pending` を追加し、起動時に `queue-legend-revalidation` を自動実行。`restore_legend_61.lisp` を swimmy-school の `ExecStartPre` に組み込み、LEGEND-ARCHIVE もライブラリロード対象に追加。
- **2026-01-31**: arXiv Scout 通知に S/A/B/C の優先度ラベルを追加（スコア閾値: S=9-10, A=8, B=6-7, C<=5）。
- **2026-01-31**: arXiv Scout の手動再送（`last_sent` リセット → `--daily` 実行）を実施。
- **2026-01-31**: arXiv Scout の `last_sent` をリセットして手動再送を実施。
- **2026-01-31**: `SWIMMY_GEMINI_API_KEY` を `/home/swimmy/swimmy/.env` に更新（ユーザー対応）。
- **2026-01-31**: arXiv Scout デーモンを再起動（Geminiキー反映）。
- **2026-01-31**: Backtest Service プロトコルを **S式に統一**（JSON要求は廃止）。Brain/School→Backtest Service→Guardian 間はS式でやり取り。
- **2026-01-31**: arXiv Scout のDiscord Webhookを更新（通知先切替）。
- **2026-01-31**: `SYSTEM_COMMAND: HEARTBEAT_NOW` を追加（ZMQ 5555経由でDiscord Heartbeatを即時送信可能に）。
- **2026-01-31**: `swimmy-school.service` を enable/active に復帰（4サービス体制を再確立）。
- **2026-01-31**: ZMQポートを `SWIMMY_PORT_*` で上書き可能に統一。Backtest Service は `SWIMMY_BACKTEST_SERVICE` で有効化。
- **2026-01-31**: Data Keeper のsystemdユニット名を `swimmy-data-keeper` に統一。
- **2026-01-31**: ZMQ bind endpointのフォーマットを修正（`tcp://*:<port>` の生成不備を解消）。
- **2026-01-31**: `tools/deep_audit.lisp` でDB/KB初期化後に比較するよう調整。
- **2026-01-31**: V2 Screeningの欠損リンク修正とGraveyard照合の正規化を反映（Rank一本化に合わせた接続）。
- **2026-01-31**: Rankディレクトリ移行ツール追加（`tools/migrate_library_to_rank.py`）。
- **V50.5.1**: Evolution Factory Reportのメトリクス同期修正 (DBからの強制リロードを追加)。
- **V50.5**: Symbolic Hashing (論理重複排除), Service Isolation (Scribe), Broken Arrow Watchdog。
- **V50.2**: Engine of Life (親子対決進化)。
- **V49.8**: SQL Migration (KBのSQLite化)。
- **V49.4**: Hot Reloading, Regime Hard Lock。

## Legend運用メモ (2026-02-01)
- V3.0「61-STRATEGY SIGNAL SYSTEM」を初期化時に自動復元＆再検証キュー投入（`initialize-system` / `tools/restore_legend_61.lisp`）。重複はHighlanderで排除。
- 再検証完了まで `strategy-revalidation-pending` を立て、Breedingから除外。BACKTEST_RESULT 適用時に自動クリア。
- LEGENDは墓場送りをブロック。弱いLegendは `archive-weak-legends` で `:legend-archive` として退避可能（`data/library/LEGEND-ARCHIVE/`）。LEGEND-ARCHIVE もロード対象に追加。
- `swimmy-school.service` の `ExecStartPre` で `tools/restore_legend_61.lisp` を実行し、起動時に Legends を自動復元＆再検証フラグセット（BACKTESTは本起動後に送信）。
- Breeding時のLegend占有率を各カテゴリ上位5本に制限、遺伝的距離しきい値を0.35へ強化。
- 保護テストは `tools/test_legend_protection.lisp`（CIでも実行）。

## 次アクション
1. ドキュメント体系化（進行中）
2. 統合テスト手順の確立

## リスク
- **複雑性**: 3言語 + SQLite + Python(Data Keeper) の複合システム。
- **ネットワーク**: WSL-Windows間の通信遮断。
