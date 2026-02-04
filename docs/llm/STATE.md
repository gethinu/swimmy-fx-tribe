# Swimmy System State

## 現在の状態
- **稼働フェーズ**: V50.5 (System Hardening II) - 2026-01-28
- **Rust (Guardian)**: `V15.x` Full Duplex, Risk Gate実装済み。Daily Loss Limit: -¥5000。
- **MQL5 (Bridge)**: `V15.2` Multi-TF対応。
- **Lisp (Brain/School)**: `V50.5` Pure Lisp Daemon。SQLite移行完了。
- **MCP Gateway**: read-only/backtest-execのみ有効。trade-capableは封印（403固定）。
- **Backtest Service**: Python ZMQサービス（`SWIMMY_BACKTEST_SERVICE=1` 時に有効）。BrainからのBACKTESTを 5580 で受信し、結果を 5581 で返却。
- **Lifecycle**: Rank（Incubator/B/A/S/Legend/Graveyard）が正義。Tierロジックは廃止（保存はRankディレクトリ）。
- **Graveyard 指標**: `data/library/GRAVEYARD/*.lisp` のファイル数が正義（学習用のsexp/DBは補助データ）。

## 決定事項
- **アーキテクチャ**: Rust Guardianを中心としたハブ＆スポーク。
- **永続化**: SQLite (`swimmy.db`) と Sharded Files (`data/library/`) のハイブリッド。
- **サービス管理**: Systemdによるコア4サービス＋補助サービス体制。
- **System Audit**: `tools/system_audit.sh` を正本とし、systemd(system) を監査・自動修復（daemon-reload + enable + restart）。`DRY_RUN=1` で修復をスキップ。`SUDO_CMD` で sudo 実行方法を上書き可能（例: `SUDO_CMD="sudo"` で対話式許可）。`.env` を自動読み込みして Discord 設定を拾う。
- **運用**: ログはDiscordに集約。`./tools/monitor_evolution.sh` で状況確認。
- **Rank一本化**: ライフサイクル判断は Rank のみ。Tierは判断ロジックから除外（ディレクトリもRankへ移行）。
- **Graveyardの正**: 公式カウントはファイル数（`data/library/GRAVEYARD/*.lisp`）。
- **B案方針**: 内部ZMQ通信＋ローカル保存をS式へ統一。**内部ZMQはS式のみでJSONは受理しない**。外部API境界はJSON維持。**ローカル保存はS式即時単独（backtest_cache/system_metrics/live_statusを .sexp に統一）**。
- **MT5プロトコル**: Brain→MT5 は S式を正本（ORDER_OPEN は `instrument` + `side`）。
- **Backtest Phase方針**: Phase1=2011-2020、Phase2=2021-CSV末尾(rolling end_time)。Backtest要求に `phase`/`range_id` と `start_time`/`end_time` を含める。Evolution Reportに Phase2 end_time を明記する。
- **Backtest Option表現**: `Option<T>` は空/1要素リストを正本（例: `(timeframe 1)` / `(timeframe)`）。

## 既知のバグ/課題
- **WSL IP**: MT5側の設定 (`InpWSL_IP`) は **空がデフォルト**。手動指定が必須。
- **Backtest Status**: `data/reports/backtest_status.txt` の `last_request_id` 監視。
- **Backtest Debug**: `SWIMMY_BACKTEST_DEBUG_RECV=1` で受信状況を `data/reports/backtest_debug.log` に追記（内部ZMQはS式のみのため、Backtest Serviceの戻りもS式であることが前提）。
- **Backtest Option値**: Guardian `--backtest-only` の `Option<T>` は「空/1要素リスト」で表現（例: `(data_id "USDJPY_M1")` / `(data_id)` / `(start_time 1700000000)`）。
- **データ不整合**: MT5とLisp間のヒストリカルデータ差異。
- **再起動耐性**: Guardianのリスク状態 (`risk_state.json`) の永続化は実装済みだが、クラッシュ時の整合性は要監視。
- **メモリ**: `load-graveyard-cache` はデフォルトのSBCLヒープで枯渇する場合がある（診断時は `--dynamic-space-size 2048` 以上を推奨）。

## 直近の変更履歴
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
1. ドキュメント体系化（完了）
2. Data Keeperのスキーマ定義確認
3. 統合テスト手順の確立
4. ローカル保存S式化の移行設計（atomic write / Python S式パーサ / 移行スクリプト）

## リスク
- **複雑性**: 3言語 + SQLite + Python(Data Keeper) の複合システム。
- **ネットワーク**: WSL-Windows間の通信遮断。
