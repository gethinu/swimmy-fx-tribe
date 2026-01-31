# Swimmy System State

## 現在の状態
- **稼働フェーズ**: V50.5 (System Hardening II) - 2026-01-28
- **Rust (Guardian)**: `V15.x` Full Duplex, Risk Gate実装済み。Daily Loss Limit: -¥5000。
- **MQL5 (Bridge)**: `V15.2` Multi-TF対応。
- **Lisp (Brain/School)**: `V50.5` Pure Lisp Daemon。SQLite移行完了。
- **Backtest Service**: Python ZMQサービス（`SWIMMY_BACKTEST_SERVICE=1` 時に有効）。BrainからのBACKTESTを 5580 で受信し、結果を 5581 で返却。
- **Lifecycle**: Rank（Incubator/B/A/S/Legend/Graveyard）が正義。Tierロジックは廃止（保存はRankディレクトリ）。
- **Graveyard 指標**: `data/library/GRAVEYARD/*.lisp` のファイル数が正義（学習用のsexp/DBは補助データ）。

## 決定事項
- **アーキテクチャ**: Rust Guardianを中心としたハブ＆スポーク。
- **永続化**: SQLite (`swimmy.db`) と Sharded Files (`data/library/`) のハイブリッド。
- **サービス管理**: Systemdによる4サービス体制。
- **運用**: ログはDiscordに集約。`./tools/monitor_evolution.sh` で状況確認。
- **Rank一本化**: ライフサイクル判断は Rank のみ。Tierは判断ロジックから除外（ディレクトリもRankへ移行）。
- **Graveyardの正**: 公式カウントはファイル数（`data/library/GRAVEYARD/*.lisp`）。

## 既知のバグ/課題
- **WSL IP**: MT5側の設定 (`InpWSL_IP`) が手動。
- **データ不整合**: MT5とLisp間のヒストリカルデータ差異。
- **再起動耐性**: Guardianのリスク状態 (`risk_state.json`) の永続化は実装済みだが、クラッシュ時の整合性は要監視。

## 直近の変更履歴
- **2026-01-31**: SBCLロード時WARNING/STYLE-WARNINGの解消（未定義関数/変数・ロード順・重複定義の整理）。
- **2026-01-31**: SBCLロード時STYLE-WARNING追加整理（未使用変数の無視/削除、LLM圧縮コンテキスト適用、Founders docstring登録）。
- **2026-01-31**: SBCLロード時の重複定義WARNING整理（`calculate-slope`/`format-duration` の衝突回避）。
- **2026-01-31**: SBCLロード時STYLE-WARNING追加整理（school-* の未使用変数/ハンドラ整理）。
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

## 次アクション
1. ドキュメント体系化（完了）
2. Data Keeperのスキーマ定義確認
3. 統合テスト手順の確立

## リスク
- **複雑性**: 3言語 + SQLite + Python(Data Keeper) の複合システム。
- **ネットワーク**: WSL-Windows間の通信遮断。
