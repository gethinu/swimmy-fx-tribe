# Swimmy System State

## 現在の状態
- **稼働フェーズ**: V50.5 (System Hardening II) - 2026-01-28
- **Rust (Guardian)**: `V15.x` Full Duplex, Risk Gate実装済み。Daily Loss Limit: -¥5000。
- **MQL5 (Bridge)**: `V15.2` Multi-TF対応。
- **Lisp (Brain/School)**: `V50.5` Pure Lisp Daemon。SQLite移行完了。

## 決定事項
- **アーキテクチャ**: Rust Guardianを中心としたハブ＆スポーク。
- **永続化**: SQLite (`swimmy.db`) と Sharded Files (`data/library/`) のハイブリッド。
- **サービス管理**: Systemdによる4サービス体制。
- **運用**: ログはDiscordに集約。`./tools/monitor_evolution.sh` で状況確認。

## 既知のバグ/課題
- **WSL IP**: MT5側の設定 (`InpWSL_IP`) が手動。
- **データ不整合**: MT5とLisp間のヒストリカルデータ差異。
- **再起動耐性**: Guardianのリスク状態 (`risk_state.json`) の永続化は実装済みだが、クラッシュ時の整合性は要監視。

## 直近の変更履歴
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
