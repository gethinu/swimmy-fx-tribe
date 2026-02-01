# Tasks

## Expert Panel 2026-01-31
- [ ] 入力の`read-from-string`経路を封鎖（`*read-eval*`無効化＋安全パーサ導入）: `src/lisp/core/message-dispatcher.lisp`, `src/lisp/school/school-evolution.lisp`, `src/lisp/core/persistence.lisp`
- [ ] Backtest V2のpayload修正とPhase2昇格ロジック実装＋テスト: `src/lisp/school/school-backtest-v2.lisp`
- [ ] 監視スクリプト修正（`import re`）＋実行テスト: `tools/report_system_status.py`
- [ ] 起動シーケンスの責務分割と重複削除: `src/lisp/main.lisp`
- [ ] ドキュメントの版本整合更新: `doc/owners_guide.md`, `doc/SYSTEM_ARCHITECTURE.md`
- [ ] systemdユニット配置の正本化とスクリプトのfail-fast化: `tools/install_services.sh`, `scripts/install-service.sh`

## Expert Panel 2026-02-01
- [ ] 入力の`read-from-string`経路を封鎖（`*read-eval*`無効化＋安全リーダ＋スキーマ検証）: `src/lisp/core/message-dispatcher.lisp`, `src/lisp/school/school-evolution.lisp`
- [ ] Backtest V2のpayload修正（`strategy-alist`統一）＋Phase2昇格ロジック実装＋テスト: `src/lisp/school/school-backtest-v2.lisp`
- [ ] systemdインストールのfail-fast化（コピー失敗で即停止＋ユニット存在検証）: `tools/install_services.sh`
- [ ] 監視レポート修正（`import re`）＋スモークテスト追加: `tools/report_system_status.py`
- [ ] 起動シーケンスの責務分割と重複削除（`load-hall-of-fame`重複排除）: `src/lisp/main.lisp`
- [ ] 設定と状態の分離（`config`からランタイム状態排除、`globals`に一本化）: `src/lisp/core/config.lisp`, `src/lisp/core/globals.lisp`
- [ ] ベンチ廃止の系統整理（戦略/アーム/通知）: `src/lisp/strategies/strategies.lisp`, `src/lisp/school/school-lifecycle.lisp`, `src/lisp/engine/positions.lisp`, `src/lisp/engine/heartbeat.lisp`

## Expert Panel 2026-02-01 (Benching Decision)
- [ ] 評価フェーズのベンチを墓場/killへ置換（Sharpe/PF/WR条件）: `src/lisp/strategies/strategies.lisp`
- [ ] 実行フェーズのアームベンチ廃止（`arm-benched-p`/`bench-arm`/`*benched-arms*`削除）: `src/lisp/engine/positions.lisp`, `src/lisp/engine/portfolio.lisp`, `src/lisp/core/config.lisp`, `src/lisp/core/globals.lisp`
- [ ] シグナル評価のbenched分岐削除: `src/lisp/school/school-evaluation.lisp`
- [ ] Heartbeatのbenched表示削除: `src/lisp/engine/heartbeat.lisp`
- [ ] 廃止後の期待動作テスト追加: `src/lisp/tests.lisp` or `tests/`
