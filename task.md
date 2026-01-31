# Tasks

## Expert Panel 2026-01-31
- [ ] 入力の`read-from-string`経路を封鎖（`*read-eval*`無効化＋安全パーサ導入）: `src/lisp/core/message-dispatcher.lisp`, `src/lisp/school/school-evolution.lisp`, `src/lisp/core/persistence.lisp`
- [ ] Backtest V2のpayload修正とPhase2昇格ロジック実装＋テスト: `src/lisp/school/school-backtest-v2.lisp`
- [ ] 監視スクリプト修正（`import re`）＋実行テスト: `tools/report_system_status.py`
- [ ] 起動シーケンスの責務分割と重複削除: `src/lisp/main.lisp`
- [ ] ドキュメントの版本整合更新: `doc/owners_guide.md`, `doc/SYSTEM_ARCHITECTURE.md`
- [ ] systemdユニット配置の正本化とスクリプトのfail-fast化: `tools/install_services.sh`, `scripts/install-service.sh`
