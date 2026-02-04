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

## Expert Panel 2026-02-03 (MCP Transport)
- [ ] MCP stdioサーバの薄いエントリを追加し、`handle_backtest_submit`/`handle_trade_submit` を呼ぶ: `tools/mcp_gateway.py`
- [ ] APIキー検証をサーバ境界で必須化し、未設定/不一致は即拒否にする: `tools/mcp_gateway_config.py`
- [ ] TransportとI/O仕様を追記し、アーキテクチャ図にMCP境界を追加する: `docs/llm/INTERFACES.md`, `doc/SYSTEM_ARCHITECTURE.md`
- [ ] MCPサーバ契約テストを追加し、trade.submit=403 と backtest.submitのrequest_id を固定化: `tools/test_mcp_gateway.py`
- [ ] HTTP/WSを検討するならsystemd/運用/公開ポート設計をドキュメント化する: `systemd/swimmy-mcp-gateway.service`, `doc/runbook.md`

## Expert Panel 2026-02-03 (MT5 Protocol Drift)
- [ ] MT5↔Guardianのエンコーディング方針を決定し、正本に明記する: `docs/llm/INTERFACES.md`, `docs/llm/STATE.md`
- [ ] `ORDER_OPEN` キー統一（`action/symbol` vs `side/instrument`）と実装/仕様の一致: `docs/llm/INTERFACES.md`, `src/lisp/core/execution-protocol.lisp`, `src/mt5/SwimmyBridge.mq5`
- [ ] `ORDER_OPEN` の文字列部分一致判定を廃止し、厳密判定に変更: `src/mt5/SwimmyBridge.mq5`
- [ ] `REQ_HISTORY` の `volume/count` 不整合を修正し、要求/応答スキーマを確定: `src/lisp/system/runner.lisp`, `src/mt5/SwimmyBridge.mq5`
- [ ] MT5起因メッセージ（HISTORY/POSITIONS/SWAP_DATA/ORDER_ACK/TRADE_CLOSED/GET_POSITIONS/GET_SWAP/CLOSE_SHORT_TF）を仕様化: `docs/llm/INTERFACES.md`
- [ ] MT5↔Guardian プロトコル契約テストを追加し、キー/型の逸脱を検知: `src/lisp/tests.lisp` or `tools/`
- [ ] `InpWSL_IP` の固定値依存を排除し、設定とSTATEを整合: `src/mt5/SwimmyBridge.mq5`, `docs/llm/STATE.md`

## Expert Panel 2026-02-03 (MCP stdio/server mode)
- [ ] MCP stdioサーバをオンデマンド起動で実装し、常駐前提のsystemdユニットは保留にする: `systemd/swimmy-mcp-gateway.service`
- [ ] APIキー検証と監査ログを実装して常駐化の前提条件を満たす: `tools/mcp_gateway_config.py`, `tools/mcp_gateway.py`
- [ ] MCP境界仕様（stdio/JSON-RPC）を明記し、アーキ図にMCP境界を追加する: `docs/llm/INTERFACES.md`, `doc/SYSTEM_ARCHITECTURE.md`
- [ ] 常駐化を検討する場合はrunbookに停止/障害/再起動手順を追加する: `doc/owners_guide.md`, `doc/runbook.md`

## Expert Panel 2026-02-03 (MCP JSON-RPC)
- [ ] JSON-RPC 2.0のフレーミング（Content-Length）を明文化し、非JSON-RPCを拒否する: `docs/llm/INTERFACES.md`
- [ ] `backtest.status` が最新サマリであることを明記し、request_id紐付け計画を追加する: `src/lisp/core/message-dispatcher.lisp`
- [ ] `system_metrics.sexp` を正本とし、JSONは境界のみと文書化する: `tools/report_status.py`, `docs/llm/INTERFACES.md`
- [ ] JSON-RPC契約テスト（403/202/エラー構造）を追加する: `tools/mcp_gateway.py`
- [ ] MCP境界をアーキ図とrunbookに追記する: `doc/SYSTEM_ARCHITECTURE.md`, `doc/owners_guide.md`

## Expert Panel 2026-02-03 (Lifecycle Stall)
- [ ] rank更新→moveの順序を修正し、旧rankファイルが確実に削除されるようにする: `src/lisp/school/school-rank-system.lisp`, `src/lisp/core/persistence.lisp`
- [ ] 新生児（trades=0/BT未完了/creation-time新鮮）はprune対象外にするガードを追加: `src/lisp/school/school-pruning.lisp`
- [ ] Breeder生成物はBT/Phase1を必須化し、`require-bt nil` を撤廃: `src/lisp/school/school-breeder.lisp`, `src/lisp/school/school-kb.lisp`
- [ ] DB/Library/KBドリフト検知をEvolution Reportに露出する: `src/lisp/school/school-db-stats.lisp`, `src/lisp/school/school-narrative.lisp`
- [ ] OOS=0.00のキャッシュを「未検証扱い」に戻して再リクエストする: `src/lisp/school/school-validation.lisp`
- [ ] ライフサイクル不変条件（旧ファイル削除/初回BT前にpruneしない）のテスト追加: `src/lisp/tests.lisp`

## Expert Panel 2026-02-04 (CMD JSON Cleanup)
- [ ] CMD送信のJSON残存箇所をS式化し、送信APIを統一: `src/lisp/school/school-stress.lisp`, `src/lisp/school/school-validation.lisp`, `src/lisp/school/school-strategy.lisp`, `src/lisp/school/school-founders.lisp`, `src/lisp/evolution.lisp`
- [ ] CMD送信のJSON禁止を検知するテストを追加（回帰防止）: `src/lisp/tests.lisp`
- [ ] 受信側のJSON許容を段階的に削除: `src/lisp/core/message-dispatcher.lisp`
- [ ] 外部境界のJSON維持を明文化し、境界モジュールのみJSON許可を明記: `docs/llm/INTERFACES.md`, `src/lisp/core/discord.lisp`, `tools/mcp_gateway.py`

## Expert Panel 2026-02-04 (Expert Panel Consultation Mode)
- [ ] expert-panel ワークフローに mode（critique / consult）を追加し、注意事項を分離: `doc/archive/workflows/expert-panel.md`
- [ ] 出力テンプレに「目的」「前提」「不確実性」を追加（相談モードのみ必須）: `doc/archive/workflows/expert-panel.md`
- [ ] 相談モードでの2択/3択提案と対抗意見の必須化を明文化: `doc/archive/workflows/expert-panel.md`

## Expert Panel 2026-02-04 (Backtest Option Format)
- [ ] `test-backtest-v2-uses-alist`を仕様準拠に修正（`(numberp (cadr ...))`等）: `src/lisp/tests.lisp`
- [ ] Backtest V2の`Option`表現が仕様と一致していることを再確認・必要なら追記: `docs/llm/INTERFACES.md`
- [ ] `STATE`に「Backtest V2の`Option`表現はリスト形式が正本」と明記: `docs/llm/STATE.md`

## Expert Panel 2026-02-04 (Brain Bootstrap Decision)
- [ ] run.sh に `brain.lisp` 存在チェックとフェイルファストの明確なエラーを追加: `run.sh`
- [ ] systemd の ExecStart と owners_guide の運用手順を一致させる（正本を明記）: `/etc/systemd/system/swimmy-brain.service`, `doc/owners_guide.md`
- [ ] `brain.lisp` の維持/廃止を決定し、`doc/SYSTEM_ARCHITECTURE.md` の記述を更新: `doc/SYSTEM_ARCHITECTURE.md`
