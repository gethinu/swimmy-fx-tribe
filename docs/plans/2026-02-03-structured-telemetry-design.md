# Structured Telemetry Design

Date: 2026-02-03
Owner: Swimmy / Codex
Status: Draft

## 1. Goal / Scope
- Goal: OOS / WFV / Heartbeat / System metrics を構造化イベントとして統一し、運用の可観測性を上げる。
- Goal: 既存の JSON 出力 (system_metrics.sexp / live_status.sexp) を維持しつつ、イベントの単一ソースを定義する。
- Non-goal: 既存の JSON を全面 S-expression に移行すること。
- Non-goal: 監視ダッシュボードの実装 (将来対応)。

## 2. Key Decisions
- JSON を正、S-expression は任意ミラー (フラグでON/OFF)。
- イベントは 1 つの JSONL ログに集約し、log/telemetry を区別する。
- correlation_id は timestamp+source 禁止。OOS は request_id、Heartbeat は protocol message id を使う。
- WFV と A-rank OOS は event_type / oos_kind で必ず分離する。

## 3. Architecture (Core)
- 新規 `src/lisp/core/telemetry.lisp` を追加。
- API: `(emit-telemetry-event event-type &key service severity correlation-id data)`
- 既存 `logger.lisp` の JSONL 出力を中核として使用。イベントは log_type="telemetry" として同一ログに追記。
- `emit-telemetry-event` は例外を握りつぶし、失敗時は safe-format-t で警告、失敗カウンタを加算。

## 4. Event Schema (JSONL)
必須キー:
- schema_version (int)
- timestamp (ISO-8601 or unix)
- log_type ("telemetry")
- event_type (string)
- service (string)
- severity ("debug" | "info" | "warn" | "error")
- correlation_id (string)
- data (object)

追加推奨キー:
- run_id (process start uuid)
- host / pid

schema_version 運用:
- 破壊的変更は schema_version を上げる。
- 旧schemaの読み取りは最低1バージョン維持。

## 5. Emission Points
### OOS (A-rank)
- `school-validation.lisp:maybe-request-oos-backtest`
  - oos.requested / oos.throttled / oos.dispatch_failed
  - data: { request_id, status, age_sec, symbol }
- `school-validation.lisp:handle-oos-backtest-result`
  - oos.result
  - data: { request_id, sharpe, latency_sec, oos_kind:"a-rank" }

### WFV
- `school-backtest.lisp:start-walk-forward-validation`
  - wfv.started
  - data: { split_ratio:0.2, is_len, oos_len }
- `school-backtest.lisp:process-wfv-result`
  - wfv.result (両結果揃った時のみ)
  - data: { is_sharpe, oos_sharpe, degradation }

### Heartbeat
- `core/executor.lisp:send-heartbeat`
  - heartbeat.sent (correlation_id = protocol message id)
- `core/message-dispatcher.lisp` (HEARTBEAT recv)
  - heartbeat.recv (correlation_id = received message id)
- `engine/heartbeat.lisp:send-discord-heartbeat`
  - heartbeat.discord_sent (correlation_id = discord heartbeat id)

### System Metrics / Live Status
- `school-telemetry.lisp:collect-system-metrics`
  - metrics.snapshot (data: heap, strategy_count, uptime)
- `shell/notifications.lisp:save-live-status`
  - status.snapshot (data: daily_pnl, leader, regime, etc)

## 6. Error Handling / I/O
- emit-telemetry-event は例外を握りつぶし、失敗時に telemetry_write_fail_count++。
- JSONL ローテーション:
  - *telemetry-max-bytes* を超えたら .1 にローテ。
  - Heartbeat は間引きせずローテで対応 (欠測による監視歪みを避ける)。
- 既存 JSON 出力は tmp -> rename の原子書き込みに変更。

## 7. Testing
- Unit: emit-telemetry-event が必須キーを含むことを検証。
- Unit: OOS/WFV の oos_kind, split_ratio, correlation_id ルールを検証。
- Unit: ローテーション/上限超過時の動作確認。

## 8. Rollout Plan
- Phase 1: emit-telemetry-event 追加 + OOS/WFV/Heartbeat の発火。
- Phase 2: system_metrics/live_status の原子書き込み化。
- Phase 3: 運用runbook更新 (監視導線の明確化)。

## 9. Open Questions
- JSONL の出力先は `swimmy.json.log` のままか、`events.jsonl` に分離するか。
- schema_version の初期値と運用ポリシーの明文化。
- S-expression ミラーの必要有無 (デフォルト OFF)。
