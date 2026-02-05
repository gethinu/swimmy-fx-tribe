# Backtest Result Tracing Design

**Date:** 2026-02-05
**Scope:** 5581受信 → 結果適用 → DB upsert のトレースを最小ログで可観測化する。

## Goal
バックテスト結果が Brain に届いた後の処理経路（受信→適用→DB upsert）を、既存の `SWIMMY_BACKTEST_DEBUG_RECV=1` に従って観測できるようにする。

## Non-Goals
- Backtest Service / Guardian の性能改善
- データ量削減や並列化の導入
- DBスキーマ変更

## Constraints
- 正本は `docs/llm/SPEC.md` / `docs/llm/ARCHITECTURE.md` / `docs/llm/INTERFACES.md` / `docs/llm/STATE.md`。
- 変更時は `STATE` と必要なら `INTERFACES` を先に更新。
- 既存の `SWIMMY_BACKTEST_DEBUG_RECV=1` を正本とし、常時ログ化は行わない。

## Design Summary
- 変更点は `src/lisp/core/message-dispatcher.lisp` に集中させる。
- `BACKTEST_RESULT` 受信時に、summaryのみを `backtest-debug-log` に出力。
- `apply-backtest-result` / `handle-v2-result` の呼び出し前後に start/end を記録。
- 例外時は `%dlq-record` と `backtest-debug-log` の両方へ記録（相関IDは `request_id`）。
- raw payload の全文は記録しない（summaryのみ）。

## Minimal Test Strategy
- `SWIMMY_BACKTEST_DEBUG_RECV=1` 時に `backtest-debug-log` が呼ばれることを検証。
- 文字列完全一致ではなく、呼び出し有無の確認のみ。

## Verification
- `SWIMMY_BACKTEST_DEBUG_RECV=1` で `data/reports/backtest_debug.log` に追記が発生すること。
- `data/reports/backtest_status.txt` の更新は既存のまま維持されること。
