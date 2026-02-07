# Backtest Queue Periodic Flush Design

Date: 2026-02-07
Owner: Codex + User
Status: Draft (validated)

## Summary
Backtest send requests can queue before the backtest requester is connected. Today the queue only flushes once during `init-backtest-zmq`. This design adds a periodic flush hook so queued backtest messages are drained after the requester becomes available, without removing existing rate/pending protections.

## Goals
- Ensure queued backtest requests are flushed after requester availability.
- Preserve pending limit and rate limit protections.
- Avoid extra load on the main loop or Guardian.

## Non-Goals
- Changing backtest batch size policy.
- Changing existing Discord notification behavior.
- Rewriting the backtest transport architecture.

## Proposed Approach
- Add `*backtest-queue-last-flush*` and `*backtest-queue-flush-interval-sec*` in `school-backtest-utils.lisp`.
- Add `maybe-flush-backtest-send-queue` that:
  - Returns immediately if the queue is empty.
  - Returns immediately if interval has not elapsed.
  - Calls `flush-backtest-queue` and updates `*backtest-queue-last-flush*` when it does run.
- Call `maybe-flush-backtest-send-queue` from `start-evolution-service` once per cycle so it runs whenever the loop is alive.

## Data Flow
1. `send-zmq-msg` enqueues backtest messages when requester is unavailable.
2. `init-backtest-zmq` still flushes immediately on connect.
3. If the requester connects after the queue starts, `maybe-flush-backtest-send-queue` will flush on the next cycle tick after interval elapses.

## Error Handling
- `flush-backtest-queue` already respects pending max and stops when pending is saturated.
- `maybe-flush-backtest-send-queue` is a no-op on empty queue or short interval.

## Testing Strategy
- Add unit test to verify periodic flush triggers and updates last-flush timestamp.
- Run targeted test for `test-backtest-queue-periodic-flush`.

## Risks
- If `start-evolution-service` is paused or blocked, periodic flush will not run. This is acceptable because it preserves current behavior and only improves when the loop is active.

## Rollout
- Deploy as part of school service restart.
- Observe logs for successful queue flushes after requester connects.
