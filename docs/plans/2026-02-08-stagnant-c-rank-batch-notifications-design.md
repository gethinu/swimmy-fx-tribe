# Design: Batch "Stagnant C-Rank" Soft-Kill Alerts

Date: 2026-02-08

## Context
We already batch Max Age Retirement soft-kill alerts in `src/lisp/core/discord.lisp`. The current Stagnant C-Rank soft-kill path emits per-strategy alerts via `kill-strategy` in `src/lisp/strategies/strategies.lisp`, which can spam the alerts channel. The user requested that these alerts be batched in the same way as Max Age.

## Goals
- Batch Stagnant C-Rank soft-kill alerts into an hourly summary.
- Keep culling behavior unchanged; only adjust notifications.
- Reuse the existing Max Age batching pattern to minimize risk.

## Non-Goals
- Do not change culling thresholds, ranking logic, or strategy lifecycle.
- Do not alter other soft-kill notifications.
- Do not introduce a generalized batching framework beyond this case.

## Proposed Approach (Recommended)
Add a dedicated Stagnant C-Rank batch buffer and flush logic in `src/lisp/core/discord.lisp`, mirroring the Max Age implementation.

### Data Flow
1. `kill-strategy` receives a reason string containing "Stagnant C-Rank" (e.g., "Cull: Stagnant C-Rank (0.00) after 10 days").
2. Instead of `notify-discord-alert`, call `queue-stagnant-c-rank` to buffer the strategy name.
3. `check-timeout-flushes` calls `maybe-flush-stagnant-c-rank` each tick.
4. After 1 hour, a summary alert is sent with total count and top 5 strategy names.

### New State and Functions
- `*stagnant-c-rank-batch-interval*` (default 3600 seconds)
- `*stagnant-c-rank-buffer*` (list of strategy names)
- `*stagnant-c-rank-first-seen*` (batch start timestamp)
- `queue-stagnant-c-rank`
- `maybe-flush-stagnant-c-rank`

### Notification Format
A new summary message, similar to Max Age, for example:

"Stagnant C-Rank Summary (Last 1h)\nTotal: N\nTop: `name1`, `name2`, ...\nAction: Individual alerts suppressed."

## Testing
Add a unit test mirroring `test-max-age-retire-batched-notification`:
- Soft-kill a strategy with a Stagnant C-Rank reason.
- Assert no immediate alert is sent.
- Assert the buffer contains the strategy name.
- Advance time by > 3600 seconds and assert one summary alert is sent and includes the name.

## Risks and Rollback
- Risk is limited to alert delivery and message formatting.
- Rollback by reverting the batching hooks in `kill-strategy` and removing the buffer/flush functions.

## Observability
- Summary alerts appear in the alerts channel once per hour when stale entries exist.
- Normal logs remain unchanged except for fewer per-strategy alerts.
