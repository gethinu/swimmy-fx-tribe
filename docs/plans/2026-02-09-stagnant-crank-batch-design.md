# Stagnant C-Rank Batch Design

**Goal:** Decouple Stagnant C-Rank batch flushes from the evolution loop, make the cull cadence explicitly daily, replace string-search reasons with reason codes, and emit buffer telemetry (length + oldest age).

## Scope
- Stagnant C-Rank cull runs once per day (day-key guard) regardless of evolution loop frequency.
- Notification batching is driven by a short-period hook (scheduler maintenance) instead of loop-end only.
- Kill reasons are structured (reason codes), with string fallback for backward compatibility.
- Emit telemetry for Stagnant C-Rank buffer length and oldest age.
- Add tests for reason-code routing and the new flush path.

## Architecture & Data Flow
- **Cull cadence:** Introduce a daily guard (YYYYMMDD key) inside the breeding cycle, so Stagnant C-Rank cull runs at most once per day. Weekly culling can remain for other weak-strategy rules if needed.
- **Reason codes:** `kill-strategy` accepts `:reason-code` and routes notifications by code. If no code is provided, fall back to string search for legacy callers.
- **Batch flush hook:** `check-timeout-flushes` is invoked from `run-periodic-maintenance` (short cycle) to ensure stale buffers flush even if the evolution loop stalls.
- **Telemetry:** `queue-stagnant-crank-retire` emits a telemetry event containing `buffer_len` and `oldest_age_seconds` with a simple time-based throttle.

## Error Handling
- Reason-code logic falls back to string heuristics for existing callers.
- Telemetry emit is best-effort; failures do not affect batching or culling.
- Batch flush keeps existing timeouts; new hook is additive and idempotent.

## Testing
- Add a test proving `kill-strategy` routes Stagnant C-Rank via reason code even if the reason string is unrelated.
- Add a test verifying the scheduler maintenance path calls `check-timeout-flushes`.
- Add a test verifying telemetry emit for stagnant buffer contains length + oldest age data.

## Rollout Notes
- Documentation should align with daily cull + hourly batch summary behavior.
- No schema changes required.
