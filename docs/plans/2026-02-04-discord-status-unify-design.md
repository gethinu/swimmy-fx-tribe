# Discord Status Unification (Bot-Side) Design

Date: 2026-02-04

## Goal
Improve reliability and correctness of Discord status notifications by unifying multiple local data sources in the bot, while clearly indicating staleness and missing data. Keep changes minimal and avoid modifying Lisp services or introducing new daemons.

## Non-Goals
- No changes to Lisp writers (e.g., `save-live-status`).
- No persistent queue or notifier redesign in this iteration.
- No schema migration or new storage format.

## Architecture
The Discord bot (`src/python/discord_bot.py`) becomes the single display integrator. It reads:
- `.opus/live_status.sexp` (primary for PnL/Goal/Updated)
- `data/system_metrics.sexp` (system health signals)
- `data/backtest_cache.sexp` (loaded for future extension; not displayed in this iteration)

Integration is read-only and tolerant to partial failures. Values are merged into a 5-field status view:
- Updated/Stale
- Daily PnL
- Total PnL
- Goal Progress
- System Health

## Data Flow
1. Bot loads `live_status.sexp` via `sexp_utils.load_sexp_alist`.
2. Bot loads `system_metrics.sexp` via `sexp_utils.load_sexp_alist`.
3. Bot optionally loads `backtest_cache.sexp` via `sexp_utils.load_sexp_list` (future use).
4. Updated timestamp uses `live_status.last_updated` when present; otherwise file mtime.
5. Stale is computed as `now - last_updated` and compared to a threshold (e.g., 180s).
6. System Health derives from `system_metrics` presence plus simple signals like `uptime_seconds` and `strategy_count`.

## Error Handling
- Each file load is isolated with try/except.
- Missing or parse errors do not crash the bot; cached values may be used, but status is forced to STALE.
- UI explicitly marks missing data (e.g., `System Health: UNKNOWN (metrics missing)`).
- If `last_updated` is in the future, display `Clock Skew?` and do not mark STALE.

## Observability
- Log which file failed to parse or was missing.
- Display staleness directly in the Discord message (e.g., `Updated: 22:52:22 (STALE 7m)`), never `datetime.now()`.

## Testing
Manual validation via controlled file states:
- Fresh live_status => Updated from file, no STALE.
- Old live_status => STALE displayed.
- Missing system_metrics => System Health UNKNOWN.
- Corrupted live_status => fallback + STALE.
- Future last_updated => Clock Skew marker.

## Rollout
- Update bot formatting and load logic.
- Deploy bot only (no service restarts beyond bot process).
- Verify by issuing `swimmy status` and checking STALE behavior.
