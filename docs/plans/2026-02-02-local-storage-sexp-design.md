# Local Storage S-Expression Migration Design

**Date:** 2026-02-02

## Goal
Switch local storage for backtest cache, telemetry, and live status to S-expression immediately (no JSON compatibility), using atomic writes and a shared Python S-expression reader so reports and Discord bot continue to function once re-enabled.

## Scope
- **In scope (S-expression only):**
  - `data/backtest_cache.sexp`
  - `data/system_metrics.sexp`
  - `.opus/live_status.sexp`
- **Out of scope (stay JSON/JSONL):**
  - `data/` and `db/data/` JSON/JSONL
  - External API boundaries (Discord/HTTP)

## File Naming & Compatibility
- **S-expression only**; JSON is not read after migration.
- Existing `.json` files are moved to `.json.bak.YYYYMMDDHHMM` by migration script.
- Each file is a **single S-expression**:
  - `backtest_cache.sexp`: list of alists
  - `system_metrics.sexp`: alist
  - `live_status.sexp`: alist
- Each file includes `(schema_version . 1)` to anchor future changes.

## Schemas (v1)
- **backtest_cache.sexp** (list of entries):
  - `((schema_version . 1) (entries . ( (name . "...") (timestamp . 123) (result . ((sharpe . 0.1) ...)) ) ... )))`
- **system_metrics.sexp** (alist):
  - `((schema_version . 1) (timestamp . 123) (heap_used_bytes . 0) (heap_used_mb . 0.0) (strategy_count . 0) (uptime_seconds . 0))`
- **live_status.sexp** (alist):
  - `((schema_version . 1) (daily_pnl . 0.0) (accumulated_pnl . 0.0) (monthly_goal . 0) (goal_progress . 0.0) (regime . "UNKNOWN") (volatility . "UNKNOWN") (leader . "UNKNOWN") (danger_level . 0) (ecosystem_health . 0) (total_trades . 0) (warmup_progress . 0) (warmup_complete . #f) (tribes . ((hunters . ((direction . "HOLD") (confidence . 0) (reason . "MACD+ADX+Kalman"))) ... ))) )`

## Atomic Write Strategy
All S-expression writes use **tmp → rename** to avoid partial writes:
1. Write to `path.tmp`
2. `rename-file` to final path
3. On error, log and keep previous file intact

## Python S-expression Parser
Create a minimal parser in `src/python/sexp_utils.py`:
- Accepts list, dotted pair, strings, numbers, booleans `#t/#f`, symbols.
- Converts alists to dicts with string keys.
- Provides `load_sexp_alist` / `load_sexp_list` helpers for report tools and Discord bot.

## Migration Script
`tools/migrate_local_storage_to_sexp.py`:
- Reads existing JSON files if present.
- Converts to S-expression (schema v1).
- Writes `.sexp` atomically.
- Backs up JSON to `.json.bak.YYYYMMDDHHMM`.

## Tests (Minimal)
- **Python:** parser accepts normal/empty/invalid input; file loaders return expected dicts/lists.
- **Lisp:** writers produce S-expression; atomic write path used; readers load expected structures.

## Docs Updates
- `docs/llm/SPEC.md`: Local Storage policy updated to S-expression only + filenames.
- `docs/llm/STATE.md`: Decision recorded; next action updated.
- `docs/llm/INTERFACES.md`: no change (local storage only).
