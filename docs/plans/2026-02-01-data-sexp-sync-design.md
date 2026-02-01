# Data SEXP Sync Design

**Context:** `apply-backtest-result` updates numeric columns when the strategy is not in memory, but it leaves `data_sexp` unchanged. Knowledge Base loads from `data_sexp`, so reports and rank evaluation read stale metrics.

## Goals
- Keep `data_sexp` in sync with the latest backtest metrics, even in fallback paths.
- Preserve current behavior when `data_sexp` is missing or corrupted.
- Avoid broad refactors; fix at the source of the mismatch.

## Approach
- In the fallback path of `apply-backtest-result`, attempt to load the strategy from `data_sexp` for the given name.
- If parsing succeeds and the object is a strategy, update metrics (`sharpe`, `profit-factor`, `win-rate`, `trades`, `max-dd`, `oos-sharpe`, `cpcv-median-sharpe`, `cpcv-pass-rate`, `revalidation-pending`).
- Call `upsert-strategy` to rewrite both columns and `data_sexp` in a consistent way.
- If the row is missing or parsing fails, log a warning and fall back to the existing column-only update.

## Data Flow
Backtest result -> `apply-backtest-result` -> update `data_sexp` (preferred) -> KB load from `data_sexp` -> accurate reports and ranking.

## Error Handling
- Wrap `read-from-string` in `handler-case`.
- If parsing fails, keep the existing column update path to avoid blocking backtest ingestion.

## Testing
- Add a regression test that inserts a strategy, removes it from memory, calls `apply-backtest-result`, and verifies that the row's `data_sexp` reflects the new Sharpe value.
