# Pair Strategy Integration Design

**Date:** 2026-02-07
**Status:** Approved

## Goal
Treat pair strategies as independent entities with their own persistence, evaluation, and rank gating, while allowing them to compete with single strategies under a per-symbol/timeframe limited slot policy. Preserve the existing overlay execution path.

## Architecture
### 1) Data Model & Persistence
- Add a new SQLite table `pair_strategies` with required fields: `pair_id`, `strategy_a`, `strategy_b`, `weight_a`, `weight_b`, `metrics` (sharpe/pf/score/corr), `rank`, and `last_updated`.
- Store evaluation and gate results alongside the pair record: `oos_sharpe`, `cpcv_median`, `cpcv_pass_rate`.
- Keep `trade_logs.pair_id` as-is. Extend `backtest_trade_logs` to include `oos_kind="CPCV"` so pair CPCV evaluation can be built from trade_list.
- `pair_id` is computed with the existing order-independent FNV1a hash of strategy names. This preserves minimal change and avoids re-keying existing logic.

### 2) Rank Slots & Competition Rules (Hybrid)
- Introduce a per-symbol/timeframe cap for pairs: `*pair-slots-per-tf*`.
- Build a unified ranking list per symbol/timeframe using a shared score (`0.7*Sharpe + 0.3*PF`).
- Select from the unified list, but enforce pair occupancy: if more pairs than `*pair-slots-per-tf*` are in the top N, keep only the top pair candidates and fill the rest with singles.
- Keep single-strategy lifecycle intact. Pair ranking and promotion logic are isolated and only converge at selection time.

### 3) Validation Gates
- A promotion: requires OOS composite evaluation from `oos_kind="OOS"` trade_list for both legs, merged into a pair composite PnL series.
- S promotion: requires CPCV composite evaluation from `oos_kind="CPCV"` trade_list for both legs.
- If trade data is insufficient (missing trade_list or min-trades not met), promotion is blocked (no rank change).

### 4) Execution Integration
- Preserve existing overlay in `execute-category-trade`. Only pairs selected by the new ranking pipeline are loaded into `*pair-active-defs*` from DB.
- Periodically update pair selection in a batch task (hooked into existing maintenance schedule). Update DB, then refresh `*pair-active-defs*`.

### 5) Error Handling
- Missing trade_list or insufficient trades results in “promotion blocked” with a clear reason logged. No hard failures.
- DB operations are upserts with `last_updated` timestamps for coherence.

### 6) Testing
- Persistence: upsert/fetch for `pair_strategies`.
- Gate behavior: OOS/CPCV data shortage → promotion blocked.
- Slot competition: unified ranking + pair cap enforced per symbol/timeframe.

## Files to Update
- `src/lisp/school/school-db.lisp` (new table, upsert/fetch, CPCV trade logging)
- `src/lisp/school/school-pair-composite.lisp` (pair evaluation helpers reused)
- `src/lisp/school/school-rank-system.lisp` or a new pair-ranking module (competition rules)
- `src/lisp/core/message-dispatcher.lisp` (record CPCV trade_list)
- `doc/knowledge/implementation_plan_v50.6.md`, `doc/owners_guide.md`, `docs/llm/STATE.md`
- Tests: `src/lisp/tests/pair-composite-tests.lisp`, `src/lisp/tests/backtest-db-tests.lisp`

