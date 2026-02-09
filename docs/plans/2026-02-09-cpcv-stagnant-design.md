# CPCV Median + Stagnant C-Rank Design

**Goal:** Align CPCV median metrics end-to-end, reduce Sharpe-only bias in live voting, and document Stagnant C-Rank daily cull behavior and batching.

## Scope
- Document Stagnant C-Rank daily cull and hourly batch notifications.
- Emit CPCV median PF/WR/MaxDD from Guardian and ingest in Lisp.
- Add PF/WR/MaxDD influence to vote weighting (reduce Sharpe-only bias) while keeping rank gates on CPCV medians.

## Architecture & Data Flow
- **CPCV_VALIDATE** request stays S-expression; Guardian runs CPCV and builds an aggregate result.
- **Guardian -> Lisp** CPCV_RESULT payload includes `median_sharpe`, `median_pf`, `median_wr`, `median_maxdd`, `pass_rate`, etc.
- **Lisp** message dispatcher normalizes keys, persists CPCV medians to strategy, and uses rank gates for S promotion.
- **Voting** uses a composite weight from IS Sharpe plus PF/WR bonuses and MaxDD penalty, clamped to avoid extreme swings.

## Error Handling
- Guardian errors keep the response consistent (median fields default to 0.0 with error populated).
- Lisp only updates strategy CPCV medians when `error` is nil; failed CPCV results are recorded but do not alter ranks.

## Testing
- **Rust**: extend CPCV payload tests to assert median fields are serialized.
- **Lisp**: add test for CPCV_RESULT ingestion of median fields; add tests for vote weighting behavior with PF/WR/MaxDD shifts.
- Existing CPCV gate tests remain as rank correctness guards.

## Rollout Notes
- Stagnant C-Rank cull already uses daily guard; documentation should match behavior.
- No API changes outside internal ZMQ S-expression contract.
