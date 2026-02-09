# Phase2 Removal (Design)

**Date:** 2026-02-09
**Status:** Approved
**Owner:** Swimmy

## Goal
Remove the unused Backtest V2 Phase2 validation path and its reporting, because OOS already provides the validation gate. Keep Phase1 screening (V2) intact and avoid touching the OOS/CPCV pipeline.

## Non-Goals
- No changes to OOS dispatch, queueing, or CPCV criteria.
- No changes to Backtest V2 payload format beyond removing Phase2 usage.
- No refactor of unrelated backtest or report logic.

## Decisions
- **Keep Phase1 only:** V2 will continue to run Phase1 screening; Phase2 is removed.
- **Remove Phase2 state:** Delete `*phase2-last-end-unix*` and any Phase2 thresholds/ranges.
- **Clean report:** Remove the "Phase2 EndTime" line from the Evolution Factory Report.
- **Tests reflect reality:** Remove Phase2 tests and validate Phase1 payload usage instead.

## Scope
### Code
- `src/lisp/school/school-backtest-v2.lisp`
  - Remove Phase2 request path and `_P2` result handler.
  - Remove Phase2 end_time tracking.
  - Update comments to reflect Phase1-only behavior.
- `src/lisp/school/school-constants.lisp`
  - Remove Phase2 constants/range and update comments.
- `src/lisp/core/globals.lisp`
  - Remove `*phase2-last-end-unix*`.
- `src/lisp/packages.lisp`
  - Stop exporting `*phase2-last-end-unix*`.
- `src/lisp/school/school-narrative.lisp`
  - Remove the Phase2 end-time snippet from report rendering.

### Tests
- `src/lisp/tests.lisp`
  - Remove Phase2 promotion test and test registry entry.
- `src/lisp/tests/school-split-tests.lisp`
  - Remove Phase2 EndTime report test.
- `src/lisp/tests/backtest-payload-tests.lisp`
  - Update phase/range test to use Phase1 values.

## Data Flow Impact
Backtest V2 still sends Phase1 payloads with `phase` and `range_id` set to Phase1 values. OOS/CPCV flow remains unchanged and continues to gate A/S promotions.

## Error Handling
No new error paths. Removal reduces unused state and avoids misleading report output.

## Tests
Run targeted tests for report output, Backtest V2 payloads, and regression smoke via the standard test suite.

## Rollout
Single implementation change with tests. No migrations required.

## Open Questions
None.
