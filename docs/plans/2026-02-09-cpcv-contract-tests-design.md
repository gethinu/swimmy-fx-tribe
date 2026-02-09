# CPCV Contract Tests Design

**Goal:** Add Lisp-side contract tests that guarantee CPCV request/response compatibility with Guardian, including optional trade metadata fields, without requiring Guardian to run.

## Summary
We will extend the existing CPCV tests to validate both ends of the contract:
- **CPCV_VALIDATE request**: ensure all required keys are present in the S-expression payload and that `strategy_params` is included even when empty. `request_id` remains optional but is asserted when generated.
- **CPCV_RESULT response**: ensure the dispatcher accepts optional `trade_list`, `trades_truncated`, and `trades_ref` fields, records trades when present, and preserves trade metadata in the result plist passed to downstream handlers.

This is a unit-level contract suite that avoids Guardian startup by stubbing `send-zmq-msg`, `record-backtest-trades`, and `notify-cpcv-result` and feeding real S-expression fixtures into the dispatcher.

## Components and Files
- Modify: `src/lisp/tests.lisp`
  - Extend `test-request-cpcv-includes-request-id` to check required keys.
  - Add a new test to validate `CPCV_RESULT` optional fields and propagation.
- Modify: `src/lisp/core/message-dispatcher.lisp`
  - Extend `result-plist` to include `:trades-truncated` and `:trades-ref`.

## Data Flow and Fixtures
1. **Request path**
   - Call `request-cpcv-validation` with a minimal strategy.
   - Capture the outbound S-expression string via a stubbed `send-zmq-msg`.
   - Assert required keys exist and that `strategy_params` is present.

2. **Response path**
   - Build a CPCV_RESULT S-expression fixture containing `trade_list`, `trades_truncated`, `trades_ref`.
   - Invoke the dispatcher entry to parse and process it.
   - Assert `record-backtest-trades` was called when `trade_list` exists.
   - Assert the plist passed to `notify-cpcv-result` includes `:trades-truncated` and `:trades-ref` with expected values.

## Error Handling Expectations
- Optional fields must not cause parsing errors.
- If `trade_list` is absent, `record-backtest-trades` should not be called.
- Metadata fields are pass-through only; no change to Discord message content.

## Testing Strategy (TDD)
- **RED:** Extend request test to fail when required keys are missing.
- **RED:** Add new response test that expects `:trades-truncated` and `:trades-ref` to be present in the notification payload.
- **GREEN:** Implement minimal changes in the dispatcher to include these fields.
- **REFACTOR:** Consolidate duplicated fixtures into small local helpers if needed.

## Completion Criteria
- Request contract test passes (all required keys, `strategy_params` present).
- Response contract test passes (optional fields accepted + propagated).
- No regressions in existing CPCV tests.
