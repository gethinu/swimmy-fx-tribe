# BACKTEST_RESULT Request ID Enforcement Design

Date: 2026-02-07

## Goal
Enforce `request_id` as a required field on all `BACKTEST_RESULT` outputs from the Backtest Service, including error paths, and make violations explicit and observable.

## Context
The OOS pipeline relies on `request_id` for correlation and queue integrity. The Backtest Service currently injects `request_id` in some cases but allows missing values in error responses and certain flows, which creates silent correlation gaps.

## Proposed Behavior
- If an incoming BACKTEST request (S-expression) lacks `request_id`, the service returns a `BACKTEST_RESULT` error immediately without invoking Guardian.
- The error response includes:
  - `error: "missing request_id"`
  - `request_id: "MISSING"` (placeholder to preserve schema shape and trace the violation).
- For requests with `request_id`, all `BACKTEST_RESULT` outputs (including errors) must carry the same `request_id`.
- Any Guardian JSON output missing `result.request_id` is patched to include it; S-expression outputs are patched in the `(result ...)` list when missing.

## Data Flow
1. Receive S-expression BACKTEST request.
2. Extract `request_id`.
3. If missing: return `BACKTEST_RESULT` error immediately.
4. If present: forward to Guardian, read result line, and inject `request_id` as needed before returning.

## Error Handling
All error paths in `tools/backtest_service.py` must include `request_id`:
- Guardian missing / process failure / empty output
- Guardian ERROR lines
- Exceptions during send/receive

## Testing
Add or update a unit test to ensure:
- A request without `request_id` yields a `BACKTEST_RESULT` containing `error: "missing request_id"` and `request_id: "MISSING"`.
- Existing request-id propagation test remains valid for normal inputs.

## Documentation
Update `docs/llm/INTERFACES.md` to explicitly state:
- `request_id` is required for `BACKTEST_RESULT`.
- Backtest Service returns a `BACKTEST_RESULT` error when it is missing.

## Non-Goals
- Changing upstream request formats.
- Introducing UUID generation for missing request IDs.
- Modifying Guardian behavior.
