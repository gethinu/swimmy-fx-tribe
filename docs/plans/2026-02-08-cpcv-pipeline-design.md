# CPCV Pipeline Recovery Design

**Goal:** Ensure CPCV validation requests are handled end-to-end, results are returned to the brain, and strategy loading is robust to unknown keys so CPCV never runs on an empty or silently dropped set.

**Architecture:** The brain/evolution path emits `CPCV_VALIDATE` messages. Guardian parses the request (S-exp or JSON), runs `cpcv::run_cpcv_validation`, and emits `CPCV_RESULT` with aggregate metrics. Lisp side tolerates unknown keys when loading strategies and logs dropped/invalid entries.

## Components

- Lisp persistence: `src/lisp/core/persistence.lisp` (`%read-strategy-from-string` allows unknown keys).
- Lisp KB init: `src/lisp/strategies/strategies.lisp` (`init-knowledge-base` filters invalid entries, logs counts).
- Guardian handler: `guardian/src/main.rs` adds CPCV message handling and response serialization.
- Tests: `src/lisp/tests.lisp` and guardian unit tests in `guardian/src/main.rs`.

## Data Flow

1. Brain/evolution emits `CPCV_VALIDATE` with strategy definition and dataset path.
2. Guardian parses payload, runs CPCV validation.
3. Guardian builds `CPCV_RESULT` with `median_sharpe`, `passed_count`, `failed_count`, `total_count`, `pass_rate`, and `is_passed`, then sends back to brain.
4. Brain applies CPCV result, updates ranks and reports.

## Error Handling

- Missing file, zero paths, or parse errors return a structured `CPCV_RESULT` with `is_passed = false`, zero counts, and `error` populated.
- Invalid strategies during load are skipped with a single summary log line.

## Testing

- Lisp: regression test for unknown keys on strategy load; KB init skips invalid entries.
- Guardian: unit test for CPCV error case; unit test for success-case payload using a synthetic aggregate result.
- Verification: `scripts/ci-test.sh` and `cargo test --release`.

## Rollout and Verification

- Restart guardian/evolution after deploy.
- Confirm `data/reports/evolution_factory_report.txt` shows CPCV queued/received > 0.
- Check `logs/guardian.log` for CPCV activity and absence of errors.
