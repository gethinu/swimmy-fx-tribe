# OOS Reliability & Resilience Hardening Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make the OOS validation pipeline reliable: persist pending queue, validate input data, add observability, harden DB writes, and surface failure statistics, following expert-panel-2 Actionable Items 1-7.

**Architecture:** Persist OOS requests in SQLite with request-id and status; gate CSV inputs with lightweight schema checks; add E2E latency/throughput metrics and dead-letter handling for BACKTEST_RESULT; wrap DB writes in WAL+transactions; collect failure stats in rank evaluation; optimize metric refresh to avoid full-table scans.

**Tech Stack:** Common Lisp (SBCL), SQLite, ZMQ, existing test harness (`test_runner.lisp`, `src/lisp/tests/*.lisp`).

### Task 1: Persisted OOS queue with request-id and retry
**Files:**
- Modify: `src/lisp/school/school-validation.lisp`
- Modify: `src/lisp/school/school-db.lisp`
- Modify: `src/lisp/core/message-dispatcher.lisp`
- Test: `src/lisp/tests/school-split-tests.lisp`

**Steps:**
1) Write failing tests for: (a) new OOS request inserts into DB queue with request-id; (b) restart (simulated) keeps pending; (c) result with matching request-id clears queue and sets oos-sharpe.
2) Implement DB table `oos_queue(request_id TEXT PRIMARY KEY, name TEXT, requested_at INT, status TEXT, last_error TEXT)` with helper fns insert/update/delete/fetch-pending.
3) Generate UUID (or timestamp+random) request-id in `run-oos-validation`; store to queue; pass request-id in BACKTEST request payload (suffix unchanged).
4) On BACKTEST_RESULT, match by request-id (fallback to name for legacy) and mark success/remove.
5) Add retry window: if pending > *oos-request-interval*, resend and update `requested_at`.
6) Ensure in-memory `*oos-pending*` mirrors DB read at startup (or keep DB-only and remove hash).

### Task 2: CSV data quality gate
**Files:**
- Modify: `src/lisp/school/school-validation.lisp`
- Test: `src/lisp/tests/school-split-tests.lisp`

**Steps:**
1) Add checks: file exists, size > 0, header contains required columns, at least 1 data row, numeric parse on first row for OHLCV.
2) On failure, log and return message "Invalid data file" without sending OOS; ensure test covers each failure case.

### Task 3: OOS metrics & tracing
**Files:**
- Modify: `src/lisp/school/school-validation.lisp`
- Modify: `src/lisp/core/message-dispatcher.lisp`
- Modify: `src/lisp/school/school-narrative.lisp` (or metrics export site)
- Test: `src/lisp/tests/school-split-tests.lisp`

**Steps:**
1) Track counters: `oos_sent`, `oos_success`, `oos_failure`, `oos_retry`, and histogram buckets for latency (request->result) using timestamps.
2) Add tracing id = request-id; log send/receive with timestamps; add summary function to produce plist for reports/Discord.
3) Expose `report-oos-metrics` callable from narrative/report.
4) Tests: send/receive flow updates counts; latency computed from stubbed timestamps.

### Task 4: Dead-letter handling for BACKTEST_RESULT
**Files:**
- Modify: `src/lisp/core/message-dispatcher.lisp`
- Modify: `src/lisp/school/school-validation.lisp`
- Test: `src/lisp/tests/school-split-tests.lisp`

**Steps:**
1) When BACKTEST_RESULT decode/apply fails (missing name/request-id etc), push to `*backtest-dead-letter*` queue with payload+error.
2) Provide `replay-dead-letter` helper (manual) to reprocess entries.
3) Test: malformed result ends in dead-letter; replay succeeds after fix.

### Task 5: SQLite safety (WAL + transactions)
**Files:**
- Modify: `src/lisp/core/sqlite-manager.lisp` (or config spot)
- Modify: `src/lisp/school/school-db.lisp`
- Test: `src/lisp/tests/backtest-db-tests.lisp`

**Steps:**
1) On connection init, set `PRAGMA journal_mode=WAL` and `synchronous=NORMAL` (or current standard) once.
2) Wrap OOS queue ops and `upsert-strategy` in explicit transactions where multiple statements occur.
3) Test: pragma is set (query returns wal), upsert-with-oos-queue runs in txn (mock by intentional failure leaves no partial row).

### Task 6: Failure statistics in rank evaluation
**Files:**
- Modify: `src/lisp/school/school-rank-system.lisp`
- Modify: `src/lisp/school/school-narrative.lisp`
- Test: `src/lisp/tests/school-split-tests.lisp`

**Steps:**
1) Add counters for OOS send failures, data invalid, DB errors during rank cycle.
2) Expose summary in narrative/report, reset per cycle.
3) Test: simulate failures, verify counters increment and summary contains them.

### Task 7: Refresh optimization
**Files:**
- Modify: `src/lisp/school/school-db.lisp`
- Test: `src/lisp/tests/backtest-db-tests.lisp`

**Steps:**
1) Add incremental refresh: accept optional `:since-timestamp` to fetch only rows changed after last sync (requires `updated_at` column; add with default now on upsert).
2) Fallback to full refresh if column missing.
3) Tests: upsert sets updated_at; incremental call returns only newer rows; old rows not touched.

### Finalization
**Steps:**
1) Update `docs/llm/STATE.md` with new OOS queue persistence, metrics, WAL setting, and incremental refresh note.
2) If new interface fields (request-id in BACKTEST payload/result), update `docs/llm/INTERFACES.md` accordingly.
3) Run `sbcl --script test_runner.lisp` and ensure green.
4) Summarize changes.
