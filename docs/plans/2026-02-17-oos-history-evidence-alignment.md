# OOS/History/Evidence Alignment Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix OOS semantic drift, extend low-frequency history horizon, and separate official/composite trade evidence in reporting without changing promotion floors.

**Architecture:** Keep rank/promotion contracts unchanged, but make OOS dispatch range explicit (`start_time/end_time`) and ensure both Guardian BACKTEST paths honor it. Extend MT5 default history for low-frequency custom TF fallback to increase sample coverage. Update Top Candidates reporting to display and use separated `official/composite` evidence for candidate ordering only.

**Tech Stack:** Common Lisp (School/Dispatcher/Tests), Rust (Guardian), MQL5 (MT5 Bridge), SQLite-backed report query.

---

### Task 1: OOS semantic alignment
- Modify `src/lisp/school/school-validation.lisp`:
  - Add helper to compute OOS range (`*oos-test-ratio*`) from CSV timestamps.
  - Dispatch OOS with explicit `:start-ts/:end-ts` and standardized suffix handling.
- Modify `src/lisp/school/school-backtest.lisp`:
  - Extend `request-backtest` to accept and emit optional `start_time/end_time`, `phase`, `range_id`.
  - Normalize split-suffix detection for WFV/OOS compatibility.
- Modify `src/lisp/core/message-dispatcher.lisp`:
  - Normalize terminal OOS suffix handling for both `-OOS` and `_OOS` without breaking WFV.

### Task 2: Guardian date-range enforcement
- Modify `guardian/src/main.rs`:
  - Apply `start_time/end_time` slicing in SXP BACKTEST branch (external route).
  - Apply same slicing in `--backtest-only` flow for parity.

### Task 3: Low-frequency history extension
- Modify `src/mt5/SwimmyBridge.mq5`:
  - When `REQ_HISTORY` falls back to M1 for high-minute custom TF and `count<=0`, use extended default (M1-equivalent long horizon) instead of 100k.
  - Keep existing D1/W1 deep-history behavior.

### Task 4: official/composite evidence separation in report
- Modify `src/lisp/school/school-narrative.lisp`:
  - Top Candidates query should pull `official trades` and persisted composite evidence count (`backtest_trade_logs`).
  - Sort candidate priority by evidence-adjusted Sharpe using composite evidence, while showing both counts.
- Modify tests to lock behavior:
  - `src/lisp/tests/backtest-payload-tests.lisp`
  - `src/lisp/tests/school-split-tests.lisp`
  - `src/lisp/tests/backtest-db-tests.lisp`

### Task 5: Contract docs sync
- Modify `docs/llm/STATE.md`:
  - Add/update contracts for explicit OOS date-range dispatch, low-frequency history fallback extension, and official/composite top-candidate evidence display split.
