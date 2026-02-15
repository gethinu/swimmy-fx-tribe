# Polymarket Weather Snapshots Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** add a snapshot collector that archives Gamma event/market state, deterministic signals, and CLOB orderbooks so we can run realistic (>30d) trading backtests later.

**Architecture:** a single Python tool `tools/polymarket_weather_snapshot.py` writes a timestamped snapshot directory under `data/snapshots/polymarket_weather/` with raw + normalized records and a manifest.

**Tech Stack:** Python 3, stdlib (`urllib`, `json`, `pathlib`), `unittest` tests.

---

### Task 1: Add failing unit test for snapshot output

**Files:**
- Create: `tools/test_polymarket_weather_snapshot.py`

**Step 1: Write the failing test**

Write a test that:
- stubs `_request_json()` to return:
  - 1 daily-weather series
  - 1 event with 1 market containing `clobTokenIds`
  - 2 CLOB `/book` payloads (YES/NO)
- stubs `subprocess.run()` to return 1 `signals.jsonl` line for that market id
- calls `collect_snapshot(...)`
- asserts that the output dir contains:
  - `manifest.json`, `gamma_series.json`, `gamma_events.json`, `markets.jsonl`, `signals.jsonl`, `clob_books.jsonl`
- asserts `clob_books.jsonl` has trimmed bids/asks to `orderbook_depth`

**Step 2: Run test to verify it fails**

Run: `python3 -m unittest tools/test_polymarket_weather_snapshot.py -v`

Expected: FAIL because `polymarket_weather_snapshot.py` does not exist.

---

### Task 2: Implement minimal snapshot collector to make the test pass

**Files:**
- Create: `tools/polymarket_weather_snapshot.py`

**Step 1: Implement minimal code**

Implement:
- `_request_json(url)` helper (Gamma + CLOB)
- parsing helpers: `parse_json_list_field`, `parse_clob_token_ids`
- `trim_book_payload(payload, depth)`
- `collect_snapshot(...)` that writes the expected files for the test scenario

**Step 2: Run tests**

Run: `python3 -m unittest tools/test_polymarket_weather_snapshot.py -v`

Expected: PASS

---

### Task 3: Add resilience + manifest metrics

**Files:**
- Modify: `tools/polymarket_weather_snapshot.py`
- Modify: `tools/test_polymarket_weather_snapshot.py`

**Step 1: Write failing test for error recording**

Add a case where one `/book` call raises an HTTP 404; assert an `errors.jsonl` record exists and the run still completes.

**Step 2: Implement error recording**

In collector:
- catch exceptions per request
- append error objects to `errors.jsonl`
- include `failures` and `counts` in `manifest.json`

**Step 3: Run tests**

Run: `python3 -m unittest tools/test_polymarket_weather_snapshot.py -v`

Expected: PASS

---

### Task 4: Add CLI + sane defaults

**Files:**
- Modify: `tools/polymarket_weather_snapshot.py`

**Steps:**
- Add argparse CLI:
  - `--output-dir` (default `data/snapshots/polymarket_weather`)
  - `--series-url`, `--events-url`, `--clob-host`
  - `--orderbook-mode all|topk|none`, `--orderbook-depth`, `--topk`
  - `--signal-command` (default from env `POLYCLAW_OPENCLAW_CMD`)
  - `--dry-run` (fetch + print manifest, don’t write)
- Ensure CLI returns non-zero on total failure (e.g., no series/events fetched)

**Verification:**
- Run: `python3 tools/polymarket_weather_snapshot.py --help`
- Run: `python3 tools/polymarket_weather_snapshot.py --orderbook-mode none --dry-run`

---

### Task 5: Document how to run periodically

**Files:**
- Modify: `docs/plans/2026-02-15-polymarket-weather-snapshots-design.md`

**Steps:**
- Add recommended systemd timer/cron cadence (e.g., every 15 min)
- Note storage growth expectations and retention suggestion (e.g., gzip/rotation)

