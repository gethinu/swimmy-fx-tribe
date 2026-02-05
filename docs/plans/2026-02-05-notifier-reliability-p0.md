# Notifier Reliability P0 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make Discord notification delivery resilient under Notifier/Discord outages by (1) making Lisp→ZMQ sends non-blocking with drop counters, and (2) exposing Notifier queue/drops/errors as metrics and surfacing them via `data/system_metrics.sexp`.

**Architecture:** Keep current Notifier design (Lisp PUSH → `tools/notifier.py` PULL → Discord HTTP). Add bounded queue + metrics spool (`data/notifier_metrics.sexp`) written by Notifier. Extend Lisp telemetry (`src/lisp/school/school-telemetry.lisp`) to merge those metrics into `data/system_metrics.sexp`.

**Tech Stack:** Common Lisp (SBCL, `pzmq`), Python 3 (`pyzmq`, `requests`), S-expression local storage (`*.sexp`).

---

### Task 1: Non-blocking ZMQ send + drop/error counters (Lisp)

**Files:**
- Modify: `src/lisp/core/globals.lisp`
- Modify: `src/lisp/core/discord.lisp`
- Test: `src/lisp/tests.lisp`

**Step 1: Write the failing test (dontwait flag)**

Add a test that captures `pzmq:send` args and asserts `:dontwait t` is passed.

**Step 2: Run test suite to verify it fails**

Run: `sbcl --script tests/test_runner.lisp`
Expected: FAIL with assertion like “Expected :dontwait in pzmq:send args”.

**Step 3: Write minimal implementation**

- Add counters in `src/lisp/core/globals.lisp`:
  - `*notifier-zmq-drop-count*`
  - `*notifier-zmq-error-count*`
- In `src/lisp/core/discord.lisp`, send with `:dontwait t`.
- On “Resource temporarily unavailable”, drop + increment `*notifier-zmq-drop-count*` (no blocking).
- On other errors, increment `*notifier-zmq-error-count*` and reset socket so next call reconnects.

**Step 4: Run test suite to verify it passes**

Run: `sbcl --script tests/test_runner.lisp`
Expected: PASS (all green).

---

### Task 2: Notifier queue bounded + metrics spool (Python)

**Files:**
- Modify: `tools/notifier.py`
- Create: `tools/test_notifier_metrics_sexp.py`

**Step 1: Write the failing test (metrics spool writer)**

Create `tools/test_notifier_metrics_sexp.py`:
- Calls a new helper (to be added) that writes a metrics `.sexp`.
- Reads it back using `src/python/sexp_utils.load_sexp_alist`.
- Asserts keys exist: `schema_version`, `timestamp`, `queue_len`, `drops`, `errors`.

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_notifier_metrics_sexp.py`
Expected: FAIL (missing function or missing keys).

**Step 3: Write minimal implementation**

In `tools/notifier.py`:
- Add `SWIMMY_NOTIFIER_QUEUE_MAX` (default e.g. 1000) and enforce bounded `deque` (drop-oldest policy).
- Track counters: `drops`, `errors`.
- Add helper to write `data/notifier_metrics.sexp` atomically (tmp→rename).

**Step 4: Run test to verify it passes**

Run: `python3 tools/test_notifier_metrics_sexp.py`
Expected: `OK`.

---

### Task 3: Merge notifier metrics into `system_metrics.sexp` (Lisp telemetry)

**Files:**
- Modify: `src/lisp/school/school-telemetry.lisp`
- Test: `src/lisp/tests/local-storage-sexp-tests.lisp`

**Step 1: Write the failing test**

Extend `test-telemetry-sexp`:
- Write a temp `notifier_metrics.sexp` containing `(queue_len . 5) (drops . 2) (errors . 1)`.
- Call `save-telemetry-sexp`.
- Assert `system_metrics.sexp` includes `notifier_queue_len/notifier_drops/notifier_errors`.

**Step 2: Run test suite to verify it fails**

Run: `sbcl --script tests/test_runner.lisp`
Expected: FAIL (missing notifier_* keys).

**Step 3: Write minimal implementation**

In `src/lisp/school/school-telemetry.lisp`:
- Add `*notifier-metrics-file*` defaulting to `data/notifier_metrics.sexp`.
- Read it with `swimmy.core:read-sexp-file` (safe reader).
- When present, merge into telemetry payload as:
  - `(notifier_queue_len . <n>)`
  - `(notifier_drops . <n>)`
  - `(notifier_errors . <n>)`

**Step 4: Run test suite to verify it passes**

Run: `sbcl --script tests/test_runner.lisp`
Expected: PASS.

---

### Task 4: Final verification

**Step 1: Lisp tests**

Run: `sbcl --script tests/test_runner.lisp`
Expected: `Passed: <N>, Failed: 0`.

**Step 2: Python smoke tests (local)**

Run:
- `python3 tools/test_notifier_metrics_sexp.py`
- `python3 tools/test_report_status_sexp.py`
Expected: `OK`.

