# Expert Panel 2 (Reliability & Resilience) - 2026-02-14

## Snapshot (Evidence)
- Host: WSL2. At 2026-02-14 12:36 JST: Mem 14Gi (used 7.2Gi), Swap 4Gi (used 1.5Gi).
- Biggest memory consumers (systemd `MemoryCurrent`):
- `swimmy-data-keeper.service`: ~2.64GB (peak ~3.68GB) (`tools/data_keeper.py`).
- `swimmy-school.service`: ~2.14GB (peak ~2.89GB), `NRestarts=10` (SBCL `--dynamic-space-size 4096`).
- `swimmy-backtest.service`: ~1.09GB (peak ~2.54GB) plus 3 guardian workers (~0.35GB RSS each).
- Logs: `logs/` is ~6.8GB, and one file is ~4.4GB (`logs/swimmy.20260214_060705.log`).

## What Is Failing (Reliability Symptoms)
- School restarts are correlated with graveyard file read errors:
- journal shows `[GRAVEYARD] ⚠️ Read error (skipping): unmatched close parenthesis` followed by `Heap exhausted during garbage collection` and SBCL fatal `GC invariant lost` (multiple times on 2026-02-14).
- School CPU is dominated by self-inflicted work and log I/O:
- journal shows `[DB] 🗄️ SQLite tables ensured.` being printed at extremely high frequency, implying `init-db` is being called in hot paths.

## Root Cause Candidates (Code-Level)
- Graveyard scan is a periodic full-file parse:
- `src/lisp/school/school-p3-learning.lisp` `analyze-graveyard-for-avoidance` scans `data/memory/graveyard.sexp` using `read` and caches only for `SWIMMY_GRAVEYARD_AVOID_CACHE_SEC` (default 300s).
- `data/memory/graveyard.sexp` is large (163MB) and contains parse-breaking corruption; re-reading it every 5 minutes is not survivable.
- Graveyard writes are not corruption-resistant:
- `src/lisp/school/school-rank-system.lisp` `save-failure-pattern` appends with `write` to `data/memory/graveyard.sexp` without locking; concurrent writers can interleave output and corrupt S-expressions.
- DB schema init is executed repeatedly:
- `src/lisp/school/school-db.lisp` `init-db` prints `[DB] 🗄️ SQLite tables ensured.` and is invoked from hot paths (example: `src/lisp/school/school-validation.lisp` `maybe-request-oos-backtest`).

## The Panel (Reliability & Resilience)

### Werner Vogels (Infrastructure / Scalability)
- Everything fails all the time; build around that.
- Periodic full scans of 163MB+ files are a “missing cache primitive” and guarantee tail-latency spikes and GC stress.
- Recommended direction: store derived artifacts (avoid regions) not raw history, make them eventually consistent, and keep hot paths O(1) and bounded.

### Joe Armstrong (Fault Tolerance / Isolation)
- A single corrupted input file should never take down the core service.
- “Let it crash” only works when crashes are isolated and cheap.
- Recommended direction: move graveyard analysis into a separate supervised worker/process; the school should treat avoid-regions as optional (fallback to “no avoid regions” instead of dying).

### Margaret Hamilton (Safety / Software Assurance)
- Untrusted inputs must be treated as hostile.
- `read` on a large corrupted file in a long-running daemon is a safety violation: it creates unbounded allocation and undefined failure modes.
- Recommended direction: hard caps, deterministic failure handling, and transactional persistence (SQLite) for patterns; eliminate non-atomic append formats.

### W. Edwards Deming (Quality / Process Control)
- Current signals (swap use, restart count, log explosion) indicate an uncontrolled system.
- Recommended direction: define and track control metrics:
- restarts/day per service
- graveyard scan duration and last success time
- parse-error rate of graveyard ingestion
- memory headroom (MemAvailable) and swap in/out rate
- log lines/sec per service
- Only then change one variable at a time and verify improvement.

### Brendan Gregg (Performance / Observability)
- You have obvious “performance bugs” before you need deep profiling:
- stop log spam in tight loops
- stop 5-min full-file parses
- reduce global memory pressure so GC has headroom
- After that, profile with evidence (perf/strace for SBCL, py-spy for Python).

## Recommendation: Which Mitigation First?

### Best first move for reliability (availability)
P0: stop school crash loops and remove unbounded work from the main daemon.
- Fix graveyard ingestion:
- Increase `SWIMMY_GRAVEYARD_AVOID_CACHE_SEC` immediately (e.g. 21600 = 6h) to stop rescanning 2.6M entries every 5 minutes.
- Replace full rescans with incremental updates (track file offset, or store patterns in SQLite and query aggregates).
- Make persistence corruption-resistant (single-line buffered writes + file lock, or move patterns to SQLite).

### Best first move for broad resource contention (CPU/log I/O)
P1: stop calling `init-db` DDL logic in hot paths and stop printing per-call.
- Gate `init-db` so it runs only once per process after DB connection is established.
- Print `[DB] ... ensured` once per process, not per request.

### Best first move for pure memory reduction
P2: reduce `data_keeper` retained in-memory dataset.
- Load fewer symbols/timeframes, lazy-load M1 only, and make retention caps env-configurable.
- Replace per-candle dict objects with a compact representation (tuple/array) or memory-mapped storage.

## Concrete Next Steps (Low Risk, High ROI)
1. Set `SWIMMY_GRAVEYARD_AVOID_CACHE_SEC=21600` and restart `swimmy-school.service` once, so the daemon stops frequent full scans.
2. Deploy the `init-db` one-time gating change and restart `swimmy-school.service` to eliminate the `[DB] ... ensured` log storm.
3. Convert `graveyard.sexp` writer to produce one S-expression per line using a buffered string write, and add a file lock (or migrate patterns to SQLite).
4. Reduce `data_keeper` retention (start with M1) and verify backtests still satisfy your minimum history needs.

