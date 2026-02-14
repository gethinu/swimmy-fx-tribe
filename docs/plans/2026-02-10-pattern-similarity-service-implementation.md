# Pattern Similarity Service Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a Pattern Similarity Service (ZMQ REQ/REP `5564`, S-expression `schema_version=1`) plus Data Keeper tick history APIs (`GET_TICKS`/`ADD_TICK`), and integrate a soft gate in Lisp that scales lots by `0.7` when pattern consensus disagrees.

**Architecture:** Lisp sends OHLCV windows (S-expression) to Pattern Similarity Service. The service embeds the window, performs kNN search over a persisted index (`data/patterns/`), returns distance-weighted probabilities (`p_up/p_down/p_flat`). Lisp applies a soft gate multiplier (default `0.7`) only for matching TF and only on `H1+` bar close, with fail-open on errors/timeouts.

**Tech Stack:** Python (`zmq`, `numpy`, `PIL`), optional (`torch`, `open_clip_torch`, `faiss-cpu`); Lisp (`pzmq`, existing `encode-sexp` / `safe-read-sexp`), systemd unit under `systemd/`.

### Task 1: Data Keeper Tick API Contract Tests

**Files:**
- Modify: `tools/test_data_keeper_sexp.py`

**Step 1: Write the failing test**

Add assertions for:
- `ADD_TICK` returns `status="ok"`
- `GET_TICKS` returns newest-first ticks with correct `count`

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_data_keeper_sexp.py`  
Expected: FAIL with `Unknown action: ADD_TICK` (or similar).

### Task 2: Implement Data Keeper `GET_TICKS` / `ADD_TICK`

**Files:**
- Modify: `tools/data_keeper.py`

**Step 1: Write minimal implementation**

- Add tick normalization (`timestamp/bid/ask/volume`) and validation.
- Persist ticks to `data/ticks/<SYMBOL>/<YYYYMMDD>.csv` (append-only).
- `GET_TICKS` supports:
  - `count` (required)
  - `start_time` / `end_time` (optional, Unix seconds)
  - returns newest-first.

**Step 2: Run tests**

Run: `python3 tools/test_data_keeper_sexp.py`  
Expected: PASS.

**Step 3: Commit**

```bash
git add tools/data_keeper.py tools/test_data_keeper_sexp.py
git commit -m "feat: add data keeper tick history APIs"
```

### Task 3: Pattern Similarity Service Contract Tests (No ML Yet)

**Files:**
- Create: `tools/pattern_similarity_service.py`
- Create: `tools/test_pattern_similarity_sexp.py`

**Step 1: Write the failing test**

`tools/test_pattern_similarity_sexp.py` should call a pure handler (no ZMQ required) and assert:
- `STATUS` returns `status="ok"` (or `status="error"` with clear reason if deps missing)
- `QUERY` returns `status="error"` when no index exists
- malformed requests return `status="error"`

**Step 2: Run test to verify it fails**

Run: `python3 tools/test_pattern_similarity_sexp.py`  
Expected: FAIL (`ModuleNotFoundError` or missing handler).

### Task 4: Implement Pattern Similarity Service Core (Parser, STATUS, QUERY errors)

**Files:**
- Modify: `tools/pattern_similarity_service.py`

**Step 1: Minimal implementation**

- Implement `handle_request_sexp(text: str) -> str` that matches `docs/llm/INTERFACES.md`.
- Implement `STATUS` and error responses (schema/type/action validation, payload size limit default 2MB).
- Stub `BUILD_INDEX` to start a background job and return `ok` (job body can be TODO initially).

**Step 2: Run tests**

Run: `python3 tools/test_pattern_similarity_sexp.py`  
Expected: PASS.

**Step 3: Commit**

```bash
git add tools/pattern_similarity_service.py tools/test_pattern_similarity_sexp.py
git commit -m "feat: add pattern similarity service protocol skeleton"
```

### Task 5: Implement Index Build + kNN (Pixel Embedding Fallback)

**Files:**
- Modify: `tools/pattern_similarity_service.py`

**Step 1: Write failing test**

Extend `tools/test_pattern_similarity_sexp.py` to:
- Build a tiny in-memory index (no disk) and assert QUERY returns `p_up/p_down/p_flat` and `top_k`.

**Step 2: Implement minimal functionality**

- Candle window validation (`window_bars` per TF).
- Image generation (PIL) from OHLC window.
- Embedding fallback: downsampled grayscale pixels (e.g. `32x32`) normalized to unit vector.
- kNN search:
  - Prefer `faiss` if installed, else `sklearn.neighbors.NearestNeighbors(metric="cosine")`.
- Distance-weighted probabilities from neighbor labels.
- Persist index artifacts under `data/patterns/` (`.npz` at minimum).

**Step 3: Run tests**

Run: `python3 tools/test_pattern_similarity_sexp.py`  
Expected: PASS.

**Step 4: Commit**

```bash
git add tools/pattern_similarity_service.py tools/test_pattern_similarity_sexp.py
git commit -m "feat: implement pattern similarity embedding and knn"
```

### Task 6: Lisp Gate Logic (Pure Function + Unit Tests)

**Files:**
- Create: `src/lisp/school/school-pattern-gate.lisp`
- Create: `src/lisp/tests/pattern-similarity-gate-tests.lisp`
- Modify: `swimmy.asd`

**Step 1: Write failing tests**

Add tests for:
- aligned + confidence>=0.60 => multiplier `1.0`
- disagreement or confidence<0.60 => multiplier `0.7`
- p_flat dominant => multiplier `0.7` (conservative)

**Step 2: Implement minimal gate function**

Implement a pure function (no IO) that returns `(values multiplier reason)` based on probabilities and direction.

**Step 3: Run Lisp tests**

Run: `sbcl --script tests/test_runner.lisp`  
Expected: PASS.

**Step 4: Commit**

```bash
git add src/lisp/school/school-pattern-gate.lisp src/lisp/tests/pattern-similarity-gate-tests.lisp swimmy.asd
git commit -m "feat: add pattern similarity gate core logic"
```

### Task 7: Lisp Client + Live Integration (Fail-Open)

**Files:**
- Create: `src/lisp/core/pattern-similarity-client.lisp`
- Modify: `src/lisp/core/config.lisp` (add `*port-pattern-similarity*`)
- Modify: `src/lisp/school/school-execution.lisp`
- Modify: `swimmy.asd`

**Step 1: Write failing tests**

Test that `execute-category-trade` applies multiplier when the gate client returns a synthetic response.

**Step 2: Implement minimal integration**

- ZMQ REQ client with short timeout (e.g. 500-1000ms).
- Query only when TF is `H1/H4/D1/W1/MN1` and new candle close is detected (existing new-candle logic).
- On client error/timeout: multiplier `1.0` and telemetry warning.
- Emit structured telemetry event with:
  - tf, symbol, p_up/p_down/p_flat, max_p, applied_multiplier, reason.

**Step 3: Run Lisp tests**

Run: `sbcl --script tests/test_runner.lisp`  
Expected: PASS.

**Step 4: Commit**

```bash
git add src/lisp/core/pattern-similarity-client.lisp src/lisp/core/config.lisp src/lisp/school/school-execution.lisp swimmy.asd
git commit -m "feat: integrate pattern similarity soft gate into execution"
```

### Task 8: Systemd Unit for Pattern Similarity Service

**Files:**
- Create: `systemd/swimmy-pattern-similarity.service`
- Modify (optional): `tools/install_services.sh`

**Steps:**
- Add unit consistent with other Swimmy python services (`User=swimmy`, `EnvironmentFile=.env`, log to `logs/pattern_similarity.log`).
- Run local syntax check: `systemd-analyze verify systemd/swimmy-pattern-similarity.service`

**Commit:**

```bash
git add systemd/swimmy-pattern-similarity.service tools/install_services.sh
git commit -m "feat: add systemd unit for pattern similarity service"
```

