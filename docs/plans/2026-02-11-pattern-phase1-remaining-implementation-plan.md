# Pattern Similarity Phase1 Remaining Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Phase 1 の未実装項目（Data Keeper Tick API と Lisp 側 Pattern Query 接続）を実装し、運用フローに接続する。

**Architecture:** Data Keeper に `ADD_TICK/GET_TICKS` を追加し、Lisp Core に Pattern Similarity クライアントを新設する。School execution の lot 計算フローで H1+ のみソフトゲート（0.7減衰）を適用する。

**Tech Stack:** Python 3, Common Lisp, ZeroMQ, S-expression, existing Swimmy telemetry.

---

### Task 1: Data Keeper Tick API (TDD)

**Files:**
- Modify: `tools/test_data_keeper_sexp.py`
- Modify: `tools/data_keeper.py`

1. `ADD_TICK` / `GET_TICKS` の失敗テストを書く。
2. テストを実行して fail を確認する。
3. Data Keeper 実装を追加する。
4. テストを再実行して pass を確認する。

### Task 2: Lisp Pattern Client + Gate wiring

**Files:**
- Create: `src/lisp/core/pattern-similarity-client.lisp`
- Modify: `src/lisp/core/config.lisp`
- Modify: `src/lisp/packages.lisp`
- Modify: `swimmy.asd`
- Modify: `src/lisp/school/school-execution.lisp`

1. Pattern Similarity REQ/REP クライアントを追加。
2. `SWIMMY_PORT_PATTERN_SIMILARITY` 設定と package export を追加。
3. School execution で H1+ のみ QUERY を実行し、不一致時 lot を0.7倍に減衰。
4. ゲート判定を structured telemetry へ記録。

### Task 3: Docs + verification

**Files:**
- Modify: `docs/llm/STATE.md`
- Modify if needed: `docs/llm/INTERFACES.md`

1. 実装状態を STATE に反映。
2. `tools/test_data_keeper_sexp.py`, `tools/test_pattern_similarity_sexp.py`, `tools/sbcl_sanity_check.sh` を実行。
3. 失敗があれば修正し再検証。
