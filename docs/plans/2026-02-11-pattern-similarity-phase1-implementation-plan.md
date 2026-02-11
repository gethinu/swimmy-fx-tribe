# Pattern Similarity Phase1 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Pattern Similarity Service (Port 5565, S-expression) の Phase 1 実装（STATUS/BUILD_INDEX/QUERY）を追加し、systemd で起動可能にする。

**Architecture:** Python の単体サービス `tools/pattern_similarity_service.py` を追加し、`data/patterns/` に埋め込み・メタを保存する。近傍検索は FAISS があれば使用し、無い場合は NumPy のコサイン類似度フォールバックを使う。

**Tech Stack:** Python 3, ZeroMQ, NumPy, 既存 `aux_sexp` / `sexp_utils`, unittest。

---

### Task 1: Contract tests first (TDD red)

**Files:**
- Create: `tools/test_pattern_similarity_sexp.py`

1. `STATUS` が `PATTERN_SIMILARITY_RESULT` と `schema_version=1` を返す失敗テストを書く。
2. `BUILD_INDEX` が `status=ok` を返し、`QUERY` が `p_up/p_down/p_flat` を返す失敗テストを書く。
3. `type/action/candles length` 不正時の `status=error` テストを書く。
4. テストを実行して fail を確認する。

### Task 2: Implement service

**Files:**
- Create: `tools/pattern_similarity_service.py`

1. `handle_request_sexp` を実装し `STATUS/BUILD_INDEX/QUERY` を処理する。
2. `BUILD_INDEX` は `data/historical/*.csv` を読み、TFごとの `window_bars/stride` で埋め込みを生成して `data/patterns/<symbol>/<tf>/` に保存する。
3. `QUERY` は入力 `candles` から埋め込みを作り、上位k近傍と距離重み確率を返す。
4. 2MB 上限チェックとエラーレスポンスを実装する。

### Task 3: Service wiring

**Files:**
- Create: `systemd/swimmy-pattern-similarity.service`
- Modify: `tools/install_services.sh`

1. systemd ユニットを追加する。
2. install script の既定サービスリストに pattern similarity を追加する。

### Task 4: Verify green

**Files:**
- Modify if needed: `docs/llm/STATE.md`, `docs/llm/INTERFACES.md`

1. `python3 tools/test_pattern_similarity_sexp.py` を実行。
2. 既存の影響確認として `python3 tools/test_data_keeper_sexp.py` と `python3 tools/test_risk_gateway_sexp.py` を実行。
3. 実装契約とドキュメント差分を確認し、必要最小限を更新する。
