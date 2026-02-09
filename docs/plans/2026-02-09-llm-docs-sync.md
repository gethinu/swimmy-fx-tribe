# LLM Docs Sync Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** docs/llm の正本4ファイルを、現行コード＋ステージ済み変更に一致させる。

**Architecture:** 現行実装に基づき、S式ローカル保存の schema_version 差分、選抜/投票の複合スコア、MCP Gateway の実体フローをドキュメントへ反映する。内容は最小限の追記・修正に留め、仕様の齟齬を排除する。

**Tech Stack:** Markdown

---

### Task 1: 現行仕様の差分ポイントを確定する

**Files:**
- Read: `src/lisp/shell/notifications.lisp`
- Read: `src/lisp/school/school-telemetry.lisp`
- Read: `src/lisp/school/school-backtest-utils.lisp`
- Read: `src/lisp/school/school-constants.lisp`
- Read: `src/lisp/school/school-voting.lisp`
- Read: `tools/mcp_stdio_server.py`
- Read: `tools/mcp_gateway.py`

**Step 1: ファイル確認**
- 目的:
  - `.opus/live_status.sexp` の schema_version=2
  - `system_metrics.sexp` / `backtest_cache.sexp` の schema_version=1
  - 進化選抜スコアの重み（Sharpe/PF/WR/MaxDD）
  - 投票ウェイトの計算式（`1.0 + 0.6*score` クランプ）
  - MCP stdio JSON-RPC -> ZMQ (5559) の流れ

**Step 2: 変更点をメモ**
- 更新対象の doc を短い箇条書きでまとめる（ローカルでメモ、ファイル変更なし）

---

### Task 2: docs/llm/SPEC.md を更新する

**Files:**
- Modify: `docs/llm/SPEC.md`

**Step 1: ローカル保存の schema_version を明記**
- `backtest_cache.sexp` / `system_metrics.sexp` は `schema_version=1`
- `.opus/live_status.sexp` は `schema_version=2`

**Step 2: 選抜/投票の複合スコアを追記**
- スコア構成: Sharpe + PF + WR + (1-MaxDD)
- 重み: 0.4 / 0.25 / 0.2 / 0.15
- 投票ウェイト: `1.0 + 0.6*score` を `0.3–2.0` にクランプ

**Step 3: 変更を確認**
- Markdown の文脈が崩れていないことを目視確認

**Step 4: テスト**
- Doc-only のためテストなし

**Step 5: Commit**
```bash
git add docs/llm/SPEC.md
git commit -m "docs: sync spec with current scoring and schema versions"
```

---

### Task 3: docs/llm/ARCHITECTURE.md を更新する

**Files:**
- Modify: `docs/llm/ARCHITECTURE.md`

**Step 1: MCP Gateway の実体を追記**
- stdio JSON‑RPC サーバ -> ZMQ PUB 5559 -> Guardian の流れを図か本文に追加

**Step 2: 既存の External Command と整合**
- “External Command (PUB 5559)” が MCP Gateway 由来であることを明記

**Step 3: 変更を確認**
- Mermaid の整合性と文脈を目視確認

**Step 4: テスト**
- Doc-only のためテストなし

**Step 5: Commit**
```bash
git add docs/llm/ARCHITECTURE.md
git commit -m "docs: add mcp gateway flow to architecture"
```

---

### Task 4: docs/llm/INTERFACES.md を更新する

**Files:**
- Modify: `docs/llm/INTERFACES.md`

**Step 1: MCP stdio の補足**
- `system.metrics` / `backtest.status` を含む実メソッドを列挙
- `trade.submit` は常に 403 である旨を明記

**Step 2: S式/JSON 境界の説明を整合**
- 既存記述と矛盾しないよう短い注記を追加

**Step 3: 変更を確認**
- 既存スキーマ例との矛盾がないことを目視確認

**Step 4: テスト**
- Doc-only のためテストなし

**Step 5: Commit**
```bash
git add docs/llm/INTERFACES.md
git commit -m "docs: align interfaces with mcp methods and boundaries"
```

---

### Task 5: docs/llm/STATE.md を更新する

**Files:**
- Modify: `docs/llm/STATE.md`

**Step 1: Local Storage schema_version の差分を追記**
- `.opus/live_status.sexp` が v2、他は v1

**Step 2: 選抜/投票の複合スコアを短く追記**
- 仕様の正本として差分を1段落にまとめる

**Step 3: 変更を確認**
- 状態の箇条書き構成が崩れていないことを目視確認

**Step 4: テスト**
- Doc-only のためテストなし

**Step 5: Commit**
```bash
git add docs/llm/STATE.md
git commit -m "docs: sync state with schema versions and scoring"
```

---

### Final Verification

Run:
```bash
rg -n "schema_version=1|schema_version=2|live_status|system_metrics|backtest_cache|MCP" docs/llm
```
Expected: 4ファイルの記述が相互に矛盾していない。

---

Plan complete and saved to `docs/plans/2026-02-09-llm-docs-sync.md`.

Two execution options:
1. **Subagent-Driven (this session)** - I dispatch a fresh subagent per task, review between tasks
2. **Parallel Session (separate)** - Open new session with executing-plans

Which approach?

