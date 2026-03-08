# WSL Ops LSP Stop Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** `swimmy-evolution` 停止後の運用契約と、WSL 上の `alive-lsp`/Codex セッションの関係を文書化し、`alive-lsp` のみを停止する。

**Architecture:** 変更対象は通信層ではなく運用ドキュメントと開発用プロセス管理に限定する。正本は `docs/llm/STATE.md` に追記し、操作系の補助説明は `docs/owners_guide.md` に追記する。実行時は VS Code Remote / Codex を残したまま `alive-lsp` の SBCL 子プロセスだけを停止する。

**Tech Stack:** systemd, VS Code Remote (WSL), SBCL, Markdown docs

---

### Task 1: Update canonical state

**Files:**
- Modify: `docs/llm/STATE.md`

**Step 1: Add the operational contract**

- `swimmy-evolution.service` の `disable --now` が symlink の解除であり unit file 削除ではないことを明記する。
- `alive-lsp` は VS Code Remote の extension host 配下で動く開発用 SBCL であり、Swimmy 本体ではないことを明記する。
- `wsl --shutdown` は WSL 上の VS Code Remote/Codex セッションを切断するため、軽量化目的ではまず `swimmy-evolution` や `alive-lsp` を対象にすることを明記する。

**Step 2: Run targeted verification**

Run: `rg -n "alive-lsp|swimmy-evolution|wsl --shutdown" docs/llm/STATE.md`
Expected: 新規追記箇所が表示される

### Task 2: Update operator guide

**Files:**
- Modify: `docs/owners_guide.md`

**Step 1: Add the operator note**

- `sudo systemctl disable --now swimmy-evolution.service` の意味と影響を説明する。
- `alive-lsp` 停止コマンドと、必要に応じて再起動されうることを追記する。
- `wsl --shutdown` は VS Code Remote/Codex 作業中は避けることを追記する。

**Step 2: Run targeted verification**

Run: `rg -n "alive-lsp|disable --now swimmy-evolution|wsl --shutdown" docs/owners_guide.md`
Expected: 新規追記箇所が表示される

### Task 3: Stop only alive-lsp

**Files:**
- Runtime only: no file creation

**Step 1: Stop the target process**

Run: `pkill -TERM -f "asdf:load-system :alive-lsp"`
Expected: exit 0 if stopped, exit 1 if already absent

**Step 2: Verify Codex remains running**

Run: `pgrep -af "alive-lsp|codex app-server|code-server"`
Expected: `alive-lsp` が消え、`codex app-server` と `code-server` は残る
