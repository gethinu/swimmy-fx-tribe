# Guarddog run.sh Cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** `tools/check_guardian_health.sh` に stray `run.sh` の自動停止を追加し、systemd-only運用を5分以内で強制する。

**Architecture:** 既存のGuardian監視フローを維持したまま、Guardianがactiveのときだけ `run.sh` プロセスを列挙し、systemd MainPID以外を停止する。テストではコマンド注入を使い実サービスに影響しない。

**Tech Stack:** Bash (cron watchdog), systemd `systemctl`, `pgrep`, `kill`.

---

### Task 1: Add failing test for stray run.sh cleanup

**Files:**
- Create: `tests/test_check_guardian_health.sh`

**Step 1: Write the failing test**

```bash
#!/bin/bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

FAKE_BIN="$TMP_DIR/bin"
mkdir -p "$FAKE_BIN"
KILL_LOG="$TMP_DIR/kill.log"

cat > "$FAKE_BIN/systemctl" <<'EOF'
#!/bin/bash
if [[ "$*" == *"is-active"* ]]; then exit 0; fi
if [[ "$*" == *"show"* ]]; then echo "MainPID=1234"; exit 0; fi
exit 1
EOF
chmod +x "$FAKE_BIN/systemctl"

cat > "$FAKE_BIN/pgrep" <<'EOF'
#!/bin/bash
echo "1234 /bin/bash /home/swimmy/swimmy/run.sh"
echo "5678 /bin/bash /home/swimmy/swimmy/run.sh"
EOF
chmod +x "$FAKE_BIN/pgrep"

cat > "$FAKE_BIN/kill" <<'EOF'
#!/bin/bash
echo "$*" >> "$KILL_LOG"
exit 0
EOF
chmod +x "$FAKE_BIN/kill"

cat > "$FAKE_BIN/sleep" <<'EOF'
#!/bin/bash
exit 0
EOF
chmod +x "$FAKE_BIN/sleep"

SYSTEMCTL_CMD="$FAKE_BIN/systemctl" \
PGREP_CMD="$FAKE_BIN/pgrep" \
KILL_CMD="$FAKE_BIN/kill" \
SLEEP_CMD="$FAKE_BIN/sleep" \
PATH="$FAKE_BIN:$PATH" \
"$ROOT_DIR/tools/check_guardian_health.sh" > "$TMP_DIR/out.log" 2>&1 || true

grep -q "STRAY run.sh" "$TMP_DIR/out.log" || { echo "FAIL: expected stray run.sh log"; exit 1; }
grep -q "\\-TERM 5678" "$KILL_LOG" || { echo "FAIL: expected TERM for 5678"; exit 1; }
grep -q "1234" "$KILL_LOG" && { echo "FAIL: should not kill MainPID"; exit 1; }

echo "PASS: stray run.sh cleanup"
```

**Step 2: Run test to verify it fails**

Run: `bash tests/test_check_guardian_health.sh`  
Expected: **FAIL** (no `STRAY run.sh` log / no kill calls).

---

### Task 2: Update STATE doc (policy before implementation)

**Files:**
- Modify: `docs/llm/STATE.md`

**Step 1: Add policy bullet**
- 決定事項/運用に「cron watchdog が systemd MainPID 以外の `run.sh` を自動停止する」旨を追記。

---

### Task 3: Implement stray run.sh cleanup (make test pass)

**Files:**
- Modify: `tools/check_guardian_health.sh`

**Step 1: Add command overrides**
- `SYSTEMCTL_CMD`, `PGREP_CMD`, `KILL_CMD`, `SLEEP_CMD` を導入（未設定時は既存コマンドを使う）。

**Step 2: Add cleanup block (Guardian active path)**

```bash
SYSTEMCTL_CMD="${SYSTEMCTL_CMD:-systemctl}"
PGREP_CMD="${PGREP_CMD:-pgrep}"
KILL_CMD="${KILL_CMD:-kill}"
SLEEP_CMD="${SLEEP_CMD:-sleep}"

MAINPID="$($SYSTEMCTL_CMD --user show -p MainPID swimmy-brain.service 2>/dev/null | cut -d= -f2)"

while read -r pid _; do
  [[ -z "$pid" ]] && continue
  if [[ -n "$MAINPID" && "$MAINPID" != "0" && "$pid" == "$MAINPID" ]]; then
    echo "[WATCHDOG] run.sh MainPID detected: $pid (skip)"
    continue
  fi
  echo "[WATCHDOG] STRAY run.sh detected: $pid (terminating)"
  $KILL_CMD -TERM "$pid" 2>/dev/null || true
  $SLEEP_CMD 2
  $KILL_CMD -KILL "$pid" 2>/dev/null || true
done < <($PGREP_CMD -af "/home/swimmy/swimmy/run.sh" | awk '{print $1}')
```

**Step 3: Run test to verify it passes**

Run: `bash tests/test_check_guardian_health.sh`  
Expected: **PASS**.

**Step 4: Syntax check**

Run: `bash -n tools/check_guardian_health.sh`  
Expected: no output / exit 0.

---

### Task 4: Commit (human-only)

**Step 1: Commit changes**  
Per workspace instruction, Codex will not commit. Human to run:

```bash
git add docs/llm/STATE.md tools/check_guardian_health.sh tests/test_check_guardian_health.sh docs/plans/2026-02-04-guarddog-runsh-cleanup-design.md docs/plans/2026-02-04-guarddog-runsh-cleanup-implementation-plan.md
git commit -m "ops: enforce systemd-only by cleaning stray run.sh"
```

---

**Skills:** @superpowers:test-driven-development, @superpowers:executing-plans
