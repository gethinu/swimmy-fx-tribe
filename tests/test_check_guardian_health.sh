#!/bin/bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

FAKE_BIN="$TMP_DIR/bin"
mkdir -p "$FAKE_BIN"
KILL_LOG="$TMP_DIR/kill.log"
export KILL_LOG

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
grep -q "\-TERM 5678" "$KILL_LOG" || { echo "FAIL: expected TERM for 5678"; exit 1; }
grep -q "1234" "$KILL_LOG" && { echo "FAIL: should not kill MainPID"; exit 1; }

echo "PASS: stray run.sh cleanup"
