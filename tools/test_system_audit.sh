#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT="$ROOT/tools/system_audit.sh"

if [[ ! -x "$SCRIPT" ]]; then
  echo "Missing $SCRIPT" >&2
  exit 1
fi

help_output="$($SCRIPT --help)"
echo "$help_output" | grep -q "Usage:" || { echo "Missing usage" >&2; exit 1; }

run_output="$(DRY_RUN=1 $SCRIPT 2>&1)"
echo "$run_output" | grep -q "Summary" || { echo "Missing summary" >&2; exit 1; }
if echo "$run_output" | grep -q "SWIMMY_DISCORD_RECRUIT not set"; then
  echo "Expected .env to supply SWIMMY_DISCORD_RECRUIT" >&2
  exit 1
fi

tmp_dir="$(mktemp -d)"
cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

mock_log="$tmp_dir/sudo.log"
mock_sudo="$tmp_dir/mock_sudo"
cat >"$mock_sudo" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
if [[ -n "${MOCK_SUDO_LOG:-}" ]]; then
  echo "mock sudo: $*" >>"$MOCK_SUDO_LOG"
fi
exit 0
EOF
chmod +x "$mock_sudo"

MOCK_SUDO_LOG="$mock_log" SUDO_CMD="$mock_sudo" DRY_RUN=1 SWIMMY_AUDIT_SERVICES="mock-service" \
  "$SCRIPT" >/dev/null

if [[ ! -s "$mock_log" ]]; then
  echo "SUDO_CMD not used" >&2
  exit 1
fi
