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
