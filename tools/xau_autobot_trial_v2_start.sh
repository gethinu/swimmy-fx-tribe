#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

TRIAL_CONFIG="${XAU_AUTOBOT_TRIAL_CONFIG:-tools/configs/xau_autobot.trial_v2_20260222.json}"
TRIAL_LIVE="${XAU_AUTOBOT_TRIAL_LIVE:-1}"
POLL_SECONDS="${XAU_AUTOBOT_TRIAL_POLL_SECONDS:-10}"
ALLOW_EXISTING="${XAU_AUTOBOT_TRIAL_ALLOW_EXISTING_PROCESSES:-0}"
LOCK_FILE="${XAU_AUTOBOT_TRIAL_LOCK_FILE:-$ROOT/data/runtime/xau_autobot_trial_v2.lock}"

existing_autobot_processes() {
  ps -eo pid=,args= | awk '
    /xau_autobot.py|xau_autobot_live_loop\.ps1/ &&
    $0 !~ /awk/ &&
    $0 !~ /xau_autobot_trial_v2_start\.sh/ {
      print $0
    }
  '
}

acquire_lock() {
  mkdir -p "$(dirname "$LOCK_FILE")"
  if command -v flock >/dev/null 2>&1; then
    exec 9>"$LOCK_FILE"
    if ! flock -n 9; then
      echo "{\"action\":\"ERROR\",\"reason\":\"trial_lock_busy\",\"lock_file\":\"$LOCK_FILE\"}" >&2
      exit 1
    fi
    return
  fi

  # Fallback lock for environments without flock.
  local fallback_lock_dir="${LOCK_FILE}.d"
  if ! mkdir "$fallback_lock_dir" 2>/dev/null; then
    echo "{\"action\":\"ERROR\",\"reason\":\"trial_lock_busy\",\"lock_file\":\"$fallback_lock_dir\"}" >&2
    exit 1
  fi
  trap 'rmdir "$fallback_lock_dir" >/dev/null 2>&1 || true' EXIT
}

if [ "$ALLOW_EXISTING" != "1" ]; then
  RUNNING="$(existing_autobot_processes || true)"
  if [ -n "$RUNNING" ]; then
    echo '{"action":"ERROR","reason":"xau_autobot_process_already_running","hint":"stop existing xau_autobot.py/xau_autobot_live_loop.ps1 or set XAU_AUTOBOT_TRIAL_ALLOW_EXISTING_PROCESSES=1"}' >&2
    echo "$RUNNING" >&2
    exit 1
  fi
fi

acquire_lock

XAU_AUTOBOT_CONFIG="$TRIAL_CONFIG" \
XAU_AUTOBOT_LIVE="$TRIAL_LIVE" \
XAU_AUTOBOT_POLL_SECONDS="$POLL_SECONDS" \
XAU_AUTOBOT_LIVE_GUARD_ENABLED=0 \
"$ROOT/tools/xau_autobot_active_runner.sh"
