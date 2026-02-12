#!/bin/bash
# tools/reload.sh - Hot Reload Trigger (Gene Kim's Lever)
# Sends SIGHUP to the running Swimmy Brain SBCL process.

set -u

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

log_info() {
  echo -e "${GREEN}[INFO] $*${NC}"
}

log_action() {
  echo -e "${GREEN}[ACTION] $*${NC}"
}

log_warn() {
  echo -e "${RED}[WARN] $*${NC}"
}

log_error() {
  echo -e "${RED}[ERROR] $*${NC}"
}

first_pid() {
  head -n 1 | tr -d '[:space:]'
}

is_positive_int() {
  [[ "${1:-}" =~ ^[0-9]+$ ]] && [ "$1" -gt 0 ]
}

resolve_brain_pid_from_systemd() {
  local main_pid cmdline child_pid

  if ! command -v systemctl >/dev/null 2>&1; then
    return 1
  fi

  main_pid=$(systemctl show -p MainPID --value swimmy-brain.service 2>/dev/null | tr -d '[:space:]')
  if ! is_positive_int "$main_pid"; then
    return 1
  fi

  cmdline=$(ps -p "$main_pid" -o args= 2>/dev/null || true)
  if [[ "$cmdline" == *"sbcl"* ]]; then
    echo "$main_pid"
    return 0
  fi

  child_pid=$(pgrep -P "$main_pid" -f "sbcl.*brain.lisp" 2>/dev/null | first_pid)
  if is_positive_int "$child_pid"; then
    echo "$child_pid"
    return 0
  fi

  child_pid=$(pgrep -P "$main_pid" -f "sbcl" 2>/dev/null | first_pid)
  if is_positive_int "$child_pid"; then
    echo "$child_pid"
    return 0
  fi

  return 1
}

resolve_brain_pid_fallback() {
  local pid
  pid=$(pgrep -f "sbcl.*brain.lisp" 2>/dev/null | first_pid)
  if is_positive_int "$pid"; then
    echo "$pid"
    return 0
  fi

  pid=$(pgrep -f "sbcl.*--load brain.lisp" 2>/dev/null | first_pid)
  if is_positive_int "$pid"; then
    echo "$pid"
    return 0
  fi

  pid=$(pgrep -f "sbcl.*swimmy" 2>/dev/null | first_pid)
  if is_positive_int "$pid"; then
    echo "$pid"
    return 0
  fi

  return 1
}

resolve_brain_pid() {
  local pid
  pid=$(resolve_brain_pid_from_systemd || true)
  if is_positive_int "$pid"; then
    echo "$pid"
    return 0
  fi

  pid=$(resolve_brain_pid_fallback || true)
  if is_positive_int "$pid"; then
    echo "$pid"
    return 0
  fi

  return 1
}

BRAIN_PID=$(resolve_brain_pid || true)
if ! is_positive_int "$BRAIN_PID"; then
  log_error "Swimmy Brain SBCL process not found. Is swimmy-brain running?"
  exit 1
fi

log_info "Target Brain SBCL PID: ${BRAIN_PID}"
log_action "Sending SIGHUP for Hot Reload..."

if kill -HUP "$BRAIN_PID"; then
  log_info "Signal sent. Verify with: journalctl -u swimmy-brain -n 50 --no-pager | rg 'Hot Reload|LOADER|ASDF'"
  exit 0
fi

log_error "Failed to send SIGHUP to PID ${BRAIN_PID}"
exit 1
