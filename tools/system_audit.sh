#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOG_DIR="$ROOT/logs"
LOG_FILE="$LOG_DIR/system_audit.log"

mkdir -p "$LOG_DIR"

usage() {
  cat <<'USAGE'
Usage: tools/system_audit.sh [--help]
Environment:
  DRY_RUN=1  Skip auto-repair steps
  SWIMMY_AUDIT_SERVICES="svc1 svc2"  Override default services
USAGE
}

if [[ "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

services_default=(swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-notifier)
if [[ -n "${SWIMMY_AUDIT_SERVICES:-}" ]]; then
  # shellcheck disable=SC2206
  services=($SWIMMY_AUDIT_SERVICES)
else
  services=("${services_default[@]}")
fi

log() {
  printf '%s %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*" | tee -a "$LOG_FILE"
}

warn_count=0
fail_count=0

mark_warn() { warn_count=$((warn_count + 1)); }
mark_fail() { fail_count=$((fail_count + 1)); }

summary() {
  echo "Summary: FAIL=$fail_count WARN=$warn_count"
}

log "[AUDIT] Starting system audit (system scope)"

# systemctl status (best effort)
if sudo -n true 2>/dev/null; then
  for svc in "${services[@]}"; do
    if ! sudo systemctl status "$svc" >/dev/null 2>&1; then
      log "[WARN] service not healthy: $svc"
      mark_warn
    fi
  done
else
  log "[WARN] sudo not available; skipping systemctl status"
  mark_warn
fi

# auto-repair
if [[ "${DRY_RUN:-0}" == "1" ]]; then
  log "[AUDIT] DRY_RUN=1; skipping auto-repair"
else
  if sudo -n true 2>/dev/null; then
    log "[REPAIR] daemon-reload"
    sudo systemctl daemon-reload || mark_fail

    for svc in "${services[@]}"; do
      log "[REPAIR] enable $svc"
      sudo systemctl enable "$svc" || mark_fail
      log "[REPAIR] restart $svc"
      sudo systemctl restart "$svc" || mark_fail
    done
  else
    log "[FAIL] sudo not available for auto-repair"
    mark_fail
  fi
fi

summary

if [[ "${DRY_RUN:-0}" == "1" ]]; then
  exit 0
fi

if [[ $fail_count -gt 0 ]]; then
  exit 1
fi
if [[ $warn_count -gt 0 ]]; then
  exit 2
fi
exit 0
