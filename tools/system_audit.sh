#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOG_DIR="$ROOT/logs"
LOG_FILE="$LOG_DIR/system_audit.log"
ENV_FILE="$ROOT/.env"

mkdir -p "$LOG_DIR"

if [[ -f "$ROOT/tools/sbcl_env.sh" ]]; then
  # shellcheck disable=SC1091
  source "$ROOT/tools/sbcl_env.sh"
fi

load_env_file() {
  local env_file="$1"
  [[ -f "$env_file" ]] || return 0
  while IFS= read -r line || [[ -n "$line" ]]; do
    line="${line%%$'\r'}"
    [[ -z "$line" ]] && continue
    [[ "$line" =~ ^[[:space:]]*# ]] && continue
    [[ "$line" != *"="* ]] && continue
    local key="${line%%=*}"
    local val="${line#*=}"
    key="$(printf '%s' "$key" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
    key="${key#export }"
    [[ "$key" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]] || continue
    val="$(printf '%s' "$val" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
    val="${val%\"}"
    val="${val#\"}"
    val="${val%\'}"
    val="${val#\'}"
    if [[ -z "${!key:-}" ]]; then
      export "$key=$val"
    fi
  done < "$env_file"
}

load_env_file "$ENV_FILE"
SWIMMY_SBCL_DYNAMIC_SPACE_MB="${SWIMMY_SBCL_DYNAMIC_SPACE_MB:-4096}"
SUDO_CMD="${SUDO_CMD:-sudo -n}"
# shellcheck disable=SC2206
SUDO_CMD_ARR=($SUDO_CMD)

usage() {
  cat <<'USAGE'
Usage: tools/system_audit.sh [--help]
Environment:
  DRY_RUN=1  Skip auto-repair steps
  SUDO_CMD="sudo -n"  Override sudo invocation (e.g., "sudo" for interactive)
  SWIMMY_AUDIT_SERVICES="svc1 svc2"  Override default services
  .env is auto-loaded (environment variables take precedence)
USAGE
}

if [[ "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

services_default=(swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-notifier swimmy-pattern-similarity swimmy-backtest)
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

run_warn() {
  local label="$1"
  shift
  log "[STEP] $label"
  if "$@"; then
    log "[OK] $label"
  else
    log "[WARN] $label failed"
    mark_warn
  fi
}

run_fail() {
  local label="$1"
  shift
  log "[STEP] $label"
  if "$@"; then
    log "[OK] $label"
  else
    log "[FAIL] $label failed"
    mark_fail
  fi
}

log "[AUDIT] Starting system audit (system scope)"

SYSTEMCTL_STATUS_MODE=""
SYSTEMCTL_REPAIR_MODE=""

if "${SUDO_CMD_ARR[@]}" true 2>/dev/null; then
  SYSTEMCTL_STATUS_MODE="sudo"
  SYSTEMCTL_REPAIR_MODE="sudo"
elif command -v systemctl >/dev/null 2>&1; then
  SYSTEMCTL_STATUS_MODE="direct"
  SYSTEMCTL_REPAIR_MODE="direct"
  log "[WARN] sudo -n unavailable; using direct systemctl"
  mark_warn
else
  log "[WARN] systemctl command not found; skipping systemd checks"
  mark_warn
fi

run_systemctl() {
  local mode="$1"
  shift
  case "$mode" in
    sudo)
      "${SUDO_CMD_ARR[@]}" systemctl "$@"
      ;;
    direct)
      systemctl "$@"
      ;;
    *)
      return 127
      ;;
  esac
}

# systemctl status (best effort)
if [[ -n "$SYSTEMCTL_STATUS_MODE" ]]; then
  for svc in "${services[@]}"; do
    if ! run_systemctl "$SYSTEMCTL_STATUS_MODE" status "$svc" >/dev/null 2>&1; then
      log "[WARN] service not healthy: $svc"
      mark_warn
    fi
  done
fi

# auto-repair
if [[ "${DRY_RUN:-0}" == "1" ]]; then
  log "[AUDIT] DRY_RUN=1; skipping auto-repair"
else
  if [[ -n "$SYSTEMCTL_REPAIR_MODE" ]]; then
    log "[REPAIR] daemon-reload"
    run_systemctl "$SYSTEMCTL_REPAIR_MODE" daemon-reload || mark_fail

    for svc in "${services[@]}"; do
      log "[REPAIR] enable $svc"
      run_systemctl "$SYSTEMCTL_REPAIR_MODE" enable "$svc" || mark_fail
      log "[REPAIR] restart $svc"
      run_systemctl "$SYSTEMCTL_REPAIR_MODE" restart "$svc" || mark_fail
    done
  else
    log "[FAIL] systemctl not available for auto-repair"
    mark_fail
  fi
fi

run_warn "Dashboard" python3 "$ROOT/tools/dashboard.py"
run_fail "Backtest systemd drift" python3 "$ROOT/tools/systemd_drift_probe.py"
run_warn "Notifier log" tail -n 200 "$ROOT/logs/notifier.log"
run_warn "Notifier direct test" python3 "$ROOT/tools/test_notifier_direct.py"
run_warn "Broadcast routing" sbcl --dynamic-space-size "$SWIMMY_SBCL_DYNAMIC_SPACE_MB" --script "$ROOT/tools/broadcast_test_v2.lisp"
run_warn "Evolution report" tail -n 120 "$ROOT/data/reports/evolution_factory_report.txt"
if [[ -f "$ROOT/tools/polymarket_openclaw_status.py" ]]; then
  polyclaw_status_args=(
    --max-age-seconds "${POLYCLAW_STATUS_MAX_AGE_SECONDS:-1800}"
    --last-runs "${POLYCLAW_STATUS_LAST_RUNS:-5}"
    --fail-on-problem
  )
  if [[ -n "${POLYCLAW_OUTPUT_DIR:-}" ]]; then
    polyclaw_status_args+=(--output-dir "$POLYCLAW_OUTPUT_DIR")
  fi
  run_warn "Polymarket OpenClaw status" python3 "$ROOT/tools/polymarket_openclaw_status.py" "${polyclaw_status_args[@]}"
fi
run_fail "Integrity audit" sbcl --dynamic-space-size "$SWIMMY_SBCL_DYNAMIC_SPACE_MB" --script "$ROOT/tools/integrity_audit.lisp"
run_fail "Deep audit" sbcl --dynamic-space-size "$SWIMMY_SBCL_DYNAMIC_SPACE_MB" --script "$ROOT/tools/deep_audit.lisp"

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
