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

services_default=(swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-pattern-similarity swimmy-notifier swimmy-backtest)
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

SYSTEMCTL_BIN=""
if command -v systemctl >/dev/null 2>&1; then
  SYSTEMCTL_BIN="$(command -v systemctl)"
  SYSTEMCTL_STATUS_MODE="direct"
  SYSTEMCTL_REPAIR_MODE="direct"
else
  log "[WARN] systemctl command not found; skipping systemd checks"
  mark_warn
fi

sudo_systemctl_probe() {
  # Determine whether SUDO_CMD can run systemctl for Swimmy units without an
  # interactive prompt (e.g., passwordless sudoers for /usr/bin/systemctl restart/status/is-active).
  #
  # Note: we intentionally do NOT probe `sudo true` because many deployments grant
  # passwordless sudo only for specific systemctl subcommands/units.
  local probe_unit="${1:-swimmy-brain.service}"
  local output=""
  if [[ -z "${SUDO_CMD_ARR[0]:-}" ]] || ! command -v "${SUDO_CMD_ARR[0]}" >/dev/null 2>&1; then
    return 1
  fi
  output="$("${SUDO_CMD_ARR[@]}" "$SYSTEMCTL_BIN" is-active "$probe_unit" 2>&1 || true)"
  if printf '%s' "$output" | rg -qi "a password is required|interactive authentication required|authentication is required"; then
    return 1
  fi
  return 0
}

if [[ -n "$SYSTEMCTL_BIN" ]]; then
  if sudo_systemctl_probe "swimmy-brain.service"; then
    SYSTEMCTL_REPAIR_MODE="sudo"
    log "[INFO] sudo available for systemctl repairs; will use SUDO_CMD for restart steps"
  else
    # Running without passwordless sudo is normal in many environments.
    # Health WARNs should reflect actual service/audit issues, not privilege shape.
    log "[INFO] sudo unavailable for systemctl repairs; using direct systemctl"
  fi
fi

run_systemctl() {
  local mode="$1"
  shift
  case "$mode" in
    sudo)
      "${SUDO_CMD_ARR[@]}" "$SYSTEMCTL_BIN" "$@"
      ;;
    direct)
      "$SYSTEMCTL_BIN" "$@"
      ;;
    *)
      return 127
      ;;
  esac
}

service_fallback_health() {
  local svc="$1"
  case "$svc" in
    swimmy-pattern-similarity)
      local pid
      pid="$(pgrep -f "$ROOT/tools/pattern_similarity_service.py" | head -n 1 || true)"
      if [[ -n "$pid" ]] && ss -tulnp 2>/dev/null | rg -q ":5565\\b"; then
        printf 'pid=%s listener=:5565' "$pid"
        return 0
      fi
      return 1
      ;;
    *)
      return 1
      ;;
  esac
}

stop_pattern_similarity_fallback() {
  local pids
  pids="$(pgrep -f "$ROOT/tools/pattern_similarity_service.py" 2>/dev/null || true)"
  [[ -n "$pids" ]] || return 0
  log "[REPAIR] stopping Pattern Similarity fallback process(es): $(printf '%s' "$pids" | tr '\n' ' ')"
  # TERM first, then escalate if port remains busy.
  kill $pids 2>/dev/null || true
  for _ in {1..20}; do
    if ! ss -tulnp 2>/dev/null | rg -q ":5565\\b"; then
      return 0
    fi
    sleep 0.2
  done
  log "[WARN] Pattern Similarity fallback still listening on :5565; escalating to SIGKILL"
  mark_warn
  kill -9 $pids 2>/dev/null || true
  return 0
}

systemd_unit_load_state() {
  # Return LoadState (e.g., loaded / not-found). Empty on error.
  local svc="$1"
  run_systemctl "$SYSTEMCTL_STATUS_MODE" show -p LoadState --value "$svc" 2>/dev/null || true
}

run_systemctl_repair_step() {
  local desc="$1"
  shift
  local output
  if output="$(run_systemctl "$SYSTEMCTL_REPAIR_MODE" "$@" 2>&1)"; then
    return 0
  fi
  if printf '%s' "$output" | rg -qi "interactive authentication required|authentication is required"; then
    log "[WARN] repair skipped ($desc): $(printf '%s' "$output" | head -n 1)"
    mark_warn
    return 2
  fi
  if printf '%s' "$output" | rg -qi "sudo:.*password|a password is required|sorry, try again"; then
    log "[WARN] repair skipped ($desc): $(printf '%s' "$output" | head -n 1)"
    mark_warn
    return 2
  fi
  printf '%s\n' "$output" >&2
  return 1
}

# systemctl status (best effort)
if [[ -n "$SYSTEMCTL_STATUS_MODE" ]]; then
  for svc in "${services[@]}"; do
    if ! run_systemctl "$SYSTEMCTL_STATUS_MODE" status "$svc" >/dev/null 2>&1; then
      if fallback="$(service_fallback_health "$svc" 2>/dev/null)"; then
        log "[WARN] service unit unhealthy but fallback process is healthy: $svc ($fallback)"
        mark_warn
      else
        log "[WARN] service not healthy: $svc"
        mark_warn
      fi
    fi
  done
fi

# auto-repair
if [[ "${DRY_RUN:-0}" == "1" ]]; then
  log "[AUDIT] DRY_RUN=1; skipping auto-repair"
else
  if [[ -n "$SYSTEMCTL_REPAIR_MODE" ]]; then
    local_rc=0
    log "[REPAIR] daemon-reload"
    # run_systemctl_repair_step may return 2 when repair is skipped due to
    # Interactive authentication required. Under `set -e`, calling it directly
    # would abort the audit. Wrap in `if` so we can capture rc and continue.
    if run_systemctl_repair_step "daemon-reload" daemon-reload; then
      local_rc=0
    else
      local_rc=$?
    fi
    [[ $local_rc -eq 1 ]] && mark_fail

    for svc in "${services[@]}"; do
      if fallback="$(service_fallback_health "$svc" 2>/dev/null)"; then
        if [[ "$svc" == "swimmy-pattern-similarity" ]]; then
          unit_state="$(run_systemctl "$SYSTEMCTL_STATUS_MODE" is-active "$svc" 2>/dev/null || true)"
          if [[ "$unit_state" == "active" ]]; then
            # No drift: systemd unit is already managing it.
            continue
          fi
          # Drift: fallback is running but systemd unit is inactive.
          if [[ "$SYSTEMCTL_REPAIR_MODE" == "sudo" ]]; then
            load_state="$(systemd_unit_load_state "$svc")"
            if [[ -z "$load_state" || "$load_state" == "not-found" ]]; then
              log "[WARN] $svc drift: unit LoadState=${load_state:-unknown} but fallback is healthy ($fallback) → keep fallback; install unit via: sudo bash $ROOT/tools/install_services.sh"
              mark_warn
              continue
            fi
            log "[WARN] $svc drift: unit=$unit_state (LoadState=$load_state) but fallback is healthy ($fallback) → migrating to systemd"
            mark_warn
            stop_pattern_similarity_fallback
          else
            log "[WARN] skip systemctl repair for $svc; fallback process healthy ($fallback)"
            mark_warn
            continue
          fi
        else
          log "[WARN] skip systemctl repair for $svc; fallback process healthy ($fallback)"
          mark_warn
          continue
        fi
      fi
      log "[REPAIR] enable $svc"
      if run_systemctl_repair_step "enable $svc" enable "$svc"; then
        local_rc=0
      else
        local_rc=$?
      fi
      [[ $local_rc -eq 1 ]] && mark_fail
      log "[REPAIR] restart $svc"
      if run_systemctl_repair_step "restart $svc" restart "$svc"; then
        local_rc=0
      else
        local_rc=$?
      fi
      [[ $local_rc -eq 1 ]] && mark_fail
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
  # Polymarket/OpenClaw is optional. Treat disabled timer as intentional disablement
  # and skip audit to avoid stale false positives.
  polyclaw_timer="swimmy-polymarket-openclaw.timer"
  polyclaw_timer_state=""
  polyclaw_enabled="0"
  if [[ -n "$SYSTEMCTL_STATUS_MODE" ]]; then
    polyclaw_timer_state="$(run_systemctl "$SYSTEMCTL_STATUS_MODE" is-enabled "$polyclaw_timer" 2>/dev/null || true)"
    case "$polyclaw_timer_state" in
      enabled|enabled-runtime)
        polyclaw_enabled="1"
        ;;
      *)
        polyclaw_enabled="0"
        ;;
    esac
  fi

  if [[ "$polyclaw_enabled" == "1" ]]; then
    polyclaw_status_args=(
      --max-age-seconds "${POLYCLAW_STATUS_MAX_AGE_SECONDS:-1800}"
      --last-runs "${POLYCLAW_STATUS_LAST_RUNS:-5}"
      --window-minutes "${POLYCLAW_STATUS_WINDOW_MINUTES:-120}"
      --min-runs-in-window "${POLYCLAW_STATUS_MIN_RUNS_IN_WINDOW:-0}"
      --min-sent-in-window "${POLYCLAW_STATUS_MIN_SENT_IN_WINDOW:-0}"
      --min-entries-in-window "${POLYCLAW_STATUS_MIN_ENTRIES_IN_WINDOW:-0}"
      --fail-on-problem
    )
    if [[ -n "${POLYCLAW_OUTPUT_DIR:-}" ]]; then
      polyclaw_status_args+=(--output-dir "$POLYCLAW_OUTPUT_DIR")
    fi
    run_warn "Polymarket OpenClaw status" python3 "$ROOT/tools/polymarket_openclaw_status.py" "${polyclaw_status_args[@]}"
  else
    if [[ -n "$SYSTEMCTL_STATUS_MODE" ]]; then
      log "[SKIP] Polymarket OpenClaw status (disabled: $polyclaw_timer state=${polyclaw_timer_state:-unknown})"
    else
      log "[SKIP] Polymarket OpenClaw status (disabled: systemctl unavailable)"
    fi
  fi
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
