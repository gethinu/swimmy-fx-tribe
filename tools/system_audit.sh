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
  RUN_REGRESSION_BUNDLE=1  Run critical regression bundle (Lisp/Python/TF audit). Set 0 to skip
  BACKTEST_HEARTBEAT_LOG=/path/to/backtest.log  Override heartbeat log path
  BACKTEST_HEARTBEAT_MAX_LOG_AGE_SECONDS=600  Max age for backtest.log mtime
  BACKTEST_HEARTBEAT_MAX_RX_AGE_SECONDS=300  Max allowed heartbeat rx_age
  BACKTEST_HEARTBEAT_MAX_TX_AGE_SECONDS=300  Max allowed heartbeat tx_age
  BACKTEST_HEARTBEAT_STRICT=0  Set 1 to fail audit when heartbeat freshness check fails
  BACKTEST_HEARTBEAT_MAX_RXTX_SEC=180  (legacy) Max allowed rx_age/tx_age
  BACKTEST_HEARTBEAT_LOG_STALE_SEC=300  (legacy) Max allowed seconds since heartbeat log update
  SUDO_CMD="sudo -n"  Override sudo invocation (e.g., "sudo" for interactive)
  SWIMMY_AUDIT_SERVICES="svc1 svc2"  Override default services
  ORDER_TF_AUDIT_LOOKBACK_MINUTES=120  Recency window for order timeframe audit
  ORDER_TF_AUDIT_SINCE=ISO8601  Fixed lower bound for order timeframe audit (overrides lookback)
  ORDER_TF_AUDIT_STRATEGY=name  Optional exact strategy filter for order timeframe audit
  LIVE_TRADE_CLOSE_LOOKBACK_MINUTES=240  Recency window for trade-close integrity audit
  LIVE_TRADE_CLOSE_TAIL_LINES=5000  Log tail lines scanned for POSITIONS/legacy markers
  LIVE_TRADE_CLOSE_AFTER_ID=0  Ignore trade_logs rows with id <= this value
  LIVE_TRADE_CLOSE_REQUIRE_RECENT_TRADES=0  Set 1 to fail when no recent trade rows
  RANK_CONF_DB=/path/to/swimmy.db  Override rank conformance DB path
  RANK_CONF_LATEST_REPORT=/path/to/rank_conformance_latest.json  Latest rank audit report path
  RANK_CONF_HISTORY_DIR=/path/to/history/dir  Rank audit history output dir
  RANK_CONF_MAX_VIOLATIONS=0  Threshold for fail-on-problem in rank conformance audit
  EDGE_SCORECARD_DB=/path/to/swimmy.db  Override edge scorecard DB path
  EDGE_SCORECARD_LATEST_REPORT=/path/to/edge_scorecard_latest.json  Latest edge scorecard report path
  EDGE_SCORECARD_HISTORY_DIR=/path/to/edge_scorecard/history  Edge scorecard history output dir
  EDGE_SCORECARD_RANK_REPORT=/path/to/rank_conformance_latest.json  Rank conformance report path for scorecard
  EDGE_SCORECARD_SHORT_DAYS=7  Short window days for edge scorecard KPI
  EDGE_SCORECARD_LONG_DAYS=30  Long window days for edge scorecard KPI
  EDGE_SCORECARD_DISCORD_WHEN=problem  Discord notify mode for edge scorecard (never/problem/always)
  EDGE_SCORECARD_DISCORD_WEBHOOK=...  Optional explicit webhook for edge scorecard notify
  EDGE_SCORECARD_DISCORD_WEBHOOK_ENV=SWIMMY_DISCORD_ALERTS  Webhook env key for edge scorecard notify
  EDGE_SCORECARD_DISCORD_ZMQ_HOST=localhost  Notifier ZMQ host for edge scorecard notify
  EDGE_SCORECARD_DISCORD_ZMQ_PORT=5562  Notifier ZMQ port for edge scorecard notify
  TREND_ARB_STATUS_LATEST_RUN=/path/to/latest_run.json  Override trend-arbitrage run file
  TREND_ARB_STATUS_MAX_AGE_SECONDS=14400  Freshness threshold when timer enabled
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

run_order_timeframe_consistency_audit() {
  local order_tf_audit_args=(
    --db "${ORDER_TF_AUDIT_DB:-$ROOT/data/memory/swimmy.db}"
    --log "${ORDER_TF_AUDIT_LOG:-$ROOT/logs/swimmy.json.log}"
    --fail-on-issues
  )
  if [[ -n "${ORDER_TF_AUDIT_STRATEGY:-}" ]]; then
    order_tf_audit_args+=(--strategy "$ORDER_TF_AUDIT_STRATEGY")
  fi
  if [[ -n "${ORDER_TF_AUDIT_SINCE:-}" ]]; then
    order_tf_audit_args+=(--since "$ORDER_TF_AUDIT_SINCE")
  else
    order_tf_audit_args+=(--lookback-minutes "${ORDER_TF_AUDIT_LOOKBACK_MINUTES:-120}")
  fi
  python3 "$ROOT/tools/check_order_timeframe_consistency.py" "${order_tf_audit_args[@]}"
}

run_backtest_heartbeat_freshness() {
  local heartbeat_log="${BACKTEST_HEARTBEAT_LOG:-$ROOT/logs/backtest.log}"
  local max_log_age="${BACKTEST_HEARTBEAT_MAX_LOG_AGE_SECONDS:-${BACKTEST_HEARTBEAT_LOG_STALE_SEC:-600}}"
  local max_rx_age="${BACKTEST_HEARTBEAT_MAX_RX_AGE_SECONDS:-${BACKTEST_HEARTBEAT_MAX_RXTX_SEC:-300}}"
  local max_tx_age="${BACKTEST_HEARTBEAT_MAX_TX_AGE_SECONDS:-${BACKTEST_HEARTBEAT_MAX_RXTX_SEC:-300}}"
  if [[ -f "$ROOT/tools/check_backtest_heartbeat.py" ]]; then
    python3 "$ROOT/tools/check_backtest_heartbeat.py" \
      --log "$heartbeat_log" \
      --max-log-age-seconds "$max_log_age" \
      --max-rx-age-seconds "$max_rx_age" \
      --max-tx-age-seconds "$max_tx_age" \
      --fail-on-problem
    return $?
  fi
  log "[WARN] heartbeat check skipped: missing tools/check_backtest_heartbeat.py"
  return 1
}

run_rank_conformance_audit() {
  local rank_conf_report="${RANK_CONF_LATEST_REPORT:-$ROOT/data/reports/rank_conformance_latest.json}"
  local rc=0
  if python3 "$ROOT/tools/check_rank_conformance.py" \
    --db "${RANK_CONF_DB:-$ROOT/data/memory/swimmy.db}" \
    --out "$rank_conf_report" \
    --history-dir "${RANK_CONF_HISTORY_DIR:-$ROOT/data/reports/rank_conformance}" \
    --fail-on-problem \
    --max-violations "${RANK_CONF_MAX_VIOLATIONS:-0}"; then
    rc=0
  else
    rc=$?
  fi

  if [[ -f "$rank_conf_report" ]]; then
    local summary
    summary="$(python3 - "$rank_conf_report" <<'PY'
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
try:
    payload = json.loads(path.read_text(encoding="utf-8"))
except Exception:
    print("[WARN] Rank conformance summary unavailable: report JSON parse failed")
    raise SystemExit(0)

if not isinstance(payload, dict):
    print("[WARN] Rank conformance summary unavailable: invalid report payload")
    raise SystemExit(0)

transitions = payload.get("transitions", {})
violations = payload.get("violations", {})
floor = violations.get("floor", {})
conformance = violations.get("conformance", {})

def as_int(value):
    try:
        return int(value)
    except Exception:
        return 0

print(
    "[INFO] Rank conformance summary "
    f"promotions={as_int(transitions.get('promotion_count'))} "
    f"demotions={as_int(transitions.get('demotion_count'))} "
    f"changed={as_int(transitions.get('changed_count'))} "
    f"violations={as_int(violations.get('total'))} "
    f"a_lt_50={as_int(floor.get('A_lt_50'))} "
    f"s_lt_100={as_int(floor.get('S_lt_100'))} "
    f"s_fail={as_int(conformance.get('S_fail'))} "
    f"a_fail_to_b={as_int(conformance.get('A_fail_to_B'))} "
    f"b_fail_to_graveyard={as_int(conformance.get('B_fail_to_graveyard'))}"
)
PY
)"
    if [[ -n "$summary" ]]; then
      log "$summary"
    fi
  else
    log "[WARN] Rank conformance summary unavailable: report file missing ($rank_conf_report)"
  fi

  return "$rc"
}

run_edge_scorecard_audit() {
  local edge_scorecard_report="${EDGE_SCORECARD_LATEST_REPORT:-$ROOT/data/reports/edge_scorecard_latest.json}"
  local edge_scorecard_rank_report="${EDGE_SCORECARD_RANK_REPORT:-${RANK_CONF_LATEST_REPORT:-$ROOT/data/reports/rank_conformance_latest.json}}"
  local output=""
  local rc=0
  if output="$(python3 "$ROOT/tools/edge_scorecard.py" \
    --db "${EDGE_SCORECARD_DB:-$ROOT/data/memory/swimmy.db}" \
    --out "$edge_scorecard_report" \
    --history-dir "${EDGE_SCORECARD_HISTORY_DIR:-$ROOT/data/reports/edge_scorecard}" \
    --rank-report "$edge_scorecard_rank_report" \
    --short-days "${EDGE_SCORECARD_SHORT_DAYS:-7}" \
    --long-days "${EDGE_SCORECARD_LONG_DAYS:-30}" \
    --discord-when "${EDGE_SCORECARD_DISCORD_WHEN:-problem}" \
    --discord-webhook "${EDGE_SCORECARD_DISCORD_WEBHOOK:-}" \
    --discord-webhook-env "${EDGE_SCORECARD_DISCORD_WEBHOOK_ENV:-SWIMMY_DISCORD_ALERTS}" \
    --discord-zmq-host "${EDGE_SCORECARD_DISCORD_ZMQ_HOST:-localhost}" \
    --discord-zmq-port "${EDGE_SCORECARD_DISCORD_ZMQ_PORT:-${SWIMMY_PORT_NOTIFIER:-5562}}" 2>&1)"; then
    rc=0
  else
    rc=$?
  fi

  if [[ $rc -ne 0 && -n "$output" ]]; then
    printf '%s\n' "$output"
  fi

  if [[ -f "$edge_scorecard_report" ]]; then
    local summary
    summary="$(python3 - "$edge_scorecard_report" <<'PY'
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
try:
    payload = json.loads(path.read_text(encoding="utf-8"))
except Exception:
    print("[WARN] Edge scorecard summary unavailable: report JSON parse failed")
    raise SystemExit(0)

if not isinstance(payload, dict):
    print("[WARN] Edge scorecard summary unavailable: invalid report payload")
    raise SystemExit(0)

def get_status(key):
    value = payload.get(key, {})
    if isinstance(value, dict):
        return value.get("status", "unknown")
    return "unknown"

print(
    "[INFO] Edge scorecard summary "
    f"status={payload.get('overall_status', 'unknown')} "
    f"k0={get_status('kpi_live_edge_guard')} "
    f"k1={get_status('kpi_live_pnl_health')} "
    f"k2={get_status('kpi_rank_conformance')} "
    f"k3={get_status('kpi_breeder_parent_quality')}"
)
PY
)"
    if [[ -n "$summary" ]]; then
      log "$summary"
    fi
  else
    log "[WARN] Edge scorecard summary unavailable: report file missing ($edge_scorecard_report)"
  fi

  return "$rc"
}

detect_pid_scope() {
  local pid="$1"
  local cgroup_file="/proc/$pid/cgroup"
  local cgroup_data=""
  if [[ ! -r "$cgroup_file" ]]; then
    printf 'unknown'
    return 0
  fi
  cgroup_data="$(cat "$cgroup_file" 2>/dev/null || true)"
  if [[ -z "$cgroup_data" ]]; then
    printf 'unknown'
    return 0
  fi
  if printf '%s\n' "$cgroup_data" | rg -q '/system.slice/'; then
    printf 'system'
    return 0
  fi
  if printf '%s\n' "$cgroup_data" | rg -q '/user.slice/'; then
    printf 'user'
    return 0
  fi
  printf 'other'
  return 0
}

audit_systemd_scope_alignment() {
  local has_issue=0
  if [[ -z "$SYSTEMCTL_STATUS_MODE" ]]; then
    log "[SKIP] scope alignment check skipped: systemctl unavailable"
    return 0
  fi

  for svc in "${services[@]}"; do
    local unit_state=""
    local main_pid=""
    local scope=""
    unit_state="$(run_systemctl "$SYSTEMCTL_STATUS_MODE" is-active "$svc" 2>/dev/null || true)"
    [[ "$unit_state" == "active" ]] || continue

    main_pid="$(run_systemctl "$SYSTEMCTL_STATUS_MODE" show -p MainPID --value "$svc" 2>/dev/null || true)"
    main_pid="${main_pid//[[:space:]]/}"
    if [[ -z "$main_pid" || "$main_pid" == "0" ]]; then
      log "[WARN] scope alignment: $svc active but MainPID unavailable"
      has_issue=1
      continue
    fi

    scope="$(detect_pid_scope "$main_pid")"
    if [[ "$scope" != "system" ]]; then
      log "[WARN] scope alignment: $svc pid=$main_pid scope=$scope (expected=system)"
      has_issue=1
    fi
  done

  if [[ $has_issue -ne 0 ]]; then
    return 1
  fi
  return 0
}

log "[AUDIT] Starting system audit (system scope)"

RUN_REGRESSION_BUNDLE="${RUN_REGRESSION_BUNDLE:-1}"

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
run_warn "Systemd cgroup scope alignment" audit_systemd_scope_alignment

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

if [[ "$RUN_REGRESSION_BUNDLE" == "1" ]]; then
  run_fail "Regression suite (Lisp test runner)" \
    env SWIMMY_DISABLE_DISCORD=1 \
      sbcl --dynamic-space-size "$SWIMMY_SBCL_DYNAMIC_SPACE_MB" \
      --script "$ROOT/tests/test_runner.lisp"
  if [[ -f "$ROOT/tools/test_backtest_service.py" ]]; then
    run_fail "Regression suite (Backtest service)" \
      python3 "$ROOT/tools/test_backtest_service.py"
  else
    log "[WARN] Regression suite (Backtest service) skipped: missing tools/test_backtest_service.py"
    mark_warn
  fi
else
  log "[SKIP] Regression bundle disabled (RUN_REGRESSION_BUNDLE=$RUN_REGRESSION_BUNDLE)"
fi

run_warn "Dashboard" python3 "$ROOT/tools/dashboard.py"
run_fail "Backtest systemd drift" python3 "$ROOT/tools/systemd_drift_probe.py"
if [[ "${BACKTEST_HEARTBEAT_STRICT:-0}" == "1" ]]; then
  run_fail "Backtest heartbeat freshness" run_backtest_heartbeat_freshness
else
  run_warn "Backtest heartbeat freshness" run_backtest_heartbeat_freshness
fi
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

if [[ -f "$ROOT/tools/trend_arbitrage_status.py" ]]; then
  trend_timer="swimmy-trend-arbitrage.timer"
  trend_timer_state=""
  trend_enabled="0"
  if [[ -n "$SYSTEMCTL_STATUS_MODE" ]]; then
    trend_timer_state="$(run_systemctl "$SYSTEMCTL_STATUS_MODE" is-enabled "$trend_timer" 2>/dev/null || true)"
    case "$trend_timer_state" in
      enabled|enabled-runtime)
        trend_enabled="1"
        ;;
      *)
        trend_enabled="0"
        ;;
    esac
  fi

  if [[ "$trend_enabled" == "1" ]]; then
    run_warn "Trend arbitrage status" \
      python3 "$ROOT/tools/trend_arbitrage_status.py" \
        --latest-run "${TREND_ARB_STATUS_LATEST_RUN:-$ROOT/data/trend_arbitrage/latest_run.json}" \
        --max-age-seconds "${TREND_ARB_STATUS_MAX_AGE_SECONDS:-14400}" \
        --fail-on-problem
  else
    if [[ -n "$SYSTEMCTL_STATUS_MODE" ]]; then
      log "[SKIP] Trend arbitrage status (disabled: $trend_timer state=${trend_timer_state:-unknown})"
    else
      log "[SKIP] Trend arbitrage status (disabled: systemctl unavailable)"
    fi
  fi
fi

if [[ -f "$ROOT/tools/pattern_backend_calibration_status.py" ]]; then
  pattern_cal_timer="${PATTERN_BACKEND_CALIBRATION_TIMER_UNIT:-swimmy-pattern-backend-calibration.timer}"
  pattern_cal_report="${PATTERN_BACKEND_CALIBRATION_REPORT:-$ROOT/data/reports/pattern_backend_calibration_latest.json}"
  pattern_cal_max_age="${PATTERN_BACKEND_CALIBRATION_MAX_AGE_SECONDS:-172800}"
  run_warn "Pattern backend calibration status" \
    python3 "$ROOT/tools/pattern_backend_calibration_status.py" \
      --report "$pattern_cal_report" \
      --timer-unit "$pattern_cal_timer" \
      --max-age-seconds "$pattern_cal_max_age" \
      --fail-on-problem
fi
if [[ -f "$ROOT/tools/check_order_timeframe_consistency.py" ]]; then
  if [[ "$RUN_REGRESSION_BUNDLE" == "1" ]]; then
    run_fail "Regression suite (Order timeframe consistency)" \
      run_order_timeframe_consistency_audit
  else
    run_warn "Order timeframe consistency" \
      run_order_timeframe_consistency_audit
  fi
fi
if [[ -f "$ROOT/tools/check_rank_conformance.py" ]]; then
  run_warn "Rank conformance audit" run_rank_conformance_audit
fi
if [[ -f "$ROOT/tools/edge_scorecard.py" ]]; then
  run_warn "Edge scorecard" run_edge_scorecard_audit
fi
if [[ -f "$ROOT/tools/check_live_trade_close_integrity.py" ]]; then
  live_trade_close_args=(
    --db "${LIVE_TRADE_CLOSE_DB:-$ROOT/data/memory/swimmy.db}"
    --log "${LIVE_TRADE_CLOSE_LOG:-$ROOT/logs/swimmy.log}"
    --lookback-minutes "${LIVE_TRADE_CLOSE_LOOKBACK_MINUTES:-240}"
    --tail-lines "${LIVE_TRADE_CLOSE_TAIL_LINES:-5000}"
    --after-id "${LIVE_TRADE_CLOSE_AFTER_ID:-0}"
  )
  if [[ "${LIVE_TRADE_CLOSE_REQUIRE_RECENT_TRADES:-0}" == "1" ]]; then
    live_trade_close_args+=(--require-recent-trades)
  fi
  run_warn "Live trade-close integrity" \
    python3 "$ROOT/tools/check_live_trade_close_integrity.py" "${live_trade_close_args[@]}"
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
