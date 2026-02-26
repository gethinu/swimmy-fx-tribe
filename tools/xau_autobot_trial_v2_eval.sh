#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

PYTHON_EXE="${XAU_AUTOBOT_PYTHON:-$ROOT/.venv/bin/python}"
DEFAULT_TRIAL_CONFIG="$ROOT/tools/configs/xau_autobot.trial_v2_20260222.json"
TRIAL_CONFIG_ENV="${XAU_AUTOBOT_TRIAL_CONFIG:-}"
TRIAL_DAYS="${XAU_AUTOBOT_TRIAL_DAYS:-14}"
RUN_META_PATH="${XAU_AUTOBOT_TRIAL_RUN_META_PATH:-$ROOT/data/reports/xau_autobot_trial_v2_current_run.json}"
WATCHDOG_ENABLED="${XAU_AUTOBOT_TRIAL_WATCHDOG_ENABLED:-0}"
LIVE_REPORT_RETRIES="${XAU_AUTOBOT_TRIAL_LIVE_REPORT_RETRIES:-3}"
LIVE_REPORT_RETRY_SLEEP_SECONDS="${XAU_AUTOBOT_TRIAL_LIVE_REPORT_RETRY_SLEEP_SECONDS:-5}"
RUNTIME_JOURNAL_PATH="${XAU_AUTOBOT_RUNTIME_JOURNAL_PATH:-$ROOT/data/reports/xau_autobot_runtime_journal_latest.jsonl}"

is_wsl_env() {
  if [ -n "${WSL_INTEROP:-}" ] || [ -n "${WSL_DISTRO_NAME:-}" ]; then
    return 0
  fi
  uname -r 2>/dev/null | tr '[:upper:]' '[:lower:]' | grep -q "microsoft"
}

find_windows_tool() {
  local tool_name="$1"
  local resolved=""
  resolved="$(command -v "$tool_name" 2>/dev/null || true)"
  if [ -n "$resolved" ]; then
    printf '%s' "$resolved"
    return 0
  fi
  case "$tool_name" in
    cmd.exe)
      if [ -x /mnt/c/WINDOWS/system32/cmd.exe ]; then
        printf '%s' /mnt/c/WINDOWS/system32/cmd.exe
        return 0
      fi
      if [ -x /mnt/c/Windows/System32/cmd.exe ]; then
        printf '%s' /mnt/c/Windows/System32/cmd.exe
        return 0
      fi
      ;;
  esac
  return 1
}

resolve_trial_run_id() {
  local explicit="${XAU_AUTOBOT_TRIAL_RUN_ID:-}"
  if [ -n "$explicit" ]; then
    printf '%s' "$explicit"
    return
  fi
  if [ -f "$RUN_META_PATH" ]; then
    local from_meta
    from_meta="$("$PYTHON_EXE" - <<'PY' "$RUN_META_PATH"
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
try:
    payload = json.loads(path.read_text(encoding="utf-8"))
except Exception:
    payload = {}
run_id = str(payload.get("run_id", "")).strip() if isinstance(payload, dict) else ""
print(run_id)
PY
)"
    if [ -n "$from_meta" ]; then
      printf '%s' "$from_meta"
      return
    fi
  fi
  printf 'trial_v2_%s' "$(date -u +%Y%m%d_%H%M%S)"
}

resolve_trial_window() {
  "$PYTHON_EXE" - <<'PY' "$RUN_META_PATH" "$TRIAL_RUN_ID" "$TRIAL_DAYS"
import json
import os
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

meta_path = Path(sys.argv[1])
trial_run_id = str(sys.argv[2] or "").strip()
trial_days_raw = sys.argv[3]
explicit_start = str(os.environ.get("XAU_AUTOBOT_TRIAL_START_UTC", "")).strip()
explicit_end = str(os.environ.get("XAU_AUTOBOT_TRIAL_END_UTC", "")).strip()


def parse_utc(text: str) -> datetime:
    dt = datetime.fromisoformat(text.replace("Z", "+00:00"))
    if dt.tzinfo is None:
        return dt.replace(tzinfo=timezone.utc)
    return dt.astimezone(timezone.utc)


if (explicit_start and not explicit_end) or (explicit_end and not explicit_start):
    raise SystemExit("both XAU_AUTOBOT_TRIAL_START_UTC and XAU_AUTOBOT_TRIAL_END_UTC are required")

if explicit_start and explicit_end:
    start = parse_utc(explicit_start)
    end = parse_utc(explicit_end)
    if start >= end:
        raise SystemExit("trial window invalid: start must be before end")
    print(start.isoformat())
    print(end.isoformat())
    raise SystemExit(0)

try:
    trial_days = float(trial_days_raw)
except Exception:
    trial_days = 14.0
if trial_days <= 0:
    trial_days = 14.0

if not meta_path.exists():
    print("")
    print("")
    raise SystemExit(0)

try:
    payload = json.loads(meta_path.read_text(encoding="utf-8"))
except Exception:
    payload = {}
if not isinstance(payload, dict):
    payload = {}

meta_run_id = str(payload.get("run_id", "")).strip()
started_at = str(payload.get("started_at_utc", "")).strip()
if not started_at or (trial_run_id and meta_run_id and meta_run_id != trial_run_id):
    print("")
    print("")
    raise SystemExit(0)

start = parse_utc(started_at)
window_end = start + timedelta(days=trial_days)
now = datetime.now(timezone.utc)
end = window_end if window_end <= now else now
if start >= end:
    raise SystemExit("trial window invalid: start must be before end")

print(start.isoformat())
print(end.isoformat())
PY
}

resolve_trial_config() {
  if [ -n "$TRIAL_CONFIG_ENV" ]; then
    printf '%s' "$TRIAL_CONFIG_ENV"
    return
  fi
  if [ -f "$RUN_META_PATH" ]; then
    local from_meta
    from_meta="$("$PYTHON_EXE" - <<'PY' "$RUN_META_PATH" "$TRIAL_RUN_ID"
import json
import sys
from pathlib import Path

meta_path = Path(sys.argv[1])
trial_run_id = str(sys.argv[2] or "").strip()
try:
    payload = json.loads(meta_path.read_text(encoding="utf-8"))
except Exception:
    payload = {}
if not isinstance(payload, dict):
    payload = {}
meta_run_id = str(payload.get("run_id", "")).strip()
trial_config = str(payload.get("trial_config", "")).strip()
if not trial_config:
    print("")
    raise SystemExit(0)
if trial_run_id and meta_run_id and trial_run_id != meta_run_id:
    print("")
    raise SystemExit(0)
print(trial_config)
PY
)"
    if [ -n "$from_meta" ]; then
      printf '%s' "$from_meta"
      return
    fi
  fi
  printf '%s' "$DEFAULT_TRIAL_CONFIG"
}

TRIAL_RUN_ID="$(resolve_trial_run_id)"
TRIAL_CONFIG="$(resolve_trial_config)"
LIVE_REPORT_PATH="${XAU_AUTOBOT_TRIAL_LIVE_REPORT:-$ROOT/data/reports/xau_autobot_live_report_${TRIAL_RUN_ID}.json}"
JUDGE_REPORT_PATH="${XAU_AUTOBOT_TRIAL_JUDGE_REPORT:-$ROOT/data/reports/xau_autobot_trial_judge_${TRIAL_RUN_ID}.json}"
LIVE_REPORT_LATEST="${XAU_AUTOBOT_TRIAL_LIVE_REPORT_LATEST:-$ROOT/data/reports/xau_autobot_live_report_trial_v2_latest.json}"
JUDGE_REPORT_LATEST="${XAU_AUTOBOT_TRIAL_JUDGE_REPORT_LATEST:-$ROOT/data/reports/xau_autobot_trial_judge.json}"
WATCHDOG_REPORT_PATH="${XAU_AUTOBOT_TRIAL_WATCHDOG_WRITE_REPORT:-$ROOT/data/reports/xau_autobot_trial_watchdog_${TRIAL_RUN_ID}.json}"

readarray -t CFG_VALUES < <("$PYTHON_EXE" - <<'PY' "$TRIAL_CONFIG"
import json
import sys
from pathlib import Path

cfg = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
print(str(cfg.get("symbol", "XAUUSD")))
print(str(int(cfg.get("magic", 560072))))
comment = str(cfg.get("comment", "xau_autobot_trial_v2_20260222"))
# MT5 deal history truncates comment field on some brokers; keep prefix filter in sync.
print(comment[:16])
PY
)

SYMBOL="${CFG_VALUES[0]}"
MAGIC="${CFG_VALUES[1]}"
COMMENT_PREFIX="${CFG_VALUES[2]}"
readarray -t WINDOW_VALUES < <(resolve_trial_window)
TRIAL_START_UTC="${WINDOW_VALUES[0]:-}"
TRIAL_END_UTC="${WINDOW_VALUES[1]:-}"

LIVE_REPORT_ARGS=(
  --symbol "$SYMBOL"
  --magic "$MAGIC"
  --comment-prefix "$COMMENT_PREFIX"
  --runtime-journal-path "$RUNTIME_JOURNAL_PATH"
  --include-details
  --diagnostics
  --write-report "$LIVE_REPORT_PATH"
)
if [ -n "$TRIAL_START_UTC" ] && [ -n "$TRIAL_END_UTC" ]; then
  LIVE_REPORT_ARGS+=(--start-utc "$TRIAL_START_UTC" --end-utc "$TRIAL_END_UTC")
else
  LIVE_REPORT_ARGS+=(--days "$TRIAL_DAYS")
fi

run_live_report_once() {
  if "$PYTHON_EXE" -c "import MetaTrader5" >/dev/null 2>&1; then
    "$PYTHON_EXE" "$ROOT/tools/xau_autobot_live_report.py" "${LIVE_REPORT_ARGS[@]}"
    return $?
  fi

  if is_wsl_env && command -v wslpath >/dev/null 2>&1; then
    local cmd_exe
    cmd_exe="$(find_windows_tool cmd.exe || true)"
    if [ -z "$cmd_exe" ]; then
      return 127
    fi

    local win_script
    local win_report
    local win_runtime_journal
    win_script="$(wslpath -w "$ROOT/tools/xau_autobot_live_report.py")"
    win_report="$(wslpath -w "$LIVE_REPORT_PATH")"
    win_runtime_journal="$(wslpath -w "$RUNTIME_JOURNAL_PATH")"
    local win_live_report_args
    win_live_report_args=("${LIVE_REPORT_ARGS[@]}")
    for i in "${!win_live_report_args[@]}"; do
      if [ "${win_live_report_args[$i]}" = "$LIVE_REPORT_PATH" ]; then
        win_live_report_args[$i]="$win_report"
      elif [ "${win_live_report_args[$i]}" = "$RUNTIME_JOURNAL_PATH" ]; then
        win_live_report_args[$i]="$win_runtime_journal"
      fi
    done
    "$cmd_exe" /c py -3 "$win_script" "${win_live_report_args[@]}"
    return $?
  fi

  return 127
}

if ! [[ "$LIVE_REPORT_RETRIES" =~ ^[0-9]+$ ]] || [ "$LIVE_REPORT_RETRIES" -lt 1 ]; then
  LIVE_REPORT_RETRIES=1
fi

LIVE_REPORT_EXIT=0
for ((attempt = 1; attempt <= LIVE_REPORT_RETRIES; attempt++)); do
  set +e
  run_live_report_once
  LIVE_REPORT_EXIT=$?
  set -e

  if [ "$LIVE_REPORT_EXIT" -eq 0 ]; then
    break
  fi

  if [ "$LIVE_REPORT_EXIT" -eq 127 ]; then
    echo '{"action":"ERROR","reason":"mt5_python_missing","hint":"install MetaTrader5 for Linux or ensure Windows py -3 works"}' >&2
    exit 1
  fi

  if [ "$attempt" -lt "$LIVE_REPORT_RETRIES" ]; then
    echo "{\"action\":\"WARN\",\"reason\":\"live_report_retry\",\"attempt\":$attempt,\"max_retries\":$LIVE_REPORT_RETRIES}" >&2
    sleep "$LIVE_REPORT_RETRY_SLEEP_SECONDS"
  fi
done

if [ "$LIVE_REPORT_EXIT" -ne 0 ]; then
  echo '{"action":"ERROR","reason":"mt5_python_missing","hint":"install MetaTrader5 for Linux or ensure Windows py -3 works"}' >&2
  exit 1
fi

JUDGE_EXIT_CODE=0
set +e
"$PYTHON_EXE" "$ROOT/tools/xau_autobot_trial_judge.py" \
  --live-report "$LIVE_REPORT_PATH" \
  --min-days "${XAU_AUTOBOT_TRIAL_MIN_DAYS:-14}" \
  --min-closed-positions "${XAU_AUTOBOT_TRIAL_MIN_CLOSED_POSITIONS:-12}" \
  --min-profit-factor "${XAU_AUTOBOT_TRIAL_MIN_PROFIT_FACTOR:-1.10}" \
  --min-win-rate "${XAU_AUTOBOT_TRIAL_MIN_WIN_RATE:-0.42}" \
  --min-net-profit "${XAU_AUTOBOT_TRIAL_MIN_NET_PROFIT:-0}" \
  --write-report "$JUDGE_REPORT_PATH" \
  --fail-on-no-go
JUDGE_EXIT_CODE=$?
set -e

mkdir -p "$(dirname "$LIVE_REPORT_LATEST")" "$(dirname "$JUDGE_REPORT_LATEST")"
cp "$LIVE_REPORT_PATH" "$LIVE_REPORT_LATEST"
cp "$JUDGE_REPORT_PATH" "$JUDGE_REPORT_LATEST"

WATCHDOG_EXIT_CODE=0
if [ "$WATCHDOG_ENABLED" = "1" ]; then
  WATCHDOG_ARGS=(
    --run-meta "$RUN_META_PATH"
    --live-report "$LIVE_REPORT_PATH"
    --judge-report "$JUDGE_REPORT_PATH"
    --min-window-hours "${XAU_AUTOBOT_TRIAL_WATCHDOG_MIN_WINDOW_HOURS:-24}"
    --max-closed-positions "${XAU_AUTOBOT_TRIAL_WATCHDOG_MAX_CLOSED_POSITIONS:-0}"
    --write-report "$WATCHDOG_REPORT_PATH"
  )

  if [ -n "${XAU_AUTOBOT_TRIAL_WATCHDOG_ROTATE_CONFIG:-}" ]; then
    WATCHDOG_ARGS+=(--execute-rotate --rotate-config "${XAU_AUTOBOT_TRIAL_WATCHDOG_ROTATE_CONFIG}")
    if [ -n "${XAU_AUTOBOT_TRIAL_WATCHDOG_ROTATE_RUN_ID:-}" ]; then
      WATCHDOG_ARGS+=(--rotate-run-id "${XAU_AUTOBOT_TRIAL_WATCHDOG_ROTATE_RUN_ID}")
    fi
  fi
  if [ "${XAU_AUTOBOT_TRIAL_WATCHDOG_FAIL_ON_TRIGGER:-0}" = "1" ]; then
    WATCHDOG_ARGS+=(--fail-on-trigger)
  fi

  set +e
  "$PYTHON_EXE" "$ROOT/tools/xau_autobot_trial_watchdog.py" "${WATCHDOG_ARGS[@]}"
  WATCHDOG_EXIT_CODE=$?
  set -e
fi

echo "{\"action\":\"INFO\",\"reason\":\"trial_eval_completed\",\"run_id\":\"$TRIAL_RUN_ID\",\"live_report\":\"$LIVE_REPORT_PATH\",\"judge_report\":\"$JUDGE_REPORT_PATH\"}"

if [ "$WATCHDOG_EXIT_CODE" -ne 0 ] && [ "$JUDGE_EXIT_CODE" -eq 0 ]; then
  JUDGE_EXIT_CODE=$WATCHDOG_EXIT_CODE
fi

if [ "$JUDGE_EXIT_CODE" -ne 0 ]; then
  exit "$JUDGE_EXIT_CODE"
fi
