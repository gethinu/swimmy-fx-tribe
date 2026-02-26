#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

PYTHON_EXE="${XAU_AUTOBOT_PYTHON:-$ROOT/.venv/bin/python}"
TRIAL_CONFIG="${XAU_AUTOBOT_TRIAL_CONFIG:-tools/configs/xau_autobot.trial_v2_20260222.json}"
TRIAL_LIVE="${XAU_AUTOBOT_TRIAL_LIVE:-1}"
POLL_SECONDS="${XAU_AUTOBOT_TRIAL_POLL_SECONDS:-10}"
ALLOW_EXISTING="${XAU_AUTOBOT_TRIAL_ALLOW_EXISTING_PROCESSES:-0}"
LOCK_FILE="${XAU_AUTOBOT_TRIAL_LOCK_FILE:-$ROOT/data/runtime/xau_autobot_trial_v2.lock}"
TRIAL_RUN_ID="${XAU_AUTOBOT_TRIAL_RUN_ID:-trial_v2_$(date -u +%Y%m%d_%H%M%S)}"
RUN_META_PATH="${XAU_AUTOBOT_TRIAL_RUN_META_PATH:-$ROOT/data/reports/xau_autobot_trial_v2_current_run.json}"

existing_autobot_processes() {
  ps -eo pid=,args= | awk '
    /xau_autobot.py|xau_autobot_live_loop\.ps1/ &&
    $0 !~ /awk/ &&
    $0 !~ /xau_autobot_trial_v2_start\.sh/ {
      print $0
    }
  '
}

resolve_trial_config_path() {
  local raw="$1"
  if [ -z "$raw" ]; then
    printf '%s' "$ROOT/tools/configs/xau_autobot.trial_v2_20260222.json"
    return
  fi
  case "$raw" in
    /*) printf '%s' "$raw" ;;
    *) printf '%s' "$ROOT/$raw" ;;
  esac
}

validate_trial_config_comment() {
  local config_path="$1"
  "$PYTHON_EXE" - <<'PY' "$config_path"
import json
import sys
from pathlib import Path

from tools.xau_autobot import MT5_COMMENT_MAX_LEN, validate_trade_comment

config_path = Path(sys.argv[1])
if not config_path.exists():
    print(
        json.dumps(
            {
                "action": "ERROR",
                "reason": "trial_config_not_found",
                "config_path": str(config_path),
            },
            ensure_ascii=True,
        ),
        file=sys.stderr,
    )
    raise SystemExit(1)

try:
    payload = json.loads(config_path.read_text(encoding="utf-8"))
except Exception as exc:
    print(
        json.dumps(
            {
                "action": "ERROR",
                "reason": "trial_config_invalid_json",
                "config_path": str(config_path),
                "detail": str(exc),
            },
            ensure_ascii=True,
        ),
        file=sys.stderr,
    )
    raise SystemExit(1)

comment = str(payload.get("comment", ""))
try:
    validate_trade_comment(comment, max_len=MT5_COMMENT_MAX_LEN)
except Exception as exc:
    print(
        json.dumps(
            {
                "action": "ERROR",
                "reason": "trial_config_invalid_comment_length",
                "config_path": str(config_path),
                "comment": comment,
                "comment_length": len(comment),
                "max_comment_length": MT5_COMMENT_MAX_LEN,
                "detail": str(exc),
            },
            ensure_ascii=True,
        ),
        file=sys.stderr,
    )
    raise SystemExit(1)
PY
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

ABS_TRIAL_CONFIG="$(resolve_trial_config_path "$TRIAL_CONFIG")"
validate_trial_config_comment "$ABS_TRIAL_CONFIG"

acquire_lock

mkdir -p "$(dirname "$RUN_META_PATH")"
"$PYTHON_EXE" - <<'PY' "$RUN_META_PATH" "$TRIAL_RUN_ID" "$TRIAL_CONFIG" "$TRIAL_LIVE" "$POLL_SECONDS"
import json
import sys
from datetime import datetime, timezone
from pathlib import Path

meta_path = Path(sys.argv[1])
run_id = str(sys.argv[2]).strip()
now_utc = datetime.now(timezone.utc).isoformat()

existing: dict = {}
if meta_path.exists():
    try:
        loaded = json.loads(meta_path.read_text(encoding="utf-8"))
    except Exception:
        loaded = {}
    if isinstance(loaded, dict):
        existing = loaded

existing_run_id = str(existing.get("run_id", "")).strip()
existing_started_at = str(existing.get("started_at_utc", "")).strip()
started_at_utc = now_utc
if existing_run_id == run_id and existing_started_at:
    # Preserve the original trial window when restarting the same run_id.
    started_at_utc = existing_started_at

payload = {
    "run_id": run_id,
    "started_at_utc": started_at_utc,
    "trial_config": str(sys.argv[3]),
    "trial_live": str(sys.argv[4]),
    "poll_seconds": str(sys.argv[5]),
}
with meta_path.open("w", encoding="utf-8") as f:
    json.dump(payload, f, ensure_ascii=True, indent=2)
    f.write("\n")
PY

echo "{\"action\":\"INFO\",\"reason\":\"trial_run_started\",\"run_id\":\"$TRIAL_RUN_ID\",\"meta_path\":\"$RUN_META_PATH\"}"

XAU_AUTOBOT_CONFIG="$ABS_TRIAL_CONFIG" \
XAU_AUTOBOT_LIVE="$TRIAL_LIVE" \
XAU_AUTOBOT_POLL_SECONDS="$POLL_SECONDS" \
XAU_AUTOBOT_TRIAL_RUN_ID="$TRIAL_RUN_ID" \
XAU_AUTOBOT_LIVE_GUARD_ENABLED=0 \
"$ROOT/tools/xau_autobot_active_runner.sh"
