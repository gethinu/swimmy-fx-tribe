#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

PYTHON_EXE="${XAU_AUTOBOT_PYTHON:-$ROOT/.venv/bin/python}"
TRIAL_CONFIG="${XAU_AUTOBOT_TRIAL_CONFIG:-$ROOT/tools/configs/xau_autobot.trial_v2_20260222.json}"
TRIAL_DAYS="${XAU_AUTOBOT_TRIAL_DAYS:-14}"
LIVE_REPORT_PATH="${XAU_AUTOBOT_TRIAL_LIVE_REPORT:-$ROOT/data/reports/xau_autobot_live_report_trial_v2_latest.json}"
JUDGE_REPORT_PATH="${XAU_AUTOBOT_TRIAL_JUDGE_REPORT:-$ROOT/data/reports/xau_autobot_trial_judge.json}"

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

readarray -t CFG_VALUES < <("$PYTHON_EXE" - <<'PY' "$TRIAL_CONFIG"
import json
import sys
from pathlib import Path

cfg = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
print(str(cfg.get("symbol", "XAUUSD")))
print(str(int(cfg.get("magic", 560072))))
print(str(cfg.get("comment", "xau_autobot_trial_v2_20260222")))
PY
)

SYMBOL="${CFG_VALUES[0]}"
MAGIC="${CFG_VALUES[1]}"
COMMENT_PREFIX="${CFG_VALUES[2]}"

if "$PYTHON_EXE" -c "import MetaTrader5" >/dev/null 2>&1; then
  "$PYTHON_EXE" "$ROOT/tools/xau_autobot_live_report.py" \
    --symbol "$SYMBOL" \
    --magic "$MAGIC" \
    --comment-prefix "$COMMENT_PREFIX" \
    --days "$TRIAL_DAYS" \
    --include-details \
    --diagnostics \
    --write-report "$LIVE_REPORT_PATH"
elif is_wsl_env && command -v wslpath >/dev/null 2>&1; then
  CMD_EXE="$(find_windows_tool cmd.exe || true)"
  if [ -z "$CMD_EXE" ] || ! "$CMD_EXE" /c py -3 -c "import MetaTrader5" >/dev/null 2>&1; then
    echo '{"action":"ERROR","reason":"mt5_python_missing","hint":"install MetaTrader5 for Linux or Windows py -3"}' >&2
    exit 1
  fi
  WIN_SCRIPT="$(wslpath -w "$ROOT/tools/xau_autobot_live_report.py")"
  WIN_REPORT="$(wslpath -w "$LIVE_REPORT_PATH")"
  "$CMD_EXE" /c py -3 "$WIN_SCRIPT" \
    --symbol "$SYMBOL" \
    --magic "$MAGIC" \
    --comment-prefix "$COMMENT_PREFIX" \
    --days "$TRIAL_DAYS" \
    --include-details \
    --diagnostics \
    --write-report "$WIN_REPORT"
else
  echo '{"action":"ERROR","reason":"mt5_python_missing","hint":"install MetaTrader5 in .venv"}' >&2
  exit 1
fi

"$PYTHON_EXE" "$ROOT/tools/xau_autobot_trial_judge.py" \
  --live-report "$LIVE_REPORT_PATH" \
  --min-days "${XAU_AUTOBOT_TRIAL_MIN_DAYS:-14}" \
  --min-closed-positions "${XAU_AUTOBOT_TRIAL_MIN_CLOSED_POSITIONS:-30}" \
  --min-profit-factor "${XAU_AUTOBOT_TRIAL_MIN_PROFIT_FACTOR:-1.10}" \
  --min-win-rate "${XAU_AUTOBOT_TRIAL_MIN_WIN_RATE:-0.42}" \
  --min-net-profit "${XAU_AUTOBOT_TRIAL_MIN_NET_PROFIT:-0}" \
  --write-report "$JUDGE_REPORT_PATH" \
  --fail-on-no-go
