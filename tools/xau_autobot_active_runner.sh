#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

if [ -f "$ROOT/.env" ]; then
  set -a
  # shellcheck disable=SC1091
  source "$ROOT/.env"
  set +a
fi

PY="${XAU_AUTOBOT_PYTHON:-$ROOT/.venv/bin/python}"
POLL_SECONDS="${XAU_AUTOBOT_POLL_SECONDS:-10}"
CONFIG_PATH="${XAU_AUTOBOT_CONFIG:-}"
LIVE_FLAG="${XAU_AUTOBOT_LIVE:-0}"

resolve_config_path() {
  local raw="${1:-}"
  if [ -z "$raw" ]; then
    printf '%s' "$ROOT/tools/configs/xau_autobot.tuned_auto_active.json"
    return
  fi
  case "$raw" in
    /*) printf '%s' "$raw" ;;
    *) printf '%s' "$ROOT/$raw" ;;
  esac
}

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
    powershell.exe)
      if [ -x /mnt/c/WINDOWS/System32/WindowsPowerShell/v1.0/powershell.exe ]; then
        printf '%s' /mnt/c/WINDOWS/System32/WindowsPowerShell/v1.0/powershell.exe
        return 0
      fi
      if [ -x /mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe ]; then
        printf '%s' /mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe
        return 0
      fi
      ;;
  esac
  return 1
}

CMD=(
  "$PY"
  "$ROOT/tools/xau_autobot.py"
  --loop
  --poll-seconds "$POLL_SECONDS"
)

if [ -n "$CONFIG_PATH" ]; then
  CMD+=(--config "$CONFIG_PATH")
fi

if [ "$LIVE_FLAG" = "1" ]; then
  CMD+=(--live)
fi

if ! "$PY" -c "import MetaTrader5" >/dev/null 2>&1; then
  # WSL fallback: run the bot with Windows Python/MetaTrader5 when Linux env lacks MetaTrader5 wheel.
  if is_wsl_env && command -v wslpath >/dev/null 2>&1; then
    CMD_EXE="$(find_windows_tool cmd.exe || true)"
    POWERSHELL_EXE="$(find_windows_tool powershell.exe || true)"
    if [ -n "$CMD_EXE" ] && [ -n "$POWERSHELL_EXE" ] && "$CMD_EXE" /c py -3 -c "import MetaTrader5" >/dev/null 2>&1; then
      WIN_LOOP_PS="$(wslpath -w "$ROOT/tools/windows/xau_autobot_live_loop.ps1")"
      WIN_ROOT="$(wslpath -w "$ROOT")"
      WIN_CONFIG="$(wslpath -w "$(resolve_config_path "$CONFIG_PATH")")"
      WIN_PY_EXE="$("$CMD_EXE" /c py -3 -c "import sys; print(sys.executable)" 2>/dev/null | tr -d '\r' | grep -E -m1 '^[A-Za-z]:\\' || true)"
      if [ -z "$WIN_PY_EXE" ]; then
        echo '{"action":"SKIP","reason":"mt5_windows_python_not_found","hint":"py -3 unavailable in Windows host"}'
        exit 0
      fi
      WIN_CMD=(
        "$POWERSHELL_EXE"
        -NoProfile
        -ExecutionPolicy
        Bypass
        -File "$WIN_LOOP_PS"
        -RepoRoot "$WIN_ROOT"
        -ConfigPath "$WIN_CONFIG"
        -PythonExe "$WIN_PY_EXE"
        -PollSeconds "$POLL_SECONDS"
      )
      if [ "$LIVE_FLAG" = "1" ]; then
        WIN_CMD+=(-Live)
      fi
      exec "${WIN_CMD[@]}"
    fi
  fi
  echo '{"action":"SKIP","reason":"mt5_python_missing","hint":"pip install MetaTrader5"}'
  exit 0
fi

exec "${CMD[@]}"
