#!/usr/bin/env bash
set -euo pipefail

# Compile src/mt5/*.mq5 via Windows MetaEditor from WSL.
#
# Usage:
#   scripts/compile_swimmybridge_mt5.sh
#   scripts/compile_swimmybridge_mt5.sh --win-user stair --terminal-id D0E820...
#   scripts/compile_swimmybridge_mt5.sh --src src/mt5/InstitutionalHunterEA.mq5
#
# Prereqs:
# - WSL with /mnt/c mounted
# - Windows MetaTrader 5 + MetaEditor64.exe installed
# - Terminal data folder exists under:
#   C:\Users\<win-user>\AppData\Roaming\MetaQuotes\Terminal\<terminal-id>\MQL5\Experts

WIN_USER="stair"
TERMINAL_ID="D0E8209F77C8CF37AD8BF550E51FF075"
METAEDITOR_WIN="C:\\Program Files\\MetaTrader 5\\MetaEditor64.exe"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC_REL="src/mt5/SwimmyBridge.mq5"
SRC_MQ5="$REPO_ROOT/$SRC_REL"
EA_BASENAME="SwimmyBridge"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --win-user)
      WIN_USER="${2:?missing value for --win-user}"
      shift 2
      ;;
    --terminal-id)
      TERMINAL_ID="${2:?missing value for --terminal-id}"
      shift 2
      ;;
    --metaeditor-win)
      METAEDITOR_WIN="${2:?missing value for --metaeditor-win}"
      shift 2
      ;;
    --src)
      SRC_REL="${2:?missing value for --src}"
      if [[ "$SRC_REL" = /* ]]; then
        SRC_MQ5="$SRC_REL"
      else
        SRC_MQ5="$REPO_ROOT/$SRC_REL"
      fi
      shift 2
      ;;
    *)
      echo "Unknown arg: $1" >&2
      exit 2
      ;;
  esac
done

if [[ "$SRC_MQ5" == *.mq5 ]]; then
  EA_BASENAME="$(basename "$SRC_MQ5" .mq5)"
fi

EXP_DIR_LINUX="/mnt/c/Users/${WIN_USER}/AppData/Roaming/MetaQuotes/Terminal/${TERMINAL_ID}/MQL5/Experts"
TARGET_MQ5="${EXP_DIR_LINUX}/${EA_BASENAME}.mq5"
TARGET_EX5="${EXP_DIR_LINUX}/${EA_BASENAME}.ex5"

if [[ ! -f "$SRC_MQ5" ]]; then
  echo "[ERR] source not found: $SRC_MQ5" >&2
  exit 1
fi
if [[ ! -d "$EXP_DIR_LINUX" ]]; then
  echo "[ERR] experts dir not found: $EXP_DIR_LINUX" >&2
  exit 1
fi
if ! command -v powershell.exe >/dev/null 2>&1; then
  echo "[ERR] powershell.exe not found in WSL PATH" >&2
  exit 1
fi
if [[ ! -f "/mnt/c/Program Files/MetaTrader 5/MetaEditor64.exe" ]] && [[ "$METAEDITOR_WIN" == "C:\\Program Files\\MetaTrader 5\\MetaEditor64.exe" ]]; then
  echo "[WARN] default MetaEditor not found at C:\\Program Files\\MetaTrader 5\\MetaEditor64.exe" >&2
fi

TS="$(date +%Y%m%d_%H%M%S)"
BACKUP_MQ5="${EXP_DIR_LINUX}/${EA_BASENAME}.mq5.bak_${TS}"
BACKUP_EX5="${EXP_DIR_LINUX}/${EA_BASENAME}.ex5.bak_${TS}"
LOG_LINUX="${EXP_DIR_LINUX}/metaeditor_compile_${EA_BASENAME}_${TS}.log"
SUMMARY_TXT="${EXP_DIR_LINUX}/metaeditor_compile_${EA_BASENAME}_${TS}.utf8.txt"

EXP_DIR_WIN="C:\\Users\\${WIN_USER}\\AppData\\Roaming\\MetaQuotes\\Terminal\\${TERMINAL_ID}\\MQL5\\Experts"
FILE_WIN="${EXP_DIR_WIN}\\${EA_BASENAME}.mq5"
LOG_WIN="${EXP_DIR_WIN}\\metaeditor_compile_${EA_BASENAME}_${TS}.log"

echo "[INFO] win-user=$WIN_USER terminal-id=$TERMINAL_ID"
echo "[INFO] source=$SRC_MQ5"
echo "[INFO] ea=$EA_BASENAME"
echo "[INFO] target=$TARGET_MQ5"
echo "[INFO] backup mq5=$BACKUP_MQ5"
echo "[INFO] backup ex5=$BACKUP_EX5"
echo "[INFO] log=$LOG_LINUX"

if [[ -f "$TARGET_MQ5" ]]; then
  cp "$TARGET_MQ5" "$BACKUP_MQ5"
fi
if [[ -f "$TARGET_EX5" ]]; then
  cp "$TARGET_EX5" "$BACKUP_EX5"
fi
cp "$SRC_MQ5" "$TARGET_MQ5"

EX5_BEFORE_MTIME="$(stat -c '%Y' "$TARGET_EX5" 2>/dev/null || echo 0)"

PS1_PATH="/tmp/swimmy_metaeditor_compile_${TS}.ps1"
cat > "$PS1_PATH" <<'PS'
param(
  [string]$meta,
  [string]$file,
  [string]$log
)
$ErrorActionPreference = 'Stop'
& $meta ("/compile:$file") ("/log:$log")
exit $LASTEXITCODE
PS

PS1_WIN="$(wslpath -w "$PS1_PATH")"
set +e
powershell.exe -NoProfile -NonInteractive -ExecutionPolicy Bypass \
  -File "$PS1_WIN" "$METAEDITOR_WIN" "$FILE_WIN" "$LOG_WIN"
PS_EXIT=$?
set -e

if [[ ! -f "$LOG_LINUX" ]]; then
  echo "[ERR] compile log not found: $LOG_LINUX" >&2
  exit 1
fi

iconv -f UTF-16LE -t UTF-8 "$LOG_LINUX" > "$SUMMARY_TXT"
RESULT_LINE="$(rg -n "result .* errors|Result: .* errors" "$SUMMARY_TXT" -i | tail -n 1 || true)"
EX5_AFTER_MTIME="$(stat -c '%Y' "$TARGET_EX5" 2>/dev/null || echo 0)"

echo "[INFO] powershell-exit=$PS_EXIT"
echo "[INFO] result=$RESULT_LINE"
echo "[INFO] ex5 mtime before=$EX5_BEFORE_MTIME after=$EX5_AFTER_MTIME"

if [[ -n "$RESULT_LINE" ]] && rg -q "0 errors, 0 warnings" <<<"$RESULT_LINE"; then
  if (( EX5_AFTER_MTIME > EX5_BEFORE_MTIME )); then
    echo "[OK] MetaEditor compile succeeded."
    echo "[OK] ex5: $TARGET_EX5"
    echo "[OK] utf8 log: $SUMMARY_TXT"
    exit 0
  fi
fi

echo "[ERR] compile did not produce clean success state" >&2
echo "[ERR] inspect: $SUMMARY_TXT" >&2
exit 1
