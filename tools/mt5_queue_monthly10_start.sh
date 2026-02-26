#!/usr/bin/env bash
set -euo pipefail

# Queue-start InstitutionalHunter monthly10 optimization after current MT5 tester load clears.

WIN_USER="${WIN_USER:-stair}"
TERMINAL_ID="${TERMINAL_ID:-D0E8209F77C8CF37AD8BF550E51FF075}"
RUN_ID="${RUN_ID:-ih_opt_monthly10_20260225_001800}"
INTERVAL_SEC="${INTERVAL_SEC:-30}"
MAX_WAIT_MINUTES="${MAX_WAIT_MINUTES:-480}"
TERMINAL_EXE_WIN="${TERMINAL_EXE_WIN:-C:\\Program Files\\MetaTrader 5\\terminal64.exe}"

TERMINAL_DIR="/mnt/c/Users/${WIN_USER}/AppData/Roaming/MetaQuotes/Terminal/${TERMINAL_ID}"
CFG_UTF16="${TERMINAL_DIR}/config/${RUN_ID}_utf16.ini"
REPORT_XML="${TERMINAL_DIR}/${RUN_ID}.xml"
REPORT_FWD_XML="${TERMINAL_DIR}/${RUN_ID}.forward.xml"

if [[ ! -f "$CFG_UTF16" ]]; then
  echo "[ERR] config not found: $CFG_UTF16" >&2
  exit 1
fi

if [[ -f "$REPORT_XML" || -f "$REPORT_FWD_XML" ]]; then
  echo "[INFO] report already exists for ${RUN_ID}; skip launch."
  exit 0
fi

cfg_win="$(wslpath -w "$CFG_UTF16")"

get_proc_count() {
  local name="$1"
  powershell.exe -NoProfile -Command \
    "(Get-Process ${name} -ErrorAction SilentlyContinue | Measure-Object).Count" \
    | tr -d '\r\n ' || true
}

waited=0
max_wait=$((MAX_WAIT_MINUTES * 60))
echo "[INFO] queue start for ${RUN_ID}"
echo "[INFO] waiting until terminal64/metatester64 counts are both 0"

while (( waited <= max_wait )); do
  term_count="$(get_proc_count terminal64)"
  tester_count="$(get_proc_count metatester64)"
  term_count="${term_count:-0}"
  tester_count="${tester_count:-0}"

  now="$(date '+%Y-%m-%d %H:%M:%S %Z')"
  echo "[INFO] ${now} term=${term_count} tester=${tester_count} waited_sec=${waited}"

  if [[ "$term_count" == "0" && "$tester_count" == "0" ]]; then
    echo "[INFO] launching monthly10 run via terminal64 /config"
    powershell.exe -NoProfile -Command \
      "Start-Process -FilePath '${TERMINAL_EXE_WIN}' -ArgumentList '/config:${cfg_win}' -WindowStyle Minimized"
    sleep 5
    term_after="$(get_proc_count terminal64)"
    tester_after="$(get_proc_count metatester64)"
    echo "[OK] launch requested. term=${term_after:-0} tester=${tester_after:-0}"
    exit 0
  fi

  sleep "$INTERVAL_SEC"
  waited=$((waited + INTERVAL_SEC))
done

echo "[ERR] timeout waiting idle MT5 tester (${MAX_WAIT_MINUTES}m)" >&2
exit 2
