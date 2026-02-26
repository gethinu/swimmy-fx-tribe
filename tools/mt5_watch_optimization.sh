#!/usr/bin/env bash
set -euo pipefail

# Watch MT5 optimization progress from WSL.
#
# Examples:
#   tools/mt5_watch_optimization.sh \
#     --report-prefix ih_opt_full_rerun_20260222_181920
#
#   tools/mt5_watch_optimization.sh \
#     --report-prefix ih_opt_full_rerun_20260222_181920 \
#     --wait --interval-sec 20

WIN_USER="stair"
TERMINAL_ID="D0E8209F77C8CF37AD8BF550E51FF075"
REPORT_PREFIX=""
WAIT_MODE=0
INTERVAL_SEC=30

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
    --report-prefix)
      REPORT_PREFIX="${2:?missing value for --report-prefix}"
      shift 2
      ;;
    --wait)
      WAIT_MODE=1
      shift
      ;;
    --interval-sec)
      INTERVAL_SEC="${2:?missing value for --interval-sec}"
      shift 2
      ;;
    *)
      echo "Unknown arg: $1" >&2
      exit 2
      ;;
  esac
done

if ! command -v powershell.exe >/dev/null 2>&1; then
  echo "[ERR] powershell.exe not found in PATH" >&2
  exit 1
fi
if ! command -v iconv >/dev/null 2>&1; then
  echo "[ERR] iconv not found" >&2
  exit 1
fi

TERMINAL_DIR="/mnt/c/Users/${WIN_USER}/AppData/Roaming/MetaQuotes/Terminal/${TERMINAL_ID}"
TESTER_LOG_DIR="${TERMINAL_DIR}/Tester/logs"

if [[ ! -d "$TERMINAL_DIR" ]]; then
  echo "[ERR] terminal dir not found: $TERMINAL_DIR" >&2
  exit 1
fi
if [[ ! -d "$TESTER_LOG_DIR" ]]; then
  echo "[ERR] tester log dir not found: $TESTER_LOG_DIR" >&2
  exit 1
fi

get_metatester_count() {
  powershell.exe -NoProfile -Command \
    "(Get-Process metatester64 -ErrorAction SilentlyContinue | Measure-Object).Count" \
    | tr -d '\r\n '
}

get_metatester_cpu_sum() {
  powershell.exe -NoProfile -Command \
    "(Get-Process metatester64 -ErrorAction SilentlyContinue | Measure-Object -Property CPU -Sum).Sum" \
    | tr -d '\r\n '
}

latest_tester_log() {
  ls -1t "$TESTER_LOG_DIR"/*.log 2>/dev/null | head -1 || true
}

latest_progress_line() {
  local log_file="$1"
  if [[ -z "$log_file" || ! -f "$log_file" ]]; then
    return 0
  fi
  iconv -f UTF-16LE -t UTF-8 "$log_file" \
    | rg -n "Best result .* Next generation|genetic optimization started|genetic calculation is over|genetic optimization finished|forward optimization finished|optimization done in" -i \
    | tail -1 || true
}

report_xml_path() {
  local suffix="$1"
  if [[ -z "$REPORT_PREFIX" ]]; then
    return 0
  fi
  echo "${TERMINAL_DIR}/${REPORT_PREFIX}${suffix}"
}

report_is_ready() {
  if [[ -z "$REPORT_PREFIX" ]]; then
    return 1
  fi
  [[ -f "$(report_xml_path ".xml")" && -f "$(report_xml_path ".forward.xml")" ]]
}

print_summary_from_reports() {
  local back_xml="$1"
  local fwd_xml="$2"
  python3 - "$back_xml" "$fwd_xml" <<'PY'
import sys
import xml.etree.ElementTree as ET

def top_row(path):
    ns = {"ss": "urn:schemas-microsoft-com:office:spreadsheet"}
    root = ET.parse(path).getroot()
    rows = root.find(".//ss:Worksheet/ss:Table", ns).findall("ss:Row", ns)
    if len(rows) < 2:
        return {}
    def vals(row):
        out = []
        for c in row.findall("ss:Cell", ns):
            d = c.find("ss:Data", ns)
            out.append(d.text if d is not None else "")
        return out
    header = vals(rows[0])
    data = vals(rows[1])
    if len(data) < len(header):
        data.extend([""] * (len(header) - len(data)))
    return dict(zip(header, data))

back = top_row(sys.argv[1])
fwd = top_row(sys.argv[2])

def g(d, key, default="-"):
    return d.get(key, default)

print("[SUMMARY] back_top "
      f"pass={g(back,'Pass')} result={g(back,'Result')} profit={g(back,'Profit')} "
      f"pf={g(back,'Profit Factor')} dd_pct={g(back,'Equity DD %')} trades={g(back,'Trades')}")
print("[SUMMARY] fwd_top "
      f"pass={g(fwd,'Pass')} fwd_result={g(fwd,'Forward Result')} back_result={g(fwd,'Back Result')} "
      f"profit={g(fwd,'Profit')} pf={g(fwd,'Profit Factor')} dd_pct={g(fwd,'Equity DD %')} trades={g(fwd,'Trades')}")
PY
}

PREV_CPU=""

print_once() {
  local now log_file progress count cpu back_xml fwd_xml
  now="$(date '+%Y-%m-%d %H:%M:%S %Z')"
  count="$(get_metatester_count)"
  cpu="$(get_metatester_cpu_sum)"
  log_file="$(latest_tester_log)"
  progress="$(latest_progress_line "$log_file")"
  back_xml="$(report_xml_path ".xml")"
  fwd_xml="$(report_xml_path ".forward.xml")"

  echo "[INFO] now=$now metatester_count=$count cpu_sum=$cpu"
  if [[ -n "$PREV_CPU" ]]; then
    python3 - "$PREV_CPU" "$cpu" <<'PY'
import sys
try:
    a = float(sys.argv[1]); b = float(sys.argv[2])
    print(f"[INFO] cpu_delta={b-a:.2f}")
except Exception:
    pass
PY
  fi
  PREV_CPU="$cpu"

  if [[ -n "$log_file" ]]; then
    echo "[INFO] latest_log=$log_file"
  fi
  if [[ -n "$progress" ]]; then
    echo "[INFO] progress=$progress"
  fi

  if [[ -n "$REPORT_PREFIX" ]]; then
    echo "[INFO] report_prefix=$REPORT_PREFIX"
    if [[ -f "$back_xml" ]]; then
      echo "[INFO] back_report=$back_xml"
    fi
    if [[ -f "$fwd_xml" ]]; then
      echo "[INFO] forward_report=$fwd_xml"
    fi
    if report_is_ready; then
      print_summary_from_reports "$back_xml" "$fwd_xml"
    fi
  fi
}

if (( WAIT_MODE == 0 )); then
  print_once
  exit 0
fi

echo "[INFO] wait-mode on: interval=${INTERVAL_SEC}s"
while true; do
  print_once
  if report_is_ready; then
    echo "[OK] reports detected, monitoring done."
    exit 0
  fi
  sleep "$INTERVAL_SEC"
done

