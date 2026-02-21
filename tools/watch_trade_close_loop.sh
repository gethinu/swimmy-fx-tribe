#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOG_FILE="${LOG_FILE:-$ROOT/logs/trade_close_watch_loop.log}"
TIMEOUT_SEC="${TIMEOUT_SEC:-3600}"
POLL_SEC="${POLL_SEC:-5}"

mkdir -p "$(dirname "$LOG_FILE")"

echo "[LOOP] start timeout=${TIMEOUT_SEC}s poll=${POLL_SEC}s log=${LOG_FILE}"

while true; do
  ts="$(date '+%Y-%m-%d %H:%M:%S')"
  echo "[LOOP] ${ts} launch watcher" | tee -a "$LOG_FILE"
  set +e
  python3 -u "$ROOT/tools/watch_trade_close_integrity.py" \
    --timeout-sec "$TIMEOUT_SEC" \
    --poll-sec "$POLL_SEC" \
    >> "$LOG_FILE" 2>&1
  rc=$?
  set -e
  ts_end="$(date '+%Y-%m-%d %H:%M:%S')"
  echo "[LOOP] ${ts_end} watcher-exit rc=${rc}" | tee -a "$LOG_FILE"

  case "$rc" in
    0)
      echo "[LOOP] success detected, exiting." | tee -a "$LOG_FILE"
      exit 0
      ;;
    1)
      echo "[LOOP] violation detected, exiting with rc=1." | tee -a "$LOG_FILE"
      exit 1
      ;;
    2)
      echo "[LOOP] timeout, retrying..." | tee -a "$LOG_FILE"
      ;;
    *)
      echo "[LOOP] unexpected rc=${rc}, retrying after 10s..." | tee -a "$LOG_FILE"
      sleep 10
      ;;
  esac
done
