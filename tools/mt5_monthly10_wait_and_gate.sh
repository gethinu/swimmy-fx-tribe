#!/usr/bin/env bash
set -euo pipefail

# Wait for MT5 optimization reports, then run IH monthly gate and optionally apply params.

WIN_USER="${WIN_USER:-stair}"
TERMINAL_ID="${TERMINAL_ID:-D0E8209F77C8CF37AD8BF550E51FF075}"
RUN_ID="${RUN_ID:-ih_opt_monthly10_20260225_001800}"
CHECK_INTERVAL_SEC="${CHECK_INTERVAL_SEC:-30}"
MAX_WAIT_MINUTES="${MAX_WAIT_MINUTES:-720}"

FORWARD_DAYS="${FORWARD_DAYS:-366}"
MIN_MONTHLY_RETURN_PCT="${MIN_MONTHLY_RETURN_PCT:-10.0}"
MIN_FORWARD_TRADES="${MIN_FORWARD_TRADES:-5}"
MIN_FORWARD_PF="${MIN_FORWARD_PF:-1.0}"
MAX_FORWARD_DD_PCT="${MAX_FORWARD_DD_PCT:-10.0}"

TERMINAL_DIR="/mnt/c/Users/${WIN_USER}/AppData/Roaming/MetaQuotes/Terminal/${TERMINAL_ID}"
FWD_XML="${TERMINAL_DIR}/${RUN_ID}.forward.xml"
BACK_XML="${TERMINAL_DIR}/${RUN_ID}.xml"
DECISION_OUT="${DECISION_OUT:-data/reports/mt5/${RUN_ID}.decision.json}"
TARGET_SET="${TARGET_SET:-src/mt5/InstitutionalHunterEA_Forward_XAU_FX4.set}"

echo "[INFO] wait-and-gate start run_id=${RUN_ID}"
echo "[INFO] waiting for reports:"
echo "       ${BACK_XML}"
echo "       ${FWD_XML}"

waited=0
max_wait=$((MAX_WAIT_MINUTES * 60))
while (( waited <= max_wait )); do
  now="$(date '+%Y-%m-%d %H:%M:%S %Z')"
  if [[ -f "$BACK_XML" && -f "$FWD_XML" ]]; then
    echo "[INFO] ${now} reports detected, running gate"
    ./.venv/bin/python tools/mt5_ih_promotion_gate.py \
      --forward-xml "$FWD_XML" \
      --forward-days "$FORWARD_DAYS" \
      --min-monthly-return-pct "$MIN_MONTHLY_RETURN_PCT" \
      --min-forward-trades "$MIN_FORWARD_TRADES" \
      --min-forward-pf "$MIN_FORWARD_PF" \
      --max-forward-dd-pct "$MAX_FORWARD_DD_PCT" \
      --write-decision "$DECISION_OUT" \
      --apply-to-set "$TARGET_SET"
    echo "[OK] gate done: ${DECISION_OUT}"
    exit 0
  fi
  echo "[INFO] ${now} waiting... waited_sec=${waited}"
  sleep "$CHECK_INTERVAL_SEC"
  waited=$((waited + CHECK_INTERVAL_SEC))
done

echo "[ERR] timeout waiting reports (${MAX_WAIT_MINUTES}m)" >&2
exit 2
