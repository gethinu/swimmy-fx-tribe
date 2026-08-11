#!/usr/bin/env bash
# tools/ops/alert_on_failure.sh
# ============================================================================
# P4 (Thread B / reliability): OnFailure alert sink for the brain-family units.
# ----------------------------------------------------------------------------
# Invoked by swimmy-alert@.service, which is wired as `OnFailure=` on the
# school/brain units. systemd fires it once the failing unit has exhausted its
# StartLimitBurst and entered the `failed` state (i.e. a PERMANENT stop, not a
# transient blip) -- exactly the condition P4 wants surfaced loudly.
#
# Contract:
#   $1 = the failed unit name (passed by the template as %i, e.g.
#        "swimmy-brain.service").
#
# This script MUST NOT itself fail-loop: it always exits 0. The Discord ping is
# strictly opt-in (only fires when SWIMMY_DISCORD_ALERTS is already configured
# by the owner) and best-effort; the durable log line is the guaranteed record.
# Mirrors the established webhook idiom in tools/check_school_health.sh.
# ============================================================================

set -uo pipefail

UNIT="${1:-unknown.unit}"
SWIMMY_HOME="${SWIMMY_HOME:-/home/swimmy/swimmy}"
LOG_DIR="${SWIMMY_HOME}/logs"
LOG_FILE="${LOG_DIR}/alert.log"
WEBHOOK_URL="${SWIMMY_DISCORD_ALERTS:-}"

mkdir -p "${LOG_DIR}" 2>/dev/null || true

ts="$(date '+%Y-%m-%d %H:%M:%S %Z')"

# Grab a little context from the failed unit's recent journal (best-effort;
# absent on non-systemd hosts).
context=""
if command -v journalctl >/dev/null 2>&1; then
    context="$(journalctl -u "${UNIT}" -n 8 --no-pager 2>/dev/null | tail -n 8)"
fi

{
    echo "==================================================================="
    echo "[${ts}] 🛑 UNIT FAILED (permanent stop after StartLimit): ${UNIT}"
    if [ -n "${context}" ]; then
        echo "--- last journal lines --------------------------------------------"
        echo "${context}"
        echo "-------------------------------------------------------------------"
    fi
} >> "${LOG_FILE}" 2>/dev/null || true

# Opt-in Discord ping: only when the owner has already set the webhook.
if [ -n "${WEBHOOK_URL}" ]; then
    desc="Unit \`${UNIT}\` exhausted its restart budget and is now in the failed state (permanent stop). New entries are already blocked by Guardian's brain-disconnect gate; existing positions are held under protective management. Manual intervention required."
    payload="$(cat <<JSON
{"content":"@here","embeds":[{"title":"🛑 SWIMMY UNIT DOWN (permanent)","description":"${desc}","color":16711680,"footer":{"text":"swimmy-alert@ / OnFailure"}}]}
JSON
)"
    curl -s -m 10 -H "Content-Type: application/json" \
        -d "${payload}" "${WEBHOOK_URL}" >/dev/null 2>&1 || true
fi

exit 0
