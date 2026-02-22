#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

TRIAL_CONFIG="${XAU_AUTOBOT_TRIAL_CONFIG:-tools/configs/xau_autobot.trial_v2_20260222.json}"
TRIAL_LIVE="${XAU_AUTOBOT_TRIAL_LIVE:-1}"
POLL_SECONDS="${XAU_AUTOBOT_TRIAL_POLL_SECONDS:-10}"

XAU_AUTOBOT_CONFIG="$TRIAL_CONFIG" \
XAU_AUTOBOT_LIVE="$TRIAL_LIVE" \
XAU_AUTOBOT_POLL_SECONDS="$POLL_SECONDS" \
XAU_AUTOBOT_LIVE_GUARD_ENABLED=0 \
"$ROOT/tools/xau_autobot_active_runner.sh"
