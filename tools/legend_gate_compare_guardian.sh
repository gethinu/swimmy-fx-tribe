#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/../guardian"

# Gate mode:
#   voltrend: realized-vol + trend-strength 2-axis regime filter (recommended)
#   model:    legacy "model gate" (kalman + dual-trend + vol switch)
GATE_MODE="${GATE_MODE:-voltrend}"

# Usage:
#   tools/legend_gate_compare_guardian.sh              # default pairs
#   tools/legend_gate_compare_guardian.sh USDJPY EURUSD GBPUSD

if [[ $# -gt 0 ]]; then
  cargo run -q --bin legend_gate_compare -- --gate-mode "$GATE_MODE" --pairs "$@" 2>/dev/null
else
  cargo run -q --bin legend_gate_compare -- --gate-mode "$GATE_MODE" 2>/dev/null
fi
