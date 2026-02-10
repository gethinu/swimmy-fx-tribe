#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/../guardian"

# Usage:
#   tools/legend_gate_compare_guardian.sh              # default pairs
#   tools/legend_gate_compare_guardian.sh USDJPY EURUSD GBPUSD

if [[ $# -gt 0 ]]; then
  cargo run -q --bin legend_gate_compare -- --pairs "$@" 2>/dev/null
else
  cargo run -q --bin legend_gate_compare -- 2>/dev/null
fi

