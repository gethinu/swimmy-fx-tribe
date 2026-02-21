#!/usr/bin/env bash
set -euo pipefail

cd /home/swimmy/swimmy

PYTHON_BIN="${PYTHON_BIN:-/home/swimmy/swimmy/.venv/bin/python}"
if [[ ! -x "$PYTHON_BIN" ]]; then
  PYTHON_BIN="python3"
fi

ARGS=(
  "tools/trend_arbitrage_engine.py"
  "--max-candidates" "${TREND_ARB_MAX_CANDIDATES:-12}"
  "--top-n" "${TREND_ARB_TOP_N:-3}"
  "--geo" "${TREND_ARB_GEO:-JP}"
  "--lang" "${TREND_ARB_LANG:-ja}"
)

if [[ "${TREND_ARB_DRY_RUN:-0}" == "1" ]]; then
  ARGS+=("--dry-run")
fi

if [[ "${TREND_ARB_SCAN_ONLY:-0}" == "1" ]]; then
  ARGS+=("--scan-only")
fi

exec "$PYTHON_BIN" "${ARGS[@]}"

