#!/usr/bin/env bash
set -euo pipefail

cd /home/swimmy/swimmy

PYTHON_BIN="${PYTHON_BIN:-/home/swimmy/swimmy/.venv/bin/python3}"
if [[ ! -x "$PYTHON_BIN" ]]; then
  PYTHON_BIN="python3"
fi

ARGS=(
  tools/check_rank_conformance.py
  --db "${RANK_CONF_DB:-data/memory/swimmy.db}"
  --out "${RANK_CONF_LATEST_REPORT:-data/reports/rank_conformance_latest.json}"
  --history-dir "${RANK_CONF_HISTORY_DIR:-data/reports/rank_conformance}"
)

if [[ "${RANK_CONF_STRICT:-0}" == "1" ]]; then
  ARGS+=(--fail-on-problem --max-violations "${RANK_CONF_MAX_VIOLATIONS:-0}")
fi

exec "$PYTHON_BIN" "${ARGS[@]}"
