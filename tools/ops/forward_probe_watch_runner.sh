#!/usr/bin/env bash
set -euo pipefail

cd /home/swimmy/swimmy

PYTHON_BIN="${PYTHON_BIN:-/home/swimmy/swimmy/.venv/bin/python3}"
if [[ ! -x "$PYTHON_BIN" ]]; then
  PYTHON_BIN="python3"
fi

STRATEGY="${FORWARD_PROBE_WATCH_STRATEGY:-Bred-Bred--518-Gen37-N3980103299-243}"
DB_PATH="${FORWARD_PROBE_WATCH_DB_PATH:-data/memory/swimmy.db}"
STATE_PATH="${FORWARD_PROBE_WATCH_STATE_PATH:-data/memory/forward_probe_watch_state.json}"
INTERVAL_SECONDS="${FORWARD_PROBE_WATCH_INTERVAL_SECONDS:-3600}"
WEBHOOK_ENV="${FORWARD_PROBE_WATCH_WEBHOOK_ENV:-SWIMMY_DISCORD_ALERTS}"

exec "$PYTHON_BIN" tools/ops/forward_probe_watch.py \
  --strategy "$STRATEGY" \
  --db-path "$DB_PATH" \
  --state-path "$STATE_PATH" \
  --interval-seconds "$INTERVAL_SECONDS" \
  --notify \
  --webhook-env "$WEBHOOK_ENV" \
  --fail-on-alert
