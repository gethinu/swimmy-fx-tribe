#!/usr/bin/env bash
set -euo pipefail

cd /home/swimmy/swimmy

PYTHON_BIN="${PYTHON_BIN:-/home/swimmy/swimmy/.venv/bin/python3}"
if [[ ! -x "$PYTHON_BIN" ]]; then
  PYTHON_BIN="python3"
fi

exec "$PYTHON_BIN" tools/edge_scorecard.py \
  --db "${EDGE_SCORECARD_DB:-data/memory/swimmy.db}" \
  --out "${EDGE_SCORECARD_LATEST_REPORT:-data/reports/edge_scorecard_latest.json}" \
  --history-dir "${EDGE_SCORECARD_HISTORY_DIR:-data/reports/edge_scorecard}" \
  --rank-report "${EDGE_SCORECARD_RANK_REPORT:-data/reports/rank_conformance_latest.json}" \
  --short-days "${EDGE_SCORECARD_SHORT_DAYS:-7}" \
  --long-days "${EDGE_SCORECARD_LONG_DAYS:-30}" \
  --discord-when "${EDGE_SCORECARD_DISCORD_WHEN:-problem}" \
  --discord-webhook "${EDGE_SCORECARD_DISCORD_WEBHOOK:-}" \
  --discord-webhook-env "${EDGE_SCORECARD_DISCORD_WEBHOOK_ENV:-SWIMMY_DISCORD_ALERTS}" \
  --discord-zmq-host "${EDGE_SCORECARD_DISCORD_ZMQ_HOST:-localhost}" \
  --discord-zmq-port "${EDGE_SCORECARD_DISCORD_ZMQ_PORT:-${SWIMMY_PORT_NOTIFIER:-5562}}"
