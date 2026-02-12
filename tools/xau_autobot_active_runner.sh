#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

if [ -f "$ROOT/.env" ]; then
  set -a
  # shellcheck disable=SC1091
  source "$ROOT/.env"
  set +a
fi

PY="${XAU_AUTOBOT_PYTHON:-$ROOT/.venv/bin/python}"
POLL_SECONDS="${XAU_AUTOBOT_POLL_SECONDS:-10}"
CONFIG_PATH="${XAU_AUTOBOT_CONFIG:-}"
LIVE_FLAG="${XAU_AUTOBOT_LIVE:-0}"

CMD=(
  "$PY"
  "$ROOT/tools/xau_autobot.py"
  --loop
  --poll-seconds "$POLL_SECONDS"
)

if [ -n "$CONFIG_PATH" ]; then
  CMD+=(--config "$CONFIG_PATH")
fi

if [ "$LIVE_FLAG" = "1" ]; then
  CMD+=(--live)
fi

exec "${CMD[@]}"
