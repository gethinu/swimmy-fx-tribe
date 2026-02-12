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

CMD=(
  "$ROOT/.venv/bin/python"
  "$ROOT/tools/xau_autobot_cycle_compare.py"
  --python-exe "$ROOT/.venv/bin/python"
  --periods "${XAU_AUTOBOT_PERIODS:-45d,60d,90d}"
  --interval 5m
  --reports-dir data/reports
  --config-dir tools/configs
  --write-comparison data/reports/xau_autobot_cycle_comparison.json
)

if [ -n "${SWIMMY_DISCORD_REPORTS:-}" ]; then
  CMD+=(--discord-webhook "${SWIMMY_DISCORD_REPORTS}")
fi

fallbacks=()
for key in SWIMMY_DISCORD_SYSTEM_LOGS SWIMMY_DISCORD_ALERTS SWIMMY_DISCORD_APEX; do
  val="${!key:-}"
  if [ -n "$val" ] && [ "$val" != "${SWIMMY_DISCORD_REPORTS:-}" ]; then
    fallbacks+=("$val")
  fi
done

if [ "${#fallbacks[@]}" -gt 0 ]; then
  joined="$(IFS=,; echo "${fallbacks[*]}")"
  CMD+=(--discord-webhook-fallbacks "$joined")
fi

CMD+=(--market-hours-only)

"${CMD[@]}"

"$ROOT/.venv/bin/python" "$ROOT/tools/xau_autobot_promote_best.py" \
  --comparison "$ROOT/data/reports/xau_autobot_cycle_comparison.json" \
  --config-dir "$ROOT/tools/configs" \
  --write-active "$ROOT/tools/configs/xau_autobot.tuned_auto_active.json" \
  --write-report "$ROOT/data/reports/xau_autobot_promotion.json"
