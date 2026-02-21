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

PRIMARY_WEBHOOK="${SWIMMY_XAU_NOTIFY_WEBHOOK:-${SWIMMY_DISCORD_REPORTS:-}}"

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

if [ -n "${PRIMARY_WEBHOOK:-}" ]; then
  CMD+=(--discord-webhook "${PRIMARY_WEBHOOK}")
fi

fallbacks=()
for key in SWIMMY_DISCORD_SYSTEM_LOGS SWIMMY_DISCORD_ALERTS SWIMMY_DISCORD_APEX; do
  val="${!key:-}"
  if [ -n "$val" ] && [ "$val" != "${PRIMARY_WEBHOOK:-}" ]; then
    fallbacks+=("$val")
  fi
done

if [ "${#fallbacks[@]}" -gt 0 ]; then
  joined="$(IFS=,; echo "${fallbacks[*]}")"
  CMD+=(--discord-webhook-fallbacks "$joined")
fi

if [ "${XAU_AUTOBOT_COMPARE_NOTIFY_STRICT:-0}" = "1" ]; then
  CMD+=(--notify-strict)
fi

CMD+=(--market-hours-only)

"${CMD[@]}"

HAS_PERIOD_SUMMARY="$("$ROOT/.venv/bin/python" - <<'PY'
import json
from pathlib import Path

path = Path("data/reports/xau_autobot_cycle_comparison.json")
data = json.loads(path.read_text(encoding="utf-8"))
periods = data.get("periods")
print("1" if isinstance(periods, list) and len(periods) > 0 else "0")
PY
)"

if [ "$HAS_PERIOD_SUMMARY" != "1" ]; then
  echo '{"action":"SKIP_PROMOTION","reason":"no_period_summaries"}'
  exit 0
fi

PROMOTE_CMD=(
  "$ROOT/.venv/bin/python"
  "$ROOT/tools/xau_autobot_promote_best.py"
  --comparison "$ROOT/data/reports/xau_autobot_cycle_comparison.json"
  --config-dir "$ROOT/tools/configs"
  --live-reports-dir "$ROOT/data/reports"
  --live-max-age-hours "${XAU_AUTOBOT_LIVE_MAX_AGE_HOURS:-48}"
  --live-min-closed-positions "${XAU_AUTOBOT_LIVE_MIN_CLOSED_POSITIONS:-10}"
  --live-min-profit-factor "${XAU_AUTOBOT_LIVE_MIN_PROFIT_FACTOR:-1.0}"
  --live-max-profit-factor-drop "${XAU_AUTOBOT_LIVE_MAX_PROFIT_FACTOR_DROP:-0.15}"
  --live-max-win-rate-drop "${XAU_AUTOBOT_LIVE_MAX_WIN_RATE_DROP:-0.08}"
  --write-active "$ROOT/tools/configs/xau_autobot.tuned_auto_active.json"
  --write-report "$ROOT/data/reports/xau_autobot_promotion.json"
)

if [ "${XAU_AUTOBOT_LIVE_IGNORE_NET_PROFIT_CHECK:-0}" = "1" ]; then
  PROMOTE_CMD+=(--live-ignore-net-profit-check)
fi

if [ "${XAU_AUTOBOT_FAIL_ON_LIVE_UNDERPERFORMING:-0}" = "1" ]; then
  PROMOTE_CMD+=(--fail-on-live-underperforming)
fi

if [ -n "${PRIMARY_WEBHOOK:-}" ]; then
  PROMOTE_CMD+=(--discord-webhook "${PRIMARY_WEBHOOK}")
fi
if [ "${#fallbacks[@]}" -gt 0 ]; then
  joined="$(IFS=,; echo "${fallbacks[*]}")"
  PROMOTE_CMD+=(--discord-webhook-fallbacks "$joined")
fi
if [ "${XAU_AUTOBOT_NOTIFY_STRICT:-0}" = "1" ]; then
  PROMOTE_CMD+=(--notify-strict)
fi

"${PROMOTE_CMD[@]}"
