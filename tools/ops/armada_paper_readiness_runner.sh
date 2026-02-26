#!/usr/bin/env bash
set -euo pipefail

cd /home/swimmy/swimmy

PYTHON_BIN="${PYTHON_BIN:-/home/swimmy/swimmy/.venv/bin/python3}"
if [[ ! -x "$PYTHON_BIN" ]]; then
  PYTHON_BIN="python3"
fi

DATE_TAG="${ARMADA_PAPER_DATE_TAG:-$(date -u +%Y%m%d)}"
TITLE="${ARMADA_PAPER_TITLE:-armada_paper_readiness_${DATE_TAG}_fix10refresh}"
OUTPUT="${ARMADA_PAPER_OUTPUT:-data/reports/armada_paper_readiness_${DATE_TAG}_fix10refresh.json}"

latest_report_or_default() {
  local pattern="$1"
  local fallback="$2"
  local latest
  latest="$(ls -1t $pattern 2>/dev/null | head -n 1 || true)"
  if [[ -n "$latest" ]]; then
    printf '%s\n' "$latest"
  else
    printf '%s\n' "$fallback"
  fi
}

B1R_SUMMARY="${ARMADA_PAPER_B1R_SUMMARY:-$(latest_report_or_default 'data/reports/armada_b1_seed_sweep_*_fix10*_summary.json' 'data/reports/armada_b1_seed_sweep_20260223_fix10_mix_holdoff_summary.json')}"
DEPLOY_READINESS="${ARMADA_PAPER_DEPLOY_READINESS:-$(latest_report_or_default 'data/reports/armada_deploy_readiness_*_fix10*.json' 'data/reports/armada_deploy_readiness_20260223_fix10refresh.json')}"
DEPLOY_READINESS_PROXY="${ARMADA_PAPER_DEPLOY_READINESS_PROXY:-$(latest_report_or_default 'data/reports/armada_deploy_readiness_*_proxy_fix10*.json' 'data/reports/armada_deploy_readiness_20260223_proxy_fix10refresh.json')}"

extra_args=()
if [[ -n "${ARMADA_PAPER_MONTHLY_TARGET_PCT:-}" ]]; then
  extra_args+=(--monthly-return-target-pct "${ARMADA_PAPER_MONTHLY_TARGET_PCT}")
fi

exec "$PYTHON_BIN" tools/ops/armada_paper_readiness.py \
  --db-path "${ARMADA_PAPER_DB:-data/memory/swimmy.db}" \
  --b1r-summary "$B1R_SUMMARY" \
  --deploy-readiness "$DEPLOY_READINESS" \
  --deploy-readiness-proxy "$DEPLOY_READINESS_PROXY" \
  --output "$OUTPUT" \
  --title "$TITLE" \
  --paper-min-trades "${ARMADA_PAPER_MIN_TRADES:-20}" \
  --slippage-min-samples "${ARMADA_PAPER_SLIPPAGE_MIN_SAMPLES:-20}" \
  --runtime-net-pnl-min "${ARMADA_PAPER_RUNTIME_NET_PNL_MIN:-0.0}" \
  --runtime-latest-loss-streak-max "${ARMADA_PAPER_RUNTIME_LOSS_STREAK_MAX:-3}" \
  --runtime-window-trades "${ARMADA_PAPER_RUNTIME_WINDOW_TRADES:-20}" \
  --drawdown-hard-dd-percent-max "${ARMADA_PAPER_DD_HARD_MAX:-12.0}" \
  --drawdown-weekly-dd-percent-max "${ARMADA_PAPER_DD_WEEKLY_MAX:-4.0}" \
  --slippage-p95-abs-pips-max "${ARMADA_PAPER_SLIPPAGE_P95_MAX:-3.0}" \
  --monthly-baseline-equity "${ARMADA_PAPER_MONTHLY_BASELINE_EQUITY:-100000.0}" \
  "${extra_args[@]}"
