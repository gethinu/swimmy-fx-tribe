#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT="$ROOT/tools/system_audit.sh"

if [[ ! -x "$SCRIPT" ]]; then
  echo "Missing $SCRIPT" >&2
  exit 1
fi

help_output="$($SCRIPT --help)"
echo "$help_output" | grep -q "Usage:" || { echo "Missing usage" >&2; exit 1; }
echo "$help_output" | grep -q "LIVE_TRADE_CLOSE_AFTER_ID" || { echo "Missing live trade-close usage" >&2; exit 1; }
echo "$help_output" | grep -q "RUN_REGRESSION_BUNDLE" || { echo "Missing regression bundle usage" >&2; exit 1; }
echo "$help_output" | grep -q "BACKTEST_HEARTBEAT_MAX_RX_AGE_SECONDS" || { echo "Missing backtest heartbeat usage" >&2; exit 1; }
echo "$help_output" | grep -q "RANK_CONF_LATEST_REPORT" || { echo "Missing rank conformance usage" >&2; exit 1; }
echo "$help_output" | grep -q "EDGE_SCORECARD_LATEST_REPORT" || { echo "Missing edge scorecard usage" >&2; exit 1; }

run_output_file="$(mktemp)"
DRY_RUN=1 RUN_REGRESSION_BUNDLE=0 "$SCRIPT" >"$run_output_file" 2>&1
grep -q "Summary" "$run_output_file" || { echo "Missing summary" >&2; rm -f "$run_output_file"; exit 1; }
if grep -q "SWIMMY_DISCORD_RECRUIT not set" "$run_output_file"; then
  echo "Expected .env to supply SWIMMY_DISCORD_RECRUIT" >&2
  rm -f "$run_output_file"
  exit 1
fi
grep -q "Regression bundle disabled" "$run_output_file" || { echo "Missing regression bundle skip log" >&2; rm -f "$run_output_file"; exit 1; }
grep -q "Systemd cgroup scope alignment" "$run_output_file" || { echo "Missing systemd scope alignment step" >&2; rm -f "$run_output_file"; exit 1; }
grep -q "Backtest heartbeat freshness" "$run_output_file" || { echo "Missing backtest heartbeat freshness step" >&2; rm -f "$run_output_file"; exit 1; }
grep -q "Pattern backend calibration status" "$run_output_file" || { echo "Missing pattern calibration status step" >&2; rm -f "$run_output_file"; exit 1; }
grep -q "Backtest heartbeat freshness" "$run_output_file" || { echo "Missing backtest heartbeat step" >&2; rm -f "$run_output_file"; exit 1; }
grep -q "Order timeframe consistency" "$run_output_file" || { echo "Missing order timeframe consistency step" >&2; rm -f "$run_output_file"; exit 1; }
grep -q "Rank conformance audit" "$run_output_file" || { echo "Missing rank conformance audit step" >&2; rm -f "$run_output_file"; exit 1; }
grep -q "Rank conformance summary" "$run_output_file" || { echo "Missing rank conformance summary output" >&2; rm -f "$run_output_file"; exit 1; }
grep -q "Edge scorecard" "$run_output_file" || { echo "Missing edge scorecard step" >&2; rm -f "$run_output_file"; exit 1; }
grep -q "Edge scorecard summary" "$run_output_file" || { echo "Missing edge scorecard summary output" >&2; rm -f "$run_output_file"; exit 1; }
grep -q "Live trade-close integrity" "$run_output_file" || { echo "Missing live trade-close integrity step" >&2; rm -f "$run_output_file"; exit 1; }
rm -f "$run_output_file"

tmp_dir="$(mktemp -d)"
cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

mock_log="$tmp_dir/sudo.log"
mock_sudo="$tmp_dir/mock_sudo"
cat >"$mock_sudo" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
if [[ -n "${MOCK_SUDO_LOG:-}" ]]; then
  echo "mock sudo: $*" >>"$MOCK_SUDO_LOG"
fi
exit 0
EOF
chmod +x "$mock_sudo"

MOCK_SUDO_LOG="$mock_log" SUDO_CMD="$mock_sudo" DRY_RUN=1 RUN_REGRESSION_BUNDLE=0 SWIMMY_AUDIT_SERVICES="mock-service" \
  "$SCRIPT" >/dev/null

if [[ ! -s "$mock_log" ]]; then
  echo "SUDO_CMD not used" >&2
  exit 1
fi
