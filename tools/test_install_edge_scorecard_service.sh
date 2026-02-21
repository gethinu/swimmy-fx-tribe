#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT="$ROOT/tools/install_edge_scorecard_service.sh"
SERVICE_UNIT="$ROOT/systemd/swimmy-edge-scorecard.service"
TIMER_UNIT="$ROOT/systemd/swimmy-edge-scorecard.timer"

[[ -f "$SCRIPT" ]] || { echo "Missing $SCRIPT" >&2; exit 1; }
[[ -f "$SERVICE_UNIT" ]] || { echo "Missing $SERVICE_UNIT" >&2; exit 1; }
[[ -f "$TIMER_UNIT" ]] || { echo "Missing $TIMER_UNIT" >&2; exit 1; }

grep -q "ExecStart=/home/swimmy/swimmy/tools/edge_scorecard_runner.sh" "$SERVICE_UNIT" || {
  echo "Service unit missing edge scorecard runner ExecStart" >&2
  exit 1
}
grep -q "OnCalendar=daily" "$TIMER_UNIT" || {
  echo "Timer unit missing daily schedule" >&2
  exit 1
}

SWIMMY_SYSTEMD_DRY_RUN=1 bash "$SCRIPT" >/dev/null

tmp_dir="$(mktemp -d)"
cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

mock_bin="$tmp_dir/bin"
mkdir -p "$mock_bin"
mock_log="$tmp_dir/systemctl.log"
cat >"$mock_bin/systemctl" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
if [[ -n "${MOCK_SYSTEMCTL_LOG:-}" ]]; then
  echo "$*" >>"$MOCK_SYSTEMCTL_LOG"
fi
exit 0
EOF
chmod +x "$mock_bin/systemctl"

user_dest="$tmp_dir/user-systemd"
HOME="$tmp_dir/home" PATH="$mock_bin:$PATH" MOCK_SYSTEMCTL_LOG="$mock_log" \
  SWIMMY_SYSTEMD_SCOPE=user SWIMMY_SYSTEMD_DEST_DIR="$user_dest" SWIMMY_SYSTEMD_DRY_RUN=0 \
  bash "$SCRIPT" >/dev/null

[[ -f "$user_dest/swimmy-edge-scorecard.service" ]] || { echo "User scope service not installed" >&2; exit 1; }
[[ -f "$user_dest/swimmy-edge-scorecard.timer" ]] || { echo "User scope timer not installed" >&2; exit 1; }
grep -q -- "--user daemon-reload" "$mock_log" || { echo "Missing --user daemon-reload call" >&2; exit 1; }
grep -q -- "--user enable --now swimmy-edge-scorecard.timer" "$mock_log" || { echo "Missing --user enable call" >&2; exit 1; }
