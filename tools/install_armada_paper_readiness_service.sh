#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SOURCE_DIR="${SWIMMY_SYSTEMD_DIR:-$ROOT/systemd}"
SCOPE_RAW="${SWIMMY_SYSTEMD_SCOPE:-system}"
DRY_RUN="${SWIMMY_SYSTEMD_DRY_RUN:-0}"

SERVICE_UNIT="swimmy-armada-paper-readiness.service"
TIMER_UNIT="swimmy-armada-paper-readiness.timer"
SYSTEMCTL_FLAGS=()

SCOPE="$(printf '%s' "$SCOPE_RAW" | tr '[:upper:]' '[:lower:]')"
case "$SCOPE" in
  system)
    DEST_DIR="${SWIMMY_SYSTEMD_DEST_DIR:-/etc/systemd/system}"
    ;;
  user)
    DEST_DIR="${SWIMMY_SYSTEMD_DEST_DIR:-${HOME:-/home/swimmy}/.config/systemd/user}"
    SYSTEMCTL_FLAGS=(--user)
    ;;
  *)
    echo "ERROR: Invalid SWIMMY_SYSTEMD_SCOPE=$SCOPE_RAW (expected: system|user)" >&2
    exit 1
    ;;
esac

if [[ "$SOURCE_DIR" != /* ]]; then
  SOURCE_DIR="$ROOT/$SOURCE_DIR"
fi

for unit in "$SERVICE_UNIT" "$TIMER_UNIT"; do
  if [[ ! -f "$SOURCE_DIR/$unit" ]]; then
    echo "ERROR: Missing unit: $SOURCE_DIR/$unit" >&2
    exit 1
  fi
done

echo "Install Armada Paper Readiness systemd units"
echo "  Source: $SOURCE_DIR"
echo "  Dest:   $DEST_DIR"
echo "  Scope:  $SCOPE"

if [[ "$DRY_RUN" == "1" ]]; then
  echo "DRY RUN: validation OK; skipping install"
  exit 0
fi

if [[ "$SCOPE" == "system" ]] && [[ "${EUID:-$(id -u)}" -ne 0 ]]; then
  echo "ERROR: Run as root (sudo) for system scope. Example: sudo bash tools/install_armada_paper_readiness_service.sh" >&2
  exit 1
fi

mkdir -p "$DEST_DIR"
if [[ "$SCOPE" == "user" ]]; then
  # user manager では User=/Group= 指定で 216/GROUP が発生するため除去する
  sed -e '/^User=/d' -e '/^Group=/d' "$SOURCE_DIR/$SERVICE_UNIT" >"$DEST_DIR/$SERVICE_UNIT"
  chmod 0644 "$DEST_DIR/$SERVICE_UNIT"
else
  install -m 0644 "$SOURCE_DIR/$SERVICE_UNIT" "$DEST_DIR/"
fi
install -m 0644 "$SOURCE_DIR/$TIMER_UNIT" "$DEST_DIR/"

systemctl "${SYSTEMCTL_FLAGS[@]}" daemon-reload
systemctl "${SYSTEMCTL_FLAGS[@]}" enable --now "$TIMER_UNIT"
systemctl "${SYSTEMCTL_FLAGS[@]}" status --no-pager "$TIMER_UNIT" || true

SYSTEMCTL_HINT="systemctl"
if [[ "$SCOPE" == "user" ]]; then
  SYSTEMCTL_HINT="systemctl --user"
fi

echo "Run once now:"
echo "  $SYSTEMCTL_HINT start $SERVICE_UNIT"
echo "  journalctl ${SYSTEMCTL_FLAGS[*]} -u $SERVICE_UNIT -n 100 --no-pager"
