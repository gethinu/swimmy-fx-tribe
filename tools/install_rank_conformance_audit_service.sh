#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SOURCE_DIR="${SWIMMY_SYSTEMD_DIR:-$ROOT/systemd}"
DEST_DIR="${SWIMMY_SYSTEMD_DEST_DIR:-/etc/systemd/system}"
DRY_RUN="${SWIMMY_SYSTEMD_DRY_RUN:-0}"

SERVICE_UNIT="swimmy-rank-conformance-audit.service"
TIMER_UNIT="swimmy-rank-conformance-audit.timer"

if [[ "$SOURCE_DIR" != /* ]]; then
  SOURCE_DIR="$ROOT/$SOURCE_DIR"
fi

for unit in "$SERVICE_UNIT" "$TIMER_UNIT"; do
  if [[ ! -f "$SOURCE_DIR/$unit" ]]; then
    echo "ERROR: Missing unit: $SOURCE_DIR/$unit" >&2
    exit 1
  fi
done

echo "Install Rank Conformance Audit systemd units"
echo "  Source: $SOURCE_DIR"
echo "  Dest:   $DEST_DIR"

if [[ "$DRY_RUN" == "1" ]]; then
  echo "DRY RUN: validation OK; skipping install"
  exit 0
fi

if [[ "${EUID:-$(id -u)}" -ne 0 ]]; then
  echo "ERROR: Run as root (sudo). Example: sudo bash tools/install_rank_conformance_audit_service.sh" >&2
  exit 1
fi

mkdir -p "$DEST_DIR"
install -m 0644 "$SOURCE_DIR/$SERVICE_UNIT" "$DEST_DIR/"
install -m 0644 "$SOURCE_DIR/$TIMER_UNIT" "$DEST_DIR/"

systemctl daemon-reload
systemctl enable --now "$TIMER_UNIT"
systemctl status --no-pager "$TIMER_UNIT" || true

echo "Run once now:"
echo "  systemctl start $SERVICE_UNIT"
echo "  journalctl -u $SERVICE_UNIT -n 100 --no-pager"
