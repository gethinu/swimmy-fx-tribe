#!/usr/bin/env bash
# tools/install_services.sh - Install and start Swimmy services via systemd (system scope)
#
# Supports fail-fast validation and a dry-run mode for CI-style checks.
#
# Env:
# - SWIMMY_SYSTEMD_DIR: source directory containing unit files (default: <repo>/systemd)
# - SWIMMY_SYSTEMD_DEST_DIR: destination for system units (default: /etc/systemd/system)
# - SWIMMY_SYSTEMD_DRY_RUN: if "1", skip copy and systemctl operations (validation still runs)
# - SWIMMY_SERVICES: whitespace-separated service base names to enable/start

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SOURCE_DIR="${SWIMMY_SYSTEMD_DIR:-$ROOT/systemd}"
DEST_DIR="${SWIMMY_SYSTEMD_DEST_DIR:-/etc/systemd/system}"
DRY_RUN="${SWIMMY_SYSTEMD_DRY_RUN:-0}"

if [[ "$SOURCE_DIR" != /* ]]; then
  SOURCE_DIR="$ROOT/$SOURCE_DIR"
fi

shopt -s nullglob
unit_files=("$SOURCE_DIR"/*.service "$SOURCE_DIR"/*.timer)
if [ "${#unit_files[@]}" -eq 0 ]; then
  echo "ERROR: No systemd unit files found in: $SOURCE_DIR" >&2
  exit 1
fi

SERVICES_STR="${SWIMMY_SERVICES:-swimmy-backtest swimmy-notifier swimmy-data-keeper swimmy-risk swimmy-pattern-similarity}"
read -r -a SERVICES <<<"$SERVICES_STR"

for svc in "${SERVICES[@]}"; do
  if [ ! -f "$SOURCE_DIR/${svc}.service" ]; then
    echo "ERROR: Missing unit: $SOURCE_DIR/${svc}.service" >&2
    exit 1
  fi
done

echo "Installing systemd units (system scope)"
echo "  Source: $SOURCE_DIR"
echo "  Dest:   $DEST_DIR"
echo "  Services: $SERVICES_STR"

if [ "$DRY_RUN" = "1" ]; then
  echo "DRY RUN: validation OK; skipping copy and systemctl operations."
  exit 0
fi

if [ "${EUID:-$(id -u)}" -ne 0 ]; then
  echo "ERROR: Run as root (sudo). Example: sudo bash tools/install_services.sh" >&2
  exit 1
fi

mkdir -p "$DEST_DIR"

echo "Copying unit files..."
for unit in "${unit_files[@]}"; do
  install -m 0644 "$unit" "$DEST_DIR/"
done

echo "Reloading systemd daemon..."
systemctl daemon-reload

echo "Enabling and starting services..."
for svc in "${SERVICES[@]}"; do
  echo "  - $svc"
  systemctl enable --now "${svc}.service"
done

echo "Status:"
systemctl status --no-pager "${SERVICES[@]/%/.service}" || true

echo "OK"
