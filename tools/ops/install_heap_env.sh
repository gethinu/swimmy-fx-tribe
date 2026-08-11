#!/usr/bin/env bash
# tools/ops/install_heap_env.sh
# ============================================================================
# P4 (Thread B / reliability): install the canonical SBCL heap config to
# /etc/swimmy/heap.env, the single source of truth loaded (last) by both
# swimmy-brain.service and swimmy-school.service.
#
# Idempotent: re-running only rewrites the target if it differs from the repo
# template. Run as root (sudo). Does NOT restart services — the caller decides
# when to `systemctl daemon-reload` and restart brain/school.
# ============================================================================

set -euo pipefail

SRC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SRC="${SRC_DIR}/config/heap.env"
DEST_DIR="/etc/swimmy"
DEST="${DEST_DIR}/heap.env"

if [ ! -f "${SRC}" ]; then
    echo "❌ Template not found: ${SRC}" >&2
    exit 1
fi

if [ "${EUID:-$(id -u)}" -ne 0 ]; then
    echo "❌ Must run as root (sudo) to write ${DEST}" >&2
    exit 1
fi

install -d -m 0755 "${DEST_DIR}"

if [ -f "${DEST}" ] && cmp -s "${SRC}" "${DEST}"; then
    echo "✅ ${DEST} already up to date."
    exit 0
fi

install -m 0644 "${SRC}" "${DEST}"
echo "✅ Installed ${DEST}:"
sed -n 's/^\(SWIMMY_[A-Z_]*=.*\)/    \1/p' "${DEST}"
echo ""
echo "Next: sudo systemctl daemon-reload && sudo systemctl restart swimmy-brain swimmy-school"
