#!/bin/bash
# consolidate_env.sh - Consolidates environment variables to root .env

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SWIMMY_HOME="${SWIMMY_HOME:-$(cd "$SCRIPT_DIR/.." && pwd)}"
echo "Consolidating environment variables to ${SWIMMY_HOME}/.env..."

# Update systemd services
sudo sed -i 's|config/.env|.env|g' /etc/systemd/system/swimmy-brain.service
sudo sed -i 's|config/.env|.env|g' /etc/systemd/system/swimmy-school.service
sudo sed -i 's|config/.env|.env|g' /etc/systemd/system/swimmy-guardian.service
sudo sed -i 's|config/.env|.env|g' /etc/systemd/system/swimmy.service

# Reload and restart
sudo systemctl daemon-reload
sudo systemctl restart swimmy-brain swimmy-school swimmy-guardian

# Cleanup
if [ -f "${SWIMMY_HOME}/config/.env" ]; then
    rm "${SWIMMY_HOME}/config/.env"
    echo "Removed redundant config/.env"
fi

echo "Consolidation complete. Services are now using the root .env file."
