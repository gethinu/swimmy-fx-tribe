#!/bin/bash
# consolidate_env.sh - Consolidates environment variables to root .env

echo "Consolidating environment variables to /home/swimmy/swimmy/.env..."

# Update systemd services
sudo sed -i 's|config/.env|.env|g' /etc/systemd/system/swimmy-brain.service
sudo sed -i 's|config/.env|.env|g' /etc/systemd/system/swimmy-school.service
sudo sed -i 's|config/.env|.env|g' /etc/systemd/system/swimmy-guardian.service
sudo sed -i 's|config/.env|.env|g' /etc/systemd/system/swimmy.service

# Reload and restart
sudo systemctl daemon-reload
sudo systemctl restart swimmy-brain swimmy-school swimmy-guardian

# Cleanup
if [ -f /home/swimmy/swimmy/config/.env ]; then
    rm /home/swimmy/swimmy/config/.env
    echo "Removed redundant config/.env"
fi

echo "Consolidation complete. Services are now using the root .env file."
