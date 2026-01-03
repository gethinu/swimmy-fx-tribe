#!/bin/bash
# install-service.sh - Install Swimmy as a systemd service
# Usage: sudo ./scripts/install-service.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
SERVICE_FILE="$SCRIPT_DIR/swimmy.service"
SYSTEMD_PATH="/etc/systemd/system/swimmy.service"

echo "🐟 Installing Swimmy systemd service..."

# Check if running as root
if [ "$EUID" -ne 0 ]; then
    echo "❌ This script must be run as root (sudo)"
    exit 1
fi

# Check if service file exists
if [ ! -f "$SERVICE_FILE" ]; then
    echo "❌ Service file not found: $SERVICE_FILE"
    exit 1
fi

# Copy service file
echo "📋 Copying service file..."
cp "$SERVICE_FILE" "$SYSTEMD_PATH"

# Reload systemd
echo "🔄 Reloading systemd daemon..."
systemctl daemon-reload

# Enable service
echo "✅ Enabling swimmy service..."
systemctl enable swimmy

echo ""
echo "🎉 Installation complete!"
echo ""
echo "Commands:"
echo "  sudo systemctl start swimmy    # Start the service"
echo "  sudo systemctl stop swimmy     # Stop the service"
echo "  sudo systemctl status swimmy   # Check status"
echo "  journalctl -u swimmy -f        # View logs"
echo ""
