#!/bin/bash
# install_services.sh - Install and start modular services via systemd (User Mode)

echo "🔧 Installing Systemd Services..."
mkdir -p ~/.config/systemd/user

cp systemd/*.service systemd/*.timer ~/.config/systemd/user/ 2>/dev/null || true

echo "🔄 Reloading Daemon..."
systemctl --user daemon-reload

echo "🚀 Enabling and Starting Services..."
SERVICES="swimmy-backtest swimmy-notifier swimmy-data-keeper swimmy-risk"

for svc in $SERVICES; do
    echo "   - $svc"
    systemctl --user enable $svc
    systemctl --user restart $svc
done

echo "✅ All modular services are running!"
echo "   Check logs inlogs/*.log"
systemctl --user status $SERVICES --no-pager
