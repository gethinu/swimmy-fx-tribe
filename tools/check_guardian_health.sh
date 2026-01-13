#!/bin/bash
# tools/check_guardian_health.sh
# V15.4: Guardian Self-Monitoring (Naval P3)
# Run via cron every 5 minutes to detect Guardian failure

SERVICE="swimmy-guardian.service"
WEBHOOK_URL="${SWIMMY_DISCORD_ALERTS:-}"
ALERT_FILE="/tmp/guardian_alert_sent"

# Check if Guardian is running
if systemctl --user is-active --quiet "$SERVICE"; then
    # Guardian is alive, remove alert flag if exists
    rm -f "$ALERT_FILE"
    exit 0
else
    # Guardian is dead!
    echo "$(date): Guardian is DOWN!"
    
    # Only alert once per failure (avoid spam)
    if [ -f "$ALERT_FILE" ]; then
        echo "Alert already sent, skipping."
        exit 1
    fi
    
    # Try to restart
    echo "Attempting restart..."
    systemctl --user restart "$SERVICE"
    sleep 5
    
    if systemctl --user is-active --quiet "$SERVICE"; then
        echo "Guardian restarted successfully."
        rm -f "$ALERT_FILE"
        exit 0
    fi
    
    # Restart failed, send alert
    if [ -n "$WEBHOOK_URL" ]; then
        curl -s -H "Content-Type: application/json" \
            -d "{\"content\":\"@here\",\"embeds\":[{\"title\":\"🛡️ GUARDIAN DOWN\",\"description\":\"Guardian service failed and could not be restarted. Manual intervention required.\",\"color\":16711680}]}" \
            "$WEBHOOK_URL" > /dev/null
        echo "Alert sent to Discord."
    fi
    
    touch "$ALERT_FILE"
    exit 1
fi
