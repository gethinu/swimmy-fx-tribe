#!/bin/bash
# tools/check_guardian_health.sh
# V15.4: Guardian Self-Monitoring (Naval P3)
# Run via cron every 5 minutes to detect Guardian failure

SERVICE="swimmy-guardian.service"
WEBHOOK_URL="${SWIMMY_DISCORD_ALERTS:-}"
ALERT_FILE="/tmp/guardian_alert_sent"
SYSTEMCTL_CMD="${SYSTEMCTL_CMD:-systemctl}"
PGREP_CMD="${PGREP_CMD:-pgrep}"
KILL_CMD="${KILL_CMD:-kill}"
SLEEP_CMD="${SLEEP_CMD:-sleep}"

# Check if Guardian is running
if "$SYSTEMCTL_CMD" --user is-active --quiet "$SERVICE"; then
    # Guardian is alive, remove alert flag if exists
    rm -f "$ALERT_FILE"

    MAINPID="$("$SYSTEMCTL_CMD" --user show -p MainPID swimmy-brain.service 2>/dev/null | cut -d= -f2)"
    while read -r pid _; do
        [ -z "$pid" ] && continue
        if [ -n "$MAINPID" ] && [ "$MAINPID" != "0" ] && [ "$pid" = "$MAINPID" ]; then
            echo "[WATCHDOG] run.sh MainPID detected: $pid (skip)"
            continue
        fi
        echo "[WATCHDOG] STRAY run.sh detected: $pid (terminating)"
        "$KILL_CMD" -TERM "$pid" 2>/dev/null || true
        "$SLEEP_CMD" 2
        "$KILL_CMD" -KILL "$pid" 2>/dev/null || true
    done < <("$PGREP_CMD" -af "/home/swimmy/swimmy/run.sh" 2>/dev/null | awk '{print $1}')

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
    "$SYSTEMCTL_CMD" --user restart "$SERVICE"
    "$SLEEP_CMD" 5
    
    if "$SYSTEMCTL_CMD" --user is-active --quiet "$SERVICE"; then
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
