#!/bin/bash
# tools/check_school_health.sh
# V50.3 Expert Panel 2: Honest Watchdog (Hamilton/Vogels)
# "Did the CPU actually do work?"
# Checks heartbeat file freshness instead of just PID.

SERVICE="swimmy-school.service"
HEARTBEAT_FILE="/home/swimmy/swimmy/data/heartbeat/school.tick"
ALERT_FILE="/tmp/school_alert_sent"
WEBHOOK_URL="${SWIMMY_DISCORD_ALERTS:-}"
THRESHOLD_SECONDS=300 # 5 Minutes

# 1. Check if service is technically active (systemd view)
if ! systemctl --user is-active --quiet "$SERVICE"; then
    echo "$(date): Service is DOWN (Systemd)."
    RESTART_NEEDED=true
else
    # 2. Honest Check: Is the heartbeat fresh?
    if [ -f "$HEARTBEAT_FILE" ]; then
        MOD_TIME=$(stat -c %Y "$HEARTBEAT_FILE")
        NOW=$(date +%s)
        DIFF=$((NOW - MOD_TIME))
        
        if [ "$DIFF" -gt "$THRESHOLD_SECONDS" ]; then
            echo "$(date): 🚨 ZOMBIE DETECTED! Heartbeat is ${DIFF}s old (Threshold: ${THRESHOLD_SECONDS}s)."
            RESTART_NEEDED=true
        else
            # Healthy
            # echo "$(date): Healthy (Lag: ${DIFF}s)"
            rm -f "$ALERT_FILE"
            exit 0
        fi
    else
        echo "$(date): 🚨 No Heartbeat File found!"
        RESTART_NEEDED=true
    fi
fi

if [ "$RESTART_NEEDED" = true ]; then
    # Kill Zombie Process if it exists
    pkill -9 -f "school-daemon.lisp"
    
    echo "Attempting restart..."
    systemctl --user restart "$SERVICE"
    sleep 10
    
    # Verify Restart
    if systemctl --user is-active --quiet "$SERVICE"; then
        echo "Service restarted successfully."
        # Give it time to write a new heartbeat before clearing alert? 
        # Actually just exit, next run will verify heartbeat.
        rm -f "$ALERT_FILE"
        
        # Send Recovery Notification if alert was previously sent
        if [ -f "$ALERT_FILE" ] && [ -n "$WEBHOOK_URL" ]; then
             curl -s -H "Content-Type: application/json" \
                -d "{\"content\":\"@here\",\"embeds\":[{\"title\":\"🧟 ZOMBIE KILLED\",\"description\":\"Swimmy School was frozen (Zombie) and has been forcibly restarted.\",\"color\":65280}]}" \
                "$WEBHOOK_URL" > /dev/null
        fi
        exit 0
    fi
    
    # Restart Failed
    echo "Restart FAILED."
    
    # Alert (Once)
    if [ ! -f "$ALERT_FILE" ] && [ -n "$WEBHOOK_URL" ]; then
        curl -s -H "Content-Type: application/json" \
            -d "{\"content\":\"@here\",\"embeds\":[{\"title\":\"🛑 BRAIN DEAD\",\"description\":\"Swimmy School is down and cannot restart. Heartbeat missing.\",\"color\":16711680}]}" \
            "$WEBHOOK_URL" > /dev/null
        touch "$ALERT_FILE"
    fi
    
    exit 1
fi
