#!/bin/bash
# tools/log_watchdog.sh - Swimmy System Sentinel
# Monitors logs for critical errors and alerts Discord

# Config
LOG_BRAIN="/home/swimmy/swimmy/logs/brain.log"
LOG_GUARDIAN="/home/swimmy/swimmy/logs/guardian.log"
CONFIG_FILE="src/lisp/core/config.lisp"
CHECK_INTERVAL=1  # Seconds between checks (tail -f is continuous, this is for loop safety if restart needed)

echo "🛡️ Starting Swimmy Sentinel..."

# Load webhook from JSON config file
CONFIG_JSON="/home/swimmy/swimmy/config/discord_webhooks.json"
if [ -f "$CONFIG_JSON" ]; then
    # Extract alerts webhook URL using jq or python
    if command -v jq &> /dev/null; then
        WEBHOOK_URL=$(jq -r '.webhooks.alerts.url' "$CONFIG_JSON")
    else
        WEBHOOK_URL=$(python3 -c "import json; print(json.load(open('$CONFIG_JSON'))['webhooks']['alerts']['url'])" 2>/dev/null)
    fi
fi

# Fallback to environment variable if JSON loading failed
if [ -z "$WEBHOOK_URL" ]; then
    WEBHOOK_URL="${SWIMMY_DISCORD_ALERTS:-$SWIMMY_DISCORD_WEBHOOK}"
fi

echo "🔗 Webhook Target: ${WEBHOOK_URL:0:40}..."

# 2. Monitoring Function
monitor_logs() {
    tail -n 0 -F "$LOG_BRAIN" "$LOG_GUARDIAN" 2>/dev/null | \
    grep --line-buffered -E "Err:|error|undefined|unbound|CRITICAL|Brain Silence|Address already in use|panicked|FATAL" | \
    while read -r line; do
        current_time=$(date "+%H:%M:%S")
        echo "🚨 [$current_time] DETECTED: $line"
        
        # Simple throttling (rudimentary) - prevent flooding (e.g. tight loops)
        # In a real script we might use a timestamp file, but for now blocking on curl helps throttle slightly.
        
        # Construct JSON payload
        # Escape quotes in the line
        safe_line=$(echo "$line" | sed 's/"/\\"/g' | cut -c 1-1900) # Limit length
        
        json_payload=$(cat <<EOF
{
  "content": "🚨 **Swimmy System Alert** 🚨\n\`\`\`\n$safe_line\n\`\`\`"
}
EOF
)
        # Send to Discord
        curl -H "Content-Type: application/json" \
             -d "$json_payload" \
             "$WEBHOOK_URL" > /dev/null 2>&1
             
        # Sleep to prevent absolute spam flood in case of infinite error loop
        sleep 2
    done
}

# 3. Main Execution
# Run in subshell to trap kills if needed, though simple execution is fine.
monitor_logs
