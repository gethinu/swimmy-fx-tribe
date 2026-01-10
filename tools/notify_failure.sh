#!/bin/bash
# notify_failure.sh
# Sends failure notification to Discord if service failed.
# Usage: ./notify_failure.sh "$SERVICE_RESULT" "Service Name"

SERVICE_RESULT=$1
SERVICE_NAME=${2:-"Service"}

# Use environment variables loaded by systemd
WEBHOOK_URL="${SWIMMY_DISCORD_ALERTS:-$SWIMMY_DISCORD_APEX}"
# Fallback if not set (optional, maybe check .env file manually if needed?)
# But EnvironmentFile in systemd should handle it.

if [ "$SERVICE_RESULT" != "success" ]; then
  if [ -n "$WEBHOOK_URL" ]; then
    # Safely construct JSON using jq if available, else simple string concat
    if command -v jq >/dev/null 2>&1; then
       JSON=$(jq -n --arg name "$SERVICE_NAME" --arg res "$SERVICE_RESULT" \
              '{content: ("🚨 **" + $name + " CRASHED** (" + $res + ")\nRestarting...")}')
       curl -s -H "Content-Type: application/json" -d "$JSON" "$WEBHOOK_URL"
    else
       # Fallback to simple string
       curl -s -H "Content-Type: application/json" \
         -d "{\"content\":\"🚨 **$SERVICE_NAME CRASHED** ($SERVICE_RESULT)\\nRestarting...\"}" \
         "$WEBHOOK_URL"
    fi
  fi
fi
