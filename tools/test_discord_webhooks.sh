#!/bin/bash
# tools/test_discord_webhooks.sh

source .env

echo "Testing Webhooks..."

# 1. LIVE FEED
if [ -n "$SWIMMY_DISCORD_LIVE_FEED" ]; then
    echo "Sending to LIVE FEED..."
    curl -H "Content-Type: application/json" -d '{"content": "🧪 FAST-TEST: Live Feed"}' "$SWIMMY_DISCORD_LIVE_FEED"
else
    echo "SKIP: LIVE FEED (Empty)"
fi

# 2. SYSTEM LOGS
if [ -n "$SWIMMY_DISCORD_SYSTEM_LOGS" ]; then
    echo "Sending to SYSTEM LOGS..."
    curl -H "Content-Type: application/json" -d '{"content": "🧪 FAST-TEST: System Logs"}' "$SWIMMY_DISCORD_SYSTEM_LOGS"
else
    echo "SKIP: SYSTEM LOGS (Empty)"
fi

# 3. REPORTS
if [ -n "$SWIMMY_DISCORD_REPORTS" ]; then
    echo "Sending to REPORTS..."
    curl -H "Content-Type: application/json" -d '{"content": "🧪 FAST-TEST: Reports"}' "$SWIMMY_DISCORD_REPORTS"
else
    echo "SKIP: REPORTS (Empty)"
fi

# 4. ALERTS
if [ -n "$SWIMMY_DISCORD_ALERTS" ]; then
    echo "Sending to ALERTS..."
    curl -H "Content-Type: application/json" -d '{"content": "🧪 FAST-TEST: Alerts"}' "$SWIMMY_DISCORD_ALERTS"
else
    echo "SKIP: ALERTS (Empty)"
fi

# 5. SWIMMY LOGS
if [ -n "$SWIMMY_DISCORD_SWIMMY_LOGS" ]; then
    echo "Sending to SWIMMY LOGS..."
    curl -H "Content-Type: application/json" -d '{"content": "🧪 FAST-TEST: Swimmy Logs"}' "$SWIMMY_DISCORD_SWIMMY_LOGS"
else
    echo "SKIP: SWIMMY LOGS (Empty)"
fi
