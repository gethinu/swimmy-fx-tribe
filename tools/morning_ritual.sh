#!/bin/bash
# morning_ritual.sh
# Runs daily at 06:00 JST to refresh data and rotate strategies.

# Ensure we are in the project root
cd "$(dirname "$0")/.."

echo "🌅 Starting Morning Ritual $(date)"

# 1. Fetch Swap Data (Requires Live Bridge)
echo "1. Fetching Swap Data..."
python3 tools/fetch_swap_data.py

# Wait for data to settle (Brain processing time)
sleep 5

# 2. Trigger Strategy Deployment (Alchemy Factory)
echo "2. Triggering Alchemy Factory..."
python3 tools/trigger_alchemy.py

echo "✅ Morning Ritual Complete $(date)"
