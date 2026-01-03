#!/bin/bash

# Load secrets from .env file
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
if [ -f "$SCRIPT_DIR/config/.env" ]; then
    source "$SCRIPT_DIR/config/.env"
else
    echo "⚠️ Warning: config/.env file not found. Copy config/.env.template to config/.env and fill in secrets."
fi

# Log file setup
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
LOG_DIR="$SCRIPT_DIR/doc/logs"
LOG_FILE="$LOG_DIR/log_$(date +%Y%m%d_%H%M%S).txt"
mkdir -p "$LOG_DIR"
echo "📝 Logging to: $LOG_FILE"

# 1. Kill old processes (force kill to ensure ports are released)
echo "💀 Killing old processes..."
pkill -9 -f "guardian" 2>/dev/null
pkill -9 -f "sbcl" 2>/dev/null
pkill -9 -f "discord_bot.py" 2>/dev/null
sleep 2  # Wait for ports to be released

# 2. Discord Bot (Python - for mobile interaction)
echo "🤖 Starting Discord Bot..."
.venv/bin/python src/python/discord_bot.py &
DISCORD_BOT_PID=$!
sleep 1

# 3. Rust Guardian
echo "🦀 Starting Guardian..."
cd guardian
./target/release/guardian 2>&1 | tee -a "$LOG_FILE" &
GUARDIAN_PID=$!
cd ..

echo "⏳ Waiting for Guardian (2 sec)..."
sleep 2

# 4. Lisp Brain (output to both terminal and log file)
echo "🧠 Starting Brain (Hyper-Evolution)..."
sbcl --script brain.lisp 2>&1 | tee -a "$LOG_FILE"

# Cleanup
kill $GUARDIAN_PID 2>/dev/null

trap "pkill -9 sbcl; pkill -9 guardian; exit" SIGINT