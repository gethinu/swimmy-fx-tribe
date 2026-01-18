#!/bin/bash
# Swimmy Launch Script with Log Rotation (Gene Kim)

cd /home/swimmy/swimmy

# Load Environment Variables
if [ -f .env ]; then
    export $(grep -v '^#' .env | xargs)
fi

# Create log directory
mkdir -p logs

# Rotate old log
if [ -f logs/swimmy.log ]; then
    mv logs/swimmy.log logs/swimmy.$(date +%Y%m%d_%H%M%S).log
fi

# Keep only last 7 log files
ls -t logs/swimmy.*.log 2>/dev/null | tail -n +8 | xargs -r rm

# Start Swimmy with logging
echo "[$(date)] Starting Swimmy Ver 41.5..."
sbcl --dynamic-space-size 4096 --noinform --load brain.lisp 2>&1 | tee logs/swimmy.log