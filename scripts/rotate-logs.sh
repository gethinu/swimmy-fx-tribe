#!/bin/bash
# rotate-logs.sh - Log rotation for Swimmy
# Keeps only last 7 days of logs, compresses old logs

set -e

LOG_DIR="/home/swimmy/swimmy/doc/logs"
JSON_LOG="/home/swimmy/logs/swimmy.json.log"
KEEP_DAYS=7
COMPRESS_DAYS=1

echo "🔄 Swimmy Log Rotation"
echo "━━━━━━━━━━━━━━━━━━━━━━"

# Delete logs older than KEEP_DAYS
echo "🗑️ Removing logs older than $KEEP_DAYS days..."
if [ -d "$LOG_DIR" ]; then
    OLD_COUNT=$(find "$LOG_DIR" -name "log_*.txt" -mtime +$KEEP_DAYS 2>/dev/null | wc -l)
    find "$LOG_DIR" -name "log_*.txt" -mtime +$KEEP_DAYS -delete 2>/dev/null || true
    echo "   Deleted: $OLD_COUNT files"
fi

# Compress logs older than 1 day
echo "📦 Compressing logs older than $COMPRESS_DAYS day..."
if [ -d "$LOG_DIR" ]; then
    find "$LOG_DIR" -name "log_*.txt" -mtime +$COMPRESS_DAYS -exec gzip -q {} \; 2>/dev/null || true
    COMPRESSED=$(find "$LOG_DIR" -name "log_*.txt.gz" 2>/dev/null | wc -l)
    echo "   Compressed files: $COMPRESSED"
fi

# Rotate JSON log if too large (>50MB)
if [ -f "$JSON_LOG" ]; then
    SIZE=$(stat -f%z "$JSON_LOG" 2>/dev/null || stat -c%s "$JSON_LOG" 2>/dev/null)
    if [ "$SIZE" -gt 52428800 ]; then
        echo "📊 Rotating JSON log ($(numfmt --to=iec $SIZE))..."
        mv "$JSON_LOG" "$JSON_LOG.$(date +%Y%m%d)"
        gzip "$JSON_LOG.$(date +%Y%m%d)"
    fi
fi

# Show current disk usage
echo ""
echo "📈 Current log usage:"
du -sh "$LOG_DIR" 2>/dev/null || echo "   N/A"

echo ""
echo "✅ Log rotation complete"
