#!/bin/bash
# tools/nuke_and_pave.sh
# Emergency cleanup script to guarantee a clean slate for Swimmy.

echo "☢️  INITIATING NUCLEAR CLEANUP SEQUENCE..."

# 1. Kill everything with extreme prejudice
echo "🔪 Killing all known processes..."
pkill -9 -f "brain.lisp"
pkill -9 -f "guardian"
pkill -9 -f "discord_bot.py"
pkill -9 -f "sbcl" 
killall -9 sbcl 2>/dev/null
killall -9 guardian 2>/dev/null

# 2. Port Vacuum
echo "🧹 Vacuuming ports 5555-5560..."
fuser -k -9 5555/tcp 2>/dev/null
fuser -k -9 5556/tcp 2>/dev/null
fuser -k -9 5557/tcp 2>/dev/null
fuser -k -9 5558/tcp 2>/dev/null
fuser -k -9 5559/tcp 2>/dev/null
fuser -k -9 5560/tcp 2>/dev/null

# 3. Verification Loop (The important part)
echo "⏳ Verifying cleanup..."
attempts=0
while [ $attempts -lt 10 ]; do
    PORT_USAGE=$(fuser 5555/tcp 2>/dev/null)
    PROCESS_USAGE=$(pgrep -f "brain.lisp")
    
    if [ -z "$PORT_USAGE" ] && [ -z "$PROCESS_USAGE" ]; then
        echo "✅ System is CLEAN. Ports are free. No zombies."
        echo "🚀 Starting System in 3 seconds..."
        sleep 3
        # Clean background run
        nohup make run > logs/swimmy.log 2>&1 &
        exit 0
    fi
    
    echo "⚠️  Waiting for zombies to die... (Attempt $attempts/10)"
    fuser -k -9 5555/tcp 2>/dev/null
    sleep 2
    attempts=$((attempts+1))
done

echo "❌ CRITICAL FAILURE: Creating dirty state could not be resolved."
exit 1
