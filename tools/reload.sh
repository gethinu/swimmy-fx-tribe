#!/bin/bash
# tools/reload.sh - Hot Reload Trigger (Gene Kim's Lever)
# Sends SIGHUP to the running Swimmy Brain process.

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Find PID of the Brain (SBCL process running main.lisp or just "sbcl" with swimmy loaded)
# We assume the main process is searchable via "sbcl" and "swimmy".Adjust if necessary.
# More robust: Look for the process created by systemd or the main entry.

BRAIN_PID=$(pgrep -f "sbcl.*swimmy" | head -n 1)

if [ -z "$BRAIN_PID" ]; then
    echo -e "${RED}[ERROR] Swimmy Brain process not found! Is it running?${NC}"
    exit 1
fi

echo -e "${GREEN}[INFO] Found Swimmy Brain at PID: ${BRAIN_PID}${NC}"
echo -e "${GREEN}[ACTION] Sending SIGHUP to trigger Hot Reload...${NC}"

kill -1 $BRAIN_PID

if [ $? -eq 0 ]; then
    echo -e "${GREEN}[SUCCESS] Signal sent. Check logs for 'Hot Reload Complete'.${NC}"
    echo -e "Command: tail -f logs/system.log | grep 'Hot Reload'"
else
    echo -e "${RED}[ERROR] Failed to send signal.${NC}"
    exit 1
fi
