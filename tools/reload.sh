#!/bin/bash
# tools/reload.sh - Hot Reload Trigger (Gene Kim's Lever)
# Sends SIGHUP to the running Swimmy Brain process.

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Prefer systemd restart for safety (avoids port rebind conflicts)
if command -v systemctl >/dev/null 2>&1 && systemctl status swimmy-brain.service >/dev/null 2>&1; then
  echo -e "${GREEN}[ACTION] Restarting swimmy-brain via systemd (system scope)...${NC}"
  if [ "${EUID:-$(id -u)}" -eq 0 ]; then
    if systemctl restart swimmy-brain.service; then
      echo -e "${GREEN}[SUCCESS] swimmy-brain restarted.${NC}"
      exit 0
    fi
    echo -e "${RED}[WARN] Failed to restart swimmy-brain via systemd. Falling back to SIGHUP.${NC}"
  elif command -v sudo >/dev/null 2>&1 && sudo -n true >/dev/null 2>&1; then
    if sudo -n systemctl restart swimmy-brain.service; then
      echo -e "${GREEN}[SUCCESS] swimmy-brain restarted.${NC}"
      exit 0
    fi
    echo -e "${RED}[WARN] Failed to restart swimmy-brain via systemd. Falling back to SIGHUP.${NC}"
  else
    echo -e "${RED}[WARN] swimmy-brain is managed by systemd, but restart requires root. Falling back to SIGHUP.${NC}"
  fi
fi

# Find PID of the Brain (SBCL process running brain.lisp)
BRAIN_PID=$(pgrep -f "sbcl.*brain.lisp" | head -n 1)

# Fallback for alternate process names
if [ -z "$BRAIN_PID" ]; then
  BRAIN_PID=$(pgrep -f "sbcl.*swimmy" | head -n 1)
fi

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
