#!/bin/bash
# tools/monitor_evolution.sh
# Real-time dashboard for the Evolution Service

clear
echo "========================================================"
echo " 🧬 SWIMMY EVOLUTION MONITOR (Real-Time)"
echo "========================================================"
echo "Filter: 'Evolving', 'Born', 'Fitness', 'Stats'"
echo "Press Ctrl+C to exit."
echo "========================================================"

# Tail the logs, filtering for relevant evolution keywords
# We use stdbuf to ensure output is not buffered
tail -f logs/swimmy.log | grep --line-buffered -E "Evolving|Born|Fitness|Stats|ACTIVE|RECRUIT|BACKTEST-SVC"
