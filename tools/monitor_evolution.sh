#!/bin/bash
# tools/monitor_evolution.sh
# Real-time dashboard for the Evolution Service

clear
echo "========================================================"
echo " 🧬 SWIMMY EVOLUTION MONITOR (Real-Time)"
echo "========================================================"

# 1. Count Strategy Populations (Snapshot)
echo -e "\n📊 Population Census:"
INCUBATOR=$(find data/library -ipath "*/incubator/*.lisp" 2>/dev/null | wc -l)
BRANK=$(find data/library -ipath "*/b/*.lisp" 2>/dev/null | wc -l)
ARANK=$(find data/library -ipath "*/a/*.lisp" 2>/dev/null | wc -l)
SRANK=$(find data/library -ipath "*/s/*.lisp" 2>/dev/null | wc -l)
GRAVEYARD=$(find data/library -ipath "*/graveyard/*.lisp" 2>/dev/null | wc -l)

echo -e "   🍼 Incubator:   $INCUBATOR"
echo -e "   🪜 B-Rank:      $BRANK"
echo -e "   🎖️ A-Rank:      $ARANK"
echo -e "   🏆 S-Rank:      $SRANK"
echo -e "   🪦 Graveyard:   $GRAVEYARD"

echo "========================================================"
echo "Live Logs (Press Ctrl+C to exit):"
echo "========================================================"

# 2. Tail Logs
# We use journalctl for the user service instead of tailing a file
journalctl --user -u swimmy-school -f -n 10
