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
SELECTION=$(find data/library -ipath "*/selection/*.lisp" 2>/dev/null | wc -l)
TRAINING=$(find data/library -ipath "*/training/*.lisp" 2>/dev/null | wc -l)
BATTLEFIELD=$(find data/library -ipath "*/battlefield/*.lisp" 2>/dev/null | wc -l)
GRAVEYARD=$(find data/library -ipath "*/graveyard/*.lisp" 2>/dev/null | wc -l)

echo -e "   🍼 Incubator:   $INCUBATOR"
echo -e "   🪜 Selection:   $SELECTION (S > 0.1)"
echo -e "   🏋️ Training:    $TRAINING (S > 0.5)"
echo -e "   ⚔️  Battlefield: $BATTLEFIELD (S > 1.0)"
echo -e "   🪦 Graveyard:   $GRAVEYARD"

echo "========================================================"
echo "Live Logs (Press Ctrl+C to exit):"
echo "========================================================"

# 2. Tail Logs
# We use journalctl for the user service instead of tailing a file
journalctl --user -u swimmy-school -f -n 10
