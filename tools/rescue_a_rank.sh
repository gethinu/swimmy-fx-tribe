#!/bin/bash
# Rescue script for A-Rank strategies (Sharpe > 0.3)
# Moves .lisp files from GRAVEYARD to A-Rank if they match criteria.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SWIMMY_HOME="${SWIMMY_HOME:-$(cd "$SCRIPT_DIR/.." && pwd)}"
GRAVE_DIR="$SWIMMY_HOME/data/library/GRAVEYARD"
TARGET_DIR="$SWIMMY_HOME/data/library/A"
mkdir -p "$TARGET_DIR"

echo "[RESCUE] 🚑 Scanning GRAVEYARD for Sharpe > 0.3..."

# Grep for SHARPE > 0.3
# Format is like: :SHARPE 0.4652671
grep -l "SHARPE [0-9]" "$GRAVE_DIR"/*.lisp | while read -r file; do
    sharpe=$(grep ":SHARPE" "$file" | awk '{print $2}')
    
    # Check if sharpe > 0.3 using bc or awk
    if (( $(echo "$sharpe >= 0.3" | bc -l) )); then
        basename=$(basename "$file")
        echo "[RESCUE] 🌟 Rescuing $basename (Sharpe=$sharpe)"
        
        # 1. Update File Content (sed)
        # Change :RANK :GRAVEYARD/B/S/VETERAN -> :RANK :A
        # Change :STATUS :INACTIVE -> :STATUS :ACTIVE
        sed -i 's/:RANK :GRAVEYARD/:RANK :A/g' "$file"
        sed -i 's/:RANK :B/:RANK :A/g' "$file" 
        sed -i 's/:RANK :S/:RANK :A/g' "$file"
        sed -i 's/:RANK :VETERAN/:RANK :A/g' "$file"
        sed -i 's/:STATUS :INACTIVE/:STATUS :ACTIVE/g' "$file"
        
        # 2. Move File
        mv "$file" "$TARGET_DIR/"
    fi
done

echo "[RESCUE] ✅ Operation Complete. Run (swimmy.school::reload-strategies) or restart brain to pick them up."
