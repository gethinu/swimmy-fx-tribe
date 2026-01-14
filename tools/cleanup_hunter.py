#!/usr/bin/env python3
"""
cleanup_hunter.py
Naval Ravikant's Auto-Funeral Service.
Removes dead strategies (listed in data/graveyard.txt) from src/lisp/school-hunter.lisp.
Prevents file bloat and "Zombie Strategy" resurrection.
"""

import os
import re

GRAVEYARD_FILE = "data/graveyard.txt"
HUNTER_FILE = "src/lisp/school/school-hunter.lisp"


def main():
    if not os.path.exists(GRAVEYARD_FILE):
        print("[Auto-Funeral] No graveyard registry found. Skipping cleanup.")
        return

    with open(GRAVEYARD_FILE, "r") as f:
        dead_names = {line.strip() for line in f if line.strip()}

    if not dead_names:
        print("[Auto-Funeral] Graveyard empty.")
        return

    print(f"[Auto-Funeral] ⚰️ Found {len(dead_names)} death certificates processing...")

    if not os.path.exists(HUNTER_FILE):
        print(f"[Auto-Funeral] Error: {HUNTER_FILE} not found!")
        return

    with open(HUNTER_FILE, "r") as f:
        content = f.read()

    # Pattern to match (def-founder "NAME" ... ) including the closing parenthesis
    # This is tricky with Lisp due to recursive parens.
    # Simple heuristic:
    # We find the start of (def-founder "NAME"
    # We strip it out?
    # Actually, Lisp parsing with Regex is famously impossible.
    # However, our founders are generated with a strict template:
    # (def-founder "NAME"
    #   :start-time ...
    #   ...
    #   :logic '(...))

    # Let's try to remove them block by block.
    # But Python regex for balanced parens is hard.
    # Alternative: Use simple line-based processing if formatted standardly.
    # Or, just replace the exact definition string if we can construct it? No.

    # State machine approach:
    new_lines = []
    lines = content.splitlines()
    skip_mode = False
    current_founder = None
    removed_count = 0

    # Regex to detect start of founder
    founder_start_pat = re.compile(r'^\s*\(def-founder\s+"([^"]+)"')

    # Count parens to detect end of block
    paren_depth = 0
    in_block = False

    for line in lines:
        match = founder_start_pat.search(line)
        if match:
            # Found a start
            name = match.group(1)
            if name in dead_names:
                skip_mode = True
                current_founder = name
                in_block = True
                paren_depth = 0  # Reset for this block logic
                # Count parens in this line to start tracking
                paren_depth += line.count("(") - line.count(")")
                # If depth is 0, it was a one-line definition (unlikely but possible)
                if paren_depth <= 0:
                    skip_mode = False
                    in_block = False
                    removed_count += 1
                    print(f"[Auto-Funeral] 🚮 Buried {name}")
                continue
            else:
                # Keep this one
                skip_mode = False
                in_block = False

        if skip_mode:
            # We are inside a dead block, check if it ends
            paren_depth += line.count("(") - line.count(")")
            if paren_depth <= 0:
                # Block ended
                skip_mode = False
                in_block = False
                removed_count += 1
                if current_founder:
                    print(f"[Auto-Funeral] 🚮 Buried {current_founder}")
            continue

        # Not skipping, keep line
        new_lines.append(line)

    # Write back
    if removed_count > 0:
        with open(HUNTER_FILE, "w") as f:
            f.write("\n".join(new_lines) + "\n")
        print(
            f"[Auto-Funeral] ✅ Removed {removed_count} strategies from {HUNTER_FILE}"
        )

        # Clear graveyard
        with open(GRAVEYARD_FILE, "w") as f:
            f.write("")  # Clear file
    else:
        print(
            "[Auto-Funeral] No matching strategies found in hunter file (might be already gone or evolved ones). Clearing graveyard."
        )
        with open(GRAVEYARD_FILE, "w") as f:
            f.write("")


if __name__ == "__main__":
    main()
