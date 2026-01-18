#!/usr/bin/env python3
import json
import os
import re
import glob

# Configuration
STRATEGIES_DIR = "src/lisp/strategies"
OPTIMIZED_JSON = "optimized_timeframes.json"


def apply_patches():
    if not os.path.exists(OPTIMIZED_JSON):
        print("❌ optimized_timeframes.json not found.")
        return

    with open(OPTIMIZED_JSON, "r") as f:
        best_timeframes = json.load(f)

    print(f"🚀 Loaded {len(best_timeframes)} optimized timeframes.")

    # Scan all Lisp files
    lisp_files = glob.glob(f"{STRATEGIES_DIR}/*.lisp")

    updated_count = 0

    for lisp_file in lisp_files:
        with open(lisp_file, "r") as f:
            content = f.read()

        original_content = content

        # We look for (defstrategy "NAME" ... or (defstrategy name ...
        # But mostly strings: (defstrategy "Name"
        # Let's iterate keys and regex replace.
        # Note: Iterate keys derived from the file content might be faster if file is huge,
        # but here we iterate keys from JSON (80 items) vs regex on file.

        for strat_name, tf in best_timeframes.items():
            # Regex to find the definition of this strategy
            # Pattern: (defstrategy "NAME" ... )
            # We want to match the whole block to find :timeframe or insert it?
            # Actually we just need to find the specific defstrategy call.

            # Case 1: :timeframe already exists
            # Pattern: (defstrategy "NAME" ... :timeframe \d+ ...)
            # We use a lookahead to ensure we are inside the right defstrategy?
            # Hard with regex.
            # Simpler approach: Locate the start index of (defstrategy "NAME"
            # get the slice until closing parenthesis.
            # operate on that slice.

            # Method A: (defstrategy "NAME"
            pattern_def = f'\\(defstrategy "{strat_name}"'
            match = re.search(pattern_def, content)

            # Method B: (make-strategy :name "NAME"
            if not match:
                pattern_make = f'\\(make-strategy :name "{strat_name}"'
                match = re.search(pattern_make, content)

            if match:
                start_idx = match.start()
                # Find the matching closing parenthesis for this defstrategy
                # Simple counter
                open_parens = 0
                end_idx = -1
                for i in range(start_idx, len(content)):
                    if content[i] == "(":
                        open_parens += 1
                    elif content[i] == ")":
                        open_parens -= 1
                        if open_parens == 0:
                            end_idx = i
                            break

                if end_idx != -1:
                    strategy_block = content[start_idx : end_idx + 1]

                    # specific regex for :timeframe inside this block
                    tf_pattern = r"(:timeframe\s+)(\d+)"

                    if re.search(tf_pattern, strategy_block):
                        # Replace logic
                        new_block = re.sub(tf_pattern, f"\\g<1>{tf}", strategy_block)
                        if new_block != strategy_block:
                            print(f"   ✏️  Updated {strat_name} -> {tf}m")
                            # Replace in content
                            # We must be careful if strategy_block is not unique, but exact string match should trigger only once per location if we iterate carefully.
                            # But since we slice effectively, we can construct new content.
                            content = (
                                content[:start_idx] + new_block + content[end_idx + 1 :]
                            )
                            updated_count += 1
                    else:
                        # Insert logic
                        # Insert before the last parenthesis
                        # We want check if it has keyword args. Usually yes.
                        # Insert :timeframe X at the end
                        print(f"   ➕ Injected {strat_name} -> {tf}m")
                        new_block = strategy_block[:-1] + f" :timeframe {tf})"
                        content = (
                            content[:start_idx] + new_block + content[end_idx + 1 :]
                        )
                        updated_count += 1

        if content != original_content:
            with open(lisp_file, "w") as f:
                f.write(content)
            print(f"💾 Saved {lisp_file}")

    print(f"✅ Applied patches to {updated_count} strategies.")


if __name__ == "__main__":
    apply_patches()
