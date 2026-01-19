#!/usr/bin/env python3
import os
import json
import json
import json
import shutil
import sys
import subprocess


def run_command(cmd, description):
    print(f"\n{description}")
    # Use sys.executable to ensure we use the same Python interpreter (and permissions)
    # cmd is a list of [script_path, args...]
    full_cmd = [sys.executable] + cmd
    try:
        result = subprocess.run(full_cmd, check=True, text=True, capture_output=False)
        return True
    except subprocess.CalledProcessError as e:
        print(f"❌ {cmd[0]} failed (RC={e.returncode}).")
        return False
    except Exception as e:
        print(f"❌ Execution error: {e}")
        return False


def run_cycle():
    print("♾️  STARTING EVOLUTION CYCLE (Super Soldier Program) ♾️")

    # 1. Optimize
    if not run_command(
        ["tools/optimize_strategies.py"],
        "[Phase 1] Running Evolutionary Optimization...",
    ):
        sys.exit(1)

    # 2. Patch & Generate
    print("\n[Phase 2] Applying Optimized Genes (Code-as-Data)...")
    if not os.path.exists("strategies.json"):
        print("❌ strategies.json missing.")
        # sys.exit(1) # Don't exit, might be first run

    # Generate Lisp Code (Hickey's requirement)
    if not run_command(
        ["tools/generate_lisp_params.py"], "Generating Lisp Parameters..."
    ):
        sys.exit(1)

    # Backup original
    if os.path.exists("strategies.json"):
        shutil.copy("strategies.json", "strategies.backup.json")

    # Overwrite for Qualification Test
    if os.path.exists("strategies_optimized.json"):
        shutil.move("strategies_optimized.json", "strategies.json")
        print("✅ strategies.json updated for WFV Test.")
    else:
        print("❌ strategies_optimized.json not found.")
        # sys.exit(1)

    # 3. Qualify (WFV Test)
    if not run_command(
        ["tools/run_qualification.py"],
        "[Phase 3] Running WFV Qualification (Test: 2024-2026)...",
    ):
        sys.exit(1)

    # 4. Purge (The Wisdom Engine)
    if not run_command(
        ["tools/purge_strategies.py"], "[Phase 4] Running The Purge (Up or Out)..."
    ):
        pass  # Non-fatal

    # 5. Recruit (The Missing Link)
    print("\n[Phase 5] Recruitment (The Wisdom Engine)...")
    # recruit_elite.py returns 0 on success (recruit found), 1 on failure (no recruit found)
    # We don't want to exit on failure, just log it.
    try:
        subprocess.run(
            [sys.executable, "tools/recruit_elite.py"],
            check=False,  # Don't throw error if RC != 0 (it returns 1 if no recruit found)
        )
    except Exception as e:
        print(f"❌ Recruitment error: {e}")

    # 6. Breeding (The Evolution Engine) - Phase 12 Fix
    # This connects the missing link, allowing Gen 0 Recruits to become Gen 1 Parents.
    print("\n[Phase 6] Evolution (Breeding & Selection)...")
    try:
        # Use simple subprocess call to invoke sbcl
        subprocess.run(
            ["sbcl", "--noinform", "--load", "tools/run_lisp_breeding.lisp"],
            check=False,
        )
    except Exception as e:
        print(f"❌ Breeding error: {e}")

    # 7. Wisdom Update (Civilization Handover)
    # Automatically extract wisdom every 50 recruits
    try:
        WISDOM_STATE = "data/wisdom_state.json"
        LISP_DYNAMIC = "src/lisp/strategies/strategies-dynamic.lisp"

        # Count current recruits
        if os.path.exists(LISP_DYNAMIC):
            with open(LISP_DYNAMIC, "r") as f:
                content = f.read()
            current_count = content.count("(defstrategy ")

            last_count = 0
            if os.path.exists(WISDOM_STATE):
                with open(WISDOM_STATE, "r") as f:
                    state = json.load(f)
                    last_count = state.get("last_count", 0)

            diff = current_count - last_count
            trigger_threshold = 50

            if diff >= trigger_threshold:
                print(f"\n[Phase 7] Wisdom Update Triggered (+{diff} recruits)...")
                if run_command(["tools/extract_wisdom.py"], "Extracting Wisdom..."):
                    if run_command(
                        ["tools/generate_lisp_params.py"], "Regenerating Genes..."
                    ):
                        # Update state
                        with open(WISDOM_STATE, "w") as f:
                            json.dump({"last_count": current_count}, f)
                        print("✅ Civilization Level Up!")
            else:
                print(
                    f"\n[Phase 7] Wisdom Accumulating... (+{diff}/{trigger_threshold})"
                )

    except Exception as e:
        print(f"⚠️ Wisdom check failed: {e}")

    # 7. Wisdom Update (Civilization Handover)
    # Automatically extract wisdom every 50 recruits
    try:
        WISDOM_STATE = "data/wisdom_state.json"
        LISP_DYNAMIC = "src/lisp/strategies/strategies-dynamic.lisp"

        # Count current recruits
        if os.path.exists(LISP_DYNAMIC):
            with open(LISP_DYNAMIC, "r") as f:
                content = f.read()
            current_count = content.count("(defstrategy ")

            last_count = 0
            if os.path.exists(WISDOM_STATE):
                with open(WISDOM_STATE, "r") as f:
                    state = json.load(f)
                    last_count = state.get("last_count", 0)

            diff = current_count - last_count
            trigger_threshold = 50

            if diff >= trigger_threshold:
                print(f"\n[Phase 7] Wisdom Update Triggered (+{diff} recruits)...")
                if run_command(["tools/extract_wisdom.py"], "Extracting Wisdom..."):
                    if run_command(
                        ["tools/generate_lisp_params.py"], "Regenerating Genes..."
                    ):
                        # Update state
                        with open(WISDOM_STATE, "w") as f:
                            json.dump({"last_count": current_count}, f)
                        print("✅ Civilization Level Up!")
            else:
                print(
                    f"\n[Phase 7] Wisdom Accumulating... (+{diff}/{trigger_threshold})"
                )

    except Exception as e:
        print(f"⚠️ Wisdom check failed: {e}")

    print("\n🎉 CYCLE COMPLETE. Check data/graveyard.json for Wisdom.")
    print(
        "⚠️  NOTE: Lisp source code is NOT yet updated. Only the runtime DB and qualification JSON are updated."
    )
    print(
        "    To make this permanent in code, we need to apply the params to .lisp files."
    )


if __name__ == "__main__":
    run_cycle()
