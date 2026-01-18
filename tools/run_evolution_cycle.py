#!/usr/bin/env python3
import os
import shutil
import sys
import subprocess


def run_cycle():
    print("♾️  STARTING EVOLUTION CYCLE (Super Soldier Program) ♾️")

    # 1. Optimize
    print("\n[Phase 1] Running Evolutionary Optimization...")
    ret = os.system("./tools/optimize_strategies.py")
    if ret != 0:
        print("❌ Optimization failed.")
        sys.exit(1)

    # 2. Patch & Generate
    print("\n[Phase 2] Applying Optimized Genes (Code-as-Data)...")
    if not os.path.exists("strategies.json"):
        print("❌ strategies.json missing.")
        sys.exit(1)

    # Generate Lisp Code (Hickey's requirement)
    ret = os.system("./tools/generate_lisp_params.py")
    if ret != 0:
        print("❌ Lisp Generation failed.")
        sys.exit(1)

    # Backup original
    shutil.copy("strategies.json", "strategies.backup.json")

    # Overwrite for Qualification Test
    if os.path.exists("strategies_optimized.json"):
        shutil.move("strategies_optimized.json", "strategies.json")
        print("✅ strategies.json updated for WFV Test.")
    else:
        print("❌ strategies_optimized.json not found.")
        sys.exit(1)

    # 3. Qualify (WFV Test)
    print("\n[Phase 3] Running WFV Qualification (Test: 2024-2026)...")
    ret = os.system("./tools/run_qualification.py")
    if ret != 0:
        print("❌ Qualification failed.")
        sys.exit(1)

    # 4. Purge (The Wisdom Engine)
    print("\n[Phase 4] Running The Purge (Up or Out)...")
    ret = os.system("./tools/purge_strategies.py")
    if ret != 0:
        print("❌ Purge failed (but cycle continues).")
        # Do not exit, purge failure is not fatal

    print("\n🎉 CYCLE COMPLETE. Check data/graveyard.json for Wisdom.")
    print(
        "⚠️  NOTE: Lisp source code is NOT yet updated. Only the runtime DB and qualification JSON are updated."
    )
    print(
        "    To make this permanent in code, we need to apply the params to .lisp files."
    )


if __name__ == "__main__":
    run_cycle()
