#!/usr/bin/env python3
"""
trigger_hunt.py
===============
Role: Automated Strategy Generator (The "Agent")
Called by: strategy_hunter.py (when shortage detected)
Action: Generates a new strategy logic based on Clan and injects it.

Note: In a full production env, this would call an LLM API.
For now, it uses "Reserve Templates" to simulate generation.
"""

import sys
import random
import time
import os
import cleanup_hunter
import argparse  # Added for argparsing futureproof


# Templates for each clan
# Templates for each clan (Dynamic Generators)
def get_templates(clan):
    if clan == "scalp":
        p_rsi = random.randint(12, 16)
        p_bb = random.randint(18, 22)
        p_bb_dev = round(random.uniform(1.8, 2.2), 1)
        return [
            {
                "logic": f"RSI({p_rsi}) + Bollinger({p_bb}, {p_bb_dev}) Squeeze",
                "code": f"""
   :indicators '((rsi {p_rsi}) (bb {p_bb} {p_bb_dev}))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)""",
            },
            {
                "logic": f"MOMENTUM_SCALP_EMA({random.randint(8,10)})_RSI({random.randint(6,8)})",
                "code": f"""
   :indicators '((ema {random.randint(8,10)}) (rsi {random.randint(6,8)}))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))""",
            },
        ]
    elif clan == "trend":
        p_fast = random.randint(10, 14)
        p_slow = random.randint(24, 28)
        p_sig = random.randint(8, 10)
        return [
            {
                "logic": f"MACD_CROSS({p_fast},{p_slow},{p_sig})",
                "code": f"""
   :indicators '((macd {p_fast} {p_slow} {p_sig}))
   :entry '(> macd-main macd-signal)
   :exit '(< macd-main macd-signal)""",
            }
        ]
    elif clan == "reversion":
        p_rsi = random.randint(12, 16)
        return [
            {
                "logic": f"RSI_OVERSOLD({p_rsi})",
                "code": f"""
   :indicators '((rsi {p_rsi}))
   :entry '(< rsi 30)
   :exit '(> rsi 50)""",
            }
        ]
    elif clan == "breakout":
        p_don = random.randint(18, 25)
        return [
            {
                "logic": f"DONCHIAN_BREAK({p_don})",
                "code": f"""
   :indicators '((donchian {p_don}))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)""",
            }
        ]
    return []


TARGET_FILE = "/home/swimmy/swimmy/src/lisp/school-hunter.lisp"
TEMPLATE_FILE = "/home/swimmy/swimmy/src/lisp/templates/founder.lisp.template"


def generate_lisp_code(clan, base_timestamp):
    # Select template logic
    templates = get_templates(clan)
    if not templates:
        templates = get_templates("scalp")  # Default fallback
    template_data = random.choice(templates)

    # Derive descriptive name from logic
    # Clean up logic string to be safe for symbol name (Remove () +, spaces)
    raw_logic = template_data["logic"]
    safe_logic = (
        raw_logic.replace("(", "")
        .replace(")", "")
        .replace(" ", "")
        .replace("+", "-")
        .replace(",", "-")
        .replace("_", "-")
    )

    # Format: Clan-Logic-Gen0-Timestamp
    # e.g., Scalp-RSI14-Bollinger20-Gen0-1768...
    # Truncate length if too long
    if len(safe_logic) > 30:
        safe_logic = safe_logic[:30]

    strategy_name = f"{clan.capitalize()}-{safe_logic}-Gen0-{base_timestamp}"

    # Load Lisp Template
    if not os.path.exists(TEMPLATE_FILE):
        print(f"❌ Template file not found: {TEMPLATE_FILE}")
        sys.exit(1)

    with open(TEMPLATE_FILE, "r") as f:
        template_str = f.read()

    # Substitute values
    lisp_code = template_str.replace("{{STRATEGY_NAME}}", strategy_name)
    lisp_code = lisp_code.replace("{{STRATEGY_KEY}}", strategy_name.lower())
    lisp_code = lisp_code.replace("{{CLAN}}", clan)
    lisp_code = lisp_code.replace("{{LOGIC_DESC}}", template_data["logic"])

    # Handle the body injection
    body_code = template_data["code"]

    # 1. Remove :exit from template if body has it (Validation)
    if ":exit" in body_code:
        # Prevent default exit from causing syntax error if template has it
        pass

    lisp_code = lisp_code.replace("{{INDICATORS_AND_ENTRY}}", body_code)

    return lisp_code, strategy_name


def inject_code(code):
    with open(TARGET_FILE, "a") as f:
        f.write(code)
    print(f"✅ Injected code into {TARGET_FILE}")


def main():
    # Naval Ravikant's Auto-Funeral: Clean dead strategies first
    print("[Agent] 🧹 Running Auto-Funeral cleanup...")
    cleanup_hunter.main()

    if len(sys.argv) < 2:
        print("Usage: trigger_hunt.py <clan>")
        sys.exit(1)

    clan = sys.argv[1].lower()

    # Use readable timestamp (YYMMDDHHMM)
    # e.g. 2601112028 (Year-Month-Day-Hour-Minute)
    # Short year to keep it compact but readable.
    from datetime import datetime

    timestamp = datetime.now().strftime("%y%m%d%H%M")

    print(f"🏹 Triggering Hunt for Clan: {clan}...")

    # 1. Generate (returns code AND name)
    code, strategy_name = generate_lisp_code(clan, timestamp)

    # 2. Inject
    inject_code(code)

    # 3. Notify Brain (via ZMQ? Or just let file watcher handle it?)
    # For now, we rely on the user or system restart.
    # Ideally, we send a RELOAD command.
    # print(json.dumps({"type": "SYSTEM_COMMAND", "action": "RELOAD_STRATEGIES"}))

    print(f"✅ Strategy {strategy_name} created. Waiting for pickup.")


if __name__ == "__main__":
    main()
