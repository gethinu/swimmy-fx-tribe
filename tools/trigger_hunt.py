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

# Templates for each clan
TEMPLATES = {
    "scalp": [
        {
            "logic": "RSI + Bollinger Squeeze",
            "code": """
   :indicators '((rsi 14) (bb 20 2.0))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0)) ; Volatility Squeeze Breakout
   :exit '(> pnl tp)""",
        },
        {
            "logic": "MOMENTUM_SCALP",
            "code": """
   :indicators '((ema 9) (rsi 7))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))""",
        },
    ],
    "trend": [
        {
            "logic": "MACD_CROSS",
            "code": """
   :indicators '((macd 12 26 9))
   :entry '(> macd-main macd-signal)
   :exit '(< macd-main macd-signal)""",
        }
    ],
    "reversion": [
        {
            "logic": "RSI_OVERSOLD",
            "code": """
   :indicators '((rsi 14))
   :entry '(< rsi 30)
   :exit '(> rsi 50)""",
        }
    ],
    "breakout": [
        {
            "logic": "DONCHIAN_BREAK",
            "code": """
   :indicators '((donchian 20))
   :entry '(> close donchian-upper)
   :exit '(< close donchian-mid)""",
        }
    ],
}

TARGET_FILE = "/home/swimmy/swimmy/src/lisp/school-hunter.lisp"


def generate_lisp_code(clan, strategy_name):
    # Select template
    templates = TEMPLATES.get(clan, TEMPLATES["scalp"])  # Default to scalp if unknown
    template = random.choice(templates)

    lisp_code = f"""
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: {strategy_name}
;;; Logic: {template['logic']} (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :{strategy_name.lower()} "{strategy_name}"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "{strategy_name}"
   :category :{clan}
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   {template['code']}
   :exit '(or (> pnl tp) (< pnl (- sl)))))
"""
    return lisp_code


def inject_code(code):
    with open(TARGET_FILE, "a") as f:
        f.write(code)
    print(f"✅ Injected code into {TARGET_FILE}")


def main():
    if len(sys.argv) < 2:
        print("Usage: trigger_hunt.py <clan>")
        sys.exit(1)

    clan = sys.argv[1].lower()
    timestamp = int(time.time())
    strategy_name = f"Auto-{clan.capitalize()}-{timestamp}"

    print(f"🏹 Triggering Hunt for Clan: {clan}...")

    # 1. Generate
    code = generate_lisp_code(clan, strategy_name)

    # 2. Inject
    inject_code(code)

    # 3. Notify Brain (via ZMQ? Or just let file watcher handle it?)
    # For now, we rely on the user or system restart.
    # Ideally, we send a RELOAD command.
    # print(json.dumps({"type": "SYSTEM_COMMAND", "action": "RELOAD_STRATEGIES"}))

    print(f"✅ Strategy {strategy_name} created. Waiting for pickup.")


if __name__ == "__main__":
    main()
