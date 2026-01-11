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


def generate_lisp_code(clan, strategy_name):
    # Select template
    templates = get_templates(clan)
    if not templates:
        templates = get_templates("scalp")  # Default fallback
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
