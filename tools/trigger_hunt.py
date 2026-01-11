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
TEMPLATE_FILE = "/home/swimmy/swimmy/src/lisp/templates/founder.lisp.template"


def generate_lisp_code(clan, strategy_name):
    # Select template logic
    templates = get_templates(clan)
    if not templates:
        templates = get_templates("scalp")  # Default fallback
    template_data = random.choice(templates)

    # Load Lisp Template
    if not os.path.exists(TEMPLATE_FILE):
        print(f"❌ Template file not found: {TEMPLATE_FILE}")
        sys.exit(1)

    with open(TEMPLATE_FILE, "r") as f:
        template_str = f.read()

    # Substitute values
    # Note: Template has :exit, so we don't need to append it.
    # But wait, the template file DOES include :exit in its definition.
    # The `template_data['code']` from `get_templates` ALSO includes :exit logic often?
    # Let's clean up get_templates to ONLY provide indicators and entry/exit logic,
    # NOT the full :indicators ... wrapper if the template does it.
    # Actually, the template expects {{INDICATORS_AND_ENTRY}}.
    # The `code` in `get_templates` provides indented lines starting with :indicators, :entry, :exit.
    # This matches {{INDICATORS_AND_ENTRY}}.

    # We need to remove the Hardcoded :exit in the Template File if the dynamic code provides it.
    # OR we fix `get_templates` to NOT provide :exit, and let the Template File provide it.
    # For now, let's assume `get_templates` provides the full body content.

    # Clean up the template placeholder
    lisp_code = template_str.replace("{{STRATEGY_NAME}}", strategy_name)
    lisp_code = template_str.replace(
        "{{STRATEGY_NAME}}", strategy_name
    )  # Replace all occurrences
    lisp_code = lisp_code.replace("{{STRATEGY_KEY}}", strategy_name.lower())
    lisp_code = lisp_code.replace("{{STRATEGY_NAME}}", strategy_name)
    lisp_code = lisp_code.replace("{{CLAN}}", clan)
    lisp_code = lisp_code.replace("{{LOGIC_DESC}}", template_data["logic"])

    # Handle the body injection
    # The template file has a default :exit.
    # The `code` in `get_templates` often has :exit. This causes duplication/syntax error.
    # We will strip the default :exit from the template file if we see :exit in the injected code.

    body_code = template_data["code"]

    # Simple heuristic: If body has :exit, allow it (and remove default from template if possible? No, simply use string replace)
    # Actually, let's just REPLACE the placeholder.
    # But the template file has `:exit ...` AFTER the placeholder.
    # If body code has `exit`, we end up with two exits.

    # Uncle Bob says: Separation of Concerns.
    # The Template should define structure. The Logic defines... Logic.
    # Let's use the Logic's exit if present.
    # Since I cannot easily modify the template file dynamically, I will just accept the duplication for now?
    # No, duplication breaks Lisp usually (or ignores one).
    # Better: The template file SHOULD NOT have default :exit if the logic aims to be flexible.
    # I will modify the template file to remove the default exit, OR I will rely on logic to ALWAYS provide exit.
    # Looking at `get_templates`, ALL have :exit.
    # So I will remove the default :exit from `founder.lisp.template` in the next step or assume I did it.
    # Wait, I wrote `founder.lisp.template` WITH default exit in previous step.
    # I will blindly replace `{{INDICATORS_AND_ENTRY}}` and if there are two exits, so be it?
    # No, I should fix the logic strings in `get_templates` to NOT have :exit?
    # OR I fix the template file?

    # DECISION: I will strip `:exit ...` from the template file string in Python before substituting if the body has it.
    if ":exit" in body_code:
        # Prevent duplicate :exit keys, but PRESERVE the closing parentheses for make-strategy/def-founder
        lisp_code = lisp_code.replace(":exit '(or (> pnl tp) (< pnl (- sl)))))", "))")

    lisp_code = lisp_code.replace("{{INDICATORS_AND_ENTRY}}", body_code)

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
