#!/usr/bin/env python3
# === REQUIRED IMPORTS ===
import time
import sys
import subprocess
import os

try:
    import requests

    HAS_REQUESTS = True
except ImportError:
    HAS_REQUESTS = False

# === REQUIRED CONSTANTS (Section 5.2) ===
MAX_CONSECUTIVE_FAILURES = 5
# Using the webhook from the workflow template (or should I use environment variable? Template has hardcoded example, but better to use the one from config if possible, but sticking to hardcoded/env logic for robustness).
# Ideally, we read from .env or config. But the workflow template example used a string.
# I'll use the SWIMMY_DISCORD_WEBHOOK_ALERTS if available, or fall back to a known one if I had it, but I don't have the explicit URL handy in the prompt other than the example in the workflow description which might be truncated.
# I'll assume I should use os.getenv("SWIMMY_DISCORD_WEBHOOK_ALERTS") which is standard in this codebase.
APEX_WEBHOOK = os.getenv("SWIMMY_DISCORD_WEBHOOK_ALERTS")


# === REQUIRED FUNCTION (Section 5.2, 5.3) ===
def send_discord_alert(message: str, is_error: bool = True):
    if not HAS_REQUESTS or not APEX_WEBHOOK:
        print(f"[ALERT] {message}")
        return
    try:
        color = 15158332 if is_error else 3066993
        payload = {
            "embeds": [
                {"title": "🧬 Evolution Daemon", "description": message, "color": color}
            ]
        }
        requests.post(APEX_WEBHOOK, json=payload, timeout=5)
    except Exception as e:
        print(f"[ALERT FAILED] {e}")


# === REQUIRED MAIN LOOP STRUCTURE (Section 5.4) ===
def main():
    # Startup notification (Section 5.6)
    send_discord_alert("✅ Evolution Daemon Started (Hyper-Time)", is_error=False)

    consecutive_failures = 0
    alert_sent = False

    # Ensure tool execution permissions
    os.system("chmod +x tools/run_evolution_cycle.py")

    try:
        while True:
            try:
                # === YOUR MAIN LOGIC HERE ===
                print("🔄 Starting Evolution Cycle...")
                start_time = time.time()

                # Run the evolution cycle script
                result = subprocess.run(
                    ["./tools/run_evolution_cycle.py"], capture_output=True, text=True
                )

                duration = time.time() - start_time

                if result.returncode != 0:
                    raise Exception(
                        f"Cycle failed (RC={result.returncode}): {result.stderr[-200:]}"
                    )  # Capture last 200 chars of error

                print(f"✅ Cycle Complete ({duration:.1f}s)")

                # Reset on success (Section 5.3)
                if consecutive_failures > 0 and alert_sent:
                    send_discord_alert("✅ Evolution Daemon Recovered", is_error=False)
                consecutive_failures = 0
                alert_sent = False

                # Sleep briefly (1s as per Hyper-Time spec)
                time.sleep(1)

            except Exception as e:
                consecutive_failures += 1
                print(f"❌ Error: {e}")

                # Alert after threshold (Section 5.2)
                if consecutive_failures >= MAX_CONSECUTIVE_FAILURES and not alert_sent:
                    send_discord_alert(f"🚨 Evolution Daemon Error: {e}")
                    alert_sent = True

                time.sleep(10)  # Backoff

    except KeyboardInterrupt:
        send_discord_alert("🛑 Evolution Daemon Stopped", is_error=False)
    except Exception as e:
        send_discord_alert(f"💥 Evolution Daemon CRASHED: {e}")
        raise


if __name__ == "__main__":
    main()
