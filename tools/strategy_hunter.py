#!/usr/bin/env python3
"""
strategy_hunter.py
==================
Role: Active Immigration Officer (Hunter)
Compliance: Article 5 (Service Resilience) - V9.2 (ZMQ Pub/Sub)

Migrated from Log Watchdog to ZMQ Subscriber (Expert Panel Recommendation).
Connects to Brain PUB socket (Port 5556) to receive real-time alerts.
"""

import time
import sys
import zmq
import json
import os

try:
    import requests

    HAS_REQUESTS = True
except ImportError:
    HAS_REQUESTS = False

# === REQUIRED CONSTANTS (Article 5.2) ===
MAX_CONSECUTIVE_FAILURES = 5
APEX_WEBHOOK = "https://discord.com/api/webhooks/1325656360435454024/7S8Z6FGF_SIT3QPKP_HKVFEVUS_Z5HU1IJN_C85A32ZH_MOYML5G0_IWRX9C3M_JILWH3NT"
BRAIN_PUB_PORT = 5556


# === REQUIRED FUNCTION (Article 5.2, 5.3) ===
def send_discord_alert(message: str, is_error: bool = True):
    if not HAS_REQUESTS:
        print(f"[ALERT] {message}")
        return
    try:
        color = 15158332 if is_error else 3066993  # Red or Green
        payload = {
            "embeds": [
                {"title": "🏹 Strategy Hunter", "description": message, "color": color}
            ]
        }
        requests.post(APEX_WEBHOOK, json=payload, timeout=5)
    except Exception as e:
        print(f"[ALERT FAILED] {e}")


# === REQUIRED MAIN LOOP STRUCTURE (Article 5.4) ===
def main():
    # Startup notification (Article 5.6)
    send_discord_alert("✅ Hunter Service Started (ZMQ Mode)", is_error=False)
    print(f"🏹 Hunter Service Connecting to SUB:{BRAIN_PUB_PORT}...")

    consecutive_failures = 0
    alert_sent = False

    context = zmq.Context()
    socket = context.socket(zmq.SUB)

    # Subscribe to all topics (empty string), or refine if Brain prefixes topics
    socket.subscribe("")

    try:
        socket.connect(f"tcp://localhost:{BRAIN_PUB_PORT}")
        print("✅ ZMQ Connected.")

        while True:
            try:
                # Receive JSON message
                # V41.7 Brain sends straight JSON strings
                msg_str = socket.recv_string()

                try:
                    data = json.loads(msg_str)
                except json.JSONDecodeError:
                    continue  # Ignore non-JSON messages (like raw Heartbeats if any)

                # Reset on success (Article 5.3)
                if consecutive_failures > 0:
                    if alert_sent:
                        send_discord_alert(
                            "✅ Hunter Service Recovered", is_error=False
                        )
                    consecutive_failures = 0
                    alert_sent = False

                # === HUNTER LOGIC ===
                msg_type = data.get("type", "")

                # 1. IMMIGRATION_SHORTAGE (From school-founders.lisp)
                if msg_type == "IMMIGRATION_SHORTAGE":
                    clan = data.get("clan", "UNKNOWN")
                    shortage = data.get("shortage", 0)
                    alert_msg = f"🚨 **SHORTAGE DETECTED**: {clan.upper()} (Deficit: {shortage:.1%})\nTriggering Hunter Protocol..."
                    print(f"[HUNTER] {alert_msg}")
                    send_discord_alert(alert_msg, is_error=True)
                    # Future: call automated hunter script here

                # 2. HEADHUNTER RECRUITMENT (Optional: if we publish Recruited events)
                if msg_type == "FOUNDER_RECRUITED":
                    founder_name = data.get("name", "Unknown")
                    send_discord_alert(
                        f"🕵️ **New Founder Recruited**: `{founder_name}`", is_error=False
                    )

            except zmq.ZMQError as e:
                consecutive_failures += 1
                print(f"❌ ZMQ Error: {e}")
                time.sleep(1)  # Backoff
            except Exception as e:
                consecutive_failures += 1
                print(f"❌ Logic Error: {e}")
                if consecutive_failures >= MAX_CONSECUTIVE_FAILURES and not alert_sent:
                    send_discord_alert(f"🚨 Hunter Logic Error: {e}")
                    alert_sent = True
                    time.sleep(5)

    except KeyboardInterrupt:
        send_discord_alert("🛑 Hunter Service Stopped", is_error=False)
    except Exception as e:
        send_discord_alert(f"💥 Hunter Service CRASHED: {e}")
        raise
    finally:
        socket.close()
        context.term()


if __name__ == "__main__":
    main()
