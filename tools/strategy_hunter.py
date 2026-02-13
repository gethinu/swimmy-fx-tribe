#!/usr/bin/env python3
"""
strategy_hunter.py
==================
Role: Active Immigration Officer (Hunter)
Compliance: Article 5 (Service Resilience) - V9.2 (ZMQ Pub/Sub)

Connects to Brain PUB socket (default port 5556) to receive real-time alerts.

SECURITY:
  - Do NOT hardcode Discord webhook URLs in this repo (tokens are embedded in the URL).
  - Read webhook from environment variables instead.
"""

from __future__ import annotations

import json
import os
import time

import zmq

try:
    import requests

    HAS_REQUESTS = True
except ImportError:  # pragma: no cover
    HAS_REQUESTS = False


MAX_CONSECUTIVE_FAILURES = 5
BRAIN_PUB_PORT = int(os.getenv("SWIMMY_PORT_BRAIN_PUB", "5556") or "5556")


def _get_alerts_webhook() -> str:
    # Prefer consolidated env names from config/.env.template.
    for key in (
        "SWIMMY_DISCORD_ALERTS",
        "SWIMMY_DISCORD_APEX",
        "SWIMMY_DISCORD_WEBHOOK",
    ):
        value = os.getenv(key, "").strip()
        if value:
            return value
    return ""


def send_discord_alert(message: str, *, is_error: bool = True) -> None:
    if not HAS_REQUESTS:
        print(f"[ALERT] {message}", flush=True)
        return

    webhook = _get_alerts_webhook()
    if not webhook:
        print(f"[ALERT] {message}", flush=True)
        return

    try:
        color = 15158332 if is_error else 3066993  # Red or Green
        payload = {"embeds": [{"title": "Strategy Hunter", "description": message, "color": color}]}
        requests.post(webhook, json=payload, timeout=5)
    except Exception as exc:
        # Never print webhook URLs (they contain tokens).
        print(f"[ALERT FAILED] {type(exc).__name__}", flush=True)


def main() -> None:
    send_discord_alert("Hunter Service Started (ZMQ Mode)", is_error=False)
    print(f"[HUNTER] Connecting to SUB:{BRAIN_PUB_PORT}...", flush=True)

    consecutive_failures = 0
    alert_sent = False

    context = zmq.Context()
    socket = context.socket(zmq.SUB)
    socket.subscribe("")  # subscribe to all topics

    try:
        socket.connect(f"tcp://localhost:{BRAIN_PUB_PORT}")
        print("[HUNTER] ZMQ Connected.", flush=True)

        while True:
            try:
                msg_str = socket.recv_string()
                try:
                    data = json.loads(msg_str)
                except json.JSONDecodeError:
                    continue

                if consecutive_failures > 0:
                    if alert_sent:
                        send_discord_alert("Hunter Service Recovered", is_error=False)
                    consecutive_failures = 0
                    alert_sent = False

                msg_type = str(data.get("type", "") or "")

                if msg_type == "IMMIGRATION_SHORTAGE":
                    clan = data.get("clan", "UNKNOWN")
                    shortage = data.get("shortage", 0)
                    alert_msg = (
                        f"SHORTAGE DETECTED: {str(clan).upper()} "
                        f"(Deficit: {shortage:.1%}) | Triggering Hunter Protocol..."
                    )
                    print(f"[HUNTER] {alert_msg}", flush=True)
                    send_discord_alert(alert_msg, is_error=True)

                if msg_type == "FOUNDER_RECRUITED":
                    founder_name = data.get("name", "Unknown")
                    send_discord_alert(f"New Founder Recruited: {founder_name}", is_error=False)

            except zmq.ZMQError as exc:
                consecutive_failures += 1
                print(f"[HUNTER] ZMQ Error: {exc}", flush=True)
                time.sleep(1)
            except Exception as exc:
                consecutive_failures += 1
                print(f"[HUNTER] Logic Error: {exc}", flush=True)
                if consecutive_failures >= MAX_CONSECUTIVE_FAILURES and not alert_sent:
                    send_discord_alert(f"Hunter Logic Error: {type(exc).__name__}")
                    alert_sent = True
                    time.sleep(5)

    except KeyboardInterrupt:
        send_discord_alert("Hunter Service Stopped", is_error=False)
    except Exception as exc:
        send_discord_alert(f"Hunter Service CRASHED: {type(exc).__name__}")
        raise
    finally:
        socket.close()
        context.term()


if __name__ == "__main__":
    main()

