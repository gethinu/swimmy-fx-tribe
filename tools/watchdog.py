#!/usr/bin/env python3
"""
swimmy-watchdog.py - Liveness Monitor & Auto-Revival Service
=============================================================
V15.5: Fowler Refactoring - Separation of Responsibilities

This service is SOLELY responsible for:
1. Monitoring Brain liveness (via ZMQ HEARTBEAT)
2. Monitoring Guardian liveness (via systemd status)
3. Sending Discord alerts on failure
4. Triggering auto-revival (systemctl restart)
5. Loop prevention (2 deaths in 5 min = halt)

Guardian is now freed to focus on:
- Market data relay (MT5 <-> Brain)
- Risk Gate (order veto)
- Neural network inference

Architecture:
    [Brain] --(heartbeat)--> [Watchdog] --(alert)--> [Discord]
                               |
                               v
                         [systemctl restart]
"""

import zmq
import json
import time
import os
import subprocess
import requests
from datetime import datetime, timedelta

# Configuration
ZMQ_BRAIN_PORT = 5556  # Brain's PUB socket (we SUB to heartbeats)
BRAIN_TIMEOUT_SECS = 90  # Slightly longer than Guardian's internal check
GUARDIAN_CHECK_INTERVAL_SECS = 60
REVIVAL_WINDOW_SECS = 300  # 5 minutes
MAX_DEATHS_IN_WINDOW = 2

# Environment
WEBHOOK_URL = os.getenv("SWIMMY_DISCORD_ALERTS", "")


# State
class RevivalTracker:
    def __init__(self, service_name: str):
        self.service = service_name
        self.death_count = 0
        self.first_death_time = datetime.now()
        self.halted = False

    def record_death(self) -> bool:
        """Record a death, return True if revival should proceed, False if halted."""
        now = datetime.now()
        if (now - self.first_death_time) > timedelta(seconds=REVIVAL_WINDOW_SECS):
            # Window expired, reset
            self.death_count = 0
            self.first_death_time = now
            self.halted = False

        self.death_count += 1

        if self.death_count >= MAX_DEATHS_IN_WINDOW:
            self.halted = True
            return False
        return True


brain_tracker = RevivalTracker("swimmy-brain")
guardian_tracker = RevivalTracker("swimmy-guardian")


def send_discord_alert(
    title: str, description: str, color: int = 16711680, mention: bool = True
):
    """Send alert to Discord."""
    if not WEBHOOK_URL:
        print(f"[WATCHDOG] No webhook configured, skipping alert: {title}")
        return

    try:
        payload = {
            "embeds": [
                {
                    "title": title,
                    "description": description,
                    "color": color,
                    "timestamp": datetime.utcnow().isoformat(),
                }
            ]
        }
        if mention:
            payload["content"] = "@here"

        resp = requests.post(WEBHOOK_URL, json=payload, timeout=10)
        if resp.ok:
            print(f"[WATCHDOG] Alert sent: {title}")
        else:
            print(f"[WATCHDOG] Alert failed: {resp.status_code}")
    except Exception as e:
        print(f"[WATCHDOG] Discord error: {e}")


def restart_service(tracker: RevivalTracker) -> bool:
    """Attempt to restart a service. Returns True on success."""
    if tracker.halted:
        print(f"[WATCHDOG] Revival HALTED for {tracker.service} (too many deaths)")
        return False

    can_revive = tracker.record_death()
    if not can_revive:
        send_discord_alert(
            "🛑 AUTO-REVIVAL HALTED",
            f"{tracker.service} died {tracker.death_count} times in {REVIVAL_WINDOW_SECS}s. "
            "Manual intervention required.",
            color=16711680,
            mention=True,
        )
        return False

    print(
        f"[WATCHDOG] Restarting {tracker.service} (death {tracker.death_count}/{MAX_DEATHS_IN_WINDOW})..."
    )
    result = subprocess.run(
        ["systemctl", "--user", "restart", tracker.service],
        capture_output=True,
        text=True,
    )

    if result.returncode == 0:
        print(f"[WATCHDOG] ✅ {tracker.service} restart SUCCESS")
        return True
    else:
        print(f"[WATCHDOG] ❌ {tracker.service} restart FAILED: {result.stderr}")
        return False


def check_guardian_alive() -> bool:
    """Check if Guardian service is running."""
    result = subprocess.run(
        ["systemctl", "--user", "is-active", "--quiet", "swimmy-guardian"],
        capture_output=True,
    )
    return result.returncode == 0


def main():
    print("=" * 60)
    print("  🐕 SWIMMY WATCHDOG - Liveness Monitor (Fowler Edition)")
    print("  V15.5: Separation of Responsibilities")
    print("=" * 60)

    # Setup ZMQ to listen for Brain heartbeats
    context = zmq.Context()
    brain_sub = context.socket(zmq.SUB)
    brain_sub.connect(f"tcp://localhost:{ZMQ_BRAIN_PORT}")
    brain_sub.setsockopt_string(zmq.SUBSCRIBE, "")  # All messages
    brain_sub.setsockopt(zmq.RCVTIMEO, 1000)  # 1 second timeout

    print(f"[WATCHDOG] Subscribed to Brain (port {ZMQ_BRAIN_PORT})")
    print(f"[WATCHDOG] Brain timeout: {BRAIN_TIMEOUT_SECS}s")
    print(f"[WATCHDOG] Guardian check interval: {GUARDIAN_CHECK_INTERVAL_SECS}s")

    last_brain_msg = time.time()
    last_guardian_check = time.time()
    brain_dead_alerted = False

    while True:
        try:
            # 1. Check for Brain messages (non-blocking with timeout)
            try:
                msg = brain_sub.recv_string(flags=0)
                last_brain_msg = time.time()
                if brain_dead_alerted:
                    # Brain is back!
                    print("[WATCHDOG] 🧠 Brain signal restored!")
                    send_discord_alert(
                        "🧠 BRAIN REVIVED",
                        "Signal restored. Resume normal operation.",
                        color=65280,  # Green
                        mention=False,
                    )
                    brain_dead_alerted = False
                    brain_tracker.death_count = 0  # Reset on recovery
            except zmq.Again:
                pass  # No message, continue

            # 2. Check Brain timeout
            brain_silence = time.time() - last_brain_msg
            if (
                brain_silence > BRAIN_TIMEOUT_SECS
                and not brain_dead_alerted
                and not brain_tracker.halted
            ):
                print(f"[WATCHDOG] 💀 Brain silence detected ({brain_silence:.0f}s)")
                brain_dead_alerted = True

                send_discord_alert(
                    "💀 BRAIN DEAD DETECTED 💀",
                    f"Brain silence > {BRAIN_TIMEOUT_SECS}s. Engaging auto-revival.",
                    color=16711680,
                )

                restart_service(brain_tracker)

            # 3. Periodically check Guardian
            if time.time() - last_guardian_check > GUARDIAN_CHECK_INTERVAL_SECS:
                last_guardian_check = time.time()
                if not check_guardian_alive():
                    print("[WATCHDOG] 🛡️ Guardian is DOWN!")
                    send_discord_alert(
                        "🛡️ GUARDIAN DOWN",
                        "Guardian service failed. Attempting restart.",
                        color=16711680,
                    )
                    restart_service(guardian_tracker)

            time.sleep(1)

        except KeyboardInterrupt:
            print("\n[WATCHDOG] Shutting down...")
            break
        except Exception as e:
            print(f"[WATCHDOG] Error: {e}")
            time.sleep(5)

    brain_sub.close()
    context.term()


if __name__ == "__main__":
    main()
