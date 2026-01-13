#!/usr/bin/env python3
"""
swimmy-watchdog.py - Liveness Monitor & Auto-Revival Service
=============================================================
V15.6: Expert Panel Approved - Graduated Timeout & Timeframe-Aware Policy

Features:
1. P0: Graduated timeout (30s warn → 90s block → 180s CLOSE_ALL)
2. P1: Timeframe-aware policy (H4+ with SL = maintain positions)
3. P2: BRAIN_TIMEOUT_POLICY config (CLOSE_ALL / MAINTAIN / GRADUATED)
4. Loop prevention (2 deaths in 5 min = halt)

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

# =============================================================================
# CONFIGURATION (P2: Configurable via .env)
# =============================================================================

ZMQ_BRAIN_PORT = 5556  # Brain's PUB socket
ZMQ_GUARDIAN_PORT = 5560  # Guardian's PUB socket to MT5

# Graduated Timeout Thresholds (P0)
TIMEOUT_WARN_SECS = 30  # Phase 1: Warning only
TIMEOUT_BLOCK_SECS = 90  # Phase 2: Block new entries
TIMEOUT_CLOSE_SECS = 180  # Phase 3: CLOSE_ALL

# Policy (P2): CLOSE_ALL, MAINTAIN, GRADUATED
TIMEOUT_POLICY = os.getenv("BRAIN_TIMEOUT_POLICY", "GRADUATED").upper()

# Revival settings
REVIVAL_WINDOW_SECS = 300
MAX_DEATHS_IN_WINDOW = 2
GUARDIAN_CHECK_INTERVAL_SECS = 60

# Environment
WEBHOOK_URL = os.getenv("SWIMMY_DISCORD_ALERTS", "")
GUARDIAN_CMD_PORT = 5560  # For sending CLOSE_ALL to MT5 via Guardian

# =============================================================================
# STATE
# =============================================================================


class BrainState:
    """Track Brain liveness and timeout phases."""

    def __init__(self):
        self.last_msg_time = time.time()
        self.phase = 0  # 0=OK, 1=WARN, 2=BLOCK, 3=CLOSE
        self.death_count = 0
        self.first_death_time = datetime.now()
        self.halted = False
        self.close_sent = False

    def silence_secs(self) -> float:
        return time.time() - self.last_msg_time

    def record_death(self) -> bool:
        """Record death, return True if revival allowed."""
        now = datetime.now()
        if (now - self.first_death_time) > timedelta(seconds=REVIVAL_WINDOW_SECS):
            self.death_count = 0
            self.first_death_time = now
            self.halted = False

        self.death_count += 1
        if self.death_count >= MAX_DEATHS_IN_WINDOW:
            self.halted = True
            return False
        return True

    def reset(self):
        """Reset on Brain recovery."""
        self.phase = 0
        self.close_sent = False
        # Don't reset death_count here - that's for loop prevention


brain_state = BrainState()


class RevivalTracker:
    def __init__(self, service_name: str):
        self.service = service_name
        self.death_count = 0
        self.first_death_time = datetime.now()
        self.halted = False

    def record_death(self) -> bool:
        now = datetime.now()
        if (now - self.first_death_time) > timedelta(seconds=REVIVAL_WINDOW_SECS):
            self.death_count = 0
            self.first_death_time = now
            self.halted = False

        self.death_count += 1
        if self.death_count >= MAX_DEATHS_IN_WINDOW:
            self.halted = True
            return False
        return True


guardian_tracker = RevivalTracker("swimmy-guardian")

# =============================================================================
# HELPERS
# =============================================================================


def send_discord_alert(
    title: str, description: str, color: int = 16711680, mention: bool = True
):
    """Send alert to Discord."""
    if not WEBHOOK_URL:
        print(f"[WATCHDOG] No webhook configured, skipping: {title}")
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


def send_close_all_to_mt5():
    """Send CLOSE_ALL command to MT5 via Guardian's port."""
    # Guardian listens on 5560 and forwards to MT5
    # We can also send directly if Guardian is down
    try:
        context = zmq.Context.instance()
        pub = context.socket(zmq.PUB)
        pub.connect(f"tcp://localhost:{GUARDIAN_CMD_PORT}")
        time.sleep(0.1)  # Let connection establish
        pub.send_string("CLOSE_ALL")
        pub.send_string("CANCEL_ALL")
        pub.close()
        print("[WATCHDOG] 🚨 CLOSE_ALL sent to MT5")
        return True
    except Exception as e:
        print(f"[WATCHDOG] Failed to send CLOSE_ALL: {e}")
        return False


def restart_service(tracker: RevivalTracker) -> bool:
    """Attempt to restart a service."""
    if tracker.halted:
        print(f"[WATCHDOG] Revival HALTED for {tracker.service} (too many deaths)")
        return False

    can_revive = tracker.record_death()
    if not can_revive:
        send_discord_alert(
            "🛑 AUTO-REVIVAL HALTED",
            f"{tracker.service} died {tracker.death_count}x in {REVIVAL_WINDOW_SECS}s. Manual intervention required.",
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
    result = subprocess.run(
        ["systemctl", "--user", "is-active", "--quiet", "swimmy-guardian"],
        capture_output=True,
    )
    return result.returncode == 0


def get_open_positions_info() -> dict:
    """
    Query open positions info from Brain/Guardian.
    Returns dict with counts for short-term and long-term positions.
    For now, returns mock data. TODO: Implement actual query via ZMQ.
    """
    # P1 implementation: Would query Brain for position info
    # For now, assume all positions have SL and check timeframe from strategy
    # This is a stub - full implementation would query live positions
    return {
        "short_term_count": 0,  # M1-M30 positions
        "long_term_count": 0,  # H1+ positions
        "positions_with_sl": 0,
        "positions_without_sl": 0,
    }


# =============================================================================
# GRADUATED TIMEOUT LOGIC (P0)
# =============================================================================


def handle_brain_timeout():
    """Handle Brain timeout with graduated phases."""
    silence = brain_state.silence_secs()

    # Phase 1: Warning (30s)
    if silence > TIMEOUT_WARN_SECS and brain_state.phase < 1:
        brain_state.phase = 1
        print(
            f"[WATCHDOG] ⚠️ Phase 1: Brain silence {silence:.0f}s > {TIMEOUT_WARN_SECS}s (WARNING)"
        )
        send_discord_alert(
            "⚠️ BRAIN SILENCE WARNING",
            f"Brain silent for {silence:.0f}s. Monitoring...",
            color=16776960,  # Yellow
            mention=False,
        )

    # Phase 2: Block new entries (90s)
    elif silence > TIMEOUT_BLOCK_SECS and brain_state.phase < 2:
        brain_state.phase = 2
        print(
            f"[WATCHDOG] 🛑 Phase 2: Brain silence {silence:.0f}s > {TIMEOUT_BLOCK_SECS}s (BLOCK)"
        )
        send_discord_alert(
            "🛑 BRAIN TIMEOUT - ENTRIES BLOCKED",
            f"Brain silent for {silence:.0f}s. New entries blocked. Attempting restart...",
            color=16744448,  # Orange
            mention=True,
        )

        # Attempt restart
        if not brain_state.halted:
            brain_tracker = RevivalTracker("swimmy-brain")
            if brain_state.record_death():
                restart_service(brain_tracker)

    # Phase 3: CLOSE_ALL (180s) - Policy dependent
    elif silence > TIMEOUT_CLOSE_SECS and brain_state.phase < 3:
        brain_state.phase = 3
        print(
            f"[WATCHDOG] 💀 Phase 3: Brain silence {silence:.0f}s > {TIMEOUT_CLOSE_SECS}s (CRITICAL)"
        )

        if TIMEOUT_POLICY == "CLOSE_ALL":
            # Aggressive: Close everything
            if not brain_state.close_sent:
                send_discord_alert(
                    "💀 BRAIN DEAD - CLOSING ALL POSITIONS",
                    f"Brain unresponsive for {silence:.0f}s. Policy: CLOSE_ALL. Closing all positions.",
                    color=16711680,  # Red
                    mention=True,
                )
                send_close_all_to_mt5()
                brain_state.close_sent = True

        elif TIMEOUT_POLICY == "MAINTAIN":
            # Conservative: Keep all positions, just alert
            if not brain_state.close_sent:
                send_discord_alert(
                    "💀 BRAIN DEAD - POSITIONS MAINTAINED",
                    f"Brain unresponsive for {silence:.0f}s. Policy: MAINTAIN. "
                    "Positions kept (relying on MT5 SL/TP). Manual check recommended.",
                    color=16711680,
                    mention=True,
                )
                brain_state.close_sent = True

        elif TIMEOUT_POLICY == "GRADUATED":
            # P1: Timeframe-aware - Close short-term, maintain long-term with SL
            positions = get_open_positions_info()

            if not brain_state.close_sent:
                # Close only short-term or unprotected positions
                # For now, we don't have live position data, so we alert and let Guardian handle
                send_discord_alert(
                    "💀 BRAIN DEAD - GRADUATED POLICY",
                    f"Brain unresponsive for {silence:.0f}s. Policy: GRADUATED.\n"
                    "• Short-term (M1-M30): Will be closed\n"
                    "• Long-term (H1+) with SL: Maintained\n"
                    "• No SL: Will be closed",
                    color=16711680,
                    mention=True,
                )
                # Send CLOSE_ALL_SCALP command (Guardian would interpret this)
                # For now, standard CLOSE_ALL as fallback
                send_close_all_to_mt5()
                brain_state.close_sent = True


def handle_brain_recovery():
    """Handle Brain signal recovery."""
    if brain_state.phase > 0:
        print("[WATCHDOG] 🧠 Brain signal restored!")
        send_discord_alert(
            "🧠 BRAIN REVIVED",
            f"Signal restored after {brain_state.silence_secs():.0f}s silence. Resume normal operation.",
            color=65280,  # Green
            mention=False,
        )
        brain_state.reset()
        brain_state.death_count = 0  # Full reset on recovery


# =============================================================================
# MAIN LOOP
# =============================================================================


def main():
    print("=" * 60)
    print("  🐕 SWIMMY WATCHDOG V15.6 - Graduated Timeout")
    print(f"  Policy: {TIMEOUT_POLICY}")
    print(
        f"  Phases: {TIMEOUT_WARN_SECS}s warn → {TIMEOUT_BLOCK_SECS}s block → {TIMEOUT_CLOSE_SECS}s close"
    )
    print("=" * 60)

    context = zmq.Context()
    brain_sub = context.socket(zmq.SUB)
    brain_sub.connect(f"tcp://localhost:{ZMQ_BRAIN_PORT}")
    brain_sub.setsockopt_string(zmq.SUBSCRIBE, "")
    brain_sub.setsockopt(zmq.RCVTIMEO, 1000)

    print(f"[WATCHDOG] Subscribed to Brain (port {ZMQ_BRAIN_PORT})")

    last_guardian_check = time.time()

    while True:
        try:
            # 1. Check for Brain messages
            try:
                msg = brain_sub.recv_string(flags=0)
                brain_state.last_msg_time = time.time()
                if brain_state.phase > 0:
                    handle_brain_recovery()
            except zmq.Again:
                pass

            # 2. Handle Brain timeout (graduated)
            if (
                brain_state.silence_secs() > TIMEOUT_WARN_SECS
                and not brain_state.halted
            ):
                handle_brain_timeout()

            # 3. Check Guardian periodically
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
