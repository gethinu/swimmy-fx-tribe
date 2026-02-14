#!/usr/bin/env python3
"""
swimmy-watchdog.py - Liveness Monitor & Auto-Revival Service
=============================================================
V15.7: Expert Panel Complete - P0 through P5 Implementation

Features:
1. P0: Graduated timeout (configurable thresholds)
2. P1: Timeframe-aware policy (GRADUATED)
3. P2: BRAIN_TIMEOUT_POLICY config
4. P3: Live position info query (via ZMQ to Brain)
5. P4: All timeouts configurable via .env
6. P5: Test mode with DEBUG_SIMULATE_TIMEOUT

Usage:
    Normal mode: python3 tools/watchdog.py
    Test mode:   python3 tools/watchdog.py --test
"""

import zmq
import json
import time
import os
import sys
import signal
import subprocess
import requests
from datetime import datetime, timedelta, timezone
from typing import Dict, List, Optional

# =============================================================================
# CONFIGURATION (P4: All configurable via .env)
# =============================================================================

ZMQ_BRAIN_PORT = 5556  # Brain's PUB socket
ZMQ_BRAIN_CMD_PORT = 5555  # Brain's command socket (for position query)
ZMQ_GUARDIAN_PORT = 5560  # Guardian's PUB socket to MT5

# Graduated Timeout Thresholds (P4: Configurable)
TIMEOUT_WARN_SECS = int(os.getenv("TIMEOUT_WARN_SECS", "30"))
TIMEOUT_BLOCK_SECS = int(os.getenv("TIMEOUT_BLOCK_SECS", "90"))
TIMEOUT_CLOSE_SECS = int(os.getenv("TIMEOUT_CLOSE_SECS", "180"))

# Policy (P2): CLOSE_ALL, MAINTAIN, GRADUATED
TIMEOUT_POLICY = os.getenv("BRAIN_TIMEOUT_POLICY", "GRADUATED").upper()

# Revival settings
REVIVAL_WINDOW_SECS = 300
MAX_DEATHS_IN_WINDOW = 2
GUARDIAN_CHECK_INTERVAL_SECS = 60

# Environment
WEBHOOK_URL = os.getenv("SWIMMY_DISCORD_ALERTS", "").strip('"').strip("'")

# Test mode flag
TEST_MODE = "--test" in sys.argv

# =============================================================================
# POSITION INFO (P3: Live query from Brain)
# =============================================================================


class PositionInfo:
    """Information about open positions."""

    def __init__(self):
        self.short_term_count = 0  # M1-M30
        self.long_term_count = 0  # H1+
        self.positions_with_sl = 0
        self.positions_without_sl = 0
        self.total_pnl = 0.0
        self.positions: List[Dict] = []

    def should_close_on_timeout(self) -> bool:
        """Returns True if positions should be closed based on GRADUATED policy."""
        # Close if: any short-term OR any without SL
        return self.short_term_count > 0 or self.positions_without_sl > 0

    def get_positions_to_close(self) -> List[Dict]:
        """Get list of positions that should be closed in GRADUATED mode."""
        to_close = []
        for pos in self.positions:
            timeframe = pos.get("timeframe", 1)
            has_sl = pos.get("sl", 0) != 0
            # Close if M1-M30 (timeframe < 60) OR no SL
            if timeframe < 60 or not has_sl:
                to_close.append(pos)
        return to_close


def query_position_info() -> PositionInfo:
    """
    Query Brain for live position information via ZMQ.
    P3 Implementation: Real query instead of stub.
    """
    info = PositionInfo()

    try:
        context = zmq.Context.instance()
        req = context.socket(zmq.REQ)
        req.setsockopt(zmq.RCVTIMEO, 3000)  # 3 second timeout
        req.setsockopt(zmq.LINGER, 0)
        req.connect(f"tcp://localhost:{ZMQ_BRAIN_CMD_PORT}")

        # Send position query command
        query = json.dumps({"action": "GET_POSITIONS"})
        req.send_string(query)

        # Wait for response
        response = req.recv_string()
        data = json.loads(response)

        if data.get("status") == "ok":
            positions = data.get("positions", [])
            info.positions = positions

            for pos in positions:
                timeframe = pos.get("timeframe", 1)
                has_sl = pos.get("sl", 0) != 0

                if timeframe >= 60:  # H1+
                    info.long_term_count += 1
                else:
                    info.short_term_count += 1

                if has_sl:
                    info.positions_with_sl += 1
                else:
                    info.positions_without_sl += 1

                info.total_pnl += pos.get("pnl", 0)

        req.close()

    except zmq.Again:
        print("[WATCHDOG] Position query timeout - Brain may be unresponsive")
    except Exception as e:
        print(f"[WATCHDOG] Position query error: {e}")

    return info


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
brain_tracker = RevivalTracker("swimmy-brain")


# =============================================================================
# HELPERS
# =============================================================================


def _pid_is_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        # If we can't signal it, it still exists.
        return True
    except Exception:
        return False


def _systemd_main_pid(service_name: str) -> Optional[int]:
    try:
        result = subprocess.run(
            ["systemctl", "show", "-p", "MainPID", "--value", service_name],
            capture_output=True,
            text=True,
        )
        if result.returncode != 0:
            print(
                f"[WATCHDOG] systemctl show MainPID FAILED for {service_name}: {result.stderr}"
            )
            return None

        raw = (result.stdout or "").strip()
        if not raw:
            return None

        pid = int(raw)
        if pid <= 0:
            return None
        return pid
    except Exception as e:
        print(f"[WATCHDOG] Failed to read MainPID for {service_name}: {e}")
        return None


def _kill_service_main_pid(service_name: str) -> bool:
    """
    Best-effort non-privileged restart for system services running as our user.

    We cannot `systemctl restart` a system unit without privileges, but we can
    signal the service's MainPID (owned by User=swimmy) and rely on `Restart=`
    to bring it back.
    """
    pid = _systemd_main_pid(service_name)
    if not pid:
        print(f"[WATCHDOG] No MainPID for {service_name}; cannot kill for restart.")
        return False

    print(
        f"[WATCHDOG] Fallback restart: SIGTERM {service_name} MainPID={pid} (systemd should relaunch)"
    )
    try:
        os.kill(pid, signal.SIGTERM)
    except ProcessLookupError:
        # Already exited; systemd should be restarting it or it's already down.
        return True
    except PermissionError as e:
        print(f"[WATCHDOG] Permission denied SIGTERM {service_name} pid={pid}: {e}")
        return False
    except Exception as e:
        print(f"[WATCHDOG] Failed SIGTERM {service_name} pid={pid}: {e}")
        return False

    # Give it a brief chance to exit cleanly, then escalate.
    for _ in range(50):  # ~5s at 0.1s
        if not _pid_is_alive(pid):
            return True
        time.sleep(0.1)

    print(f"[WATCHDOG] {service_name} pid={pid} still alive; escalating to SIGKILL")
    try:
        os.kill(pid, signal.SIGKILL)
        return True
    except ProcessLookupError:
        return True
    except PermissionError as e:
        print(f"[WATCHDOG] Permission denied SIGKILL {service_name} pid={pid}: {e}")
        return False
    except Exception as e:
        print(f"[WATCHDOG] Failed SIGKILL {service_name} pid={pid}: {e}")
        return False


def _reset_silence_timer_on_pid_change(
    state: "BrainState",
    *,
    last_pid: Optional[int],
    current_pid: Optional[int],
    now: Optional[float] = None,
) -> Optional[int]:
    """
    Reset Brain silence timer when systemd MainPID changes.

    Rationale: The watchdog measures silence since the last message received on
    Brain's PUB socket. When the Brain restarts, there can be a long startup
    period before it publishes anything. Without resetting the timer, the
    watchdog can falsely treat the *new* Brain as silent for the *old* Brain's
    downtime and repeatedly kill it, preventing successful startup.
    """
    if now is None:
        now = time.time()

    if not current_pid or current_pid <= 0:
        return last_pid

    # First observation: treat as a fresh start.
    if last_pid is None:
        state.last_msg_time = now
        state.reset()
        return current_pid

    if current_pid != last_pid:
        print(
            f"[WATCHDOG] Detected swimmy-brain MainPID change {last_pid}->{current_pid}; resetting silence timer (startup grace)"
        )
        state.last_msg_time = now
        state.reset()
        return current_pid

    return last_pid


def send_discord_alert(
    title: str, description: str, color: int = 16711680, mention: bool = True
):
    """Send alert to Discord."""
    if not WEBHOOK_URL:
        print(f"[WATCHDOG] No webhook configured, skipping: {title}")
        return

    if TEST_MODE:
        print(f"[TEST] Would send Discord: {title}")
        return

    try:
        payload = {
            "embeds": [
                {
                    "title": title,
                    "description": description,
                    "color": color,
                    "timestamp": datetime.now(timezone.utc).isoformat(),
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


def send_close_command(command: str = "CLOSE_ALL"):
    """Send close command to MT5 via Guardian."""
    if TEST_MODE:
        print(f"[TEST] Would send: {command}")
        return True

    try:
        context = zmq.Context.instance()
        pub = context.socket(zmq.PUB)
        pub.connect(f"tcp://localhost:{ZMQ_GUARDIAN_PORT}")
        time.sleep(0.1)
        pub.send_string(command)
        pub.close()
        print(f"[WATCHDOG] 🚨 {command} sent to MT5")
        return True
    except Exception as e:
        print(f"[WATCHDOG] Failed to send {command}: {e}")
        return False


def restart_service(tracker: RevivalTracker) -> bool:
    """Attempt to restart a service."""
    if tracker.halted:
        print(f"[WATCHDOG] Revival HALTED for {tracker.service}")
        return False

    can_revive = tracker.record_death()
    if not can_revive:
        send_discord_alert(
            "🛑 AUTO-REVIVAL HALTED",
            f"{tracker.service} died {tracker.death_count}x in {REVIVAL_WINDOW_SECS}s.",
            color=16711680,
            mention=True,
        )
        return False

    if TEST_MODE:
        print(f"[TEST] Would restart: {tracker.service}")
        return True

    print(f"[WATCHDOG] Restarting {tracker.service}...")
    systemctl_path = "/usr/bin/systemctl"
    sudo_path = "/usr/bin/sudo"

    # Prefer non-interactive sudo when available so we can manage system units
    # without triggering polkit prompts. Falls back to direct systemctl, then
    # finally to MainPID-based SIGTERM/SIGKILL (systemd Restart= relaunch).
    attempts = [
        ([sudo_path, "-n", systemctl_path, "restart", tracker.service], "sudo -n systemctl"),
        ([systemctl_path, "restart", tracker.service], "systemctl"),
    ]

    result = None
    label = None
    for cmd, attempt_label in attempts:
        label = attempt_label
        try:
            result = subprocess.run(cmd, capture_output=True, text=True)
        except FileNotFoundError as e:
            # Should not happen in normal deployments, but keep watchdog alive.
            print(f"[WATCHDOG] ❌ {attempt_label} restart FAILED: {e}")
            result = None
            continue

        if result.returncode == 0:
            break

    if result is not None and result.returncode == 0:
        print(f"[WATCHDOG] ✅ {tracker.service} restart SUCCESS")
        return True
    else:
        stderr = ""
        if result is not None:
            stderr = (result.stderr or "").strip()
        print(f"[WATCHDOG] ❌ {tracker.service} restart FAILED ({label}): {stderr}")
        # Fallback: for system units we may lack privileges; kill MainPID and let systemd Restart= relaunch.
        if _kill_service_main_pid(tracker.service):
            print(
                f"[WATCHDOG] ✅ {tracker.service} fallback restart SUCCESS (signaled MainPID)"
            )
            return True
        return False


def check_guardian_alive() -> bool:
    if TEST_MODE:
        return True
    # V48.0: Fixed - swimmy-guardian is a system service, not user service
    result = subprocess.run(
        ["systemctl", "is-active", "--quiet", "swimmy-guardian"],
        capture_output=True,
    )
    return result.returncode == 0


# =============================================================================
# GRADUATED TIMEOUT LOGIC (P0 + P1 + P3)
# =============================================================================


def handle_brain_timeout():
    """Handle Brain timeout with graduated phases."""
    silence = brain_state.silence_secs()

    # Phase 1: Warning
    if silence > TIMEOUT_WARN_SECS and brain_state.phase < 1:
        brain_state.phase = 1
        print(f"[WATCHDOG] ⚠️ Phase 1: WARNING ({silence:.0f}s)")
        send_discord_alert(
            "⚠️ BRAIN SILENCE WARNING",
            f"Brain silent for {silence:.0f}s. Monitoring...",
            color=16776960,
            mention=False,
        )

    # Phase 2: Block + Restart
    elif silence > TIMEOUT_BLOCK_SECS and brain_state.phase < 2:
        brain_state.phase = 2
        print(f"[WATCHDOG] 🛑 Phase 2: BLOCK ({silence:.0f}s)")
        send_discord_alert(
            "🛑 BRAIN TIMEOUT - ENTRIES BLOCKED",
            f"Brain silent for {silence:.0f}s. Attempting restart...",
            color=16744448,
            mention=True,
        )

        if not brain_state.halted:
            if brain_state.record_death():
                restart_service(brain_tracker)

    # Phase 3: Execute Policy
    elif silence > TIMEOUT_CLOSE_SECS and brain_state.phase < 3:
        brain_state.phase = 3
        print(f"[WATCHDOG] 💀 Phase 3: EXECUTE POLICY ({silence:.0f}s)")

        if brain_state.close_sent:
            return

        # P3: Query position info for GRADUATED policy
        position_info = query_position_info()

        if TIMEOUT_POLICY == "CLOSE_ALL":
            send_discord_alert(
                "💀 BRAIN DEAD - CLOSING ALL",
                f"Policy: CLOSE_ALL. Closing all positions.",
                color=16711680,
                mention=True,
            )
            send_close_command("CLOSE_ALL")
            send_close_command("CANCEL_ALL")
            brain_state.close_sent = True

        elif TIMEOUT_POLICY == "MAINTAIN":
            send_discord_alert(
                "💀 BRAIN DEAD - POSITIONS MAINTAINED",
                f"Policy: MAINTAIN. All positions kept.\n"
                f"Short-term: {position_info.short_term_count}, Long-term: {position_info.long_term_count}",
                color=16711680,
                mention=True,
            )
            brain_state.close_sent = True

        elif TIMEOUT_POLICY == "GRADUATED":
            # P3: Timeframe-aware closing
            to_close = position_info.get_positions_to_close()

            if to_close:
                desc = (
                    f"Policy: GRADUATED\n"
                    f"• Closing: {len(to_close)} short-term/unprotected\n"
                    f"• Maintaining: {position_info.long_term_count} long-term with SL"
                )
                send_discord_alert(
                    "💀 BRAIN DEAD - GRADUATED CLOSE",
                    desc,
                    color=16711680,
                    mention=True,
                )

                # Close individual positions (if Brain supports per-ticket close)
                # Fallback to CLOSE_ALL for short-term
                for pos in to_close:
                    ticket = pos.get("ticket")
                    if ticket:
                        send_close_command(f"CLOSE_TICKET:{ticket}")
                    else:
                        # If no ticket info, use CLOSE_ALL as fallback
                        send_close_command("CLOSE_ALL")
                        break
            else:
                send_discord_alert(
                    "💀 BRAIN DEAD - NO POSITIONS TO CLOSE",
                    f"Policy: GRADUATED. All {position_info.long_term_count} positions are long-term with SL.",
                    color=16744448,
                    mention=True,
                )

            brain_state.close_sent = True


def handle_brain_recovery():
    """Handle Brain signal recovery."""
    if brain_state.phase > 0:
        print("[WATCHDOG] 🧠 Brain signal restored!")
        send_discord_alert(
            "🧠 BRAIN REVIVED",
            f"Signal restored after {brain_state.silence_secs():.0f}s silence.",
            color=65280,
            mention=False,
        )
        brain_state.reset()
        brain_state.death_count = 0


# =============================================================================
# TEST MODE (P5)
# =============================================================================


def run_tests():
    """P5: Phase transition tests."""
    print("=" * 60)
    print("  🧪 WATCHDOG TEST MODE - Phase Transition Tests")
    print("=" * 60)

    results = []

    # Test 1: Phase transitions
    print("\n[TEST 1] Phase Transition Test")
    test_state = BrainState()
    test_state.last_msg_time = time.time() - 35  # 35s ago

    assert (
        test_state.silence_secs() > TIMEOUT_WARN_SECS
    ), "Should detect silence > warn threshold"
    print(
        f"  ✅ Silence detection: {test_state.silence_secs():.1f}s > {TIMEOUT_WARN_SECS}s"
    )
    results.append(("Phase detection", True))

    # Test 2: Revival tracker
    print("\n[TEST 2] Revival Tracker Test")
    tracker = RevivalTracker("test-service")
    assert tracker.record_death() == True, "First death should allow revival"
    assert tracker.record_death() == False, "Second death should halt revival"
    assert tracker.halted == True, "Should be halted after 2 deaths"
    print(f"  ✅ Loop prevention: halted after {tracker.death_count} deaths")
    results.append(("Loop prevention", True))

    # Test 3: Position info
    print("\n[TEST 3] Position Info Test")
    info = PositionInfo()
    info.short_term_count = 2
    info.long_term_count = 3
    info.positions_without_sl = 1
    info.positions = [
        {"timeframe": 5, "sl": 0, "ticket": 123},  # M5, no SL
        {"timeframe": 15, "sl": 100, "ticket": 124},  # M15, with SL
        {"timeframe": 240, "sl": 100, "ticket": 125},  # H4, with SL
    ]
    to_close = info.get_positions_to_close()
    assert len(to_close) == 2, "Should close M5 (no SL) and M15 (short-term)"
    print(f"  ✅ GRADUATED logic: {len(to_close)} positions to close")
    results.append(("GRADUATED logic", True))

    # Test 4: Config loading
    print("\n[TEST 4] Configuration Test")
    assert TIMEOUT_WARN_SECS > 0, "TIMEOUT_WARN_SECS should be positive"
    assert TIMEOUT_BLOCK_SECS > TIMEOUT_WARN_SECS, "BLOCK should be > WARN"
    assert TIMEOUT_CLOSE_SECS > TIMEOUT_BLOCK_SECS, "CLOSE should be > BLOCK"
    print(
        f"  ✅ Thresholds: {TIMEOUT_WARN_SECS}s → {TIMEOUT_BLOCK_SECS}s → {TIMEOUT_CLOSE_SECS}s"
    )
    results.append(("Config loading", True))

    # Test 5: Brain PID change should reset silence timer (startup grace)
    print("\n[TEST 5] Brain PID Change Grace Test")
    test_state = BrainState()
    test_state.last_msg_time = 0.0
    test_state.phase = 2
    test_state.close_sent = True
    try:
        # Helper will be added in production code. For now this should fail (RED).
        new_pid = _reset_silence_timer_on_pid_change(
            test_state, last_pid=111, current_pid=222, now=100.0
        )
    except Exception:
        new_pid = None
    assert (
        new_pid == 222
    ), "PID change should return new PID and reset silence timer (startup grace)"
    assert test_state.last_msg_time == 100.0, "Silence timer should reset to now"
    assert test_state.phase == 0, "Phase should reset to 0 after PID change"
    assert test_state.close_sent is False, "close_sent should reset after PID change"
    print("  ✅ PID change grace: resets silence timer + phases")
    results.append(("PID change grace", True))

    # Summary
    print("\n" + "=" * 60)
    passed = sum(1 for _, r in results if r)
    print(f"  Results: {passed}/{len(results)} tests passed")
    print("=" * 60)

    return all(r for _, r in results)


# =============================================================================
# MAIN LOOP
# =============================================================================


def main():
    if TEST_MODE:
        success = run_tests()
        sys.exit(0 if success else 1)

    print("=" * 60)
    print(f"  🐕 SWIMMY WATCHDOG V15.7 - Full Implementation")
    print(f"  Policy: {TIMEOUT_POLICY}")
    print(
        f"  Phases: {TIMEOUT_WARN_SECS}s → {TIMEOUT_BLOCK_SECS}s → {TIMEOUT_CLOSE_SECS}s"
    )
    print("=" * 60)

    context = zmq.Context()
    brain_sub = context.socket(zmq.SUB)
    brain_sub.connect(f"tcp://localhost:{ZMQ_BRAIN_PORT}")
    brain_sub.setsockopt_string(zmq.SUBSCRIBE, "")
    brain_sub.setsockopt(zmq.RCVTIMEO, 1000)

    print(f"[WATCHDOG] Subscribed to Brain (port {ZMQ_BRAIN_PORT})")

    last_brain_main_pid: Optional[int] = None

    last_guardian_check = time.time()

    while True:
        try:
            # Detect Brain restarts and reset silence timer so we don't kill a
            # fresh startup for the previous instance's downtime.
            current_brain_main_pid = _systemd_main_pid("swimmy-brain")
            last_brain_main_pid = _reset_silence_timer_on_pid_change(
                brain_state,
                last_pid=last_brain_main_pid,
                current_pid=current_brain_main_pid,
            )

            # 1. Check for Brain messages
            try:
                msg = brain_sub.recv_string(flags=0)
                brain_state.last_msg_time = time.time()
                if brain_state.phase > 0:
                    handle_brain_recovery()
            except zmq.Again:
                pass

            # 2. Handle Brain timeout
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
                        "🛡️ GUARDIAN DOWN", "Attempting restart.", color=16711680
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
