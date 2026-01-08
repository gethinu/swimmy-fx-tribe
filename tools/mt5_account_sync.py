#!/usr/bin/env python3
"""
mt5_account_sync.py

独立した MT5 口座情報同期モジュール (1機能1モジュール原則)

This module runs as an independent service that:
1. Connects to MT5 terminal
2. Periodically fetches account info (equity, balance, margin)
3. Sends ACCOUNT_INFO messages to Swimmy Brain via ZMQ

Usage:
    python3 tools/mt5_account_sync.py

Architecture:
    MT5 Terminal <-- (API) -- mt5_account_sync.py -- (ZMQ) --> Guardian --> Brain

Port: 5580 (dedicated for account sync, avoids conflict with other services)
"""

import json
import time
import sys
from datetime import datetime

# Configuration
SYNC_INTERVAL_SECONDS = 30  # Sync every 30 seconds
ZMQ_PORT = 5580  # Dedicated port for account sync
MAX_CONSECUTIVE_FAILURES = 5  # Alert after 5 consecutive failures
MT5_RECONNECT_INTERVAL = 60  # Retry MT5 connection every 60s

# Apex webhook for crash alerts
APEX_WEBHOOK = "https://discord.com/api/webhooks/1458820892623634686/Nv_POY_W0E_iD130bTQM1eDJyTJmU5ZweDOEOpMvEW6ZnEmMCSoconLlxqd5bUuug72k"

# Try to import dependencies
try:
    import zmq
except ImportError:
    print("❌ zmq package not installed. Run: pip install pyzmq")
    sys.exit(1)

try:
    import requests

    HAS_REQUESTS = True
except ImportError:
    HAS_REQUESTS = False

try:
    import MetaTrader5 as mt5

    HAS_MT5 = True
except ImportError:
    HAS_MT5 = False
    print("⚠️  MetaTrader5 package not installed.")
    print("    This module requires MT5 Python API (Windows only).")


def send_discord_alert(message: str, is_error: bool = True):
    """Send alert to Apex Discord webhook."""
    if not HAS_REQUESTS:
        print(f"[ALERT] {message}")
        return
    try:
        color = 15158332 if is_error else 3066993  # Red or Green
        payload = {
            "embeds": [
                {"title": "🐟 MT5 Account Sync", "description": message, "color": color}
            ]
        }
        requests.post(APEX_WEBHOOK, json=payload, timeout=5)
    except Exception as e:
        print(f"[ALERT FAILED] {e}")


def get_account_info():
    """Fetch account information from MT5."""
    if not HAS_MT5:
        return None

    account = mt5.account_info()
    if account is None:
        return None

    return {
        "type": "ACCOUNT_INFO",
        "timestamp": int(time.time()),
        "equity": account.equity,
        "balance": account.balance,
        "margin": account.margin,
        "free_margin": account.margin_free,
        "margin_level": account.margin_level if account.margin > 0 else 0,
        "profit": account.profit,
        "currency": account.currency,
        "leverage": account.leverage,
    }


def run_sync_loop(socket):
    """Main sync loop - fetches account info and sends to Brain."""
    print(f"🔄 Account sync running (interval: {SYNC_INTERVAL_SECONDS}s)")

    while True:
        try:
            account_info = get_account_info()

            if account_info:
                msg = json.dumps(account_info)
                socket.send_string(msg)

                print(
                    f"[{datetime.now().strftime('%H:%M:%S')}] "
                    f"💰 Equity: ¥{account_info['equity']:,.0f} "
                    f"Balance: ¥{account_info['balance']:,.0f} "
                    f"P/L: ¥{account_info['profit']:+,.0f}"
                )
            else:
                print(
                    f"[{datetime.now().strftime('%H:%M:%S')}] ⚠️ Failed to get account info"
                )

            time.sleep(SYNC_INTERVAL_SECONDS)

        except KeyboardInterrupt:
            print("\n🛑 Sync stopped by user")
            break
        except Exception as e:
            print(f"❌ Error: {e}")
            time.sleep(5)  # Wait before retry


def main():
    print("=" * 50)
    print("🐟 Swimmy MT5 Account Sync Module")
    print("=" * 50)
    print(f"   Port: {ZMQ_PORT}")
    print(f"   Interval: {SYNC_INTERVAL_SECONDS}s")
    print(f"   Max Failures: {MAX_CONSECUTIVE_FAILURES}")
    print("=" * 50)

    # Check MT5
    if not HAS_MT5:
        print("\n❌ Cannot run without MT5 Python API.")
        print("   Install: pip install MetaTrader5")
        print("   Note: Only works on Windows with MT5 terminal installed.")
        send_discord_alert("❌ MT5 Account Sync cannot start: MT5 API not installed")
        sys.exit(1)

    # Initialize MT5 with retry
    mt5_connected = False
    retry_count = 0
    while not mt5_connected:
        if mt5.initialize():
            mt5_connected = True
            print(f"✅ MT5 Connected: {mt5.terminal_info().name}")
        else:
            retry_count += 1
            print(
                f"❌ MT5 initialization failed (attempt {retry_count}): {mt5.last_error()}"
            )
            if retry_count >= 3:
                send_discord_alert(
                    f"❌ MT5 Account Sync failed to connect after {retry_count} attempts"
                )
            time.sleep(MT5_RECONNECT_INTERVAL)

    # Get initial account info
    account = mt5.account_info()
    if account:
        print(f"   Account: {account.login} ({account.company})")
        print(f"   Balance: {account.currency} {account.balance:,.2f}")
        print(f"   Equity:  {account.currency} {account.equity:,.2f}")

    # Setup ZMQ
    context = zmq.Context()
    socket = context.socket(zmq.PUB)
    socket.bind(f"tcp://*:{ZMQ_PORT}")
    print(f"\n📡 ZMQ PUB socket bound to port {ZMQ_PORT}")

    # Connect to Guardian to forward messages
    push_socket = context.socket(zmq.PUSH)
    push_socket.connect("tcp://localhost:5555")
    print(f"📡 Connected to Guardian on port 5555")

    # Send startup notification
    send_discord_alert("✅ MT5 Account Sync Service Started", is_error=False)
    print("\n🚀 Starting sync loop...\n")

    # Failure tracking
    consecutive_failures = 0
    alert_sent = False

    try:
        while True:
            try:
                account_info = get_account_info()

                if account_info:
                    msg = json.dumps(account_info)
                    socket.send_string(msg)
                    push_socket.send_string(msg)

                    print(
                        f"[{datetime.now().strftime('%H:%M:%S')}] "
                        f"💰 Equity: ¥{account_info['equity']:,.0f} "
                        f"Balance: ¥{account_info['balance']:,.0f} "
                        f"P/L: ¥{account_info['profit']:+,.0f}"
                    )

                    # Reset failure counter on success
                    if consecutive_failures > 0 and alert_sent:
                        send_discord_alert(
                            f"✅ MT5 Account Sync Recovered ({consecutive_failures} failures)",
                            is_error=False,
                        )
                    consecutive_failures = 0
                    alert_sent = False
                else:
                    consecutive_failures += 1
                    print(
                        f"[{datetime.now().strftime('%H:%M:%S')}] ⚠️ Failed to get account info ({consecutive_failures})"
                    )

                    # Try to reconnect MT5
                    if consecutive_failures >= 3:
                        print("🔄 Attempting MT5 reconnection...")
                        mt5.shutdown()
                        time.sleep(2)
                        if mt5.initialize():
                            print("✅ MT5 reconnected!")
                        else:
                            print(f"❌ MT5 reconnection failed: {mt5.last_error()}")

                    # Send alert after MAX_CONSECUTIVE_FAILURES
                    if (
                        consecutive_failures >= MAX_CONSECUTIVE_FAILURES
                        and not alert_sent
                    ):
                        send_discord_alert(
                            f"⚠️ MT5 Account Sync: {consecutive_failures} consecutive failures"
                        )
                        alert_sent = True

                time.sleep(SYNC_INTERVAL_SECONDS)

            except Exception as e:
                consecutive_failures += 1
                print(f"❌ Error in sync loop: {e}")

                if consecutive_failures >= MAX_CONSECUTIVE_FAILURES and not alert_sent:
                    send_discord_alert(f"🚨 MT5 Account Sync Error: {e}")
                    alert_sent = True

                time.sleep(5)

    except KeyboardInterrupt:
        print("\n🛑 Sync stopped by user")
        send_discord_alert("🛑 MT5 Account Sync Service Stopped (user)", is_error=False)
    except Exception as e:
        print(f"💥 Fatal error: {e}")
        send_discord_alert(f"💥 MT5 Account Sync CRASHED: {e}")
        raise
    finally:
        mt5.shutdown()
        socket.close()
        push_socket.close()
        context.term()
        print("👋 Cleanup complete")


if __name__ == "__main__":
    main()
