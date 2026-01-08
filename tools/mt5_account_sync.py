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
GUARDIAN_ADDR = "tcp://localhost:5557"  # Guardian's market data port

# Try to import dependencies
try:
    import zmq
except ImportError:
    print("❌ zmq package not installed. Run: pip install pyzmq")
    sys.exit(1)

try:
    import MetaTrader5 as mt5

    HAS_MT5 = True
except ImportError:
    HAS_MT5 = False
    print("⚠️  MetaTrader5 package not installed.")
    print("    This module requires MT5 Python API (Windows only).")


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
    print("=" * 50)

    # Check MT5
    if not HAS_MT5:
        print("\n❌ Cannot run without MT5 Python API.")
        print("   Install: pip install MetaTrader5")
        print("   Note: Only works on Windows with MT5 terminal installed.")
        sys.exit(1)

    # Initialize MT5
    if not mt5.initialize():
        print(f"❌ MT5 initialization failed: {mt5.last_error()}")
        print("   Make sure MT5 terminal is running.")
        sys.exit(1)

    print(f"✅ MT5 Connected: {mt5.terminal_info().name}")

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

    # Also connect to Guardian to forward messages
    push_socket = context.socket(zmq.PUSH)
    push_socket.connect("tcp://localhost:5555")  # Guardian's PULL port
    print(f"📡 Connected to Guardian on port 5555")

    print("\n🚀 Starting sync loop...\n")

    try:
        while True:
            account_info = get_account_info()

            if account_info:
                msg = json.dumps(account_info)

                # Send to local subscribers (for debugging)
                socket.send_string(msg)

                # Send to Guardian for forwarding to Brain
                push_socket.send_string(msg)

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
    finally:
        mt5.shutdown()
        socket.close()
        push_socket.close()
        context.term()
        print("👋 Cleanup complete")


if __name__ == "__main__":
    main()
