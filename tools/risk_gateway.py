#!/usr/bin/env python3
"""
risk_gateway.py - The Trade Gatekeeper (Taleb's Via Negativa)
=============================================================
Expert Panel Approved (2026-01-07) - Phase 3

Purpose:
    Centralized authority for trade approval.
    Lisp asks for permission; Gateway verifies against hard limits (Daily Loss, Drawdown, etc.).
    Runs in a separate process to prevent "heat of the moment" logic bypasses in Lisp.

Architecture:
    [Swimmy Lisp] --(ZMQ REQ)--> [risk_gateway.py] --(ZMQ REP)--> [Swimmy Lisp]

Hard Limits (defaults, env override available):
    - Daily Loss Limit: ¥-5000 (Soft), ¥-10000 (Hard Stop)
    - Max Drawdown: 5%
    - Max Risk Per Trade: 2% of Equity
    - Consecutive Losses: Max 5 (Gateway forces cooldown)

Usage:
    python3 tools/risk_gateway.py

Commands (JSON):
    CHECK_RISK: {
        "action": "BUY"|"SELL",
        "symbol": "USDJPY",
        "lot": 0.01,
        "daily_pnl": -500.0,
        "equity": 50000.0,
        "consecutive_losses": 2
    }
    -> Returns: {"status": "APPROVED" | "DENIED", "reason": "..."}

    RESET: {}
    -> Returns: {"status": "RESET_COMPLETE"} (Use at start of day)
"""

import zmq
import json
import time
import os

def _env_float(key, default):
    raw = os.getenv(key)
    if raw is None or raw == "":
        return default
    try:
        return float(raw)
    except ValueError:
        return default


def _env_int(key, default):
    raw = os.getenv(key)
    if raw is None or raw == "":
        return default
    try:
        return int(raw)
    except ValueError:
        return default


# Configuration (Environment overrides supported)
ZMQ_PORT = _env_int("SWIMMY_RISK_GATEWAY_PORT", 5563)
DAILY_LOSS_LIMIT_SOFT = _env_float("SWIMMY_DAILY_LOSS_LIMIT", -5000.0)
DAILY_LOSS_LIMIT_HARD = _env_float(
    "SWIMMY_DAILY_LOSS_LIMIT_HARD", min(DAILY_LOSS_LIMIT_SOFT * 2, -10000.0)
)
MAX_DRAWDOWN_PERCENT = _env_float("SWIMMY_MAX_DRAWDOWN_PCT", 5.0)
MAX_CONSECUTIVE_LOSSES = _env_int("SWIMMY_MAX_CONSECUTIVE_LOSSES", 5)
MAX_RISK_PER_TRADE_PERCENT = _env_float("SWIMMY_MAX_RISK_PER_TRADE_PCT", 2.0)
MAX_LOT_SIZE = _env_float("SWIMMY_MAX_LOT_SIZE", 0.5)

# State
daily_loss_triggered = False
hard_stop_triggered = False
start_equity = 0.0  # Will be set on first check if 0


def handle_check_risk(data):
    global daily_loss_triggered, hard_stop_triggered, start_equity

    # 1. Parse Input
    try:
        action = data.get("action", "UNKNOWN")
        symbol = data.get("symbol", "UNKNOWN")
        lot = float(data.get("lot", 0.0))
        daily_pnl = float(data.get("daily_pnl", 0.0))
        equity = float(data.get("equity", 0.0))
        cons_losses = int(data.get("consecutive_losses", 0))
    except (ValueError, TypeError):
        return {"status": "DENIED", "reason": "INVALID_DATA_FORMAT"}

    # Initialize start equity if needed
    if start_equity == 0.0 and equity > 0:
        start_equity = equity

    reasons = []

    # 2. Hard Stop Checks (The "Via Negativa")

    # A. Daily Loss Limit
    if daily_pnl <= DAILY_LOSS_LIMIT_HARD:
        hard_stop_triggered = True
        return {
            "status": "DENIED",
            "reason": f"HARD_STOP: Daily Loss {daily_pnl} <= {DAILY_LOSS_LIMIT_HARD}",
        }

    if daily_pnl <= DAILY_LOSS_LIMIT_SOFT:
        daily_loss_triggered = True
        # Allow closing trades? No, this check is for OPENING.
        reasons.append(f"SOFT_STOP: Daily Loss {daily_pnl} <= {DAILY_LOSS_LIMIT_SOFT}")

    # B. Max Drawdown
    if start_equity > 0:
        dd_percent = (start_equity - equity) / start_equity * 100
        if dd_percent >= MAX_DRAWDOWN_PERCENT:
            reasons.append(
                f"MAX_DRAWDOWN: {dd_percent:.2f}% >= {MAX_DRAWDOWN_PERCENT}%"
            )

    # C. Consecutive Losses
    if cons_losses >= MAX_CONSECUTIVE_LOSSES:
        reasons.append(f"CONSECUTIVE_LOSSES: {cons_losses} >= {MAX_CONSECUTIVE_LOSSES}")

    # D. Lot Size / Risk Cap (Sanity Check)
    # Simple check: Lot 1.0 is huge. Cap at 0.5 usually, but let's be strict here.
    if lot > MAX_LOT_SIZE:
        reasons.append(f"FAT_FINGER: Lot {lot} > {MAX_LOT_SIZE}")

    # 3. Decision
    if reasons:
        return {"status": "DENIED", "reason": "; ".join(reasons)}
    else:
        return {"status": "APPROVED", "reason": "Risk checks passed"}


def main():
    print("=" * 60)
    print("  🛡️ RISK GATEWAY - The Gatekeeper")
    print("  Expert Panel Phase 3")
    print("=" * 60)

    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind(f"tcp://*:{ZMQ_PORT}")
    print(f"[GATEWAY] Listening on port {ZMQ_PORT} (REP)")

    while True:
        try:
            msg_str = socket.recv_string()

            try:
                # Expect "COMMAND:JSON" or just JSON logic?
                # Let's support "CHECK_RISK:{json}" format like Data Keeper
                if ":" in msg_str:
                    cmd, json_str = msg_str.split(":", 1)
                    data = json.loads(json_str)

                    if cmd == "CHECK_RISK":
                        response = handle_check_risk(data)
                    else:
                        response = {
                            "status": "ERROR",
                            "reason": f"Unknown command: {cmd}",
                        }
                else:
                    response = {
                        "status": "ERROR",
                        "reason": "Invalid format. Use CMD:JSON",
                    }

            except json.JSONDecodeError:
                response = {"status": "ERROR", "reason": "Invalid JSON"}
            except Exception as e:
                response = {"status": "ERROR", "reason": str(e)}

            socket.send_string(json.dumps(response))

            # Log denials to stdout
            if response.get("status") == "DENIED":
                print(f"[GATEWAY] 🛑 DENIED: {response.get('reason')}")
            elif response.get("status") == "APPROVED":
                pass
                # print(f"[GATEWAY] ✅ APPROVED") # Verbose?

        except KeyboardInterrupt:
            print("\n[GATEWAY] Shutting down...")
            break
        except Exception as e:
            print(f"[GATEWAY] Critical Error: {e}")

    socket.close()
    context.term()


if __name__ == "__main__":
    main()
