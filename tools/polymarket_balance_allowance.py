#!/usr/bin/env python3
"""
polymarket_balance_allowance.py
===============================
Utility: fetch the current collateral (USDC) balance + allowance info for the
configured Polymarket CLOB wallet.

This is meant for diagnosing "not enough balance / allowance" failures without
printing secrets (private keys / API creds / webhook URLs).

Reads runtime from env (same variables as live execution):
- POLYCLAW_LIVE_HOST (default: https://clob.polymarket.com)
- POLYCLAW_LIVE_CHAIN_ID (default: 137)
- POLYCLAW_LIVE_SIGNATURE_TYPE (default: 0)
- POLYCLAW_LIVE_FUNDER (optional)
- POLYCLAW_LIVE_PRIVATE_KEY or POLYCLAW_LIVE_PRIVATE_KEY_FILE (required)
- POLYCLAW_LIVE_API_KEY / _SECRET / _PASSPHRASE (optional; otherwise derives)
"""

from __future__ import annotations

import argparse
import json
import os
from typing import Any, Dict, Mapping

from py_clob_client.client import ClobClient
from py_clob_client.clob_types import AssetType, BalanceAllowanceParams, ApiCreds

from polymarket_openclaw_execute import load_private_key


def _env_int(env: Mapping[str, str], key: str, default: int) -> int:
    text = str(env.get(key, "")).strip()
    if not text:
        return default
    try:
        return int(text)
    except ValueError:
        return default


def _env_str(env: Mapping[str, str], key: str, default: str = "") -> str:
    text = str(env.get(key, "")).strip()
    return text if text else default


def resolve_api_creds(env: Mapping[str, str], client: ClobClient) -> ApiCreds:
    api_key = _env_str(env, "POLYCLAW_LIVE_API_KEY")
    api_secret = _env_str(env, "POLYCLAW_LIVE_API_SECRET")
    api_passphrase = _env_str(env, "POLYCLAW_LIVE_API_PASSPHRASE")
    if api_key and api_secret and api_passphrase:
        return ApiCreds(api_key=api_key, api_secret=api_secret, api_passphrase=api_passphrase)
    return client.create_or_derive_api_creds()


def main() -> None:
    parser = argparse.ArgumentParser(description="Fetch Polymarket CLOB collateral balance/allowance for live wallet")
    parser.add_argument("--host", default="")
    parser.add_argument("--chain-id", type=int, default=0)
    parser.add_argument("--signature-type", type=int, default=-999)
    parser.add_argument("--funder", default="")
    parser.add_argument("--json", action="store_true", help="Print JSON (default)")
    args = parser.parse_args()

    env: Dict[str, str] = dict(os.environ)

    host = (args.host or _env_str(env, "POLYCLAW_LIVE_HOST", "https://clob.polymarket.com")).strip()
    chain_id = int(args.chain_id or _env_int(env, "POLYCLAW_LIVE_CHAIN_ID", 137))
    signature_type = (
        int(args.signature_type)
        if int(args.signature_type) != -999
        else int(_env_int(env, "POLYCLAW_LIVE_SIGNATURE_TYPE", 0))
    )
    funder = (args.funder or _env_str(env, "POLYCLAW_LIVE_FUNDER", "")).strip() or None
    private_key = load_private_key(env)
    if not private_key:
        raise SystemExit("missing private key: set POLYCLAW_LIVE_PRIVATE_KEY or POLYCLAW_LIVE_PRIVATE_KEY_FILE")

    client = ClobClient(
        host,
        chain_id=chain_id,
        key=private_key,
        signature_type=signature_type,
        funder=funder,
    )
    client.set_api_creds(resolve_api_creds(env, client))

    params = BalanceAllowanceParams(asset_type=AssetType.COLLATERAL)
    # Nudge the server to refresh chain-derived values, then read them.
    try:
        update_result: Any = client.update_balance_allowance(params)
    except Exception as exc:
        update_result = {"error": f"{type(exc).__name__}: {exc}"}

    try:
        balance_allowance: Any = client.get_balance_allowance(params)
    except Exception as exc:
        balance_allowance = {"error": f"{type(exc).__name__}: {exc}"}

    out = {
        "address": client.get_address(),
        "chain_id": chain_id,
        "host": host,
        "funder": funder,
        "collateral": client.get_collateral_address(),
        "exchange": client.get_exchange_address(False),
        "exchange_neg_risk": client.get_exchange_address(True),
        "update_balance_allowance": update_result,
        "balance_allowance": balance_allowance,
    }
    print(json.dumps(out, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
