#!/usr/bin/env python3
"""
trigger_recruit_special_forces.py

Send RECRUIT_SPECIAL_FORCES S-expression command to Brain sensory port (5555, PULL).
This can re-inject founders for selected symbols and optionally flush deferred Phase1 backtests.
"""

from __future__ import annotations

import argparse
import time
from typing import Iterable, List

import zmq

DEFAULT_ENDPOINT = "tcp://127.0.0.1:5555"
DEFAULT_SYMBOLS = ["EURUSD", "GBPUSD"]
DEFAULT_CONNECT_WAIT_SEC = 1.0


def normalize_symbol(symbol: str) -> str:
    return symbol.strip().upper()


def normalize_founder_key(founder_key: str) -> str:
    return founder_key.strip().upper().lstrip(":")


def _sexp_bool(value: bool) -> str:
    return "t" if value else "nil"


def build_recruit_special_forces_message(
    symbols: Iterable[str],
    founder_keys: Iterable[str] | None = None,
    recruit_limit: int | None = None,
    disable_preflight: bool = False,
    bypass_graveyard: bool = False,
    flush_phase1: bool = True,
    flush_limit: int | None = None,
) -> str:
    normalized = [normalize_symbol(symbol) for symbol in symbols if normalize_symbol(symbol)]
    if not normalized:
        raise ValueError("at least one symbol is required")
    normalized_founder_keys = []
    if founder_keys is not None:
        normalized_founder_keys = [
            normalize_founder_key(founder_key)
            for founder_key in founder_keys
            if normalize_founder_key(founder_key)
        ]
    if recruit_limit is not None and recruit_limit < 0:
        raise ValueError("recruit_limit must be >= 0")
    symbols_expr = " ".join(f'"{symbol}"' for symbol in normalized)
    parts = [
        '((type . "RECRUIT_SPECIAL_FORCES")',
        f"(symbols . ({symbols_expr}))",
    ]
    if normalized_founder_keys:
        founder_keys_expr = " ".join(f'"{founder_key}"' for founder_key in normalized_founder_keys)
        parts.append(f"(founder_keys . ({founder_keys_expr}))")
    if recruit_limit is not None:
        parts.append(f"(recruit_limit . {int(recruit_limit)})")
    parts.extend(
        [
            f"(disable_preflight . {_sexp_bool(disable_preflight)})",
            f"(bypass_graveyard . {_sexp_bool(bypass_graveyard)})",
            f"(flush_phase1 . {_sexp_bool(flush_phase1)})",
        ]
    )
    if flush_limit is not None:
        if flush_limit < 0:
            raise ValueError("flush_limit must be >= 0")
        parts.append(f"(flush_limit . {int(flush_limit)})")
    return " ".join(parts) + ")"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Trigger RECRUIT_SPECIAL_FORCES for selected symbols."
    )
    parser.add_argument(
        "--endpoint",
        default=DEFAULT_ENDPOINT,
        help=f"Brain sensory endpoint (default: {DEFAULT_ENDPOINT})",
    )
    parser.add_argument(
        "--symbols",
        nargs="+",
        default=DEFAULT_SYMBOLS,
        help=f"Symbols to recruit for (default: {' '.join(DEFAULT_SYMBOLS)})",
    )
    parser.add_argument(
        "--founder-keys",
        nargs="+",
        default=None,
        help="Optional founder key filter (e.g., hunted-d1-adx hunted-w1-ema).",
    )
    parser.add_argument(
        "--recruit-limit",
        type=int,
        default=None,
        help="Optional max founder recruit attempts for this dispatch.",
    )
    parser.add_argument(
        "--disable-preflight",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="Disable founder preflight screen for this dispatch (default: false).",
    )
    parser.add_argument(
        "--bypass-graveyard",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="Bypass founder graveyard-pattern rejection for this dispatch (default: false).",
    )
    parser.add_argument(
        "--flush-phase1",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="Whether to flush deferred Phase1 immediately (default: true).",
    )
    parser.add_argument(
        "--flush-limit",
        type=int,
        default=None,
        help="Optional Phase1 deferred flush limit.",
    )
    parser.add_argument(
        "--connect-wait",
        type=float,
        default=DEFAULT_CONNECT_WAIT_SEC,
        help=f"Seconds to wait after connect (default: {DEFAULT_CONNECT_WAIT_SEC})",
    )
    return parser.parse_args()


def send_recruit_special_forces(
    endpoint: str,
    symbols: Iterable[str],
    founder_keys: Iterable[str] | None = None,
    recruit_limit: int | None = None,
    disable_preflight: bool = False,
    bypass_graveyard: bool = False,
    flush_phase1: bool = True,
    flush_limit: int | None = None,
    connect_wait: float = DEFAULT_CONNECT_WAIT_SEC,
) -> None:
    message = build_recruit_special_forces_message(
        symbols=symbols,
        founder_keys=founder_keys,
        recruit_limit=recruit_limit,
        disable_preflight=disable_preflight,
        bypass_graveyard=bypass_graveyard,
        flush_phase1=flush_phase1,
        flush_limit=flush_limit,
    )
    context = zmq.Context()
    socket = context.socket(zmq.PUSH)
    socket.connect(endpoint)
    try:
        time.sleep(max(0.0, connect_wait))
        socket.send_string(message)
        print(
            f"[SEND] RECRUIT_SPECIAL_FORCES symbols={list(symbols)} "
            f"founder_keys={list(founder_keys) if founder_keys else None} "
            f"recruit_limit={recruit_limit} "
            f"disable_preflight={disable_preflight} bypass_graveyard={bypass_graveyard} "
            f"flush_phase1={flush_phase1} flush_limit={flush_limit}"
        )
    finally:
        socket.close(0)
        context.term()


def main() -> int:
    args = parse_args()
    try:
        send_recruit_special_forces(
            endpoint=args.endpoint,
            symbols=args.symbols,
            founder_keys=args.founder_keys,
            recruit_limit=args.recruit_limit,
            disable_preflight=args.disable_preflight,
            bypass_graveyard=args.bypass_graveyard,
            flush_phase1=args.flush_phase1,
            flush_limit=args.flush_limit,
            connect_wait=args.connect_wait,
        )
    except ValueError as exc:
        print(f"[ERROR] {exc}")
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
