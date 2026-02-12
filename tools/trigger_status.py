#!/usr/bin/env python3
"""
trigger_status.py

Send STATUS_NOW S-expression commands to Brain's sensory port (5555, PULL).
This forces immediate status reports for selected symbols.

Examples:
    python3 tools/trigger_status.py \
      --pairs USDJPY:151.200 EURUSD:1.08200 GBPUSD:1.37100

    python3 tools/trigger_status.py \
      --symbols USDJPY EURUSD GBPUSD \
      --bids 151.200 1.08200 1.37100
"""

from __future__ import annotations

import argparse
import time
from typing import Iterable, List, Sequence, Tuple

import zmq

DEFAULT_ENDPOINT = "tcp://127.0.0.1:5555"
DEFAULT_SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]
DEFAULT_CONNECT_WAIT_SEC = 1.0
DEFAULT_SEND_INTERVAL_SEC = 0.2


def normalize_symbol(symbol: str) -> str:
    return symbol.strip().upper()


def parse_pair_spec(spec: str) -> Tuple[str, float]:
    if ":" not in spec:
        raise ValueError(f"invalid pair spec '{spec}' (expected SYMBOL:BID)")
    symbol_raw, bid_raw = spec.split(":", 1)
    symbol = normalize_symbol(symbol_raw)
    if not symbol:
        raise ValueError(f"invalid symbol in pair spec '{spec}'")
    try:
        bid = float(bid_raw)
    except ValueError as exc:
        raise ValueError(f"invalid bid in pair spec '{spec}'") from exc
    if bid <= 0.0:
        raise ValueError(f"bid must be positive in pair spec '{spec}'")
    return symbol, bid


def format_bid(bid: float) -> str:
    text = f"{bid:.10f}".rstrip("0").rstrip(".")
    return text if text else "0"


def build_status_now_message(symbol: str, bid: float) -> str:
    return (
        f'((type . "STATUS_NOW") (symbol . "{normalize_symbol(symbol)}") '
        f"(bid . {format_bid(bid)}))"
    )


def _parse_symbol_bid_lists(symbols: Sequence[str], bids: Sequence[str]) -> List[Tuple[str, float]]:
    if len(symbols) != len(bids):
        raise ValueError("--symbols and --bids must have the same length")
    pairs: List[Tuple[str, float]] = []
    for symbol, bid_text in zip(symbols, bids):
        symbol_norm = normalize_symbol(symbol)
        if not symbol_norm:
            raise ValueError("symbol must not be empty")
        try:
            bid = float(bid_text)
        except ValueError as exc:
            raise ValueError(f"invalid bid '{bid_text}' for symbol '{symbol_norm}'") from exc
        if bid <= 0.0:
            raise ValueError(f"bid must be positive for symbol '{symbol_norm}'")
        pairs.append((symbol_norm, bid))
    return pairs


def resolve_pairs(
    pairs_specs: Sequence[str] | None,
    symbols: Sequence[str],
    bids: Sequence[str] | None,
) -> List[Tuple[str, float]]:
    if pairs_specs:
        return [parse_pair_spec(spec) for spec in pairs_specs]
    if not bids:
        symbols_hint = " ".join(DEFAULT_SYMBOLS)
        raise ValueError(
            "missing bids: pass --pairs SYMBOL:BID ... "
            f"or --symbols {symbols_hint} --bids <bid...>"
        )
    return _parse_symbol_bid_lists(symbols, bids)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Send STATUS_NOW commands for one or more symbols."
    )
    parser.add_argument(
        "--endpoint",
        default=DEFAULT_ENDPOINT,
        help=f"Brain sensory endpoint (default: {DEFAULT_ENDPOINT})",
    )
    parser.add_argument(
        "--pairs",
        nargs="+",
        help='Symbol/bid pairs in "SYMBOL:BID" format.',
    )
    parser.add_argument(
        "--symbols",
        nargs="+",
        default=DEFAULT_SYMBOLS,
        help=f"Symbols list for --bids mode (default: {' '.join(DEFAULT_SYMBOLS)})",
    )
    parser.add_argument(
        "--bids",
        nargs="+",
        help="Bid list aligned with --symbols order.",
    )
    parser.add_argument(
        "--connect-wait",
        type=float,
        default=DEFAULT_CONNECT_WAIT_SEC,
        help=f"Seconds to wait after connect (default: {DEFAULT_CONNECT_WAIT_SEC})",
    )
    parser.add_argument(
        "--send-interval",
        type=float,
        default=DEFAULT_SEND_INTERVAL_SEC,
        help=f"Seconds between sends (default: {DEFAULT_SEND_INTERVAL_SEC})",
    )
    return parser.parse_args()


def send_status_now(
    endpoint: str,
    pairs: Iterable[Tuple[str, float]],
    connect_wait: float = DEFAULT_CONNECT_WAIT_SEC,
    send_interval: float = DEFAULT_SEND_INTERVAL_SEC,
) -> None:
    context = zmq.Context()
    socket = context.socket(zmq.PUSH)
    socket.connect(endpoint)

    try:
        time.sleep(max(0.0, connect_wait))
        print(f"[CONNECT] {endpoint}")
        for symbol, bid in pairs:
            payload = build_status_now_message(symbol, bid)
            socket.send_string(payload)
            print(f"[SEND] STATUS_NOW {symbol} bid={format_bid(bid)}")
            time.sleep(max(0.0, send_interval))
        print("[DONE] STATUS_NOW dispatch complete. Check notifier logs / Discord.")
    finally:
        socket.close(0)
        context.term()


def main() -> int:
    args = parse_args()
    try:
        pairs = resolve_pairs(args.pairs, args.symbols, args.bids)
    except ValueError as exc:
        print(f"[ERROR] {exc}")
        return 2

    send_status_now(
        endpoint=args.endpoint,
        pairs=pairs,
        connect_wait=args.connect_wait,
        send_interval=args.send_interval,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
