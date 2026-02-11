#!/usr/bin/env python3
"""
Send one-off CPCV_VALIDATE smoke messages to Guardian PUB/SUB command channel.

Examples:
  python3 tools/ops/cpcv_smoke.py --mode runtime
  python3 tools/ops/cpcv_smoke.py --mode criteria --send-count 3
  python3 tools/ops/cpcv_smoke.py --mode runtime --dry-run
"""

from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path


DEFAULT_ENDPOINT = "tcp://127.0.0.1:5559"
DEFAULT_RUNTIME_CANDLES = "/tmp/does-not-exist.csv"


def repo_root() -> Path:
    return Path(__file__).resolve().parents[2]


def sexp_escape(value: str) -> str:
    return value.replace("\\", "\\\\").replace('"', '\\"')


def build_name(mode: str, ts: int) -> str:
    prefix = "MANUAL-RUNTIME" if mode == "runtime" else "MANUAL-CRITERIA"
    return f"{prefix}-{ts}"


def build_request_id(mode: str, ts: int) -> str:
    prefix = "RID-MANUAL-RUNTIME" if mode == "runtime" else "RID-MANUAL-CRITERIA"
    return f"{prefix}-{ts}"


def build_default_criteria_candles(symbol: str) -> str:
    return str(repo_root() / "data" / "historical" / f"{symbol}_M1.csv")


def build_strategy_params(mode: str, name: str) -> str:
    if mode == "runtime":
        return "NIL"
    safe_name = sexp_escape(name)
    return (
        f'((name . "{safe_name}") '
        "(sma_short . 2) "
        "(sma_long . 400) "
        "(sl . 8.0) "
        "(tp . 8.0) "
        "(volume . 0.01) "
        '(indicator_type . "sma"))'
    )


def build_message(
    *,
    mode: str,
    symbol: str,
    strategy_name: str,
    request_id: str,
    candles_file: str,
) -> str:
    safe_name = sexp_escape(strategy_name)
    safe_symbol = sexp_escape(symbol)
    safe_candles = sexp_escape(candles_file)
    safe_rid = sexp_escape(request_id)
    params = build_strategy_params(mode, strategy_name)
    return (
        f'((action . "CPCV_VALIDATE") '
        f'(strategy_name . "{safe_name}") '
        f'(symbol . "{safe_symbol}") '
        f'(candles_file . "{safe_candles}") '
        f'(request_id . "{safe_rid}") '
        f"(strategy_params . {params}))"
    )


def parse_args(argv: list[str]) -> argparse.Namespace:
    ap = argparse.ArgumentParser(description="CPCV smoke sender")
    ap.add_argument(
        "--mode",
        choices=("runtime", "criteria"),
        required=True,
        help="runtime=ERROR path, criteria=FAILED path",
    )
    ap.add_argument("--symbol", default="USDJPY", help="symbol for CPCV request")
    ap.add_argument("--endpoint", default=DEFAULT_ENDPOINT, help="Guardian command endpoint")
    ap.add_argument("--strategy-name", default="", help="override strategy_name")
    ap.add_argument("--request-id", default="", help="override request_id")
    ap.add_argument(
        "--candles-file",
        default="",
        help="override candles_file (defaults by mode)",
    )
    ap.add_argument("--send-count", type=int, default=1, help="number of sends")
    ap.add_argument(
        "--interval-sec",
        type=float,
        default=0.4,
        help="interval between sends",
    )
    ap.add_argument(
        "--settle-sec",
        type=float,
        default=1.0,
        help="PUB connect settle time before first send",
    )
    ap.add_argument("--dry-run", action="store_true", help="print message without sending")
    return ap.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv or sys.argv[1:])
    if args.send_count < 1:
        print("--send-count must be >= 1", file=sys.stderr)
        return 2
    if args.interval_sec < 0 or args.settle_sec < 0:
        print("--interval-sec and --settle-sec must be >= 0", file=sys.stderr)
        return 2

    ts = int(time.time())
    strategy_name = args.strategy_name or build_name(args.mode, ts)
    request_id = args.request_id or build_request_id(args.mode, ts)

    if args.candles_file:
        candles_file = args.candles_file
    elif args.mode == "runtime":
        candles_file = DEFAULT_RUNTIME_CANDLES
    else:
        candles_file = build_default_criteria_candles(args.symbol)

    msg = build_message(
        mode=args.mode,
        symbol=args.symbol,
        strategy_name=strategy_name,
        request_id=request_id,
        candles_file=candles_file,
    )

    print(f"mode={args.mode}")
    print(f"strategy_name={strategy_name}")
    print(f"request_id={request_id}")
    print(f"endpoint={args.endpoint}")
    print(msg)

    if args.dry_run:
        return 0

    try:
        import zmq  # type: ignore
    except Exception as e:
        print(f"failed to import zmq: {e}", file=sys.stderr)
        return 3

    ctx = zmq.Context.instance()
    pub = ctx.socket(zmq.PUB)
    pub.connect(args.endpoint)
    try:
        time.sleep(args.settle_sec)
        for i in range(args.send_count):
            pub.send_string(msg)
            if i + 1 < args.send_count:
                time.sleep(args.interval_sec)
        print(f"sent={args.send_count}")
        return 0
    finally:
        pub.close(0)


if __name__ == "__main__":
    raise SystemExit(main())
