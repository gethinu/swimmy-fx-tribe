#!/usr/bin/env python3
"""
Send ORDER_OPEN smoke cases to Guardian external command input (S-expression).

Default flow:
  PUB -> tcp://127.0.0.1:5559 (Guardian external SXP command channel)
  Optional echo check:
    SUB <- tcp://127.0.0.1:5560 (Guardian -> MT5 execution channel)

Examples:
  .venv/bin/python3 tools/ops/order_open_smoke.py --dry-run
  .venv/bin/python3 tools/ops/order_open_smoke.py --cases invalid-all missing-instrument invalid-comment
  .venv/bin/python3 tools/ops/order_open_smoke.py --cases all --allow-live-order
"""

from __future__ import annotations

import argparse
import sys
import time
from dataclasses import dataclass


DEFAULT_ENDPOINT = "tcp://127.0.0.1:5559"
DEFAULT_ECHO_ENDPOINT = "tcp://127.0.0.1:5560"
DEFAULT_SETTLE_SEC = 1.0
DEFAULT_INTERVAL_SEC = 0.35
DEFAULT_FORWARD_TIMEOUT_SEC = 1.5
DEFAULT_SEND_COUNT = 3
DEFAULT_SYMBOL = "USDJPY"
DEFAULT_SIDE = "BUY"
DEFAULT_LOT = 0.01
DEFAULT_SL = 1.0
DEFAULT_TP = 2.0
DEFAULT_MAGIC_BASE = 990000
DEFAULT_COMMENT_TF = "H1"

CASE_ALL = "all"
CASE_INVALID_ALL = "invalid-all"
CASE_MISSING_INSTRUMENT = "missing-instrument"
CASE_INVALID_COMMENT = "invalid-comment"
CASE_VALID_ORDER = "valid-order"
ALL_CASES = [CASE_INVALID_ALL, CASE_MISSING_INSTRUMENT, CASE_INVALID_COMMENT, CASE_VALID_ORDER]


@dataclass(frozen=True)
class SmokeCase:
    name: str
    order_id: str
    payload: str
    expected: str
    requires_live_order: bool = False


def sexp_escape(value: str) -> str:
    return value.replace("\\", "\\\\").replace('"', '\\"')


def build_order_open(
    *,
    order_id: str,
    side: str,
    instrument: str,
    lot: float,
    sl: float,
    tp: float,
    magic: int,
    comment: str,
) -> str:
    side_norm = side.upper().strip()
    return (
        f'((type . "ORDER_OPEN") '
        f'(id . "{sexp_escape(order_id)}") '
        f'(side . "{sexp_escape(side_norm)}") '
        f'(instrument . "{sexp_escape(instrument)}") '
        f"(lot . {float(lot):.2f}) "
        f"(sl . {float(sl):.5f}) "
        f"(tp . {float(tp):.5f}) "
        f"(magic . {int(magic)}) "
        f'(comment . "{sexp_escape(comment)}"))'
    )


def build_smoke_cases(
    *,
    symbol: str,
    side: str,
    lot: float,
    sl: float,
    tp: float,
    magic_base: int,
    comment_tf: str,
    run_id: str,
) -> list[SmokeCase]:
    strategy = "SMOKE-ORDER-OPEN"
    valid_comment = f"{strategy}|{comment_tf}"

    invalid_all_id = f"{run_id}-INVALID-ALL"
    missing_id = f"{run_id}-MISSING-INSTRUMENT"
    invalid_comment_id = f"{run_id}-INVALID-COMMENT"
    valid_id = f"{run_id}-VALID"

    return [
        SmokeCase(
            name=CASE_INVALID_ALL,
            order_id=invalid_all_id,
            expected="ORDER_REJECT reason=INVALID_INSTRUMENT",
            payload=build_order_open(
                order_id=invalid_all_id,
                side=side,
                instrument="ALL",
                lot=lot,
                sl=sl,
                tp=tp,
                magic=magic_base + 1,
                comment=valid_comment,
            ),
        ),
        SmokeCase(
            name=CASE_MISSING_INSTRUMENT,
            order_id=missing_id,
            expected="ORDER_REJECT reason=MISSING_INSTRUMENT",
            payload=build_order_open(
                order_id=missing_id,
                side=side,
                instrument="NIL",
                lot=lot,
                sl=sl,
                tp=tp,
                magic=magic_base + 2,
                comment=valid_comment,
            ),
        ),
        SmokeCase(
            name=CASE_INVALID_COMMENT,
            order_id=invalid_comment_id,
            expected="ORDER_REJECT reason=INVALID_COMMENT_FORMAT",
            payload=build_order_open(
                order_id=invalid_comment_id,
                side=side,
                instrument=symbol,
                lot=lot,
                sl=sl,
                tp=tp,
                magic=magic_base + 3,
                comment="StrategyOnly",
            ),
        ),
        SmokeCase(
            name=CASE_VALID_ORDER,
            order_id=valid_id,
            expected="ORDER_ACK (or broker-side ORDER_REJECT)",
            requires_live_order=True,
            payload=build_order_open(
                order_id=valid_id,
                side=side,
                instrument=symbol,
                lot=lot,
                sl=sl,
                tp=tp,
                magic=magic_base + 4,
                comment=valid_comment,
            ),
        ),
    ]


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="ORDER_OPEN sink-guard smoke sender")
    parser.add_argument(
        "--endpoint",
        default=DEFAULT_ENDPOINT,
        help=f"Guardian command endpoint (default: {DEFAULT_ENDPOINT})",
    )
    parser.add_argument(
        "--echo-endpoint",
        default=DEFAULT_ECHO_ENDPOINT,
        help=f"Optional forward-check endpoint (default: {DEFAULT_ECHO_ENDPOINT})",
    )
    parser.add_argument(
        "--cases",
        nargs="+",
        choices=[CASE_ALL] + ALL_CASES,
        default=[CASE_ALL],
        help="Cases to send (default: all).",
    )
    parser.add_argument("--symbol", default=DEFAULT_SYMBOL, help="Instrument for valid/invalid-comment cases.")
    parser.add_argument("--side", choices=["BUY", "SELL"], default=DEFAULT_SIDE)
    parser.add_argument("--lot", type=float, default=DEFAULT_LOT)
    parser.add_argument("--sl", type=float, default=DEFAULT_SL)
    parser.add_argument("--tp", type=float, default=DEFAULT_TP)
    parser.add_argument("--comment-tf", default=DEFAULT_COMMENT_TF, help="TF token for valid comment.")
    parser.add_argument("--magic-base", type=int, default=DEFAULT_MAGIC_BASE)
    parser.add_argument(
        "--settle-sec",
        type=float,
        default=DEFAULT_SETTLE_SEC,
        help=f"PUB/SUB settle time seconds (default: {DEFAULT_SETTLE_SEC})",
    )
    parser.add_argument(
        "--interval-sec",
        type=float,
        default=DEFAULT_INTERVAL_SEC,
        help=f"Seconds between repeated sends (default: {DEFAULT_INTERVAL_SEC})",
    )
    parser.add_argument(
        "--send-count",
        type=int,
        default=DEFAULT_SEND_COUNT,
        help=f"Send each case N times to reduce PUB/SUB slow-join drops (default: {DEFAULT_SEND_COUNT})",
    )
    parser.add_argument(
        "--forward-timeout-sec",
        type=float,
        default=DEFAULT_FORWARD_TIMEOUT_SEC,
        help=f"Seconds to wait for forwarded echo on 5560 (default: {DEFAULT_FORWARD_TIMEOUT_SEC})",
    )
    parser.add_argument(
        "--allow-live-order",
        action="store_true",
        help="Allow sending the valid-order case (may open a real position).",
    )
    parser.add_argument(
        "--no-echo-check",
        action="store_true",
        help="Disable forwarded echo check on 5560.",
    )
    parser.add_argument("--dry-run", action="store_true", help="Print payloads only.")
    return parser.parse_args(argv)


def expand_case_selection(case_args: list[str]) -> list[str]:
    if CASE_ALL in case_args:
        return list(ALL_CASES)
    seen = set()
    ordered = []
    for name in case_args:
        if name not in seen:
            seen.add(name)
            ordered.append(name)
    return ordered


def wait_for_forwarded_id(sub_socket, order_id: str, timeout_sec: float) -> bool:
    if timeout_sec <= 0:
        return False
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        try:
            remaining_ms = int(max(1.0, (deadline - time.monotonic()) * 1000.0))
            if sub_socket.poll(remaining_ms) == 0:
                continue
            raw = sub_socket.recv(flags=0)
            msg = raw.decode("utf-8", errors="ignore")
            if order_id in msg and "ORDER_OPEN" in msg:
                return True
        except Exception:
            continue
    return False


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv or sys.argv[1:])
    if args.settle_sec < 0 or args.interval_sec < 0 or args.forward_timeout_sec < 0:
        print("timing args must be >= 0", file=sys.stderr)
        return 2
    if args.send_count < 1:
        print("--send-count must be >= 1", file=sys.stderr)
        return 2
    if args.lot <= 0:
        print("--lot must be > 0", file=sys.stderr)
        return 2

    selected_names = expand_case_selection(args.cases)
    run_id = f"ORDER-SMOKE-{int(time.time())}"
    all_cases = build_smoke_cases(
        symbol=args.symbol,
        side=args.side,
        lot=args.lot,
        sl=args.sl,
        tp=args.tp,
        magic_base=args.magic_base,
        comment_tf=args.comment_tf,
        run_id=run_id,
    )
    cases_by_name = {case.name: case for case in all_cases}

    selected_cases: list[SmokeCase] = []
    skipped_live: list[str] = []
    for name in selected_names:
        case = cases_by_name[name]
        if case.requires_live_order and not args.allow_live_order:
            skipped_live.append(case.name)
            continue
        selected_cases.append(case)

    print(f"run_id={run_id}")
    print(f"endpoint={args.endpoint}")
    print(f"echo_check={'off' if args.no_echo_check else 'on'}")
    print(f"send_count={args.send_count}")
    if skipped_live:
        print(f"skipped_live={','.join(skipped_live)} (use --allow-live-order)")
    if not selected_cases:
        print("no cases selected to send")
        return 0

    for case in selected_cases:
        print(f"[case] {case.name} id={case.order_id} expected={case.expected}")
        print(case.payload)
    if args.dry_run:
        return 0

    try:
        import zmq  # type: ignore
    except Exception as exc:
        print(f"failed to import zmq: {exc}", file=sys.stderr)
        return 3

    ctx = zmq.Context.instance()
    pub = ctx.socket(zmq.PUB)
    pub.connect(args.endpoint)

    sub = None
    if not args.no_echo_check:
        sub = ctx.socket(zmq.SUB)
        sub.setsockopt_string(zmq.SUBSCRIBE, "")
        sub.connect(args.echo_endpoint)

    results: list[tuple[str, str, bool | None]] = []
    try:
        time.sleep(args.settle_sec)
        for i, case in enumerate(selected_cases):
            forwarded = None
            for rep in range(args.send_count):
                pub.send_string(case.payload)
                if sub is not None:
                    if forwarded is None:
                        forwarded = False
                    if not forwarded:
                        forwarded = wait_for_forwarded_id(sub, case.order_id, args.forward_timeout_sec)
                if rep + 1 < args.send_count:
                    time.sleep(args.interval_sec)
            results.append((case.name, case.order_id, forwarded))
            if i + 1 < len(selected_cases):
                time.sleep(args.interval_sec)
    finally:
        pub.close(0)
        if sub is not None:
            sub.close(0)

    print("summary:")
    for name, order_id, forwarded in results:
        forwarded_label = "n/a" if forwarded is None else ("yes" if forwarded else "no")
        print(f"  - {name}: id={order_id} forwarded_to_5560={forwarded_label}")

    print("next: check MT5 Journal/Experts for ORDER_ACK/ORDER_REJECT with the ids above.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
