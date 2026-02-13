#!/usr/bin/env python3
"""Live performance report for xau_autobot from MT5 deal history."""

from __future__ import annotations

import argparse
import json
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Sequence

try:
    import MetaTrader5 as mt5  # type: ignore
except Exception:
    mt5 = None


BUY_DEAL_TYPE = 0
SELL_DEAL_TYPE = 1
EXIT_ENTRIES = {1, 2, 3}


@dataclass
class PositionAggregate:
    position_id: int
    net_profit: float = 0.0
    close_time: int = 0
    deal_count: int = 0
    exit_deals: int = 0


def _deal_value(deal: Any, key: str, default: Any = None) -> Any:
    if isinstance(deal, dict):
        return deal.get(key, default)
    return getattr(deal, key, default)


def _to_int(value: Any, default: int = 0) -> int:
    try:
        if value is None:
            return default
        return int(value)
    except Exception:
        return default


def _to_float(value: Any, default: float = 0.0) -> float:
    try:
        if value is None:
            return default
        return float(value)
    except Exception:
        return default


def _is_strategy_deal(
    deal: Any,
    *,
    symbol: str,
    magic: int,
    comment_prefix: str,
) -> bool:
    deal_symbol = str(_deal_value(deal, "symbol", "")).upper()
    if deal_symbol != symbol.upper():
        return False

    deal_magic = _to_int(_deal_value(deal, "magic", 0), default=-1)
    if deal_magic != int(magic):
        return False

    if comment_prefix:
        comment = str(_deal_value(deal, "comment", ""))
        if not comment.startswith(comment_prefix):
            return False

    deal_type = _to_int(_deal_value(deal, "type", -1), default=-1)
    if deal_type not in (BUY_DEAL_TYPE, SELL_DEAL_TYPE):
        return False
    return True


def _position_id_for_deal(deal: Any) -> int:
    position_id = _to_int(_deal_value(deal, "position_id", 0), default=0)
    if position_id > 0:
        return position_id
    order_id = _to_int(_deal_value(deal, "order", 0), default=0)
    if order_id > 0:
        return order_id
    return _to_int(_deal_value(deal, "ticket", 0), default=0)


def aggregate_closed_positions(
    *,
    deals: Sequence[Any],
    symbol: str,
    magic: int,
    comment_prefix: str,
) -> List[Dict[str, Any]]:
    by_position: Dict[int, PositionAggregate] = {}

    for deal in deals:
        if not _is_strategy_deal(deal, symbol=symbol, magic=magic, comment_prefix=comment_prefix):
            continue

        position_id = _position_id_for_deal(deal)
        if position_id <= 0:
            continue

        agg = by_position.get(position_id)
        if agg is None:
            agg = PositionAggregate(position_id=position_id)
            by_position[position_id] = agg

        profit = _to_float(_deal_value(deal, "profit", 0.0))
        swap = _to_float(_deal_value(deal, "swap", 0.0))
        commission = _to_float(_deal_value(deal, "commission", 0.0))
        fee = _to_float(_deal_value(deal, "fee", 0.0))
        net = profit + swap + commission + fee

        agg.net_profit += net
        agg.deal_count += 1

        entry = _to_int(_deal_value(deal, "entry", 0), default=0)
        if entry in EXIT_ENTRIES:
            agg.exit_deals += 1
            t = _to_int(_deal_value(deal, "time", 0), default=0)
            if t > agg.close_time:
                agg.close_time = t

    closed = [item for item in by_position.values() if item.exit_deals > 0]
    closed.sort(key=lambda x: x.close_time)

    out: List[Dict[str, Any]] = []
    for item in closed:
        out.append(
            {
                "position_id": int(item.position_id),
                "close_time": int(item.close_time),
                "net_profit": float(item.net_profit),
                "deal_count": int(item.deal_count),
                "exit_deals": int(item.exit_deals),
            }
        )
    return out


def _max_drawdown_abs(pnls: Iterable[float]) -> float:
    equity = 0.0
    peak = 0.0
    max_dd = 0.0
    for pnl in pnls:
        equity += pnl
        if equity > peak:
            peak = equity
        dd = peak - equity
        if dd > max_dd:
            max_dd = dd
    return max_dd


def summarize_closed_positions(closed_positions: Sequence[Dict[str, Any]]) -> Dict[str, float]:
    pnls = [float(item.get("net_profit", 0.0)) for item in closed_positions]
    wins = [x for x in pnls if x > 0.0]
    losses = [x for x in pnls if x < 0.0]
    gross_profit = sum(wins)
    gross_loss = sum(losses)
    closed_count = len(pnls)

    profit_factor = 0.0
    if losses:
        profit_factor = gross_profit / abs(gross_loss)
    elif wins:
        profit_factor = 99.0

    return {
        "closed_positions": float(closed_count),
        "win_rate": (float(len(wins)) / float(closed_count)) if closed_count > 0 else 0.0,
        "net_profit": float(sum(pnls)),
        "gross_profit": float(gross_profit),
        "gross_loss": float(gross_loss),
        "avg_win": (float(gross_profit) / float(len(wins))) if wins else 0.0,
        "avg_loss": (float(gross_loss) / float(len(losses))) if losses else 0.0,
        "profit_factor": float(profit_factor),
        "max_drawdown_abs": float(_max_drawdown_abs(pnls)),
    }


def _parse_utc(value: str) -> datetime:
    text = value.strip()
    if not text:
        raise ValueError("empty datetime")
    dt = datetime.fromisoformat(text.replace("Z", "+00:00"))
    if dt.tzinfo is None:
        dt = dt.replace(tzinfo=timezone.utc)
    else:
        dt = dt.astimezone(timezone.utc)
    return dt


def _fetch_mt5_deals(*, start_utc: datetime, end_utc: datetime) -> List[Any]:
    if mt5 is None:
        raise RuntimeError("MetaTrader5 Python package is missing. Install with `pip install MetaTrader5`.")
    if not mt5.initialize():
        raise RuntimeError(f"mt5.initialize() failed: {mt5.last_error()}")
    try:
        deals = mt5.history_deals_get(start_utc, end_utc)
        if deals is None:
            raise RuntimeError(f"mt5.history_deals_get() failed: {mt5.last_error()}")
        return list(deals)
    finally:
        mt5.shutdown()


def _fetch_open_positions_snapshot(*, symbol: str, magic: int, comment_prefix: str) -> Dict[str, float]:
    if mt5 is None:
        return {"open_positions": 0.0, "open_volume": 0.0, "open_floating_profit": 0.0}
    if not mt5.initialize():
        raise RuntimeError(f"mt5.initialize() failed: {mt5.last_error()}")
    try:
        positions = mt5.positions_get(symbol=symbol)
        if not positions:
            return {"open_positions": 0.0, "open_volume": 0.0, "open_floating_profit": 0.0}

        count = 0
        volume = 0.0
        floating = 0.0
        for position in positions:
            pos_magic = _to_int(getattr(position, "magic", 0), default=-1)
            if pos_magic != int(magic):
                continue
            if comment_prefix:
                comment = str(getattr(position, "comment", ""))
                if not comment.startswith(comment_prefix):
                    continue
            count += 1
            volume += _to_float(getattr(position, "volume", 0.0))
            floating += _to_float(getattr(position, "profit", 0.0))
        return {
            "open_positions": float(count),
            "open_volume": float(volume),
            "open_floating_profit": float(floating),
        }
    finally:
        mt5.shutdown()


def main() -> None:
    parser = argparse.ArgumentParser(description="Generate xau_autobot live performance report from MT5 history")
    parser.add_argument("--symbol", default="XAUUSD")
    parser.add_argument("--magic", type=int, default=560070)
    parser.add_argument("--comment-prefix", default="xau_autobot_tuned_auto")
    parser.add_argument("--days", type=int, default=30)
    parser.add_argument("--start-utc", default="")
    parser.add_argument("--end-utc", default="")
    parser.add_argument("--include-details", action="store_true")
    parser.add_argument("--write-report", default="")
    args = parser.parse_args()

    end_utc = _parse_utc(args.end_utc) if args.end_utc else datetime.now(timezone.utc)
    if args.start_utc:
        start_utc = _parse_utc(args.start_utc)
    else:
        start_utc = end_utc - timedelta(days=max(1, args.days))
    if start_utc >= end_utc:
        raise RuntimeError("start-utc must be earlier than end-utc")

    deals = _fetch_mt5_deals(start_utc=start_utc, end_utc=end_utc)
    closed_positions = aggregate_closed_positions(
        deals=deals,
        symbol=args.symbol,
        magic=args.magic,
        comment_prefix=args.comment_prefix,
    )
    summary = summarize_closed_positions(closed_positions)
    open_snapshot = _fetch_open_positions_snapshot(
        symbol=args.symbol,
        magic=args.magic,
        comment_prefix=args.comment_prefix,
    )

    output: Dict[str, Any] = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "symbol": args.symbol,
        "magic": int(args.magic),
        "comment_prefix": args.comment_prefix,
        "start_utc": start_utc.isoformat(),
        "end_utc": end_utc.isoformat(),
        "history_deals_scanned": float(len(deals)),
        "summary": summary,
        "open_snapshot": open_snapshot,
    }
    if args.include_details:
        output["closed_positions_detail"] = closed_positions

    print(json.dumps(output, ensure_ascii=True))

    if args.write_report:
        path = Path(args.write_report)
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("w", encoding="utf-8") as f:
            json.dump(output, f, ensure_ascii=True, indent=2)
            f.write("\n")
        print(json.dumps({"written_report": str(path)}, ensure_ascii=True))


if __name__ == "__main__":
    main()
