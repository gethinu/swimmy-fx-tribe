#!/usr/bin/env python3
"""Live performance report for xau_autobot from MT5 deal history."""

from __future__ import annotations

import argparse
import json
import os
import time
import urllib.error
import urllib.request
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
DEAL_REASON_SL = 4
DEAL_REASON_TP = 5
DEFAULT_RUNTIME_JOURNAL_PATH = "data/reports/xau_autobot_runtime_journal_latest.jsonl"
REPO_ROOT = Path(__file__).resolve().parent.parent


@dataclass
class PositionAggregate:
    position_id: int
    net_profit: float = 0.0
    close_time: int = 0
    deal_count: int = 0
    exit_deals: int = 0
    close_reason_code: int = 0
    close_reason: str = "OTHER"


def _normalize_close_reason(reason_code: int) -> str:
    if int(reason_code) == DEAL_REASON_TP:
        return "TP"
    if int(reason_code) == DEAL_REASON_SL:
        return "SL"
    return "OTHER"


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


def _is_tradable_deal_for_symbol(
    deal: Any,
    *,
    symbol: str,
) -> bool:
    symbol_filter = (symbol or "").strip()
    deal_symbol = str(_deal_value(deal, "symbol", "")).upper()
    if symbol_filter and symbol_filter != "*" and deal_symbol != symbol_filter.upper():
        return False

    deal_type = _to_int(_deal_value(deal, "type", -1), default=-1)
    if deal_type not in (BUY_DEAL_TYPE, SELL_DEAL_TYPE):
        return False
    return True


def _is_seed_deal_for_strategy(
    deal: Any,
    *,
    symbol: str,
    magic: int,
    comment_prefix: str,
) -> bool:
    if not _is_tradable_deal_for_symbol(deal, symbol=symbol):
        return False
    if int(magic) >= 0:
        deal_magic = _to_int(_deal_value(deal, "magic", 0), default=-1)
        if deal_magic != int(magic):
            return False
    return _comment_matches(deal, comment_prefix)


def _comment_matches(deal: Any, comment_prefix: str) -> bool:
    if not comment_prefix:
        return True
    comment = str(_deal_value(deal, "comment", ""))
    if comment.startswith(comment_prefix):
        return True
    # MT5 comment fields may truncate long prefixes on some brokers/servers.
    # Accept the truncated deal comment when it is still a prefix of the configured comment.
    if comment and comment_prefix.startswith(comment):
        return True
    return False


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
    target_positions: set[int] = set()

    for deal in deals:
        if not _is_seed_deal_for_strategy(
            deal,
            symbol=symbol,
            magic=magic,
            comment_prefix=comment_prefix,
        ):
            continue

        position_id = _position_id_for_deal(deal)
        if position_id <= 0:
            continue
        target_positions.add(position_id)

    for deal in deals:
        if not _is_tradable_deal_for_symbol(deal, symbol=symbol):
            continue
        position_id = _position_id_for_deal(deal)
        if position_id <= 0 or position_id not in target_positions:
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
                deal_reason = _to_int(_deal_value(deal, "reason", 0), default=0)
                agg.close_reason_code = int(deal_reason)
                agg.close_reason = _normalize_close_reason(deal_reason)

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
                "close_reason_code": int(item.close_reason_code),
                "close_reason": str(item.close_reason),
            }
        )
    return out


def build_filter_diagnostics(
    *,
    deals: Sequence[Any],
    symbol: str,
    magic: int,
    comment_prefix: str,
) -> Dict[str, Any]:
    tradable: List[Any] = []
    for deal in deals:
        deal_type = _to_int(_deal_value(deal, "type", -1), default=-1)
        if deal_type in (BUY_DEAL_TYPE, SELL_DEAL_TYPE):
            tradable.append(deal)

    symbol_filter = (symbol or "").strip()
    symbol_matched = []
    for deal in tradable:
        ds = str(_deal_value(deal, "symbol", "")).upper()
        if not symbol_filter or symbol_filter == "*" or ds == symbol_filter.upper():
            symbol_matched.append(deal)

    magic_matched = []
    for deal in symbol_matched:
        if int(magic) < 0:
            magic_matched.append(deal)
            continue
        dm = _to_int(_deal_value(deal, "magic", 0), default=-1)
        if dm == int(magic):
            magic_matched.append(deal)

    comment_matched = []
    for deal in magic_matched:
        if _comment_matches(deal, comment_prefix):
            comment_matched.append(deal)

    symbol_counts: Dict[str, int] = {}
    magic_counts: Dict[str, int] = {}
    for deal in tradable:
        ds = str(_deal_value(deal, "symbol", "")).upper()
        symbol_counts[ds] = symbol_counts.get(ds, 0) + 1
        dm = str(_to_int(_deal_value(deal, "magic", 0), default=0))
        magic_counts[dm] = magic_counts.get(dm, 0) + 1

    top_symbols = sorted(symbol_counts.items(), key=lambda kv: kv[1], reverse=True)[:8]
    top_magics = sorted(magic_counts.items(), key=lambda kv: kv[1], reverse=True)[:8]

    return {
        "total_deals": float(len(deals)),
        "tradable_deals": float(len(tradable)),
        "after_symbol_filter": float(len(symbol_matched)),
        "after_magic_filter": float(len(magic_matched)),
        "after_comment_prefix_filter": float(len(comment_matched)),
        "top_symbols": [{"symbol": k, "count": float(v)} for k, v in top_symbols],
        "top_magics": [{"magic": k, "count": float(v)} for k, v in top_magics],
    }


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


def summarize_closed_positions(
    closed_positions: Sequence[Dict[str, Any]],
    *,
    window_days: float = 0.0,
) -> Dict[str, Any]:
    pnls = [float(item.get("net_profit", 0.0)) for item in closed_positions]
    wins = [x for x in pnls if x > 0.0]
    losses = [x for x in pnls if x < 0.0]
    gross_profit = sum(wins)
    gross_loss = sum(losses)
    closed_count = len(pnls)
    close_reason_counts = {"tp": 0.0, "sl": 0.0, "other": 0.0}
    for item in closed_positions:
        close_reason = str(item.get("close_reason", "OTHER")).upper()
        if close_reason == "TP":
            close_reason_counts["tp"] += 1.0
        elif close_reason == "SL":
            close_reason_counts["sl"] += 1.0
        else:
            close_reason_counts["other"] += 1.0

    profit_factor = 0.0
    if losses:
        profit_factor = gross_profit / abs(gross_loss)
    elif wins:
        profit_factor = 99.0

    closed_per_day = 0.0
    if window_days > 0.0:
        closed_per_day = float(closed_count) / float(window_days)

    tp_count = float(close_reason_counts.get("tp", 0.0))
    sl_count = float(close_reason_counts.get("sl", 0.0))
    tp_sl_ratio = 0.0
    if sl_count > 0.0:
        tp_sl_ratio = tp_count / sl_count
    elif tp_count > 0.0:
        tp_sl_ratio = 99.0

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
        "closed_per_day": float(closed_per_day),
        "close_reason_counts": close_reason_counts,
        "tp_sl_ratio": float(tp_sl_ratio),
    }


def build_discord_headers() -> Dict[str, str]:
    return {
        "Content-Type": "application/json",
        "User-Agent": "Mozilla/5.0 (compatible; xau-autobot-live-report/1.0)",
    }


def should_notify_threshold(*, closed_positions: float, threshold: int, state: Dict[str, Any]) -> bool:
    if threshold <= 0:
        return False
    if float(closed_positions) < float(threshold):
        return False
    thresholds = state.get("threshold_notified", {})
    if not isinstance(thresholds, dict):
        return True
    return str(threshold) not in thresholds


def update_notify_state(
    *,
    state: Dict[str, Any],
    threshold: int,
    closed_positions: float,
    now_utc: str,
) -> Dict[str, Any]:
    out = dict(state)
    thresholds = out.get("threshold_notified", {})
    if not isinstance(thresholds, dict):
        thresholds = {}
    thresholds[str(threshold)] = {
        "closed_positions": float(closed_positions),
        "notified_at": now_utc,
    }
    out["threshold_notified"] = thresholds
    return out


def _load_notify_state(path: Path) -> Dict[str, Any]:
    if not path.exists():
        return {}
    try:
        with path.open("r", encoding="utf-8") as f:
            data = json.load(f)
        return data if isinstance(data, dict) else {}
    except Exception:
        return {}


def _write_notify_state(path: Path, state: Dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as f:
        json.dump(state, f, ensure_ascii=True, indent=2)
        f.write("\n")


def _post_discord_webhook(webhook_url: str, payload: Dict[str, Any]) -> None:
    body = json.dumps(payload, ensure_ascii=True).encode("utf-8")
    req = urllib.request.Request(
        webhook_url,
        data=body,
        method="POST",
        headers=build_discord_headers(),
    )
    with urllib.request.urlopen(req, timeout=10):
        return


def _build_threshold_payload(
    *,
    threshold: int,
    output: Dict[str, Any],
) -> Dict[str, Any]:
    summary = output.get("summary", {}) if isinstance(output.get("summary"), dict) else {}
    closed = summary.get("closed_positions", 0.0)
    net_profit = summary.get("net_profit", 0.0)
    win_rate = summary.get("win_rate", 0.0)
    pf = summary.get("profit_factor", 0.0)
    period = f"{output.get('start_utc')} -> {output.get('end_utc')}"
    lines = [
        f"threshold={threshold} closed_positions={closed}",
        f"net_profit={net_profit} win_rate={win_rate} pf={pf}",
        f"window={period}",
    ]
    return {
        "embeds": [
            {
                "title": "XAU AutoBot Threshold Reached",
                "description": "\n".join(lines),
                "color": 3066993,
            }
        ]
    }


def _resolve_notify_webhook(cli_value: str) -> str:
    if cli_value.strip():
        return cli_value.strip()
    for key in ("SWIMMY_XAU_NOTIFY_WEBHOOK", "SWIMMY_DISCORD_REPORTS"):
        value = os.getenv(key, "").strip()
        if value:
            return value
    return ""


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


def _parse_utc_optional(value: Any) -> Optional[datetime]:
    text = str(value or "").strip()
    if not text:
        return None
    try:
        return _parse_utc(text)
    except Exception:
        return None


def _normalize_runtime_metrics(metrics: Any, *, payload: Optional[Dict[str, Any]] = None) -> Optional[Dict[str, Any]]:
    if not isinstance(metrics, dict):
        return None
    signal_eval_count = _to_int(metrics.get("signal_eval_count"), default=-1)
    gap_reject_count = _to_int(metrics.get("gap_reject_count"), default=-1)
    gate_check_count = _to_int(metrics.get("gate_check_count"), default=-1)
    gate_reject_gap_count = _to_int(metrics.get("gate_reject_gap_count"), default=-1)
    if signal_eval_count < 0:
        signal_eval_count = gate_check_count
    if gap_reject_count < 0:
        gap_reject_count = gate_reject_gap_count
    if gate_check_count < 0:
        gate_check_count = signal_eval_count
    if gate_reject_gap_count < 0:
        gate_reject_gap_count = gap_reject_count
    if gate_check_count < 0 or gate_reject_gap_count < 0:
        return None
    signal_counts_raw = metrics.get("signal_counts", {})
    signal_counts = signal_counts_raw if isinstance(signal_counts_raw, dict) else {}
    gap_reject_rate = _to_float(metrics.get("gap_reject_rate"), default=0.0)
    if gate_check_count > 0 and (gap_reject_rate <= 0.0):
        gap_reject_rate = float(gate_reject_gap_count) / float(gate_check_count)
    payload_obj = payload if isinstance(payload, dict) else {}
    snapshot_time_utc = str(
        metrics.get("snapshot_time_utc")
        or payload_obj.get("timestamp_utc")
        or payload_obj.get("timestamp")
        or payload_obj.get("generated_at")
        or ""
    )
    run_id = str(metrics.get("run_id") or payload_obj.get("run_id") or payload_obj.get("trial_run_id") or "")
    magic = _to_int(metrics.get("magic"), default=_to_int(payload_obj.get("magic"), default=-1))
    schema_version = _to_int(metrics.get("schema_version"), default=1)
    return {
        "signal_eval_count": int(signal_eval_count),
        "gap_reject_count": int(gap_reject_count),
        "spread_reject_count": int(_to_int(metrics.get("spread_reject_count"), 0)),
        "session_reject_count": int(_to_int(metrics.get("session_reject_count"), 0)),
        "maxpos_reject_count": int(_to_int(metrics.get("maxpos_reject_count"), 0)),
        "gate_check_count": int(gate_check_count),
        "gate_reject_gap_count": int(gate_reject_gap_count),
        "gap_reject_rate": float(gap_reject_rate),
        "signal_counts": {
            "BUY": int(_to_int(signal_counts.get("BUY"), 0)),
            "SELL": int(_to_int(signal_counts.get("SELL"), 0)),
            "HOLD": int(_to_int(signal_counts.get("HOLD"), 0)),
        },
        "snapshot_time_utc": snapshot_time_utc,
        "run_id": run_id,
        "magic": int(magic),
        "schema_version": int(schema_version),
    }


def _runtime_snapshot_matches(
    snapshot: Dict[str, Any],
    *,
    expected_run_id: str,
    expected_magic: int,
) -> bool:
    if expected_run_id:
        if str(snapshot.get("run_id", "")).strip() != expected_run_id:
            return False
    if expected_magic >= 0:
        if _to_int(snapshot.get("magic"), default=-1) != int(expected_magic):
            return False
    return True


def load_latest_runtime_metrics(
    runtime_journal_path: Path,
    *,
    expected_run_id: str = "",
    expected_magic: int = -1,
) -> Optional[Dict[str, Any]]:
    path = Path(runtime_journal_path)
    if not path.exists() or not path.is_file():
        return None

    latest_index = -1
    latest_ts: Optional[datetime] = None
    latest_snapshot: Optional[Dict[str, Any]] = None
    with path.open("r", encoding="utf-8", errors="ignore") as f:
        for index, raw in enumerate(f):
            line = raw.strip()
            if not line.startswith("{"):
                continue
            try:
                payload = json.loads(line)
            except json.JSONDecodeError:
                continue
            if not isinstance(payload, dict):
                continue
            snapshot = _normalize_runtime_metrics(payload.get("runtime_metrics"), payload=payload)
            if snapshot is None:
                continue
            if not _runtime_snapshot_matches(
                snapshot,
                expected_run_id=str(expected_run_id or "").strip(),
                expected_magic=int(expected_magic),
            ):
                continue
            ts = (
                _parse_utc_optional(payload.get("timestamp_utc"))
                or _parse_utc_optional(payload.get("timestamp"))
                or _parse_utc_optional(payload.get("generated_at"))
            )
            if latest_snapshot is None:
                latest_snapshot = snapshot
                latest_ts = ts
                latest_index = index
                continue
            if ts is None:
                if latest_ts is None and index > latest_index:
                    latest_snapshot = snapshot
                    latest_index = index
                continue
            if latest_ts is None or ts >= latest_ts:
                latest_snapshot = snapshot
                latest_ts = ts
                latest_index = index
    return latest_snapshot


def resolve_runtime_journal_path(cli_value: str) -> Path:
    candidate = (
        str(cli_value or "").strip()
        or os.getenv("XAU_AUTOBOT_RUNTIME_JOURNAL_PATH", "").strip()
        or DEFAULT_RUNTIME_JOURNAL_PATH
    )
    path = Path(candidate).expanduser()
    if not path.is_absolute():
        path = REPO_ROOT / path
    return path


def _fetch_mt5_deals(*, start_utc: datetime, end_utc: datetime) -> List[Any]:
    if mt5 is None:
        raise RuntimeError("MetaTrader5 Python package is missing. Install with `pip install MetaTrader5`.")
    max_init_attempts = 3
    initialized = False
    for attempt in range(max_init_attempts):
        if mt5.initialize():
            initialized = True
            break
        err = mt5.last_error()
        err_code = _to_int(err[0], default=0) if isinstance(err, (tuple, list)) and err else 0
        if err_code == -10005 and attempt + 1 < max_init_attempts:
            time.sleep(1.0)
            continue
        raise RuntimeError(f"mt5.initialize() failed: {err}")
    if not initialized:
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
        symbol_filter = (symbol or "").strip()
        if symbol_filter and symbol_filter != "*":
            positions = mt5.positions_get(symbol=symbol_filter)
        else:
            positions = mt5.positions_get()
        if not positions:
            return {"open_positions": 0.0, "open_volume": 0.0, "open_floating_profit": 0.0}

        count = 0
        volume = 0.0
        floating = 0.0
        for position in positions:
            if int(magic) >= 0:
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
    parser.add_argument("--comment-prefix", default="xau_autobot")
    parser.add_argument("--days", type=int, default=30)
    parser.add_argument("--start-utc", default="")
    parser.add_argument("--end-utc", default="")
    parser.add_argument("--include-details", action="store_true")
    parser.add_argument("--diagnostics", action="store_true")
    parser.add_argument("--notify-threshold-closed", type=int, default=0)
    parser.add_argument("--notify-webhook", default="")
    parser.add_argument("--notify-state-path", default="data/reports/xau_autobot_live_notify_state.json")
    parser.add_argument("--runtime-journal-path", default="")
    parser.add_argument("--run-id", default="")
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
    window_days = float((end_utc - start_utc).total_seconds() / 86400.0)
    summary = summarize_closed_positions(closed_positions, window_days=window_days)
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
    if str(args.run_id or "").strip():
        output["run_id"] = str(args.run_id).strip()
    runtime_journal_path = resolve_runtime_journal_path(args.runtime_journal_path)
    runtime_metrics = load_latest_runtime_metrics(
        runtime_journal_path,
        expected_run_id=str(args.run_id or "").strip(),
        expected_magic=int(args.magic),
    )
    if runtime_metrics is not None:
        output["runtime_metrics"] = runtime_metrics
        output["runtime_metrics_source"] = str(runtime_journal_path)
    if args.diagnostics:
        output["diagnostics"] = build_filter_diagnostics(
            deals=deals,
            symbol=args.symbol,
            magic=args.magic,
            comment_prefix=args.comment_prefix,
        )
    if args.include_details:
        output["closed_positions_detail"] = closed_positions

    notify_result: Dict[str, Any] = {"enabled": bool(args.notify_threshold_closed > 0), "notified": False}
    if args.notify_threshold_closed > 0:
        now_iso = datetime.now(timezone.utc).isoformat()
        webhook_url = _resolve_notify_webhook(args.notify_webhook)
        state_path = Path(args.notify_state_path)
        state = _load_notify_state(state_path)
        closed_value = float(summary.get("closed_positions", 0.0))
        if should_notify_threshold(
            closed_positions=closed_value,
            threshold=int(args.notify_threshold_closed),
            state=state,
        ):
            if webhook_url:
                payload = _build_threshold_payload(
                    threshold=int(args.notify_threshold_closed),
                    output=output,
                )
                try:
                    _post_discord_webhook(webhook_url, payload)
                    state = update_notify_state(
                        state=state,
                        threshold=int(args.notify_threshold_closed),
                        closed_positions=closed_value,
                        now_utc=now_iso,
                    )
                    _write_notify_state(state_path, state)
                    notify_result.update(
                        {
                            "notified": True,
                            "threshold": int(args.notify_threshold_closed),
                            "closed_positions": closed_value,
                            "state_path": str(state_path),
                        }
                    )
                except (urllib.error.URLError, RuntimeError, OSError) as exc:
                    notify_result.update(
                        {
                            "notified": False,
                            "threshold": int(args.notify_threshold_closed),
                            "closed_positions": closed_value,
                            "error": str(exc),
                        }
                    )
            else:
                notify_result.update(
                    {
                        "notified": False,
                        "threshold": int(args.notify_threshold_closed),
                        "closed_positions": closed_value,
                        "error": "webhook_missing",
                    }
                )
        else:
            notify_result.update(
                {
                    "notified": False,
                    "threshold": int(args.notify_threshold_closed),
                    "closed_positions": closed_value,
                    "reason": "threshold_not_reached_or_already_notified",
                }
            )
    if notify_result.get("enabled"):
        output["notification"] = notify_result

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
