#!/usr/bin/env python3
"""Parameter optimizer for xau_autobot strategy using Yahoo Finance 5m data."""

from __future__ import annotations

import argparse
import json
from itertools import product
from statistics import mean
from typing import Dict, List, Sequence, Tuple, Union

try:
    from tools.xau_autobot_data import load_ohlc
except Exception:
    from xau_autobot_data import load_ohlc  # type: ignore

try:
    from tools.xau_autobot import validate_trade_comment
    from tools.xau_autobot import decide_signal_with_mode
    from tools.xau_autobot import validate_strategy_mode
except Exception:
    from xau_autobot import decide_signal_with_mode  # type: ignore
    from xau_autobot import validate_strategy_mode  # type: ignore
    from xau_autobot import validate_trade_comment  # type: ignore

Candidate = Union[
    Tuple[int, int, float, float, float, int, int, float, float],
    Tuple[int, int, float, float, float, int, int, float, float, str, float, float, float, float],
]


def interval_to_timeframe(interval: str) -> str:
    normalized = str(interval or "").strip().lower()
    mapping = {
        "1m": "M1",
        "5m": "M5",
        "15m": "M15",
        "30m": "M30",
        "60m": "H1",
        "1h": "H1",
        "240m": "H4",
        "4h": "H4",
    }
    timeframe = mapping.get(normalized, "")
    if timeframe == "":
        raise ValueError(
            f"unsupported interval for MT5 timeframe mapping: {interval} "
            "(supported: 1m,5m,15m,30m,60m,1h,240m,4h)"
        )
    return timeframe


def _unpack_candidate(candidate: Candidate) -> Dict[str, object]:
    if len(candidate) == 9:
        (
            fast,
            slow,
            pullback,
            sl_atr,
            tp_atr,
            session_start,
            session_end,
            min_ratio,
            max_ratio,
        ) = candidate
        return {
            "fast_ema": int(fast),
            "slow_ema": int(slow),
            "pullback_atr": float(pullback),
            "sl_atr": float(sl_atr),
            "tp_atr": float(tp_atr),
            "session_start_hour_utc": int(session_start),
            "session_end_hour_utc": int(session_end),
            "min_atr_ratio_to_median": float(min_ratio),
            "max_atr_ratio_to_median": float(max_ratio),
            "strategy_mode": "trend",
            "regime_trend_threshold": 1.2,
            "reversion_atr": 0.8,
            "reversion_sl_atr": 1.2,
            "reversion_tp_atr": 1.2,
        }

    (
        fast,
        slow,
        pullback,
        sl_atr,
        tp_atr,
        session_start,
        session_end,
        min_ratio,
        max_ratio,
        strategy_mode,
        regime_threshold,
        reversion_atr,
        reversion_sl_atr,
        reversion_tp_atr,
    ) = candidate
    return {
        "fast_ema": int(fast),
        "slow_ema": int(slow),
        "pullback_atr": float(pullback),
        "sl_atr": float(sl_atr),
        "tp_atr": float(tp_atr),
        "session_start_hour_utc": int(session_start),
        "session_end_hour_utc": int(session_end),
        "min_atr_ratio_to_median": float(min_ratio),
        "max_atr_ratio_to_median": float(max_ratio),
        "strategy_mode": str(strategy_mode),
        "regime_trend_threshold": float(regime_threshold),
        "reversion_atr": float(reversion_atr),
        "reversion_sl_atr": float(reversion_sl_atr),
        "reversion_tp_atr": float(reversion_tp_atr),
    }


def score_candidate(split_metrics: List[Dict[str, float]], min_oos_trades: int = 40) -> float:
    if not split_metrics:
        return -999.0
    worst_oos_total = min(m["oos_total"] for m in split_metrics)
    avg_oos_total = mean(m["oos_total"] for m in split_metrics)
    worst_is_total = min(m["is_total"] for m in split_metrics)
    avg_oos_pf = mean(m["oos_pf"] for m in split_metrics)
    avg_oos_dd = mean(m["oos_max_dd"] for m in split_metrics)

    trade_penalty = 0.0
    for m in split_metrics:
        if m["oos_trades"] < min_oos_trades:
            deficit = (min_oos_trades - m["oos_trades"]) / float(min_oos_trades)
            trade_penalty += deficit
    trade_penalty /= float(len(split_metrics))

    return (
        8.0 * worst_oos_total
        + 4.0 * avg_oos_total
        + 0.5 * (avg_oos_pf - 1.0)
        + 1.5 * worst_is_total
        - 2.0 * avg_oos_dd
        - 0.8 * trade_penalty
    )


def candidate_to_config(candidate: Candidate, *, magic: int, comment: str, timeframe: str = "M5") -> Dict[str, object]:
    unpacked = _unpack_candidate(candidate)
    normalized_comment = validate_trade_comment(comment)
    normalized_timeframe = str(timeframe or "").strip().upper()
    if normalized_timeframe not in {"M1", "M5", "M15", "M30", "H1", "H4"}:
        raise ValueError(f"unsupported timeframe: {timeframe}")
    return {
        "symbol": "XAUUSD",
        "timeframe": normalized_timeframe,
        "bars": 800,
        "fast_ema": int(unpacked["fast_ema"]),
        "slow_ema": int(unpacked["slow_ema"]),
        "atr_period": 14,
        "strategy_mode": str(unpacked["strategy_mode"]),
        "regime_trend_threshold": float(unpacked["regime_trend_threshold"]),
        "pullback_atr": float(unpacked["pullback_atr"]),
        "reversion_atr": float(unpacked["reversion_atr"]),
        "sl_atr": float(unpacked["sl_atr"]),
        "tp_atr": float(unpacked["tp_atr"]),
        "reversion_sl_atr": float(unpacked["reversion_sl_atr"]),
        "reversion_tp_atr": float(unpacked["reversion_tp_atr"]),
        "lot": 0.01,
        "max_spread_points": 80.0,
        "max_positions": 1,
        "session_start_hour_utc": int(unpacked["session_start_hour_utc"]),
        "session_end_hour_utc": int(unpacked["session_end_hour_utc"]),
        "atr_filter_window": 288,
        "atr_filter_min_samples": 120,
        "min_atr_ratio_to_median": float(unpacked["min_atr_ratio_to_median"]),
        "max_atr_ratio_to_median": float(unpacked["max_atr_ratio_to_median"]),
        "deviation": 30,
        "magic": magic,
        "comment": normalized_comment,
        "dry_run": True,
        "once": True,
        "poll_seconds": 10,
        "max_cycles": 0,
    }


def _parse_float_list(value: str) -> List[float]:
    out: List[float] = []
    for token in value.split(","):
        token = token.strip()
        if token:
            out.append(float(token))
    return out


def _parse_int_list(value: str) -> List[int]:
    out: List[int] = []
    for token in value.split(","):
        token = token.strip()
        if token:
            out.append(int(token))
    return out


def _parse_sl_tp_pairs(value: str) -> List[Tuple[float, float]]:
    pairs: List[Tuple[float, float]] = []
    for token in value.split(","):
        token = token.strip()
        if not token:
            continue
        sl, tp = token.split(":")
        pairs.append((float(sl), float(tp)))
    return pairs


def _parse_sessions(value: str) -> List[Tuple[int, int]]:
    sessions: List[Tuple[int, int]] = []
    for token in value.split(","):
        token = token.strip()
        if not token:
            continue
        start, end = token.split("-")
        sessions.append((int(start), int(end)))
    return sessions


def _parse_ratio_pairs(value: str) -> List[Tuple[float, float]]:
    pairs: List[Tuple[float, float]] = []
    for token in value.split(","):
        token = token.strip()
        if not token:
            continue
        lo, hi = token.split(":")
        pairs.append((float(lo), float(hi)))
    return pairs


def _parse_str_list(value: str) -> List[str]:
    out: List[str] = []
    for token in value.split(","):
        token = token.strip()
        if token:
            out.append(token)
    return out


def _parse_split_ratios(value: str) -> List[float]:
    ratios = _parse_float_list(value)
    for ratio in ratios:
        if ratio <= 0.0 or ratio >= 1.0:
            raise ValueError("split ratios must be between 0 and 1")
    return ratios


def _load_ohlc(ticker: str, period: str, interval: str) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    return load_ohlc(ticker=ticker, period=period, interval=interval)


def _ema_series(values: Sequence[float], period: int) -> List[float]:
    alpha = 2.0 / (period + 1.0)
    out = [0.0] * len(values)
    ema = float(values[0])
    out[0] = ema
    for i in range(1, len(values)):
        ema = ema + alpha * (float(values[i]) - ema)
        out[i] = ema
    return out


def _atr_series(highs: Sequence[float], lows: Sequence[float], closes: Sequence[float], period: int) -> List[float]:
    true_ranges = [0.0] * len(highs)
    prev_close = float(closes[0])
    for i in range(len(highs)):
        high = float(highs[i])
        low = float(lows[i])
        tr = (high - low) if i == 0 else max(high - low, abs(high - prev_close), abs(low - prev_close))
        true_ranges[i] = tr
        prev_close = float(closes[i])
    out = [0.0] * len(highs)
    rolling = 0.0
    for i, tr in enumerate(true_ranges):
        rolling += tr
        if i >= period:
            rolling -= true_ranges[i - period]
        out[i] = rolling / float(period) if i >= period - 1 else rolling / float(i + 1)
    return out


def _is_session_allowed(hour_utc: int, start: int, end: int) -> bool:
    if start == end:
        return True
    if start < end:
        return start <= hour_utc <= end
    return hour_utc >= start or hour_utc <= end


def _simulate_segment(
    candidate: Candidate,
    *,
    start_idx: int,
    end_idx: int,
    times: Sequence,
    opens: Sequence[float],
    highs: Sequence[float],
    lows: Sequence[float],
    closes: Sequence[float],
    ema_cache: Dict[int, List[float]],
    atr_values: Sequence[float],
    atr_pct_values: Sequence[float],
    cost_per_side: float,
) -> List[float]:
    c = _unpack_candidate(candidate)
    fast = int(c["fast_ema"])
    slow = int(c["slow_ema"])
    pullback = float(c["pullback_atr"])
    sl_atr = float(c["sl_atr"])
    tp_atr = float(c["tp_atr"])
    session_start = int(c["session_start_hour_utc"])
    session_end = int(c["session_end_hour_utc"])
    min_ratio = float(c["min_atr_ratio_to_median"])
    max_ratio = float(c["max_atr_ratio_to_median"])
    strategy_mode = str(c["strategy_mode"])
    regime_threshold = float(c["regime_trend_threshold"])
    reversion_atr = float(c["reversion_atr"])
    reversion_sl_atr = float(c["reversion_sl_atr"])
    reversion_tp_atr = float(c["reversion_tp_atr"])
    warmup = max(slow, 14) + 5
    start = max(start_idx, warmup)
    end = min(end_idx - 1, len(closes) - 1)

    position = 0
    entry = 0.0
    sl = 0.0
    tp = 0.0
    returns: List[float] = []

    for i in range(start, end):
        if position == 1:
            sl_hit = lows[i] <= sl
            tp_hit = highs[i] >= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                returns.append((exit_px - entry) / entry - 2.0 * cost_per_side)
                position = 0
                continue
        elif position == -1:
            sl_hit = highs[i] >= sl
            tp_hit = lows[i] <= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                returns.append((entry - exit_px) / entry - 2.0 * cost_per_side)
                position = 0
                continue

        signal_ctx = decide_signal_with_mode(
            strategy_mode=strategy_mode,
            last_close=closes[i],
            ema_fast=ema_cache[fast][i],
            ema_slow=ema_cache[slow][i],
            atr_value=atr_values[i],
            pullback_atr=pullback,
            reversion_atr=reversion_atr,
            trend_threshold=regime_threshold,
        )
        raw_signal = str(signal_ctx.get("signal", "HOLD")).upper()
        signal = 1 if raw_signal == "BUY" else -1 if raw_signal == "SELL" else 0
        signal_source = str(signal_ctx.get("source", "trend"))

        if position == 0 and signal != 0:
            hour_utc = times[i].hour
            if not _is_session_allowed(hour_utc, session_start, session_end):
                continue

            window_start = max(0, i - 288 + 1)
            window = atr_pct_values[window_start : i + 1]
            if len(window) >= 120:
                ordered = sorted(window)
                median = ordered[len(ordered) // 2]
                if median > 0.0:
                    ratio = atr_pct_values[i] / median
                    if ratio < min_ratio or ratio > max_ratio:
                        continue

            entry = opens[i + 1]
            use_sl_atr = reversion_sl_atr if signal_source == "reversion" else sl_atr
            use_tp_atr = reversion_tp_atr if signal_source == "reversion" else tp_atr
            if signal == 1:
                sl = entry - atr_values[i] * use_sl_atr
                tp = entry + atr_values[i] * use_tp_atr
            else:
                sl = entry + atr_values[i] * use_sl_atr
                tp = entry - atr_values[i] * use_tp_atr
            position = signal
            continue

        if position != 0 and signal != 0 and ((position == 1 and signal == -1) or (position == -1 and signal == 1)):
            exit_px = opens[i + 1]
            gross = (exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry
            returns.append(gross - 2.0 * cost_per_side)
            position = 0

    if position != 0:
        exit_px = closes[min(end_idx - 1, len(closes) - 1)]
        gross = (exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry
        returns.append(gross - 2.0 * cost_per_side)

    return returns


def _returns_metrics(returns: Sequence[float]) -> Dict[str, float]:
    if not returns:
        return {"trades": 0.0, "pf": 0.0, "total": 0.0, "max_dd": 0.0}
    wins = [r for r in returns if r > 0.0]
    losses = [r for r in returns if r < 0.0]
    pf = (sum(wins) / abs(sum(losses))) if losses else 99.0
    equity = 1.0
    peak = 1.0
    max_dd = 0.0
    for r in returns:
        equity *= 1.0 + r
        if equity > peak:
            peak = equity
        max_dd = max(max_dd, (peak - equity) / peak)
    return {"trades": float(len(returns)), "pf": float(pf), "total": float(equity - 1.0), "max_dd": float(max_dd)}


def _build_candidates(
    fasts: Sequence[int],
    slows: Sequence[int],
    pullbacks: Sequence[float],
    sl_tp_pairs: Sequence[Tuple[float, float]],
    sessions: Sequence[Tuple[int, int]],
    vol_bands: Sequence[Tuple[float, float]],
    strategy_modes: Sequence[str],
    regime_thresholds: Sequence[float],
    reversion_atrs: Sequence[float],
    reversion_sl_tp_pairs: Sequence[Tuple[float, float]],
) -> List[Candidate]:
    candidates: List[Candidate] = []
    for (
        fast,
        slow,
        pullback,
        (sl_atr, tp_atr),
        (start, end),
        (min_ratio, max_ratio),
        strategy_mode,
        regime_threshold,
        reversion_atr,
        (reversion_sl_atr, reversion_tp_atr),
    ) in product(
        fasts,
        slows,
        pullbacks,
        sl_tp_pairs,
        sessions,
        vol_bands,
        strategy_modes,
        regime_thresholds,
        reversion_atrs,
        reversion_sl_tp_pairs,
    ):
        if fast >= slow:
            continue
        candidates.append(
            (
                fast,
                slow,
                pullback,
                sl_atr,
                tp_atr,
                start,
                end,
                min_ratio,
                max_ratio,
                strategy_mode,
                regime_threshold,
                reversion_atr,
                reversion_sl_atr,
                reversion_tp_atr,
            )
        )
    return candidates


def main() -> None:
    parser = argparse.ArgumentParser(description="Optimize xau_autobot parameters on intraday gold data")
    parser.add_argument("--ticker", default="GC=F")
    parser.add_argument("--period", default="60d")
    parser.add_argument("--interval", default="5m")
    parser.add_argument("--cost-per-side", type=float, default=0.0002)
    parser.add_argument("--split-ratios", default="0.5,0.6,0.7")
    parser.add_argument("--min-oos-trades", type=int, default=45)
    parser.add_argument("--top-k", type=int, default=10)
    parser.add_argument("--fast-emas", default="20,24,28,30")
    parser.add_argument("--slow-emas", default="100,120,140,160")
    parser.add_argument("--pullbacks", default="0.1,0.2,0.3,0.4")
    parser.add_argument("--sl-tp-pairs", default="1.2:2.0,1.5:2.5,2.0:3.0")
    parser.add_argument("--sessions", default="7-19,8-18")
    parser.add_argument("--vol-bands", default="0.8:1.6,0.9:1.4,0.7:1.8")
    parser.add_argument("--strategy-modes", default="trend")
    parser.add_argument("--regime-thresholds", default="1.2")
    parser.add_argument("--reversion-atrs", default="0.8")
    parser.add_argument("--reversion-sl-tp-pairs", default="1.2:1.2")
    parser.add_argument("--write-config", default="")
    parser.add_argument("--magic", type=int, default=560070)
    parser.add_argument("--comment", default="xau_autobot_tuned_auto")
    args = parser.parse_args()

    split_ratios = _parse_split_ratios(args.split_ratios)
    validated_comment = validate_trade_comment(args.comment)
    fasts = _parse_int_list(args.fast_emas)
    slows = _parse_int_list(args.slow_emas)
    pullbacks = _parse_float_list(args.pullbacks)
    sl_tp_pairs = _parse_sl_tp_pairs(args.sl_tp_pairs)
    sessions = _parse_sessions(args.sessions)
    vol_bands = _parse_ratio_pairs(args.vol_bands)
    strategy_modes = [validate_strategy_mode(mode) for mode in _parse_str_list(args.strategy_modes)]
    regime_thresholds = _parse_float_list(args.regime_thresholds)
    reversion_atrs = _parse_float_list(args.reversion_atrs)
    reversion_sl_tp_pairs = _parse_sl_tp_pairs(args.reversion_sl_tp_pairs)

    candidates = _build_candidates(
        fasts,
        slows,
        pullbacks,
        sl_tp_pairs,
        sessions,
        vol_bands,
        strategy_modes,
        regime_thresholds,
        reversion_atrs,
        reversion_sl_tp_pairs,
    )
    times, opens, highs, lows, closes = _load_ohlc(args.ticker, args.period, args.interval)
    atr_values = _atr_series(highs, lows, closes, 14)
    atr_pct_values = [(atr_values[i] / closes[i]) if closes[i] > 0.0 else 0.0 for i in range(len(closes))]

    ema_periods = sorted({c[0] for c in candidates} | {c[1] for c in candidates})
    ema_cache = {period: _ema_series(closes, period) for period in ema_periods}

    scored: List[Tuple[float, Candidate, List[Dict[str, float]]]] = []
    for candidate in candidates:
        split_metrics: List[Dict[str, float]] = []
        for ratio in split_ratios:
            split = int(len(closes) * ratio)
            is_returns = _simulate_segment(
                candidate,
                start_idx=0,
                end_idx=split,
                times=times,
                opens=opens,
                highs=highs,
                lows=lows,
                closes=closes,
                ema_cache=ema_cache,
                atr_values=atr_values,
                atr_pct_values=atr_pct_values,
                cost_per_side=args.cost_per_side,
            )
            oos_returns = _simulate_segment(
                candidate,
                start_idx=split,
                end_idx=len(closes),
                times=times,
                opens=opens,
                highs=highs,
                lows=lows,
                closes=closes,
                ema_cache=ema_cache,
                atr_values=atr_values,
                atr_pct_values=atr_pct_values,
                cost_per_side=args.cost_per_side,
            )
            is_metrics = _returns_metrics(is_returns)
            oos_metrics = _returns_metrics(oos_returns)
            split_metrics.append(
                {
                    "ratio": ratio,
                    "is_total": is_metrics["total"],
                    "is_pf": is_metrics["pf"],
                    "is_trades": is_metrics["trades"],
                    "is_max_dd": is_metrics["max_dd"],
                    "oos_total": oos_metrics["total"],
                    "oos_pf": oos_metrics["pf"],
                    "oos_trades": oos_metrics["trades"],
                    "oos_max_dd": oos_metrics["max_dd"],
                }
            )
        score = score_candidate(split_metrics, min_oos_trades=args.min_oos_trades)
        scored.append((score, candidate, split_metrics))

    scored.sort(key=lambda x: x[0], reverse=True)
    top = scored[: max(1, args.top_k)]

    print(
        json.dumps(
            {
                "ticker": args.ticker,
                "period": args.period,
                "interval": args.interval,
                "bars": len(closes),
                "cost_per_side": args.cost_per_side,
                "candidate_count": len(candidates),
                "split_ratios": split_ratios,
                "range_start": str(times[0]),
                "range_end": str(times[-1]),
            },
            ensure_ascii=True,
        )
    )

    for rank, (score, candidate, split_metrics) in enumerate(top, start=1):
        c = _unpack_candidate(candidate)
        print(
            json.dumps(
                {
                    "rank": rank,
                    "score": score,
                    "candidate": c,
                    "splits": split_metrics,
                },
                ensure_ascii=True,
            )
        )

    best_candidate = top[0][1]
    config = candidate_to_config(
        best_candidate,
        magic=args.magic,
        comment=validated_comment,
        timeframe=interval_to_timeframe(args.interval),
    )
    print(json.dumps({"best_config": config}, ensure_ascii=True))

    if args.write_config:
        with open(args.write_config, "w", encoding="utf-8") as f:
            json.dump(config, f, ensure_ascii=True, indent=2)
            f.write("\n")
        print(json.dumps({"written_config": args.write_config}, ensure_ascii=True))


if __name__ == "__main__":
    main()
