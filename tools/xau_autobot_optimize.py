#!/usr/bin/env python3
"""Parameter optimizer for xau_autobot strategy using Yahoo Finance 5m data."""

from __future__ import annotations

import argparse
import json
from itertools import product
from statistics import mean
from typing import Dict, List, Sequence, Tuple

try:
    from tools.xau_autobot_data import load_ohlc
except Exception:
    from xau_autobot_data import load_ohlc  # type: ignore

try:
    import yfinance as yf
except Exception:
    yf = None


Candidate = Tuple[int, int, float, float, float, int, int, float, float]


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


def candidate_to_config(candidate: Candidate, *, magic: int, comment: str) -> Dict[str, object]:
    fast, slow, pullback, sl_atr, tp_atr, session_start, session_end, min_ratio, max_ratio = candidate
    return {
        "symbol": "XAUUSD",
        "timeframe": "M5",
        "bars": 800,
        "fast_ema": fast,
        "slow_ema": slow,
        "atr_period": 14,
        "pullback_atr": pullback,
        "sl_atr": sl_atr,
        "tp_atr": tp_atr,
        "lot": 0.01,
        "max_spread_points": 80.0,
        "max_positions": 1,
        "session_start_hour_utc": session_start,
        "session_end_hour_utc": session_end,
        "atr_filter_window": 288,
        "atr_filter_min_samples": 120,
        "min_atr_ratio_to_median": min_ratio,
        "max_atr_ratio_to_median": max_ratio,
        "deviation": 30,
        "magic": magic,
        "comment": comment,
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


def _parse_split_ratios(value: str) -> List[float]:
    ratios = _parse_float_list(value)
    for ratio in ratios:
        if ratio <= 0.0 or ratio >= 1.0:
            raise ValueError("split ratios must be between 0 and 1")
    return ratios


def _load_ohlc(ticker: str, period: str, interval: str) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    return load_ohlc(ticker=ticker, period=period, interval=interval, yf_module=yf)


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
    fast, slow, pullback, sl_atr, tp_atr, session_start, session_end, min_ratio, max_ratio = candidate
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

        pull = atr_values[i] * pullback
        signal = 0
        if ema_cache[fast][i] > ema_cache[slow][i] and closes[i] <= ema_cache[fast][i] - pull:
            signal = 1
        elif ema_cache[fast][i] < ema_cache[slow][i] and closes[i] >= ema_cache[fast][i] + pull:
            signal = -1

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
            if signal == 1:
                sl = entry - atr_values[i] * sl_atr
                tp = entry + atr_values[i] * tp_atr
            else:
                sl = entry + atr_values[i] * sl_atr
                tp = entry - atr_values[i] * tp_atr
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
) -> List[Candidate]:
    candidates: List[Candidate] = []
    for fast, slow, pullback, (sl_atr, tp_atr), (start, end), (min_ratio, max_ratio) in product(
        fasts, slows, pullbacks, sl_tp_pairs, sessions, vol_bands
    ):
        if fast >= slow:
            continue
        candidates.append((fast, slow, pullback, sl_atr, tp_atr, start, end, min_ratio, max_ratio))
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
    parser.add_argument("--write-config", default="")
    parser.add_argument("--magic", type=int, default=560070)
    parser.add_argument("--comment", default="xau_autobot_tuned_auto")
    args = parser.parse_args()

    split_ratios = _parse_split_ratios(args.split_ratios)
    fasts = _parse_int_list(args.fast_emas)
    slows = _parse_int_list(args.slow_emas)
    pullbacks = _parse_float_list(args.pullbacks)
    sl_tp_pairs = _parse_sl_tp_pairs(args.sl_tp_pairs)
    sessions = _parse_sessions(args.sessions)
    vol_bands = _parse_ratio_pairs(args.vol_bands)

    candidates = _build_candidates(fasts, slows, pullbacks, sl_tp_pairs, sessions, vol_bands)
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
        print(
            json.dumps(
                {
                    "rank": rank,
                    "score": score,
                    "candidate": {
                        "fast_ema": candidate[0],
                        "slow_ema": candidate[1],
                        "pullback_atr": candidate[2],
                        "sl_atr": candidate[3],
                        "tp_atr": candidate[4],
                        "session_start_hour_utc": candidate[5],
                        "session_end_hour_utc": candidate[6],
                        "min_atr_ratio_to_median": candidate[7],
                        "max_atr_ratio_to_median": candidate[8],
                    },
                    "splits": split_metrics,
                },
                ensure_ascii=True,
            )
        )

    best_candidate = top[0][1]
    config = candidate_to_config(best_candidate, magic=args.magic, comment=args.comment)
    print(json.dumps({"best_config": config}, ensure_ascii=True))

    if args.write_config:
        with open(args.write_config, "w", encoding="utf-8") as f:
            json.dump(config, f, ensure_ascii=True, indent=2)
            f.write("\n")
        print(json.dumps({"written_config": args.write_config}, ensure_ascii=True))


if __name__ == "__main__":
    main()
