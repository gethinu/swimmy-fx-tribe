#!/usr/bin/env python3
"""Readiness report for xau_autobot config using offline intraday backtest."""

from __future__ import annotations

import argparse
import json
from typing import Dict, List, Sequence, Tuple

try:
    from tools.xau_autobot_data import load_ohlc
except Exception:
    from xau_autobot_data import load_ohlc  # type: ignore

try:
    import yfinance as yf
except Exception:
    yf = None


def total_return_from_gross(gross_returns: Sequence[float], cost_per_side: float) -> float:
    equity = 1.0
    for gross in gross_returns:
        net = gross - (2.0 * cost_per_side)
        equity *= 1.0 + net
    return equity - 1.0


def estimate_break_even_cost(
    total_return_fn,
    *,
    lo: float = 0.0,
    hi: float = 0.002,
    iterations: int = 40,
) -> float:
    if total_return_fn(lo) <= 0.0:
        return lo
    if total_return_fn(hi) > 0.0:
        return hi
    left = lo
    right = hi
    for _ in range(iterations):
        mid = (left + right) / 2.0
        if total_return_fn(mid) > 0.0:
            left = mid
        else:
            right = mid
    return left


def readiness_verdict(*, assumed_cost_side: float, break_even_cost_side: float) -> str:
    if break_even_cost_side <= 0.0:
        return "NO_GO"
    if assumed_cost_side <= break_even_cost_side * 0.7:
        return "GO"
    if assumed_cost_side <= break_even_cost_side:
        return "CAUTION"
    return "NO_GO"


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
    tr = [0.0] * len(closes)
    prev_close = float(closes[0])
    for i in range(len(closes)):
        high = float(highs[i])
        low = float(lows[i])
        tr[i] = (high - low) if i == 0 else max(high - low, abs(high - prev_close), abs(low - prev_close))
        prev_close = float(closes[i])
    out = [0.0] * len(closes)
    rolling = 0.0
    for i, value in enumerate(tr):
        rolling += value
        if i >= period:
            rolling -= tr[i - period]
        out[i] = rolling / float(period) if i >= period - 1 else rolling / float(i + 1)
    return out


def _is_session_allowed(hour_utc: int, start: int, end: int) -> bool:
    if start == end:
        return True
    if start < end:
        return start <= hour_utc <= end
    return hour_utc >= start or hour_utc <= end


def _simulate_gross_returns(
    cfg: Dict[str, float],
    *,
    start_idx: int,
    end_idx: int,
    times: Sequence,
    opens: Sequence[float],
    highs: Sequence[float],
    lows: Sequence[float],
    closes: Sequence[float],
    ema_fast: Sequence[float],
    ema_slow: Sequence[float],
    atr_values: Sequence[float],
) -> List[float]:
    warmup = max(int(cfg["slow_ema"]), int(cfg["atr_period"])) + 5
    start = max(start_idx, warmup)
    end = min(end_idx - 1, len(closes) - 1)

    atr_pct = [(atr_values[i] / closes[i]) if closes[i] > 0.0 else 0.0 for i in range(len(closes))]

    position = 0
    entry = 0.0
    sl = 0.0
    tp = 0.0
    gross_returns: List[float] = []

    for i in range(start, end):
        if position == 1:
            sl_hit = lows[i] <= sl
            tp_hit = highs[i] >= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                gross_returns.append((exit_px - entry) / entry)
                position = 0
                continue
        elif position == -1:
            sl_hit = highs[i] >= sl
            tp_hit = lows[i] <= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                gross_returns.append((entry - exit_px) / entry)
                position = 0
                continue

        hour = times[i].hour
        if not _is_session_allowed(hour, int(cfg["session_start_hour_utc"]), int(cfg["session_end_hour_utc"])):
            continue

        window = atr_pct[max(0, i - int(cfg["atr_filter_window"]) + 1) : i + 1]
        if len(window) >= int(cfg["atr_filter_min_samples"]):
            ordered = sorted(window)
            median = ordered[len(ordered) // 2]
            if median > 0.0:
                ratio = atr_pct[i] / median
                if ratio < float(cfg["min_atr_ratio_to_median"]) or ratio > float(cfg["max_atr_ratio_to_median"]):
                    continue

        pull = atr_values[i] * float(cfg["pullback_atr"])
        signal = 0
        if ema_fast[i] > ema_slow[i] and closes[i] <= ema_fast[i] - pull:
            signal = 1
        elif ema_fast[i] < ema_slow[i] and closes[i] >= ema_fast[i] + pull:
            signal = -1

        if position == 0 and signal != 0:
            entry = opens[i + 1]
            sl_dist = atr_values[i] * float(cfg["sl_atr"])
            tp_dist = atr_values[i] * float(cfg["tp_atr"])
            if signal == 1:
                sl = entry - sl_dist
                tp = entry + tp_dist
            else:
                sl = entry + sl_dist
                tp = entry - tp_dist
            position = signal
            continue

        if position != 0 and signal != 0 and ((position == 1 and signal == -1) or (position == -1 and signal == 1)):
            exit_px = opens[i + 1]
            gross_returns.append((exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry)
            position = 0

    if position != 0:
        exit_px = closes[min(end_idx - 1, len(closes) - 1)]
        gross_returns.append((exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry)

    return gross_returns


def _metrics(gross_returns: Sequence[float], cost_per_side: float) -> Dict[str, float]:
    net = [r - (2.0 * cost_per_side) for r in gross_returns]
    if not net:
        return {"trades": 0.0, "win_rate": 0.0, "pf": 0.0, "total_return": 0.0, "max_dd": 0.0}
    wins = [r for r in net if r > 0.0]
    losses = [r for r in net if r < 0.0]
    pf = (sum(wins) / abs(sum(losses))) if losses else 99.0
    equity = 1.0
    peak = 1.0
    max_dd = 0.0
    for r in net:
        equity *= 1.0 + r
        if equity > peak:
            peak = equity
        max_dd = max(max_dd, (peak - equity) / peak)
    return {
        "trades": float(len(net)),
        "win_rate": float(len(wins) / len(net)),
        "pf": float(pf),
        "total_return": float(equity - 1.0),
        "max_dd": float(max_dd),
    }


def _parse_split_ratios(value: str) -> List[float]:
    out: List[float] = []
    for token in value.split(","):
        token = token.strip()
        if not token:
            continue
        ratio = float(token)
        if ratio <= 0.0 or ratio >= 1.0:
            raise ValueError("split ratios must be between 0 and 1")
        out.append(ratio)
    return out


def main() -> None:
    parser = argparse.ArgumentParser(description="Readiness report for xau_autobot tuned config")
    parser.add_argument("--config", required=True, help="Config JSON file path")
    parser.add_argument("--ticker", default="GC=F")
    parser.add_argument("--period", default="60d")
    parser.add_argument("--interval", default="5m")
    parser.add_argument("--assumed-cost-side", type=float, default=0.0002)
    parser.add_argument("--split-ratios", default="0.5,0.6,0.7")
    parser.add_argument("--write-report", default="")
    args = parser.parse_args()

    cfg: Dict[str, float] = json.load(open(args.config, "r", encoding="utf-8"))
    split_ratios = _parse_split_ratios(args.split_ratios)
    times, opens, highs, lows, closes = _load_ohlc(args.ticker, args.period, args.interval)
    ema_fast = _ema_series(closes, int(cfg["fast_ema"]))
    ema_slow = _ema_series(closes, int(cfg["slow_ema"]))
    atr_values = _atr_series(highs, lows, closes, int(cfg["atr_period"]))

    gross_all = _simulate_gross_returns(
        cfg,
        start_idx=0,
        end_idx=len(closes),
        times=times,
        opens=opens,
        highs=highs,
        lows=lows,
        closes=closes,
        ema_fast=ema_fast,
        ema_slow=ema_slow,
        atr_values=atr_values,
    )
    total_fn = lambda cps: total_return_from_gross(gross_all, cps)
    break_even_cost_side = estimate_break_even_cost(total_fn, lo=0.0, hi=0.002, iterations=40)
    verdict = readiness_verdict(assumed_cost_side=args.assumed_cost_side, break_even_cost_side=break_even_cost_side)

    report: Dict[str, object] = {
        "config": args.config,
        "ticker": args.ticker,
        "period": args.period,
        "interval": args.interval,
        "bars": len(closes),
        "range_start": str(times[0]),
        "range_end": str(times[-1]),
        "assumed_cost_side": args.assumed_cost_side,
        "break_even_cost_side": break_even_cost_side,
        "break_even_roundtrip_cost": 2.0 * break_even_cost_side,
        "verdict": verdict,
        "all": _metrics(gross_all, args.assumed_cost_side),
        "splits": [],
    }

    for ratio in split_ratios:
        split = int(len(closes) * ratio)
        is_gross = _simulate_gross_returns(
            cfg,
            start_idx=0,
            end_idx=split,
            times=times,
            opens=opens,
            highs=highs,
            lows=lows,
            closes=closes,
            ema_fast=ema_fast,
            ema_slow=ema_slow,
            atr_values=atr_values,
        )
        oos_gross = _simulate_gross_returns(
            cfg,
            start_idx=split,
            end_idx=len(closes),
            times=times,
            opens=opens,
            highs=highs,
            lows=lows,
            closes=closes,
            ema_fast=ema_fast,
            ema_slow=ema_slow,
            atr_values=atr_values,
        )
        report["splits"].append(
            {
                "ratio": ratio,
                "is": _metrics(is_gross, args.assumed_cost_side),
                "oos": _metrics(oos_gross, args.assumed_cost_side),
            }
        )

    print(json.dumps(report, ensure_ascii=True))

    if args.write_report:
        with open(args.write_report, "w", encoding="utf-8") as f:
            json.dump(report, f, ensure_ascii=True, indent=2)
            f.write("\n")
        print(json.dumps({"written_report": args.write_report}, ensure_ascii=True))


if __name__ == "__main__":
    main()
