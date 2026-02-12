#!/usr/bin/env python3
"""Backtest helper for xau_autobot strategy presets using Yahoo Finance data."""

from __future__ import annotations

import argparse
import json
from dataclasses import dataclass
from typing import Dict, List, Sequence, Tuple

try:
    import yfinance as yf
except Exception:
    yf = None


@dataclass(frozen=True)
class StrategyPreset:
    name: str
    fast_ema: int
    slow_ema: int
    pullback_atr: float
    sl_atr: float
    tp_atr: float
    session_start_hour_utc: int
    session_end_hour_utc: int
    min_atr_ratio_to_median: float
    max_atr_ratio_to_median: float
    atr_period: int = 14
    atr_filter_window: int = 288
    atr_filter_min_samples: int = 120


BASE_PRESET = StrategyPreset(
    name="base",
    fast_ema=20,
    slow_ema=80,
    pullback_atr=0.6,
    sl_atr=1.5,
    tp_atr=2.0,
    session_start_hour_utc=0,
    session_end_hour_utc=23,
    min_atr_ratio_to_median=0.0,
    max_atr_ratio_to_median=999.0,
)

TUNED_PRESET = StrategyPreset(
    name="tuned",
    fast_ema=24,
    slow_ema=120,
    pullback_atr=0.2,
    sl_atr=1.5,
    tp_atr=2.5,
    session_start_hour_utc=7,
    session_end_hour_utc=19,
    min_atr_ratio_to_median=0.9,
    max_atr_ratio_to_median=1.4,
)


def _is_session_allowed(hour_utc: int, start: int, end: int) -> bool:
    if start == end:
        return True
    if start < end:
        return start <= hour_utc <= end
    return hour_utc >= start or hour_utc <= end


def _load_ohlc(ticker: str, period: str, interval: str) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    if yf is None:
        raise RuntimeError("yfinance is required. Use .venv python with yfinance installed.")
    df = yf.download(ticker, period=period, interval=interval, auto_adjust=False, progress=False, threads=False)
    if len(df) == 0:
        raise RuntimeError(f"no data: ticker={ticker} period={period} interval={interval}")
    cols = {c[0].lower(): c for c in df.columns}
    times = df.index.to_pydatetime().tolist()
    opens = df[cols["open"]].astype(float).tolist()
    highs = df[cols["high"]].astype(float).tolist()
    lows = df[cols["low"]].astype(float).tolist()
    closes = df[cols["close"]].astype(float).tolist()
    return times, opens, highs, lows, closes


def _ema_series(values: List[float], period: int) -> List[float]:
    alpha = 2.0 / (period + 1.0)
    out = [0.0] * len(values)
    ema = values[0]
    out[0] = ema
    for i in range(1, len(values)):
        ema = ema + alpha * (values[i] - ema)
        out[i] = ema
    return out


def _atr_series(highs: List[float], lows: List[float], closes: List[float], period: int) -> List[float]:
    true_ranges = [0.0] * len(highs)
    prev = closes[0]
    for i in range(len(highs)):
        tr = (highs[i] - lows[i]) if i == 0 else max(highs[i] - lows[i], abs(highs[i] - prev), abs(lows[i] - prev))
        true_ranges[i] = tr
        prev = closes[i]
    out = [0.0] * len(highs)
    rolling = 0.0
    for i, tr in enumerate(true_ranges):
        rolling += tr
        if i >= period:
            rolling -= true_ranges[i - period]
        out[i] = rolling / float(period) if i >= period - 1 else rolling / float(i + 1)
    return out


def _simulate(
    times: List,
    opens: List[float],
    highs: List[float],
    lows: List[float],
    closes: List[float],
    *,
    preset: StrategyPreset,
    cost_per_side: float,
    start_idx: int,
    end_idx: int,
    ema_cache: Dict[int, List[float]],
    atr_values: List[float],
) -> List[float]:
    n = len(closes)
    warmup = max(preset.slow_ema, preset.atr_period) + 5
    start = max(warmup, start_idx)
    end = min(end_idx - 1, n - 1)
    atr_pct_values = [(atr_values[i] / closes[i]) if closes[i] > 0.0 else 0.0 for i in range(n)]

    position = 0
    entry = 0.0
    sl = 0.0
    tp = 0.0
    trade_returns: List[float] = []

    for i in range(start, end):
        if position == 1:
            sl_hit = lows[i] <= sl
            tp_hit = highs[i] >= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                gross = (exit_px - entry) / entry
                trade_returns.append(gross - 2.0 * cost_per_side)
                position = 0
                continue
        elif position == -1:
            sl_hit = highs[i] >= sl
            tp_hit = lows[i] <= tp
            if sl_hit or tp_hit:
                exit_px = sl if sl_hit else tp
                gross = (entry - exit_px) / entry
                trade_returns.append(gross - 2.0 * cost_per_side)
                position = 0
                continue

        pullback = atr_values[i] * preset.pullback_atr
        signal = 0
        if ema_cache[preset.fast_ema][i] > ema_cache[preset.slow_ema][i] and closes[i] <= ema_cache[preset.fast_ema][i] - pullback:
            signal = 1
        elif ema_cache[preset.fast_ema][i] < ema_cache[preset.slow_ema][i] and closes[i] >= ema_cache[preset.fast_ema][i] + pullback:
            signal = -1

        if position == 0 and signal != 0:
            hour_utc = times[i].hour
            if not _is_session_allowed(hour_utc, preset.session_start_hour_utc, preset.session_end_hour_utc):
                continue

            if (
                preset.min_atr_ratio_to_median > 0.0
                or preset.max_atr_ratio_to_median < 999.0
            ):
                window_start = max(0, i - preset.atr_filter_window + 1)
                window_values = atr_pct_values[window_start : i + 1]
                if len(window_values) >= preset.atr_filter_min_samples:
                    ordered = sorted(window_values)
                    median = ordered[len(ordered) // 2]
                    if median > 0.0:
                        ratio = atr_pct_values[i] / median
                        if ratio < preset.min_atr_ratio_to_median or ratio > preset.max_atr_ratio_to_median:
                            continue

            entry = opens[i + 1]
            if signal == 1:
                sl = entry - atr_values[i] * preset.sl_atr
                tp = entry + atr_values[i] * preset.tp_atr
            else:
                sl = entry + atr_values[i] * preset.sl_atr
                tp = entry - atr_values[i] * preset.tp_atr
            position = signal
            continue

        if position != 0 and signal != 0 and ((position == 1 and signal == -1) or (position == -1 and signal == 1)):
            exit_px = opens[i + 1]
            gross = (exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry
            trade_returns.append(gross - 2.0 * cost_per_side)
            position = 0

    if position != 0:
        exit_px = closes[min(end_idx - 1, n - 1)]
        gross = (exit_px - entry) / entry if position == 1 else (entry - exit_px) / entry
        trade_returns.append(gross - 2.0 * cost_per_side)
    return trade_returns


def _metrics(returns: Sequence[float]) -> Dict[str, float]:
    if not returns:
        return {"trades": 0, "win_rate": 0.0, "pf": 0.0, "total_return": 0.0, "max_dd": 0.0}
    wins = [r for r in returns if r > 0.0]
    losses = [r for r in returns if r < 0.0]
    pf = (sum(wins) / abs(sum(losses))) if losses else 99.0
    equity = 1.0
    peak = 1.0
    max_dd = 0.0
    for r in returns:
        equity *= (1.0 + r)
        if equity > peak:
            peak = equity
        dd = (peak - equity) / peak
        if dd > max_dd:
            max_dd = dd
    return {
        "trades": float(len(returns)),
        "win_rate": float(len(wins) / len(returns)),
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
    parser = argparse.ArgumentParser(description="Backtest xau_autobot presets")
    parser.add_argument("--ticker", default="GC=F", help="Yahoo ticker (default: GC=F)")
    parser.add_argument("--period", default="60d", help="Data period (default: 60d)")
    parser.add_argument("--interval", default="5m", help="Data interval (default: 5m)")
    parser.add_argument("--cost-per-side", type=float, default=0.0002, help="Trading cost per side (default: 0.0002)")
    parser.add_argument("--mode", choices=["base", "tuned", "both"], default="both")
    parser.add_argument("--split-ratios", default="0.5,0.6,0.7,0.8", help="Comma-separated IS/OOS split ratios")
    args = parser.parse_args()

    split_ratios = _parse_split_ratios(args.split_ratios)
    times, opens, highs, lows, closes = _load_ohlc(args.ticker, args.period, args.interval)
    presets = [BASE_PRESET, TUNED_PRESET] if args.mode == "both" else [BASE_PRESET if args.mode == "base" else TUNED_PRESET]

    needed_ema_periods = {p.fast_ema for p in presets} | {p.slow_ema for p in presets}
    ema_cache = {period: _ema_series(closes, period) for period in needed_ema_periods}
    atr_values = _atr_series(highs, lows, closes, 14)

    print(
        json.dumps(
            {
                "ticker": args.ticker,
                "period": args.period,
                "interval": args.interval,
                "bars": len(closes),
                "range_start": str(times[0]),
                "range_end": str(times[-1]),
                "cost_per_side": args.cost_per_side,
            },
            ensure_ascii=True,
        )
    )

    for preset in presets:
        full_returns = _simulate(
            times,
            opens,
            highs,
            lows,
            closes,
            preset=preset,
            cost_per_side=args.cost_per_side,
            start_idx=0,
            end_idx=len(closes),
            ema_cache=ema_cache,
            atr_values=atr_values,
        )
        full_metrics = _metrics(full_returns)
        print(json.dumps({"preset": preset.name, "segment": "all", **full_metrics}, ensure_ascii=True))

        for ratio in split_ratios:
            split = int(len(closes) * ratio)
            is_returns = _simulate(
                times,
                opens,
                highs,
                lows,
                closes,
                preset=preset,
                cost_per_side=args.cost_per_side,
                start_idx=0,
                end_idx=split,
                ema_cache=ema_cache,
                atr_values=atr_values,
            )
            oos_returns = _simulate(
                times,
                opens,
                highs,
                lows,
                closes,
                preset=preset,
                cost_per_side=args.cost_per_side,
                start_idx=split,
                end_idx=len(closes),
                ema_cache=ema_cache,
                atr_values=atr_values,
            )
            print(
                json.dumps(
                    {
                        "preset": preset.name,
                        "segment": f"split_{ratio:.2f}_is",
                        **_metrics(is_returns),
                    },
                    ensure_ascii=True,
                )
            )
            print(
                json.dumps(
                    {
                        "preset": preset.name,
                        "segment": f"split_{ratio:.2f}_oos",
                        **_metrics(oos_returns),
                    },
                    ensure_ascii=True,
                )
            )


if __name__ == "__main__":
    main()
