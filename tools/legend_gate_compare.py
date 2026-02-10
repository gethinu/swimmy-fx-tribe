#!/usr/bin/env python3
"""Legend gate comparison helpers (local, stdlib only)."""

from __future__ import annotations

import math
from typing import List, Optional


SHORT_TREND = 5
LONG_TREND = 50
BLEND_ALPHA = 0.7
VOL_PERIOD = 20
VOL_LOW = 0.5
VOL_HIGH = 1.5
KALMAN_Q = 0.01
KALMAN_R = 0.1
KALMAN_VEL_ADJ = 0.1
KALMAN_VEL_DECAY = 0.95
KALMAN_THRESH = 0.0001


def sma_series(values: List[float], period: int) -> List[Optional[float]]:
    out: List[Optional[float]] = [None] * len(values)
    if period <= 0:
        return out
    total = 0.0
    for i, v in enumerate(values):
        total += v
        if i >= period:
            total -= values[i - period]
        if i >= period - 1:
            out[i] = total / period
    return out


def rsi_series(values: List[float], period: int) -> List[Optional[float]]:
    n = len(values)
    out: List[Optional[float]] = [None] * n
    if n <= period:
        return out
    gains = [0.0] * n
    losses = [0.0] * n
    for i in range(1, n):
        diff = values[i] - values[i - 1]
        if diff > 0:
            gains[i] = diff
        elif diff < 0:
            losses[i] = -diff
    sum_gain = sum(gains[1 : period + 1])
    sum_loss = sum(losses[1 : period + 1])
    for i in range(period, n):
        if i > period:
            sum_gain += gains[i] - gains[i - period]
            sum_loss += losses[i] - losses[i - period]
        avg_gain = sum_gain / period
        avg_loss = sum_loss / period
        if avg_loss == 0:
            out[i] = 100.0
        else:
            rs = avg_gain / avg_loss
            out[i] = 100.0 - (100.0 / (1.0 + rs))
    return out


def _kalman_velocity(closes: List[float]) -> List[float]:
    n = len(closes)
    vels = [0.0] * n
    if n == 0:
        return vels
    x = closes[0]
    v = 0.0
    p_x = 1.0
    p_v = 1.0
    for i in range(1, n):
        z = closes[i]
        x_pred = x + v
        v_pred = v
        p_x_pred = p_x + p_v + KALMAN_Q
        p_v_pred = p_v + KALMAN_Q
        k_x = p_x_pred / (p_x_pred + KALMAN_R)
        innovation = z - x_pred
        x = x_pred + k_x * innovation
        v = v_pred + KALMAN_VEL_ADJ * innovation
        p_x = (1.0 - k_x) * p_x_pred
        p_v = KALMAN_VEL_DECAY * p_v_pred
        vels[i] = v
    return vels


def _trend_strength_series(closes: List[float], period: int) -> List[Optional[float]]:
    n = len(closes)
    out: List[Optional[float]] = [None] * n
    if period <= 1:
        return out
    for i in range(period - 1, n):
        first_price = closes[i - period + 1]
        last_price = closes[i]
        change = last_price - first_price
        avg_price = (first_price + last_price) / 2.0
        if avg_price > 0:
            val = change / (avg_price * 0.1)
            if val > 1.0:
                val = 1.0
            if val < -1.0:
                val = -1.0
            out[i] = val
        else:
            out[i] = 0.0
    return out


def _realized_volatility_series(closes: List[float], period: int = VOL_PERIOD) -> List[Optional[float]]:
    n = len(closes)
    out: List[Optional[float]] = [None] * n
    returns = [0.0] * n
    for i in range(1, n):
        prev = closes[i - 1]
        returns[i] = 0.0 if prev == 0 else 100.0 * (closes[i] - prev) / prev
    for i in range(period, n):
        window = returns[i - period + 1 : i + 1]
        mean = sum(window) / period
        var = sum((r - mean) ** 2 for r in window) / period
        out[i] = math.sqrt(var)
    return out


def model_gate_predict(closes: List[float]) -> List[Optional[str]]:
    n = len(closes)
    preds: List[Optional[str]] = [None] * n
    if n == 0:
        return preds

    vels = _kalman_velocity(closes)
    kalman_dir: List[str] = ["FLAT"] * n
    for i, v in enumerate(vels):
        if v > KALMAN_THRESH:
            kalman_dir[i] = "UP"
        elif v < -KALMAN_THRESH:
            kalman_dir[i] = "DOWN"

    short_ts = _trend_strength_series(closes, SHORT_TREND)
    long_ts = _trend_strength_series(closes, LONG_TREND)
    dual_dir: List[Optional[str]] = [None] * n
    dual_agree: List[Optional[str]] = [None] * n
    for i in range(n):
        st = short_ts[i]
        lt = long_ts[i]
        if st is None or lt is None:
            continue
        blended = BLEND_ALPHA * st + (1.0 - BLEND_ALPHA) * lt
        if blended > 0.1:
            d = "UP"
        elif blended < -0.1:
            d = "DOWN"
        else:
            d = "FLAT"
        dual_dir[i] = d
        if (st > 0 and lt > 0) or (st < 0 and lt < 0):
            dual_agree[i] = "aligned"
        else:
            dual_agree[i] = "divergent"

    vols = _realized_volatility_series(closes, VOL_PERIOD)
    for i in range(n):
        vol = vols[i]
        mode = "ensemble"
        if vol is not None and vol < VOL_LOW:
            mode = "kalman"
        if mode == "kalman":
            k = kalman_dir[i]
            if k == "UP":
                preds[i] = "BUY"
            elif k == "DOWN":
                preds[i] = "SELL"
            else:
                preds[i] = "HOLD"
        else:
            k = kalman_dir[i]
            d = dual_dir[i]
            a = dual_agree[i]
            if d is not None and a == "aligned" and k == d:
                if k == "UP":
                    preds[i] = "BUY"
                elif k == "DOWN":
                    preds[i] = "SELL"
                else:
                    preds[i] = "HOLD"
            else:
                preds[i] = "HOLD"
    return preds


def sharpe_ratio(returns: List[float]) -> float:
    """Match Guardian's time-based (daily) Sharpe definition.

    Guardian filters out near-zero returns when there are enough active samples,
    then annualizes by sqrt(252).
    """
    if not returns:
        return 0.0
    active = [r for r in returns if abs(r) > 1e-12]
    sample = active if len(active) >= 2 else returns
    mean = sum(sample) / len(sample)
    var = sum((r - mean) ** 2 for r in sample) / len(sample)
    std = math.sqrt(var)
    if std == 0.0:
        return 0.0
    return mean / std * math.sqrt(252.0)


def _max_drawdown(returns: List[float]) -> float:
    equity = 1.0
    peak = 1.0
    maxdd = 0.0
    for r in returns:
        equity *= (1.0 + r)
        if equity > peak:
            peak = equity
        dd = (peak - equity) / peak
        if dd > maxdd:
            maxdd = dd
    return maxdd


def daily_returns_from_trades(timestamps: List[int], trades: List[tuple]) -> List[float]:
    """Aggregate realized trade returns into a daily return series.

    - Includes 0.0 for days with no realized PnL, matching Guardian behavior.
    - Compounds multiple trade returns within the same day.
    """
    if not timestamps:
        return []
    start_day = timestamps[0] // 86400
    end_day = timestamps[-1] // 86400
    by_day: dict[int, List[float]] = {}
    for _entry_idx, exit_idx, ret in trades:
        day = timestamps[exit_idx] // 86400
        by_day.setdefault(day, []).append(ret)
    equity = 1.0
    out: List[float] = []
    for day in range(start_day, end_day + 1):
        eq_start = equity
        for ret in by_day.get(day, []):
            equity *= (1.0 + ret)
        out.append(0.0 if eq_start == 0.0 else (equity / eq_start - 1.0))
    return out


def compute_metrics(trade_returns: List[float], *, daily_returns: Optional[List[float]] = None) -> dict:
    """Return metrics where Sharpe/MaxDD are time-based (daily) if provided."""
    daily = daily_returns or []
    sharpe = sharpe_ratio(daily)
    maxdd = _max_drawdown(daily) if daily else 0.0

    if not trade_returns:
        return {"sharpe": sharpe, "pf": 0.0, "win": 0.0, "trades": 0, "maxdd": maxdd}

    wins = [r for r in trade_returns if r > 0]
    losses = [r for r in trade_returns if r < 0]
    if losses:
        pf = sum(wins) / abs(sum(losses))
    else:
        pf = float("inf") if wins else 0.0
    win = len(wins) / len(trade_returns)
    return {"sharpe": sharpe, "pf": pf, "win": win, "trades": len(trade_returns), "maxdd": maxdd}


def simulate_trades(
    opens: List[float],
    highs: List[float],
    lows: List[float],
    closes: List[float],
    *,
    entries: List[int],
    exits: List[int],
    sl: float,
    tp: float,
    slippage: float,
) -> List[tuple]:
    trades: List[tuple] = []
    entry_set = set(entries)
    exit_set = set(exits)
    in_pos = False
    entry_price = 0.0
    entry_idx = 0
    n = len(closes)
    for i in range(n):
        if in_pos:
            sl_hit = lows[i] <= entry_price - sl if sl > 0 else False
            tp_hit = highs[i] >= entry_price + tp if tp > 0 else False
            if sl_hit or tp_hit:
                exit_price = (entry_price - sl) if sl_hit else (entry_price + tp)
                ret = (exit_price - entry_price) / entry_price
                trades.append((entry_idx, i, ret))
                in_pos = False
                continue
            if i in exit_set and i + 1 < n:
                exit_price = opens[i + 1]
                ret = (exit_price - entry_price) / entry_price
                trades.append((entry_idx, i + 1, ret))
                in_pos = False
                continue
        else:
            if i in entry_set and i + 1 < n:
                entry_price = opens[i + 1] + slippage
                entry_idx = i + 1
                in_pos = True
    return trades


def _load_ohlc(path: str) -> tuple:
    import csv

    timestamps: List[int] = []
    opens: List[float] = []
    highs: List[float] = []
    lows: List[float] = []
    closes: List[float] = []
    with open(path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            timestamps.append(int(float(row["timestamp"])))
            opens.append(float(row["open"]))
            highs.append(float(row["high"]))
            lows.append(float(row["low"]))
            closes.append(float(row["close"]))
    return timestamps, opens, highs, lows, closes


def _golden_cross_signals(closes: List[float], short: int = 50, long: int = 200) -> tuple:
    sma_s = sma_series(closes, short)
    sma_l = sma_series(closes, long)
    entries: List[int] = []
    exits: List[int] = []
    for i in range(1, len(closes)):
        if sma_s[i] is None or sma_l[i] is None or sma_s[i - 1] is None or sma_l[i - 1] is None:
            continue
        if sma_s[i] > sma_l[i] and sma_s[i - 1] <= sma_l[i - 1]:
            entries.append(i)
        if sma_s[i] < sma_l[i] and sma_s[i - 1] >= sma_l[i - 1]:
            exits.append(i)
    return entries, exits


def _rsi_reversion_signals(closes: List[float], period: int = 2, entry_th: float = 10.0, exit_th: float = 90.0) -> tuple:
    rsi = rsi_series(closes, period)
    entries: List[int] = []
    exits: List[int] = []
    for i in range(len(closes)):
        if rsi[i] is None:
            continue
        if rsi[i] < entry_th:
            entries.append(i)
        if rsi[i] > exit_th:
            exits.append(i)
    return entries, exits


def _pip_size(pair: str) -> float:
    return 0.01 if pair.endswith("JPY") else 0.0001


def run_comparison(pairs: List[str], data_dir: str = "data/historical") -> List[dict]:
    strategies = [
        {
            "name": "Legend-Golden-Cross-Classic",
            "tf": "H1",
            "type": "golden_cross",
            "sl": 1.0,
            "tp": 2.0,
        },
        {
            "name": "Legend-RSI-Reversion-V1",
            "tf": "M5",
            "type": "rsi_reversion",
            "sl": 0.10,
            "tp": 0.10,
        },
    ]
    results: List[dict] = []
    for pair in pairs:
        for strat in strategies:
            path = f"{data_dir}/{pair}_{strat['tf']}.csv"
            timestamps, opens, highs, lows, closes = _load_ohlc(path)
            if not closes:
                raise ValueError(f"Empty data: {path}")
            split_idx = int(len(closes) * 0.8)
            preds = model_gate_predict(closes)
            if strat["type"] == "golden_cross":
                entries, exits = _golden_cross_signals(closes)
            else:
                entries, exits = _rsi_reversion_signals(closes)

            slippage = 0.5 * _pip_size(pair)

            def _rel(idxs: List[int], start: int, end: int) -> List[int]:
                return [i - start for i in idxs if start <= i < end]

            def _segment_metrics(start: int, end: int, *, gate: bool) -> dict:
                ts = timestamps[start:end]
                op = opens[start:end]
                hi = highs[start:end]
                lo = lows[start:end]
                cl = closes[start:end]

                seg_entries = entries
                if gate:
                    seg_entries = [i for i in entries if preds[i] == "BUY"]

                ent = _rel(seg_entries, start, end)
                ex = _rel(exits, start, end)
                trades = simulate_trades(
                    op,
                    hi,
                    lo,
                    cl,
                    entries=ent,
                    exits=ex,
                    sl=strat["sl"],
                    tp=strat["tp"],
                    slippage=slippage,
                )
                trade_returns = [ret for _e, _x, ret in trades]
                daily = daily_returns_from_trades(ts, trades)
                return compute_metrics(trade_returns, daily_returns=daily)

            base_is = _segment_metrics(0, split_idx, gate=False)
            base_oos = _segment_metrics(split_idx, len(closes), gate=False)
            gate_is = _segment_metrics(0, split_idx, gate=True)
            gate_oos = _segment_metrics(split_idx, len(closes), gate=True)
            results.append(
                {
                    "pair": pair,
                    "strategy": strat["name"],
                    "base_is": base_is,
                    "base_oos": base_oos,
                    "gate_is": gate_is,
                    "gate_oos": gate_oos,
                }
            )
    return results


def _fmt(val: float) -> str:
    if isinstance(val, float) and math.isinf(val):
        return "inf"
    return f"{val:.2f}"


def _print_table(results: List[dict], key: str, title: str) -> None:
    print(title)
    print(
        "Pair/Strategy | Base DailySharpe | Base PF | Base Win% | Base Trades | Base MaxDD | "
        "Gate DailySharpe | Gate PF | Gate Win% | Gate Trades | Gate MaxDD"
    )
    for r in results:
        base = r[f"base_{key}"]
        gate = r[f"gate_{key}"]
        print(
            f"{r['pair']}/{r['strategy']} | "
            f"{_fmt(base['sharpe'])} | {_fmt(base['pf'])} | {_fmt(base['win']*100)} | {base['trades']} | {_fmt(base['maxdd']*100)} | "
            f"{_fmt(gate['sharpe'])} | {_fmt(gate['pf'])} | {_fmt(gate['win']*100)} | {gate['trades']} | {_fmt(gate['maxdd']*100)}"
        )


def main(argv: Optional[List[str]] = None) -> int:
    import argparse

    parser = argparse.ArgumentParser(description="Legend gate comparison (local)")
    parser.add_argument("--pairs", nargs="*", default=["USDJPY", "EURUSD", "GBPUSD"])
    args = parser.parse_args(argv)

    results = run_comparison(args.pairs)
    _print_table(results, "is", "IS Metrics (80% oldest)")
    print()
    _print_table(results, "oos", "OOS Metrics (newest 20%)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
