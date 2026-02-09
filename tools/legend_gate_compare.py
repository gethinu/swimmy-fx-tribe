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
