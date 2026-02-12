#!/usr/bin/env python3
"""Shared Yahoo data loading helpers for XAU autobot tools."""

from __future__ import annotations

import re
from datetime import datetime, timedelta, timezone
from typing import List, Optional, Sequence, Tuple

try:
    import pandas as pd
except Exception:
    pd = None

try:
    import yfinance as yf
except Exception:
    yf = None


def parse_period_to_timedelta(period: str) -> Optional[timedelta]:
    text = str(period).strip().lower()
    match = re.fullmatch(r"(\d+)\s*([a-z]+)", text)
    if not match:
        return None
    amount = int(match.group(1))
    unit = match.group(2)
    if unit in {"d", "day", "days"}:
        return timedelta(days=amount)
    if unit in {"w", "wk", "wks", "week", "weeks"}:
        return timedelta(days=amount * 7)
    if unit in {"mo", "mon", "month", "months"}:
        return timedelta(days=amount * 30)
    if unit in {"y", "yr", "yrs", "year", "years"}:
        return timedelta(days=amount * 365)
    return None


def is_intraday_interval(interval: str) -> bool:
    key = str(interval).strip().lower()
    if key.endswith("mo"):
        return False
    if key.endswith("m") or key.endswith("h"):
        return True
    return False


def requires_chunked_intraday(*, interval: str, period: str, max_intraday_days: int = 60) -> bool:
    if not is_intraday_interval(interval):
        return False
    period_td = parse_period_to_timedelta(period)
    if period_td is None:
        return False
    return period_td > timedelta(days=max_intraday_days)


def build_chunk_windows(
    *,
    start: datetime,
    end: datetime,
    chunk_days: int = 59,
) -> List[Tuple[datetime, datetime]]:
    if end <= start:
        return []
    windows: List[Tuple[datetime, datetime]] = []
    cursor = start
    delta = timedelta(days=max(1, chunk_days))
    while cursor < end:
        nxt = min(cursor + delta, end)
        windows.append((cursor, nxt))
        cursor = nxt
    return windows


def _find_price_column(df, name: str):
    target = name.lower()
    for col in df.columns:
        if isinstance(col, tuple):
            key = str(col[0]).lower()
        else:
            key = str(col).lower()
        if key == target:
            return col
    raise KeyError(name)


def _to_ohlc(df) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    open_col = _find_price_column(df, "open")
    high_col = _find_price_column(df, "high")
    low_col = _find_price_column(df, "low")
    close_col = _find_price_column(df, "close")
    times = df.index.to_pydatetime().tolist()
    opens = df[open_col].astype(float).tolist()
    highs = df[high_col].astype(float).tolist()
    lows = df[low_col].astype(float).tolist()
    closes = df[close_col].astype(float).tolist()
    return times, opens, highs, lows, closes


def _download_once(*, yf_module, ticker: str, period: str, interval: str):
    return yf_module.download(
        ticker,
        period=period,
        interval=interval,
        auto_adjust=False,
        progress=False,
        threads=False,
    )


def _download_chunked(*, yf_module, ticker: str, period: str, interval: str):
    if pd is None:
        raise RuntimeError("pandas is required for chunked download")
    span = parse_period_to_timedelta(period)
    if span is None:
        return _download_once(yf_module=yf_module, ticker=ticker, period=period, interval=interval)

    end = datetime.now(timezone.utc)
    start = end - span
    windows = build_chunk_windows(start=start, end=end, chunk_days=59)

    frames = []
    for lo, hi in windows:
        df = yf_module.download(
            ticker,
            start=lo,
            end=hi,
            interval=interval,
            auto_adjust=False,
            progress=False,
            threads=False,
        )
        if len(df) > 0:
            frames.append(df)

    if not frames:
        return frames

    merged = pd.concat(frames)
    merged = merged.sort_index()
    merged = merged[~merged.index.duplicated(keep="last")]
    return merged


def load_ohlc(
    *,
    ticker: str,
    period: str,
    interval: str,
    yf_module=None,
) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    module = yf_module if yf_module is not None else yf
    if module is None:
        raise RuntimeError("yfinance is required. Use .venv Python where yfinance is installed.")

    if requires_chunked_intraday(interval=interval, period=period):
        df = _download_chunked(yf_module=module, ticker=ticker, period=period, interval=interval)
        if isinstance(df, Sequence) and len(df) == 0:
            raise RuntimeError(f"no data: ticker={ticker} period={period} interval={interval}")
    else:
        df = _download_once(yf_module=module, ticker=ticker, period=period, interval=interval)

    if len(df) == 0:
        raise RuntimeError(f"no data: ticker={ticker} period={period} interval={interval}")
    return _to_ohlc(df)
