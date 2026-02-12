#!/usr/bin/env python3
"""Shared Yahoo data loading helpers for XAU autobot tools."""

from __future__ import annotations

import json
import re
from datetime import datetime, timedelta, timezone
from typing import Callable, List, Optional, Sequence, Tuple
from urllib.parse import quote, urlencode
from urllib.request import Request, urlopen

try:
    import pandas as pd
except Exception:
    pd = None

try:
    import yfinance as yf
except Exception:
    yf = None

YAHOO_CHART_ENDPOINT = "https://query1.finance.yahoo.com/v8/finance/chart"
DEFAULT_HTTP_USER_AGENT = "Mozilla/5.0 (X11; Linux x86_64) SwimmyXAUAutoBot/1.0"


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


def _normalize_period(period: str) -> str:
    return re.sub(r"\s+", "", str(period).strip().lower())


def _chart_url_for_ticker(ticker: str) -> str:
    return f"{YAHOO_CHART_ENDPOINT}/{quote(str(ticker), safe='')}"


def _http_get_json(
    *,
    url: str,
    params: dict,
    opener: Optional[Callable] = None,
):
    query = urlencode(params)
    req = Request(
        f"{url}?{query}",
        headers={
            "Accept": "application/json",
            "User-Agent": DEFAULT_HTTP_USER_AGENT,
        },
    )
    open_fn = opener if opener is not None else urlopen
    with open_fn(req, timeout=20) as resp:
        body = resp.read()
    return json.loads(body.decode("utf-8"))


def _extract_ohlc_from_chart(payload) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    if not isinstance(payload, dict):
        raise RuntimeError("invalid chart payload")
    chart = payload.get("chart") or {}
    error = chart.get("error")
    if error:
        raise RuntimeError(f"chart api error: {error}")

    result = chart.get("result") or []
    if not result:
        raise RuntimeError("chart api result is empty")
    node = result[0] or {}
    timestamps = node.get("timestamp") or []
    indicators = node.get("indicators") or {}
    quote_nodes = indicators.get("quote") or []
    if not quote_nodes:
        raise RuntimeError("chart api quote data is missing")

    quote_node = quote_nodes[0] or {}
    opens = quote_node.get("open") or []
    highs = quote_node.get("high") or []
    lows = quote_node.get("low") or []
    closes = quote_node.get("close") or []
    size = min(len(timestamps), len(opens), len(highs), len(lows), len(closes))
    if size == 0:
        raise RuntimeError("chart api returned no rows")

    out_times: List[datetime] = []
    out_opens: List[float] = []
    out_highs: List[float] = []
    out_lows: List[float] = []
    out_closes: List[float] = []
    for i in range(size):
        op = opens[i]
        hi = highs[i]
        lo = lows[i]
        cl = closes[i]
        if op is None or hi is None or lo is None or cl is None:
            continue
        ts = datetime.fromtimestamp(int(timestamps[i]), tz=timezone.utc)
        out_times.append(ts)
        out_opens.append(float(op))
        out_highs.append(float(hi))
        out_lows.append(float(lo))
        out_closes.append(float(cl))

    if not out_times:
        raise RuntimeError("chart api returned only null rows")

    return out_times, out_opens, out_highs, out_lows, out_closes


def _download_chart_once(
    *,
    ticker: str,
    period: str,
    interval: str,
    opener: Optional[Callable] = None,
) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    payload = _http_get_json(
        url=_chart_url_for_ticker(ticker),
        params={
            "interval": interval,
            "range": _normalize_period(period),
            "includePrePost": "false",
            "events": "history",
        },
        opener=opener,
    )
    return _extract_ohlc_from_chart(payload)


def _download_chart_window(
    *,
    ticker: str,
    start: datetime,
    end: datetime,
    interval: str,
    opener: Optional[Callable] = None,
) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    payload = _http_get_json(
        url=_chart_url_for_ticker(ticker),
        params={
            "interval": interval,
            "period1": str(int(start.timestamp())),
            "period2": str(int(end.timestamp())),
            "includePrePost": "false",
            "events": "history",
        },
        opener=opener,
    )
    return _extract_ohlc_from_chart(payload)


def _merge_ohlc_chunks(
    chunks: Sequence[Tuple[List, List[float], List[float], List[float], List[float]]]
) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    by_time = {}
    for times, opens, highs, lows, closes in chunks:
        size = min(len(times), len(opens), len(highs), len(lows), len(closes))
        for i in range(size):
            by_time[times[i]] = (opens[i], highs[i], lows[i], closes[i])
    if not by_time:
        raise RuntimeError("no chunk rows")

    out_times = sorted(by_time.keys())
    out_opens: List[float] = []
    out_highs: List[float] = []
    out_lows: List[float] = []
    out_closes: List[float] = []
    for ts in out_times:
        op, hi, lo, cl = by_time[ts]
        out_opens.append(float(op))
        out_highs.append(float(hi))
        out_lows.append(float(lo))
        out_closes.append(float(cl))
    return out_times, out_opens, out_highs, out_lows, out_closes


def _load_ohlc_chart_api(
    *,
    ticker: str,
    period: str,
    interval: str,
    http_opener: Optional[Callable] = None,
) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    if not requires_chunked_intraday(interval=interval, period=period):
        return _download_chart_once(ticker=ticker, period=period, interval=interval, opener=http_opener)

    span = parse_period_to_timedelta(period)
    if span is None:
        return _download_chart_once(ticker=ticker, period=period, interval=interval, opener=http_opener)

    end = datetime.now(timezone.utc)
    start = end - span
    windows = build_chunk_windows(start=start, end=end, chunk_days=59)
    chunks = []
    for lo, hi in windows:
        try:
            chunk = _download_chart_window(
                ticker=ticker,
                start=lo,
                end=hi,
                interval=interval,
                opener=http_opener,
            )
            chunks.append(chunk)
        except RuntimeError:
            continue
    if not chunks:
        raise RuntimeError(f"no data: ticker={ticker} period={period} interval={interval}")
    return _merge_ohlc_chunks(chunks)


def _load_ohlc_yfinance(
    *,
    yf_module,
    ticker: str,
    period: str,
    interval: str,
) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    if yf_module is None:
        raise RuntimeError("yfinance unavailable")

    if requires_chunked_intraday(interval=interval, period=period):
        df = _download_chunked(yf_module=yf_module, ticker=ticker, period=period, interval=interval)
        if isinstance(df, Sequence) and len(df) == 0:
            raise RuntimeError(f"no data: ticker={ticker} period={period} interval={interval}")
    else:
        df = _download_once(yf_module=yf_module, ticker=ticker, period=period, interval=interval)

    if len(df) == 0:
        raise RuntimeError(f"no data: ticker={ticker} period={period} interval={interval}")
    return _to_ohlc(df)


def load_ohlc(
    *,
    ticker: str,
    period: str,
    interval: str,
    yf_module=None,
    http_opener: Optional[Callable] = None,
) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    try:
        return _load_ohlc_chart_api(
            ticker=ticker,
            period=period,
            interval=interval,
            http_opener=http_opener,
        )
    except Exception as chart_err:
        module = yf_module if yf_module is not None else yf
        if module is None:
            raise RuntimeError(
                "chart api unavailable and yfinance is unavailable; "
                f"ticker={ticker} period={period} interval={interval}"
            ) from chart_err
        return _load_ohlc_yfinance(
            yf_module=module,
            ticker=ticker,
            period=period,
            interval=interval,
        )
