#!/usr/bin/env python3
"""Shared Yahoo data loading helpers for XAU autobot tools."""

from __future__ import annotations

import csv
import json
import os
import re
from datetime import datetime, timedelta, timezone
from pathlib import Path
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
DEFAULT_RESAMPLE_ANCHOR = "UTC_00:00"
DEFAULT_RESAMPLE_OFFSET_MINUTES = 0
SUPPORTED_DATA_SOURCES = {"auto", "yahoo", "mt5_csv"}


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


def normalize_interval_alias(interval: str) -> Tuple[str, int]:
    key = str(interval or "").strip().lower()
    if key == "":
        raise ValueError("interval must not be empty")

    alias_map = {
        "1m": "1m",
        "m1": "1m",
        "2m": "2m",
        "m2": "2m",
        "5m": "5m",
        "m5": "5m",
        "20m": "5m",
        "m20": "5m",
        "15m": "15m",
        "m15": "15m",
        "45m": "15m",
        "m45": "15m",
        "30m": "30m",
        "m30": "30m",
        "60m": "60m",
        "m60": "60m",
        "1h": "60m",
        "h1": "60m",
        "120m": "60m",
        "m120": "60m",
        "2h": "60m",
        "h2": "60m",
        "180m": "60m",
        "m180": "60m",
        "3h": "60m",
        "h3": "60m",
        "90m": "90m",
        "m90": "90m",
        "240m": "4h",
        "m240": "4h",
        "4h": "4h",
        "h4": "4h",
        "1d": "1d",
        "d1": "1d",
        "1w": "1wk",
        "1wk": "1wk",
        "w1": "1wk",
        "1mo": "1mo",
        "mn": "1mo",
        "mn1": "1mo",
        "month": "1mo",
    }
    normalized = alias_map.get(key, "")
    if normalized != "":
        if key in {"20m", "m20"}:
            return normalized, 4
        if key in {"45m", "m45"}:
            return normalized, 3
        if key in {"120m", "m120", "2h", "h2"}:
            return normalized, 2
        if key in {"180m", "m180", "3h", "h3"}:
            return normalized, 3
        return normalized, 1

    # Synthetic timeframe aliases for research paths (e.g., h5, h60).
    if key in {"5h", "h5", "300m", "m300"}:
        return "60m", 5
    if key in {"60h", "h60", "3600m", "m3600"}:
        return "60m", 60

    return key, 1


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


def _resample_ohlc_fixed(
    *,
    times: Sequence,
    opens: Sequence[float],
    highs: Sequence[float],
    lows: Sequence[float],
    closes: Sequence[float],
    factor: int,
    base_interval_minutes: int,
    anchor: str,
    offset_minutes: int,
) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    if factor <= 1:
        return list(times), list(opens), list(highs), list(lows), list(closes)

    if base_interval_minutes <= 0:
        raise ValueError(f"base_interval_minutes must be > 0: {base_interval_minutes}")
    if str(anchor).strip().upper() != DEFAULT_RESAMPLE_ANCHOR:
        raise ValueError(f"unsupported resample anchor: {anchor}")

    size = min(len(times), len(opens), len(highs), len(lows), len(closes))
    if size < factor:
        raise RuntimeError(f"not enough bars to resample: size={size} factor={factor}")

    bucket_seconds = int(base_interval_minutes * factor * 60)
    offset_seconds = int(offset_minutes * 60)
    out_times: List = []
    out_opens: List[float] = []
    out_highs: List[float] = []
    out_lows: List[float] = []
    out_closes: List[float] = []

    current_bucket: Optional[int] = None
    bucket_indices: List[int] = []

    def _to_utc(ts: object) -> datetime:
        if isinstance(ts, datetime):
            dt = ts
        elif hasattr(ts, "to_pydatetime"):
            dt = ts.to_pydatetime()
        elif isinstance(ts, (int, float)):
            dt = datetime.fromtimestamp(float(ts), tz=timezone.utc)
        else:
            raise ValueError(f"unsupported timestamp type: {type(ts)!r}")
        if dt.tzinfo is None:
            return dt.replace(tzinfo=timezone.utc)
        return dt.astimezone(timezone.utc)

    def _flush(indices: List[int]) -> None:
        if len(indices) < factor:
            return
        lo = indices[0]
        hi = indices[-1]
        out_times.append(times[hi])
        out_opens.append(float(opens[lo]))
        out_highs.append(float(max(highs[idx] for idx in indices)))
        out_lows.append(float(min(lows[idx] for idx in indices)))
        out_closes.append(float(closes[hi]))

    for idx in range(size):
        ts_utc = _to_utc(times[idx])
        day_anchor = ts_utc.replace(hour=0, minute=0, second=0, microsecond=0) + timedelta(seconds=offset_seconds)
        if ts_utc < day_anchor:
            day_anchor = day_anchor - timedelta(days=1)
        elapsed = int((ts_utc - day_anchor).total_seconds())
        bucket = int((day_anchor + timedelta(seconds=(elapsed // bucket_seconds) * bucket_seconds)).timestamp())
        if current_bucket is None:
            current_bucket = bucket
        if bucket != current_bucket:
            _flush(bucket_indices)
            bucket_indices = []
            current_bucket = bucket
        bucket_indices.append(idx)

    _flush(bucket_indices)

    if not out_times:
        raise RuntimeError(
            f"not enough aligned bars to resample: size={size} factor={factor} "
            f"anchor={anchor} offset_minutes={offset_minutes}"
        )

    return out_times, out_opens, out_highs, out_lows, out_closes


def _interval_to_minutes(interval: str) -> int:
    key = str(interval or "").strip().lower()
    if key.endswith("m") and key[:-1].isdigit():
        return int(key[:-1])
    if key.endswith("h") and key[:-1].isdigit():
        return int(key[:-1]) * 60
    mapping = {
        "1d": 1440,
        "1wk": 10080,
        "1mo": 43200,
    }
    out = mapping.get(key, 0)
    if out <= 0:
        raise ValueError(f"unsupported interval minutes conversion: {interval}")
    return out


def _resolve_resample_contract(
    *,
    anchor: Optional[str],
    offset_minutes: Optional[int],
) -> Tuple[str, int]:
    raw_anchor = str(anchor or os.getenv("XAU_AUTOBOT_RESAMPLE_ANCHOR", DEFAULT_RESAMPLE_ANCHOR)).strip()
    if raw_anchor == "":
        raw_anchor = DEFAULT_RESAMPLE_ANCHOR
    normalized_anchor = raw_anchor.upper()
    if normalized_anchor != DEFAULT_RESAMPLE_ANCHOR:
        raise ValueError(f"unsupported resample anchor: {raw_anchor}")

    raw_offset: object
    if offset_minutes is None:
        raw_offset = os.getenv("XAU_AUTOBOT_RESAMPLE_OFFSET_MINUTES", str(DEFAULT_RESAMPLE_OFFSET_MINUTES))
    else:
        raw_offset = offset_minutes
    try:
        normalized_offset = int(raw_offset)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"invalid resample offset minutes: {raw_offset}") from exc
    return normalized_anchor, normalized_offset


def _parse_csv_timestamp(raw_value: object) -> datetime:
    text = str(raw_value).strip()
    if text == "":
        raise ValueError("empty timestamp")
    numeric = re.fullmatch(r"-?\d+(?:\.\d+)?", text)
    if numeric:
        return datetime.fromtimestamp(float(text), tz=timezone.utc)

    for fmt in ("%Y.%m.%d %H:%M:%S", "%Y-%m-%d %H:%M:%S"):
        try:
            dt = datetime.strptime(text, fmt)
            return dt.replace(tzinfo=timezone.utc)
        except ValueError:
            continue

    try:
        parsed = datetime.fromisoformat(text.replace("Z", "+00:00"))
    except ValueError as exc:
        raise ValueError(f"unsupported timestamp format: {text}") from exc
    if parsed.tzinfo is None:
        return parsed.replace(tzinfo=timezone.utc)
    return parsed.astimezone(timezone.utc)


def _resolve_csv_column_indices(header: Sequence[str]) -> Tuple[int, int, int, int, int]:
    normalized = [str(col).strip().lower() for col in header]
    if {"timestamp", "open", "high", "low", "close"}.issubset(set(normalized)):
        return (
            normalized.index("timestamp"),
            normalized.index("open"),
            normalized.index("high"),
            normalized.index("low"),
            normalized.index("close"),
        )
    return 0, 1, 2, 3, 4


def _load_ohlc_mt5_csv(
    *,
    source_csv_path: str,
    period: str,
) -> Tuple[List[datetime], List[float], List[float], List[float], List[float], int]:
    path = Path(str(source_csv_path or "").strip()).expanduser()
    if str(path) == "":
        raise RuntimeError("mt5_csv source requires source_csv_path")
    if not path.is_absolute():
        path = Path.cwd() / path
    if not path.exists():
        raise RuntimeError(f"mt5_csv source not found: {path}")

    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.reader(f)
        rows = list(reader)
    if not rows:
        raise RuntimeError(f"mt5_csv source is empty: {path}")

    first_row = [str(cell).strip() for cell in rows[0]]
    has_header = bool(first_row and first_row[0].lower() == "timestamp")
    col_ts, col_open, col_high, col_low, col_close = _resolve_csv_column_indices(first_row)
    data_rows = rows[1:] if has_header else rows

    by_time: dict[datetime, Tuple[float, float, float, float]] = {}
    for raw in data_rows:
        if len(raw) < 5:
            continue
        try:
            ts = _parse_csv_timestamp(raw[col_ts])
            op = float(raw[col_open])
            hi = float(raw[col_high])
            lo = float(raw[col_low])
            cl = float(raw[col_close])
        except (ValueError, IndexError):
            continue
        by_time[ts] = (op, hi, lo, cl)

    if not by_time:
        raise RuntimeError(f"mt5_csv source has no usable rows: {path}")

    times = sorted(by_time.keys())
    period_td = parse_period_to_timedelta(period)
    if period_td is not None:
        start = times[-1] - period_td
        times = [ts for ts in times if ts >= start]
    if len(times) < 2:
        raise RuntimeError(f"not enough mt5_csv rows after period filter: {path}")

    opens: List[float] = []
    highs: List[float] = []
    lows: List[float] = []
    closes: List[float] = []
    for ts in times:
        op, hi, lo, cl = by_time[ts]
        opens.append(op)
        highs.append(hi)
        lows.append(lo)
        closes.append(cl)

    deltas_minutes: List[int] = []
    prev = times[0]
    for current in times[1:]:
        delta_seconds = int((current - prev).total_seconds())
        if delta_seconds > 0:
            deltas_minutes.append(delta_seconds // 60)
        prev = current
    if not deltas_minutes:
        raise RuntimeError(f"cannot infer base interval from mt5_csv source: {path}")
    base_interval_minutes = max(1, min(deltas_minutes))
    return times, opens, highs, lows, closes, base_interval_minutes


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
    resample_anchor: Optional[str] = None,
    resample_offset_minutes: Optional[int] = None,
    data_source: str = "auto",
    source_csv_path: Optional[str] = None,
) -> Tuple[List, List[float], List[float], List[float], List[float]]:
    normalized_interval, resample_factor = normalize_interval_alias(interval)
    requested_source = str(data_source or "").strip().lower()
    if requested_source == "":
        requested_source = "auto"
    if requested_source not in SUPPORTED_DATA_SOURCES:
        raise ValueError(
            f"unsupported data source: {requested_source} "
            "(supported: auto, yahoo, mt5_csv)"
        )
    anchor, offset_minutes = _resolve_resample_contract(
        anchor=resample_anchor,
        offset_minutes=resample_offset_minutes,
    )
    target_interval_minutes = _interval_to_minutes(normalized_interval) * int(resample_factor)

    if requested_source == "mt5_csv":
        source = str(source_csv_path or "").strip()
        if source == "":
            raise RuntimeError("data_source=mt5_csv requires source_csv_path")
        times, opens, highs, lows, closes, base_interval_minutes = _load_ohlc_mt5_csv(
            source_csv_path=source,
            period=period,
        )
        if target_interval_minutes < base_interval_minutes:
            raise RuntimeError(
                "requested interval is smaller than mt5_csv base interval: "
                f"target={target_interval_minutes}m base={base_interval_minutes}m"
            )
        if target_interval_minutes % base_interval_minutes != 0:
            raise RuntimeError(
                "requested interval is not divisible by mt5_csv base interval: "
                f"target={target_interval_minutes}m base={base_interval_minutes}m"
            )
        total_factor = max(1, target_interval_minutes // base_interval_minutes)
        return _resample_ohlc_fixed(
            times=times,
            opens=opens,
            highs=highs,
            lows=lows,
            closes=closes,
            factor=total_factor,
            base_interval_minutes=base_interval_minutes,
            anchor=anchor,
            offset_minutes=offset_minutes,
        )

    base_interval_minutes = _interval_to_minutes(normalized_interval)
    try:
        times, opens, highs, lows, closes = _load_ohlc_chart_api(
            ticker=ticker,
            period=period,
            interval=normalized_interval,
            http_opener=http_opener,
        )
        return _resample_ohlc_fixed(
            times=times,
            opens=opens,
            highs=highs,
            lows=lows,
            closes=closes,
            factor=resample_factor,
            base_interval_minutes=base_interval_minutes,
            anchor=anchor,
            offset_minutes=offset_minutes,
        )
    except Exception as chart_err:
        module = yf_module if yf_module is not None else yf
        if module is None:
            raise RuntimeError(
                "chart api unavailable and yfinance is unavailable; "
                f"ticker={ticker} period={period} interval={interval}"
            ) from chart_err
        times, opens, highs, lows, closes = _load_ohlc_yfinance(
            yf_module=module,
            ticker=ticker,
            period=period,
            interval=normalized_interval,
        )
        return _resample_ohlc_fixed(
            times=times,
            opens=opens,
            highs=highs,
            lows=lows,
            closes=closes,
            factor=resample_factor,
            base_interval_minutes=base_interval_minutes,
            anchor=anchor,
            offset_minutes=offset_minutes,
        )
