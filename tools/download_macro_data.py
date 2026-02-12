#!/usr/bin/env python3
"""Phase 19: Global Macro Matrix Data Fetcher."""

from __future__ import annotations

import csv
import json
import os
from datetime import datetime, timezone
from typing import Callable, Dict, List, Mapping, Optional, Sequence
from urllib.parse import quote, urlencode
from urllib.request import Request, urlopen

# DATA MAP (Yahoo Finance Tickers)
MACRO_MAP = {
    "DXY": "DX-Y.NYB",
    "US10Y": "^TNX",
    "SPX": "^GSPC",
    "NI225": "^N225",
    "USDJPY": "USDJPY=X",
    "DAX": "^GDAXI",
    "EURUSD": "EURUSD=X",
    "FTSE": "^FTSE",
    "GBPUSD": "GBPUSD=X",
    "WTI": "CL=F",
    "XAU": "GC=F",
    "VIX": "^VIX",
}

DATA_DIR = "/home/swimmy/swimmy/data/macro"
YAHOO_CHART_ENDPOINT = "https://query1.finance.yahoo.com/v8/finance/chart"
DEFAULT_HTTP_USER_AGENT = "Mozilla/5.0 (X11; Linux x86_64) SwimmyMacroFetcher/1.0"

CSV_HEADER = ["Datetime", "Open", "High", "Low", "Close", "Volume"]


def _http_get_json(
    *,
    ticker: str,
    period: str,
    interval: str,
    opener: Optional[Callable] = None,
) -> Dict[str, object]:
    params = urlencode(
        {
            "interval": interval,
            "range": period,
            "includePrePost": "false",
            "events": "history",
        }
    )
    url = f"{YAHOO_CHART_ENDPOINT}/{quote(str(ticker), safe='')}?{params}"
    request = Request(
        url,
        headers={
            "Accept": "application/json",
            "User-Agent": DEFAULT_HTTP_USER_AGENT,
        },
    )
    open_fn = opener if opener is not None else urlopen
    with open_fn(request, timeout=20) as response:
        body = response.read()
    data = json.loads(body.decode("utf-8"))
    if not isinstance(data, dict):
        raise RuntimeError("chart payload is not an object")
    return data


def _extract_rows_from_chart(payload: Dict[str, object]) -> List[List[object]]:
    chart = payload.get("chart") if isinstance(payload, dict) else None
    if not isinstance(chart, dict):
        raise RuntimeError("chart payload missing chart node")
    if chart.get("error"):
        raise RuntimeError(f"chart api error: {chart.get('error')}")

    result = chart.get("result")
    if not isinstance(result, list) or not result:
        return []
    node = result[0] if isinstance(result[0], dict) else {}
    timestamps = node.get("timestamp")
    if not isinstance(timestamps, list) or not timestamps:
        return []

    indicators = node.get("indicators") if isinstance(node, dict) else {}
    quote_nodes = indicators.get("quote") if isinstance(indicators, dict) else []
    quote = quote_nodes[0] if isinstance(quote_nodes, list) and quote_nodes and isinstance(quote_nodes[0], dict) else {}

    opens = quote.get("open") if isinstance(quote, dict) else []
    highs = quote.get("high") if isinstance(quote, dict) else []
    lows = quote.get("low") if isinstance(quote, dict) else []
    closes = quote.get("close") if isinstance(quote, dict) else []
    volumes = quote.get("volume") if isinstance(quote, dict) else []

    if not all(isinstance(x, list) for x in (opens, highs, lows, closes)):
        return []
    if not isinstance(volumes, list):
        volumes = []

    size = min(len(timestamps), len(opens), len(highs), len(lows), len(closes))
    rows: List[List[object]] = []
    for i in range(size):
        open_v = opens[i]
        high_v = highs[i]
        low_v = lows[i]
        close_v = closes[i]
        if open_v is None or high_v is None or low_v is None or close_v is None:
            continue
        volume_v = volumes[i] if i < len(volumes) else 0
        if volume_v is None:
            volume_v = 0
        ts = datetime.fromtimestamp(int(timestamps[i]), tz=timezone.utc).isoformat(sep=" ")
        rows.append([ts, float(open_v), float(high_v), float(low_v), float(close_v), float(volume_v)])
    return rows


def fetch_rows_for_ticker(
    *,
    ticker: str,
    period: str = "1y",
    interval: str = "1h",
    opener: Optional[Callable] = None,
) -> List[List[object]]:
    payload = _http_get_json(ticker=ticker, period=period, interval=interval, opener=opener)
    return _extract_rows_from_chart(payload)


def _write_rows_csv(path: str, rows: Sequence[Sequence[object]]) -> None:
    with open(path, "w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(CSV_HEADER)
        writer.writerows(rows)


def fetch_data(
    *,
    data_dir: str = DATA_DIR,
    macro_map: Mapping[str, str] = MACRO_MAP,
    fetch_rows_fn: Callable[..., List[List[object]]] = fetch_rows_for_ticker,
    period: str = "1y",
    interval: str = "1h",
    opener: Optional[Callable] = None,
) -> None:
    if not os.path.exists(data_dir):
        os.makedirs(data_dir)

    print(f"[MACRO] Fetching Global Drivers to {data_dir}...")

    for name, ticker in macro_map.items():
        try:
            print(f"  > Downloading {name} ({ticker})...")
            rows = fetch_rows_fn(ticker=ticker, period=period, interval=interval, opener=opener)

            if not rows:
                print(f"    [WARNING] No data for {name}")
                continue

            # Atomic Write: Save to .tmp then rename
            outfile = os.path.join(data_dir, f"{name}.csv")
            tmpfile = os.path.join(data_dir, f"{name}.tmp")

            _write_rows_csv(tmpfile, rows)
            os.replace(tmpfile, outfile)
            print(f"    [OK] Saved {len(rows)} rows to {name}.csv (Atomic)")

        except Exception as e:
            print(f"    [ERROR] Failed to fetch {name}: {e}")


if __name__ == "__main__":
    fetch_data()
