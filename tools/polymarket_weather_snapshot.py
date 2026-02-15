#!/usr/bin/env python3
"""Snapshot Polymarket daily-weather markets for realistic (>30d) backtests.

Writes a timestamped snapshot directory containing:
- Gamma series/events/market state (raw + normalized)
- Deterministic signal output (as-run)
- CLOB orderbooks for YES/NO tokens (trimmed to a configurable depth)
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, Iterable, List, Mapping, MutableMapping, Optional, Sequence
from urllib.parse import urlencode
from urllib.request import Request, urlopen
from urllib.error import HTTPError


SCHEMA_VERSION = 1


def _request_json(url: str) -> Any:
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-polymarket-snapshot/1.0"})
    with urlopen(req, timeout=30) as resp:
        return json.loads(resp.read().decode("utf-8"))


def _parse_json_list_field(value: Any) -> List[Any]:
    if isinstance(value, list):
        return value
    if isinstance(value, str):
        text = value.strip()
        if not text:
            return []
        try:
            parsed = json.loads(text)
        except json.JSONDecodeError:
            return []
        return parsed if isinstance(parsed, list) else []
    return []


def parse_clob_token_ids(market: Mapping[str, Any]) -> List[str]:
    raw = market.get("clobTokenIds")
    return [str(item).strip() for item in _parse_json_list_field(raw) if str(item).strip()]


def trim_book_payload(payload: Mapping[str, Any], depth: int) -> Dict[str, Any]:
    cap = max(0, int(depth))
    out = dict(payload)
    bids = payload.get("bids")
    asks = payload.get("asks")
    if isinstance(bids, list):
        out["bids"] = bids[:cap] if cap > 0 else []
    if isinstance(asks, list):
        out["asks"] = asks[:cap] if cap > 0 else []
    return out


def _iter_jsonl_records(stdout: str) -> Iterable[Mapping[str, Any]]:
    for raw in (stdout or "").splitlines():
        line = raw.strip()
        if not line:
            continue
        try:
            payload = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(payload, Mapping):
            yield payload


def load_signals_from_command(command: str) -> List[Dict[str, Any]]:
    cmd = str(command or "").strip()
    if not cmd:
        return []
    proc = subprocess.run(cmd, shell=True, capture_output=True, text=True, check=False)
    if proc.returncode != 0:
        return []
    return [dict(row) for row in _iter_jsonl_records(proc.stdout)]


def _snapshot_dir(base: Path, now: datetime) -> Path:
    ts = now.astimezone(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    day = now.astimezone(timezone.utc).strftime("%Y-%m-%d")
    return (base / day / ts).resolve()


def fetch_gamma_weather_series(*, series_url: str, limit: int = 100, max_pages: int = 50) -> List[Mapping[str, Any]]:
    out: List[Mapping[str, Any]] = []
    size = max(1, int(limit))
    pages = max(1, int(max_pages))
    for page in range(pages):
        offset = page * size
        url = series_url.rstrip("/") + "?" + urlencode(
            {"limit": str(size), "offset": str(offset), "recurrence": "daily", "active": "true", "closed": "false"}
        )
        payload = _request_json(url)
        rows = [row for row in payload if isinstance(row, Mapping)] if isinstance(payload, list) else []
        if not rows:
            break
        out.extend(rows)
        if len(rows) < size:
            break
    # Daily weather series are identified by slug suffix.
    return [row for row in out if str(row.get("slug") or "").lower().endswith("-daily-weather")]


def fetch_gamma_events_for_series(
    *,
    events_url: str,
    series_id: str,
    active: bool = True,
    closed: bool = False,
    limit: int = 200,
    max_pages: int = 20,
) -> List[Mapping[str, Any]]:
    out: List[Mapping[str, Any]] = []
    sid = str(series_id or "").strip()
    if not sid:
        return out
    size = max(1, int(limit))
    pages = max(1, int(max_pages))
    for page in range(pages):
        offset = page * size
        url = events_url.rstrip("/") + "?" + urlencode(
            {
                "series_id": sid,
                "active": "true" if active else "false",
                "closed": "true" if closed else "false",
                "limit": str(size),
                "offset": str(offset),
            }
        )
        payload = _request_json(url)
        rows = [row for row in payload if isinstance(row, Mapping)] if isinstance(payload, list) else []
        if not rows:
            break
        out.extend(rows)
        if len(rows) < size:
            break
    return out


def _write_json(path: Path, payload: Any) -> None:
    path.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")


def _write_jsonl(path: Path, rows: Sequence[Mapping[str, Any]]) -> None:
    with path.open("w", encoding="utf-8") as handle:
        for row in rows:
            handle.write(json.dumps(row, ensure_ascii=False) + "\n")


def collect_snapshot(
    *,
    output_dir: Path,
    now: datetime,
    series_url: str = "https://gamma-api.polymarket.com/series",
    events_url: str = "https://gamma-api.polymarket.com/events",
    clob_host: str = "https://clob.polymarket.com",
    signal_command: str = "",
    orderbook_mode: str = "all",  # all|topk|none (topk not implemented yet)
    orderbook_depth: int = 10,
) -> Path:
    base = Path(output_dir)
    out_dir = _snapshot_dir(base, now)
    out_dir.mkdir(parents=True, exist_ok=True)

    errors: List[Dict[str, Any]] = []

    try:
        series = fetch_gamma_weather_series(series_url=series_url)
    except Exception as exc:
        errors.append(
            {
                "type": "gamma_series_fetch_error",
                "url": str(series_url),
                "error": f"{type(exc).__name__}: {exc}",
            }
        )
        series = []
    _write_json(out_dir / "gamma_series.json", list(series))

    events: List[Dict[str, Any]] = []
    markets_normalized: List[Dict[str, Any]] = []
    market_by_id: Dict[str, Mapping[str, Any]] = {}
    for s in series:
        sid = str(s.get("id") or "").strip()
        if not sid:
            continue
        try:
            series_events = fetch_gamma_events_for_series(
                events_url=events_url, series_id=sid, active=True, closed=False, limit=200, max_pages=1
            )
        except Exception as exc:
            errors.append(
                {
                    "type": "gamma_events_fetch_error",
                    "series_id": sid,
                    "url": str(events_url),
                    "error": f"{type(exc).__name__}: {exc}",
                }
            )
            continue

        for ev in series_events:
            ev_dict = dict(ev)
            ev_dict["_series_id"] = sid
            events.append(ev_dict)
            mkts = ev.get("markets")
            if not isinstance(mkts, list):
                continue
            for m in mkts:
                if not isinstance(m, Mapping):
                    continue
                mid = str(m.get("id") or m.get("market_id") or m.get("conditionId") or "").strip()
                if not mid:
                    continue
                market_by_id[mid] = m
                markets_normalized.append(
                    {
                        "series_id": sid,
                        "event_id": str(ev.get("id") or "").strip(),
                        "market_id": mid,
                        "question": str(m.get("question") or m.get("title") or "").strip(),
                        "condition_id": str(m.get("conditionId") or "").strip(),
                        "clob_token_ids": parse_clob_token_ids(m),
                        "active": m.get("active"),
                        "closed": m.get("closed"),
                        "archived": m.get("archived"),
                        "accepting_orders": m.get("acceptingOrders"),
                    }
                )

    _write_json(out_dir / "gamma_events.json", events)
    _write_jsonl(out_dir / "markets.jsonl", markets_normalized)

    signals = load_signals_from_command(signal_command)
    _write_jsonl(out_dir / "signals.jsonl", signals)

    books_written: List[Dict[str, Any]] = []
    if str(orderbook_mode).lower() != "none":
        market_ids = [str(row.get("market_id") or "").strip() for row in signals if isinstance(row, Mapping)]
        selected_ids = [mid for mid in market_ids if mid]
        token_ids: List[str] = []
        seen_tokens: set[str] = set()
        for mid in selected_ids:
            market = market_by_id.get(mid)
            if not market:
                continue
            for token in parse_clob_token_ids(market):
                if token in seen_tokens:
                    continue
                seen_tokens.add(token)
                token_ids.append(token)
        for token_id in token_ids:
            url = clob_host.rstrip("/") + "/book?" + urlencode({"token_id": token_id})
            try:
                payload = _request_json(url)
            except HTTPError as exc:
                body = ""
                try:
                    body = exc.read().decode("utf-8", errors="replace")
                except Exception:
                    body = ""
                errors.append(
                    {
                        "type": "clob_book_http_error",
                        "token_id": token_id,
                        "url": url,
                        "http_status": int(getattr(exc, "code", 0) or 0),
                        "body": body[:2000],
                    }
                )
                continue
            except Exception as exc:
                errors.append(
                    {
                        "type": "clob_book_error",
                        "token_id": token_id,
                        "url": url,
                        "error": f"{type(exc).__name__}: {exc}",
                    }
                )
                continue

            if isinstance(payload, MutableMapping):
                trimmed = trim_book_payload(payload, orderbook_depth)
            else:
                errors.append(
                    {
                        "type": "clob_book_unexpected_payload",
                        "token_id": token_id,
                        "url": url,
                        "payload_type": type(payload).__name__,
                    }
                )
                continue
            trimmed["token_id"] = token_id
            books_written.append(dict(trimmed))

    _write_jsonl(out_dir / "clob_books.jsonl", books_written)
    if errors:
        _write_jsonl(out_dir / "errors.jsonl", errors)

    manifest = {
        "schema_version": SCHEMA_VERSION,
        "generated_at": now.astimezone(timezone.utc).isoformat().replace("+00:00", "Z"),
        "counts": {
            "series": len(series),
            "events": len(events),
            "markets": len(markets_normalized),
            "signals": len(signals),
            "books": len(books_written),
        },
        "failures": len(errors),
    }
    _write_json(out_dir / "manifest.json", manifest)
    return out_dir


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", default="data/snapshots/polymarket_weather")
    parser.add_argument("--series-url", default="https://gamma-api.polymarket.com/series")
    parser.add_argument("--events-url", default="https://gamma-api.polymarket.com/events")
    parser.add_argument("--clob-host", default="https://clob.polymarket.com")
    parser.add_argument("--signal-command", default=os.getenv("POLYCLAW_OPENCLAW_CMD", "").strip())
    parser.add_argument("--orderbook-mode", default="all", choices=["all", "topk", "none"])
    parser.add_argument("--orderbook-depth", type=int, default=10)
    args = parser.parse_args()

    now = datetime.now(tz=timezone.utc)
    out_dir = collect_snapshot(
        output_dir=Path(args.output_dir),
        now=now,
        series_url=args.series_url,
        events_url=args.events_url,
        clob_host=args.clob_host,
        signal_command=args.signal_command,
        orderbook_mode=args.orderbook_mode,
        orderbook_depth=args.orderbook_depth,
    )
    print(str(out_dir))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
