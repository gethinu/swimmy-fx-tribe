#!/usr/bin/env python3
"""Fetch Polymarket settlements for market ids observed in paper-trade journal."""

from __future__ import annotations

import argparse
import json
import os
import time
from pathlib import Path
from typing import Any, Dict, Iterable, List, Mapping, Optional, Sequence, Set, Tuple
from urllib.parse import urlencode
from urllib.request import Request, urlopen


def _to_float(value: Any) -> Optional[float]:
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def _parse_list(value: Any) -> List[Any]:
    if isinstance(value, list):
        return value
    if isinstance(value, str):
        text = value.strip()
        if not text:
            return []
        try:
            parsed = json.loads(text)
            return parsed if isinstance(parsed, list) else []
        except json.JSONDecodeError:
            return []
    return []


def _normalize_outcome(value: Any) -> Optional[str]:
    text = str(value or "").strip().upper()
    if text in {"YES", "Y", "TRUE", "1"}:
        return "YES"
    if text in {"NO", "N", "FALSE", "0"}:
        return "NO"
    return None


def extract_winner_from_market(
    market: Mapping[str, Any],
    *,
    min_price_for_win: float = 0.98,
    min_gap: float = 0.05,
) -> Optional[str]:
    direct_fields = ["winner", "outcome", "result", "winningOutcome", "resolution"]
    for key in direct_fields:
        winner = _normalize_outcome(market.get(key))
        if winner:
            return winner

    tokens = market.get("tokens")
    if isinstance(tokens, list):
        for token in tokens:
            if not isinstance(token, Mapping):
                continue
            winner = _normalize_outcome(token.get("winner"))
            if winner:
                return winner
            winner_bool = token.get("winning", token.get("isWinner"))
            if isinstance(winner_bool, bool) and winner_bool:
                return _normalize_outcome(token.get("outcome"))

    outcomes = [str(item).strip() for item in _parse_list(market.get("outcomes"))]
    prices = [_to_float(item) for item in _parse_list(market.get("outcomePrices"))]
    if len(outcomes) != len(prices) or len(outcomes) < 2:
        return None

    normalized_outcomes = [_normalize_outcome(item) for item in outcomes]
    if normalized_outcomes.count("YES") != 1 or normalized_outcomes.count("NO") != 1:
        return None

    winning_idx = max(range(len(prices)), key=lambda idx: (prices[idx] if prices[idx] is not None else -1.0))
    losing_idx = min(range(len(prices)), key=lambda idx: (prices[idx] if prices[idx] is not None else 2.0))
    winning_price = prices[winning_idx]
    losing_price = prices[losing_idx]
    if winning_price is None or losing_price is None:
        return None
    if winning_price < min_price_for_win:
        return None
    if (winning_price - losing_price) < min_gap:
        return None
    return normalized_outcomes[winning_idx]


def _iter_journal_records(path: Path) -> Iterable[Mapping[str, Any]]:
    if not path.exists():
        return
    with path.open("r", encoding="utf-8") as handle:
        for raw in handle:
            line = raw.strip()
            if not line:
                continue
            try:
                payload = json.loads(line)
            except json.JSONDecodeError:
                continue
            if isinstance(payload, Mapping):
                yield payload


def collect_market_ids_from_journal(
    journal_file: Path,
    *,
    target_date: str = "",
    skip_market_ids: Optional[Set[str]] = None,
) -> List[str]:
    skip = skip_market_ids or set()
    out: List[str] = []
    seen: Set[str] = set()
    for row in _iter_journal_records(journal_file):
        if str(row.get("type", "")) != "entry":
            continue
        if target_date and str(row.get("date", "")) != target_date:
            continue
        market_id = str(row.get("market_id", "")).strip()
        if not market_id or market_id in seen or market_id in skip:
            continue
        seen.add(market_id)
        out.append(market_id)
    return out


def _request_json(url: str) -> Any:
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-polyclaw-settlement-fetch/1.0"})
    with urlopen(req, timeout=20) as resp:
        return json.loads(resp.read().decode("utf-8"))


def fetch_market_by_id(
    market_id: str,
    *,
    gamma_url: str = "https://gamma-api.polymarket.com/markets",
) -> Optional[Dict[str, Any]]:
    query = urlencode({"id": market_id})
    url = f"{gamma_url}?{query}" if "?" not in gamma_url else f"{gamma_url}&{query}"
    payload = _request_json(url)
    if isinstance(payload, list) and payload:
        first = payload[0]
        return first if isinstance(first, dict) else None
    return None


def fetch_settlements_for_market_ids(
    market_ids: Sequence[str],
    *,
    gamma_url: str = "https://gamma-api.polymarket.com/markets",
    min_price_for_win: float = 0.98,
    min_gap: float = 0.05,
    sleep_ms: int = 0,
) -> Tuple[Dict[str, str], List[str]]:
    settlements: Dict[str, str] = {}
    unresolved: List[str] = []
    sleep_seconds = max(0.0, sleep_ms / 1000.0)
    for market_id in market_ids:
        try:
            market = fetch_market_by_id(market_id, gamma_url=gamma_url)
            winner = extract_winner_from_market(
                market or {},
                min_price_for_win=min_price_for_win,
                min_gap=min_gap,
            )
            if winner:
                settlements[market_id] = winner
            else:
                unresolved.append(market_id)
        except Exception:
            unresolved.append(market_id)
        if sleep_seconds > 0.0:
            time.sleep(sleep_seconds)
    return settlements, unresolved


def _load_existing_settlements(path: Path) -> Dict[str, str]:
    if not path.exists():
        return {}
    payload = json.loads(path.read_text(encoding="utf-8"))
    out: Dict[str, str] = {}
    if isinstance(payload, Mapping):
        for market_id, value in payload.items():
            norm = _normalize_outcome(value)
            if norm:
                out[str(market_id)] = norm
    return out


def main() -> None:
    parser = argparse.ArgumentParser(description="Fetch settlements for market ids in paper-trade journal")
    parser.add_argument("--journal-file", required=True)
    parser.add_argument("--date", default="", help="Optional YYYY-MM-DD filter")
    parser.add_argument("--existing-settlements", default="", help="Optional existing settlements JSON to merge")
    parser.add_argument("--write-settlements", required=True, help="Output JSON path")
    parser.add_argument("--gamma-url", default="https://gamma-api.polymarket.com/markets")
    parser.add_argument("--min-price-for-win", type=float, default=0.98)
    parser.add_argument("--min-gap", type=float, default=0.05)
    parser.add_argument("--sleep-ms", type=int, default=0)
    args = parser.parse_args()

    existing: Dict[str, str] = {}
    if args.existing_settlements:
        existing = _load_existing_settlements(Path(args.existing_settlements))

    market_ids = collect_market_ids_from_journal(
        Path(args.journal_file),
        target_date=args.date.strip(),
        skip_market_ids=set(existing.keys()),
    )
    fetched, unresolved = fetch_settlements_for_market_ids(
        market_ids,
        gamma_url=args.gamma_url,
        min_price_for_win=args.min_price_for_win,
        min_gap=args.min_gap,
        sleep_ms=args.sleep_ms,
    )

    merged = dict(existing)
    merged.update(fetched)
    out_path = Path(args.write_settlements)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(merged, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")

    result = {
        "date_filter": args.date.strip() or "ALL",
        "existing_count": len(existing),
        "requested_market_ids": len(market_ids),
        "fetched_count": len(fetched),
        "total_settlements": len(merged),
        "unresolved_count": len(unresolved),
        "unresolved_market_ids": unresolved[:100],
        "write_settlements": str(out_path),
    }
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
