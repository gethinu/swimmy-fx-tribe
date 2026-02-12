#!/usr/bin/env python3
"""Heuristic OpenClaw-like signal generator for Polymarket markets."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any, Dict, List, Mapping, Sequence, Tuple
from urllib.parse import parse_qsl, urlencode, urlsplit, urlunsplit
from urllib.request import Request, urlopen


def _to_float(value: Any, default: float = 0.0) -> float:
    try:
        return float(value)
    except (TypeError, ValueError):
        return default


def _clamp(value: float, lo: float, hi: float) -> float:
    return max(lo, min(hi, value))


def _parse_array(value: Any) -> List[Any]:
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


def parse_yes_price(market: Mapping[str, Any]) -> float:
    tokens = market.get("tokens")
    if isinstance(tokens, list):
        for token in tokens:
            if not isinstance(token, Mapping):
                continue
            outcome = str(token.get("outcome") or token.get("name") or "").strip().lower()
            if outcome == "yes":
                return _to_float(token.get("price", token.get("bestAsk")), 0.0)

    outcomes = [str(item).strip().lower() for item in _parse_array(market.get("outcomes"))]
    prices = [_to_float(item, 0.0) for item in _parse_array(market.get("outcomePrices"))]
    if len(outcomes) == len(prices):
        for idx, outcome in enumerate(outcomes):
            if outcome == "yes":
                return prices[idx]
    return 0.0


def infer_model_prob(
    *,
    yes_price: float,
    question: str,
    favorite_threshold: float,
    favorite_fade: float,
    underdog_threshold: float,
    underdog_lift: float,
) -> Tuple[float, float]:
    model = yes_price
    if yes_price >= favorite_threshold:
        model -= favorite_fade
    elif yes_price <= underdog_threshold:
        model += underdog_lift

    q = question.lower()
    if any(word in q for word in ("nba", "nfl", "mlb", "lakers", "warriors", "chiefs", "yankees")):
        if yes_price >= favorite_threshold:
            model -= 0.005
        elif yes_price <= underdog_threshold:
            model += 0.005

    model = _clamp(model, 0.01, 0.99)
    confidence = _clamp(0.6 + abs(model - yes_price) * 4.0, 0.55, 0.95)
    return model, confidence


def _match_keywords(question: str, question_keywords: Sequence[str]) -> bool:
    if not question_keywords:
        return True
    text = question.lower()
    return any(keyword.strip().lower() in text for keyword in question_keywords if keyword and keyword.strip())


def build_signals_from_markets(
    *,
    markets: Sequence[Mapping[str, Any]],
    favorite_threshold: float,
    favorite_fade: float,
    underdog_threshold: float,
    underdog_lift: float,
    question_keywords: Sequence[str],
) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for market in markets:
        market_id = str(market.get("id") or "").strip()
        question = str(market.get("question") or market.get("title") or "").strip()
        if not market_id or not question:
            continue
        if not _match_keywords(question, question_keywords):
            continue

        yes_price = parse_yes_price(market)
        if yes_price <= 0.0 or yes_price >= 1.0:
            continue
        p_yes, confidence = infer_model_prob(
            yes_price=yes_price,
            question=question,
            favorite_threshold=favorite_threshold,
            favorite_fade=favorite_fade,
            underdog_threshold=underdog_threshold,
            underdog_lift=underdog_lift,
        )
        out.append(
            {
                "market_id": market_id,
                "p_yes": round(p_yes, 6),
                "confidence": round(confidence, 6),
                "question": question,
                "source_yes_price": round(yes_price, 6),
            }
        )
    return out


def render_jsonl(signals: Sequence[Mapping[str, Any]]) -> str:
    return "".join(json.dumps(dict(row), ensure_ascii=False) + "\n" for row in signals)


def fetch_markets(*, gamma_url: str, limit: int) -> List[Dict[str, Any]]:
    parts = urlsplit(gamma_url)
    query = dict(parse_qsl(parts.query, keep_blank_values=True))
    query["active"] = "true"
    query["closed"] = "false"
    query["limit"] = str(max(1, limit))
    url = urlunsplit((parts.scheme, parts.netloc, parts.path, urlencode(query), parts.fragment))
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-openclaw-heuristic/1.0"})
    with urlopen(req, timeout=20) as resp:
        payload = json.loads(resp.read().decode("utf-8"))
    if isinstance(payload, list):
        return payload
    if isinstance(payload, dict) and isinstance(payload.get("markets"), list):
        return payload["markets"]
    return []


def main() -> None:
    parser = argparse.ArgumentParser(description="Generate heuristic OpenClaw-style signals from Polymarket markets")
    parser.add_argument("--gamma-url", default="https://gamma-api.polymarket.com/markets")
    parser.add_argument("--limit", type=int, default=250)
    parser.add_argument("--favorite-threshold", type=float, default=0.60)
    parser.add_argument("--favorite-fade", type=float, default=0.03)
    parser.add_argument("--underdog-threshold", type=float, default=0.40)
    parser.add_argument("--underdog-lift", type=float, default=0.01)
    parser.add_argument("--question-keyword", action="append", default=[])
    parser.add_argument("--write-jsonl", default="")
    args = parser.parse_args()

    markets = fetch_markets(gamma_url=args.gamma_url, limit=args.limit)
    signals = build_signals_from_markets(
        markets=markets,
        favorite_threshold=args.favorite_threshold,
        favorite_fade=args.favorite_fade,
        underdog_threshold=args.underdog_threshold,
        underdog_lift=args.underdog_lift,
        question_keywords=args.question_keyword,
    )
    text = render_jsonl(signals)
    print(text, end="")
    if args.write_jsonl:
        Path(args.write_jsonl).write_text(text, encoding="utf-8")


if __name__ == "__main__":
    main()
