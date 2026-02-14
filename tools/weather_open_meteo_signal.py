#!/usr/bin/env python3
"""Deterministic weather signals (temperature range markets) using Open-Meteo.

This is an OpenClaw replacement for weather markets:
- Discover temperature-range questions from Polymarket markets
- Geocode city -> lat/lon (Open-Meteo geocoding)
- Fetch daily max temperature forecast (Open-Meteo forecast)
- Convert forecast to bucket probability via a simple uncertainty model

Network calls are isolated so core logic is unit-testable.
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from dataclasses import dataclass
from datetime import date, datetime, timezone
from pathlib import Path
from typing import Any, Callable, Dict, Iterable, List, Mapping, Optional, Sequence, Tuple
from urllib.parse import urlencode
from urllib.request import Request, urlopen


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
if str(BASE_DIR) not in sys.path:
    sys.path.insert(0, str(BASE_DIR))

from tools.weather_range_model import normal_range_probability, parse_temperature_range_question


def _clamp(value: float, lo: float, hi: float) -> float:
    return max(lo, min(hi, value))


def sigma_f_for_lead_days(lead_days: int) -> float:
    """Rough uncertainty model (Fahrenheit) for daily max temperature forecasts."""
    d = max(0, int(lead_days))
    # 1-2 day forecasts are fairly tight; longer horizons widen.
    return _clamp(2.0 + (0.7 * float(d)), 2.0, 10.0)


def confidence_for_sigma(sigma_f: float) -> float:
    # Map [~2F, ~10F] -> [~0.85, ~0.25]
    return _clamp(1.0 - (float(sigma_f) / 12.0), 0.2, 0.95)


@dataclass(frozen=True)
class GeocodeResult:
    name: str
    latitude: float
    longitude: float
    timezone: str


@dataclass(frozen=True)
class ForecastDay:
    day: date
    temperature_max_f: float
    lead_days: int


def _rows_from_payload(payload: Any) -> List[Mapping[str, Any]]:
    if isinstance(payload, Mapping):
        return [payload]
    if isinstance(payload, list):
        return [item for item in payload if isinstance(item, Mapping)]
    return []


def fetch_gamma_markets(*, gamma_url: str, limit: int) -> List[Mapping[str, Any]]:
    query = urlencode({"active": "true", "closed": "false", "limit": str(max(1, int(limit)))})
    url = f"{gamma_url}?{query}" if "?" not in gamma_url else gamma_url
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-weather-open-meteo/1.0"})
    with urlopen(req, timeout=20) as resp:
        payload = json.loads(resp.read().decode("utf-8"))
    rows = _rows_from_payload(payload)
    if rows:
        return rows
    if isinstance(payload, Mapping) and isinstance(payload.get("markets"), list):
        return [item for item in payload["markets"] if isinstance(item, Mapping)]
    return []


def geocode_city_open_meteo(
    *,
    city: str,
    geocode_url: str = "https://geocoding-api.open-meteo.com/v1/search",
) -> Optional[GeocodeResult]:
    name = " ".join(str(city or "").strip().split())
    if not name:
        return None
    params = urlencode({"name": name, "count": "1", "language": "en", "format": "json"})
    url = f"{geocode_url}?{params}"
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-weather-open-meteo/1.0"})
    with urlopen(req, timeout=20) as resp:
        payload = json.loads(resp.read().decode("utf-8"))
    results = payload.get("results") if isinstance(payload, Mapping) else None
    if not isinstance(results, list) or not results:
        return None
    top = results[0]
    if not isinstance(top, Mapping):
        return None
    try:
        lat = float(top.get("latitude"))
        lon = float(top.get("longitude"))
    except (TypeError, ValueError):
        return None
    timezone_name = str(top.get("timezone") or "").strip() or "auto"
    return GeocodeResult(name=name, latitude=lat, longitude=lon, timezone=timezone_name)


def fetch_forecast_open_meteo(
    *,
    latitude: float,
    longitude: float,
    timezone_name: str,
    forecast_url: str = "https://api.open-meteo.com/v1/forecast",
    forecast_days: int = 16,
) -> List[ForecastDay]:
    params = urlencode(
        {
            "latitude": str(latitude),
            "longitude": str(longitude),
            "daily": "temperature_2m_max",
            "temperature_unit": "fahrenheit",
            "forecast_days": str(max(1, int(forecast_days))),
            "timezone": timezone_name or "auto",
        }
    )
    url = f"{forecast_url}?{params}"
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-weather-open-meteo/1.0"})
    with urlopen(req, timeout=20) as resp:
        payload = json.loads(resp.read().decode("utf-8"))
    daily = payload.get("daily") if isinstance(payload, Mapping) else None
    if not isinstance(daily, Mapping):
        return []
    times = daily.get("time")
    temps = daily.get("temperature_2m_max")
    if not isinstance(times, list) or not isinstance(temps, list):
        return []
    out: List[ForecastDay] = []
    for idx, (t, temp) in enumerate(zip(times, temps)):
        if not isinstance(t, str):
            continue
        try:
            day = date.fromisoformat(t.strip()[:10])
        except ValueError:
            continue
        try:
            temp_f = float(temp)
        except (TypeError, ValueError):
            continue
        out.append(ForecastDay(day=day, temperature_max_f=temp_f, lead_days=idx))
    return out


def _extract_market_id(market: Mapping[str, Any]) -> str:
    return str(market.get("id") or market.get("market_id") or market.get("conditionId") or "").strip()


def _extract_question(market: Mapping[str, Any]) -> str:
    return str(market.get("question") or market.get("title") or "").strip()


def build_weather_signals_from_markets(
    *,
    markets: Sequence[Mapping[str, Any]],
    now_utc: datetime,
    geocode_city: Callable[[str], Optional[GeocodeResult]],
    fetch_forecast: Callable[..., List[ForecastDay]],
    p_min: float = 0.01,
    p_max: float = 0.99,
) -> List[Dict[str, Any]]:
    """Return JSONL-ready OpenClaw-like rows for weather markets."""
    _ = now_utc  # Reserved for future lead-time / horizon logic.
    geocode_cache: Dict[str, Optional[GeocodeResult]] = {}
    forecast_cache: Dict[Tuple[float, float, str], List[ForecastDay]] = {}
    out: List[Dict[str, Any]] = []

    for market in markets:
        market_id = _extract_market_id(market)
        question = _extract_question(market)
        if not market_id or not question:
            continue
        parsed = parse_temperature_range_question(question)
        if parsed is None:
            continue

        city = parsed.city
        geo = geocode_cache.get(city)
        if geo is None and city not in geocode_cache:
            geo = geocode_city(city)
            geocode_cache[city] = geo
        if geo is None:
            continue

        key = (float(geo.latitude), float(geo.longitude), str(geo.timezone))
        forecast = forecast_cache.get(key)
        if forecast is None and key not in forecast_cache:
            forecast = fetch_forecast(latitude=geo.latitude, longitude=geo.longitude, timezone_name=geo.timezone)
            forecast_cache[key] = forecast
        if not forecast:
            continue

        target: Optional[ForecastDay] = None
        for day in forecast:
            if day.day.month == parsed.month and day.day.day == parsed.day:
                target = day
                break
        if target is None:
            continue

        sigma = sigma_f_for_lead_days(target.lead_days)
        p_yes = normal_range_probability(
            mu=float(target.temperature_max_f),
            sigma=float(sigma),
            low_f=int(parsed.low_f),
            high_f=int(parsed.high_f),
        )
        p_yes = float(_clamp(float(p_yes), float(p_min), float(p_max)))
        confidence = float(confidence_for_sigma(sigma))

        out.append(
            {
                "market_id": market_id,
                "p_yes": round(p_yes, 6),
                "confidence": round(confidence, 6),
                "question": question,
                "source": "weather_open_meteo",
                "forecast_temperature_max_f": round(float(target.temperature_max_f), 3),
                "sigma_f": round(float(sigma), 3),
                "lead_days": int(target.lead_days),
            }
        )
    return out


def render_jsonl(rows: Iterable[Mapping[str, Any]]) -> str:
    return "".join(json.dumps(dict(row), ensure_ascii=False) + "\n" for row in rows)


def main() -> None:
    parser = argparse.ArgumentParser(description="Generate temperature-range weather signals via Open-Meteo")
    parser.add_argument("--gamma-url", default="https://gamma-api.polymarket.com/markets")
    parser.add_argument("--limit", type=int, default=800)
    parser.add_argument("--forecast-days", type=int, default=16)
    parser.add_argument("--write-jsonl", default="")
    args = parser.parse_args()

    markets = fetch_gamma_markets(gamma_url=args.gamma_url, limit=max(1, args.limit))
    rows = build_weather_signals_from_markets(
        markets=markets,
        now_utc=datetime.now(timezone.utc),
        geocode_city=lambda city: geocode_city_open_meteo(city=city),
        fetch_forecast=lambda **kwargs: fetch_forecast_open_meteo(
            forecast_days=max(1, int(args.forecast_days)),
            **kwargs,
        ),
    )
    text = render_jsonl(rows)
    print(text, end="")
    if args.write_jsonl:
        Path(args.write_jsonl).write_text(text, encoding="utf-8")


if __name__ == "__main__":
    main()

