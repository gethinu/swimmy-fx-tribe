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
import math
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

from tools.weather_range_model import normal_bucket_probability, parse_temperature_bucket_question


def _clamp(value: float, lo: float, hi: float) -> float:
    return max(lo, min(hi, value))


_WEATHER_EVENT_TITLE_PREFIX = "highest temperature in "
_WEATHER_SERIES_SLUG_SUFFIX = "-daily-weather"


def extract_weather_event_ids(events: Sequence[Mapping[str, Any]]) -> List[str]:
    """Return event ids for active weather events.

    We rely on the event title prefix used by Polymarket's recurring weather series.
    """
    out: List[str] = []
    for event in events:
        title = str(event.get("title") or "").strip()
        if not title or not title.lower().startswith(_WEATHER_EVENT_TITLE_PREFIX):
            continue
        event_id = str(event.get("id") or "").strip()
        if event_id:
            out.append(event_id)
    return out


def extract_weather_series_ids(series: Sequence[Mapping[str, Any]]) -> List[str]:
    """Return series ids for daily weather series (e.g. '*-daily-weather')."""
    out: List[str] = []
    for row in series:
        slug = str(row.get("slug") or "").strip().lower()
        if not slug or not slug.endswith(_WEATHER_SERIES_SLUG_SUFFIX):
            continue
        series_id = str(row.get("id") or "").strip()
        if series_id:
            out.append(series_id)
    return out


def collect_weather_event_ids(
    *,
    fetch_events_page: Callable[..., Sequence[Mapping[str, Any]]],
    page_size: int,
    max_pages: int,
    max_events: int,
) -> List[str]:
    """Collect weather event ids across paginated event listings."""
    size = max(1, int(page_size))
    pages = max(1, int(max_pages))
    cap = max(0, int(max_events))
    out: List[str] = []
    for page in range(pages):
        offset = page * size
        events = list(fetch_events_page(limit=size, offset=offset))
        if not events:
            break
        out.extend(extract_weather_event_ids(events))
        if cap > 0 and len(out) >= cap:
            return out[:cap]
        if len(events) < size:
            break
    return out


def collect_weather_series_ids(
    *,
    fetch_series_page: Callable[..., Sequence[Mapping[str, Any]]],
    page_size: int,
    max_pages: int,
    max_series: int,
) -> List[str]:
    """Collect weather series ids across paginated series listings."""
    size = max(1, int(page_size))
    pages = max(1, int(max_pages))
    cap = max(0, int(max_series))
    out: List[str] = []
    for page in range(pages):
        offset = page * size
        rows = list(fetch_series_page(limit=size, offset=offset))
        if not rows:
            break
        out.extend(extract_weather_series_ids(rows))
        if cap > 0 and len(out) >= cap:
            return out[:cap]
        if len(rows) < size:
            break
    return out


def sigma_f_for_lead_days(lead_days: int) -> float:
    """Rough uncertainty model (Fahrenheit) for daily max temperature forecasts."""
    d = max(0, int(lead_days))
    # 1-2 day forecasts are fairly tight; longer horizons widen.
    return _clamp(2.0 + (0.7 * float(d)), 2.0, 10.0)


def confidence_for_sigma(sigma_f: float) -> float:
    # Map [~2F, ~10F] -> [~0.85, ~0.25]
    return _clamp(1.0 - (float(sigma_f) / 12.0), 0.2, 0.95)


def extract_markets_from_events(events: Sequence[Mapping[str, Any]]) -> List[Mapping[str, Any]]:
    markets: List[Mapping[str, Any]] = []
    for event in events:
        embedded = event.get("markets")
        if isinstance(embedded, list):
            markets.extend([item for item in embedded if isinstance(item, Mapping)])
    return markets


def collect_weather_markets_from_series_ids(
    *,
    series_ids: Sequence[str],
    fetch_events_for_series: Callable[..., Sequence[Mapping[str, Any]]],
    events_limit: int,
    max_markets: int,
) -> List[Mapping[str, Any]]:
    limit = max(1, int(events_limit))
    cap = max(0, int(max_markets))
    seen: set[str] = set()
    out: List[Mapping[str, Any]] = []
    for series_id in series_ids:
        series_text = str(series_id or "").strip()
        if not series_text:
            continue
        events = list(fetch_events_for_series(series_id=series_text, limit=limit, offset=0))
        for market in extract_markets_from_events(events):
            market_id = str(market.get("id") or market.get("market_id") or market.get("conditionId") or "").strip()
            if not market_id or market_id in seen:
                continue
            out.append(market)
            seen.add(market_id)
            if cap > 0 and len(out) >= cap:
                return out[:cap]
    return out


def fetch_weather_markets_from_series(
    *,
    fetch_series_page: Callable[..., Sequence[Mapping[str, Any]]],
    fetch_events_for_series: Callable[..., Sequence[Mapping[str, Any]]],
    series_page_size: int,
    series_max_pages: int,
    series_max_count: int,
    events_limit: int,
    max_markets: int,
) -> List[Mapping[str, Any]]:
    series_ids = collect_weather_series_ids(
        fetch_series_page=fetch_series_page,
        page_size=series_page_size,
        max_pages=series_max_pages,
        max_series=series_max_count,
    )
    if not series_ids:
        return []
    return collect_weather_markets_from_series_ids(
        series_ids=series_ids,
        fetch_events_for_series=fetch_events_for_series,
        events_limit=events_limit,
        max_markets=max_markets,
    )


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
    try:
        with urlopen(req, timeout=20) as resp:
            payload = json.loads(resp.read().decode("utf-8"))
    except (OSError, ValueError):
        return []
    rows = _rows_from_payload(payload)
    if rows:
        return rows
    if isinstance(payload, Mapping) and isinstance(payload.get("markets"), list):
        return [item for item in payload["markets"] if isinstance(item, Mapping)]
    return []


def fetch_gamma_series_page(
    *,
    series_url: str,
    limit: int,
    offset: int,
    recurrence: str = "daily",
    active: bool = True,
    closed: bool = False,
) -> List[Mapping[str, Any]]:
    params = {
        "limit": str(max(1, int(limit))),
        "offset": str(max(0, int(offset))),
        "recurrence": str(recurrence or "daily"),
        "active": "true" if active else "false",
        "closed": "true" if closed else "false",
    }
    url = f"{series_url}?{urlencode(params)}"
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-weather-open-meteo/1.0"})
    try:
        with urlopen(req, timeout=20) as resp:
            payload = json.loads(resp.read().decode("utf-8"))
    except (OSError, ValueError):
        return []
    return _rows_from_payload(payload)


def fetch_gamma_events_page(
    *,
    events_url: str,
    limit: int,
    offset: int,
    active: bool = True,
    closed: bool = False,
    series_id: Optional[str] = None,
) -> List[Mapping[str, Any]]:
    params = {
        "limit": str(max(1, int(limit))),
        "offset": str(max(0, int(offset))),
        "active": "true" if active else "false",
        "closed": "true" if closed else "false",
    }
    if series_id:
        params["series_id"] = str(series_id)
    url = f"{events_url}?{urlencode(params)}"
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-weather-open-meteo/1.0"})
    try:
        with urlopen(req, timeout=20) as resp:
            payload = json.loads(resp.read().decode("utf-8"))
    except (OSError, ValueError):
        return []
    return _rows_from_payload(payload)


def fetch_gamma_event_detail(*, event_id: str, events_base_url: str = "https://gamma-api.polymarket.com/events") -> Mapping[str, Any]:
    event_text = str(event_id or "").strip()
    if not event_text:
        return {}
    url = events_base_url.rstrip("/") + "/" + event_text
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-weather-open-meteo/1.0"})
    try:
        with urlopen(req, timeout=20) as resp:
            payload = json.loads(resp.read().decode("utf-8"))
    except (OSError, ValueError):
        return {}
    return payload if isinstance(payload, Mapping) else {}


def fetch_weather_markets_from_gamma(
    *,
    series_url: str = "https://gamma-api.polymarket.com/series",
    events_url: str = "https://gamma-api.polymarket.com/events",
    recurrence: str = "daily",
    series_page_size: int = 100,
    series_max_pages: int = 20,
    series_max_count: int = 200,
    events_limit: int = 4,
    max_markets: int = 4000,
) -> List[Mapping[str, Any]]:
    """Discover active daily weather markets via gamma series -> events."""

    def _fetch_series_page(*, limit: int, offset: int) -> Sequence[Mapping[str, Any]]:
        return fetch_gamma_series_page(
            series_url=series_url,
            limit=limit,
            offset=offset,
            recurrence=str(recurrence or "daily"),
            active=True,
            closed=False,
        )

    def _fetch_events_for_series(*, series_id: str, limit: int, offset: int) -> Sequence[Mapping[str, Any]]:
        return fetch_gamma_events_page(
            events_url=events_url,
            limit=limit,
            offset=offset,
            active=True,
            closed=False,
            series_id=series_id,
        )

    markets = fetch_weather_markets_from_series(
        fetch_series_page=_fetch_series_page,
        fetch_events_for_series=_fetch_events_for_series,
        series_page_size=series_page_size,
        series_max_pages=series_max_pages,
        series_max_count=series_max_count,
        events_limit=events_limit,
        max_markets=max_markets,
    )
    if markets:
        return markets

    # Fallback (older discovery): scan events pages by title prefix.
    def _fetch_events_page(*, limit: int, offset: int) -> Sequence[Mapping[str, Any]]:
        return fetch_gamma_events_page(events_url=events_url, limit=limit, offset=offset, active=True, closed=False)

    event_ids = collect_weather_event_ids(
        fetch_events_page=_fetch_events_page,
        page_size=200,
        max_pages=50,
        max_events=200,
    )
    fallback_markets: List[Mapping[str, Any]] = []
    for event_id in event_ids:
        detail = fetch_gamma_event_detail(event_id=event_id, events_base_url=events_url)
        embedded = detail.get("markets")
        if isinstance(embedded, list):
            fallback_markets.extend([item for item in embedded if isinstance(item, Mapping)])
    return fallback_markets


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
    try:
        with urlopen(req, timeout=20) as resp:
            payload = json.loads(resp.read().decode("utf-8"))
    except (OSError, ValueError):
        return None
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
    model: str = "",
) -> List[ForecastDay]:
    model_text = str(model or "").strip()
    params = urlencode(
        {
            "latitude": str(latitude),
            "longitude": str(longitude),
            "daily": "temperature_2m_max",
            "temperature_unit": "fahrenheit",
            "forecast_days": str(max(1, int(forecast_days))),
            "timezone": timezone_name or "auto",
            **({"models": model_text} if model_text else {}),
        }
    )
    url = f"{forecast_url}?{params}"
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-weather-open-meteo/1.0"})
    try:
        with urlopen(req, timeout=20) as resp:
            payload = json.loads(resp.read().decode("utf-8"))
    except (OSError, ValueError):
        return []
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
    forecast_models: Sequence[str] = (),
    calibration: Optional[Mapping[str, Any]] = None,
    p_min: float = 0.01,
    p_max: float = 0.99,
) -> List[Dict[str, Any]]:
    """Return JSONL-ready OpenClaw-like rows for weather markets."""
    _ = now_utc  # Reserved for future lead-time / horizon logic.
    geocode_cache: Dict[str, Optional[GeocodeResult]] = {}
    # Cache across repeated markets for the same city/location. When using multiple
    # forecast models, we include the model name in the cache key.
    forecast_cache: Dict[Tuple[float, float, str, str], List[ForecastDay]] = {}
    out: List[Dict[str, Any]] = []

    models = [str(item).strip() for item in forecast_models if str(item).strip()]

    for market in markets:
        market_id = _extract_market_id(market)
        question = _extract_question(market)
        if not market_id or not question:
            continue
        parsed = parse_temperature_bucket_question(question)
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
        temps_f: List[float] = []
        lead_days: Optional[int] = None
        model_temps: Dict[str, float] = {}
        if models:
            for model_name in models:
                cache_key = (key[0], key[1], key[2], model_name)
                forecast = forecast_cache.get(cache_key)
                if forecast is None and cache_key not in forecast_cache:
                    forecast = fetch_forecast(latitude=geo.latitude, longitude=geo.longitude, timezone_name=geo.timezone, model=model_name)
                    forecast_cache[cache_key] = forecast
                if not forecast:
                    continue
                target: Optional[ForecastDay] = None
                for day in forecast:
                    if day.day.month == parsed.month and day.day.day == parsed.day:
                        target = day
                        break
                if target is None:
                    continue
                try:
                    temp_val = float(target.temperature_max_f)
                except (TypeError, ValueError):
                    continue
                temps_f.append(temp_val)
                model_temps[model_name] = temp_val
                if lead_days is None or int(target.lead_days) < lead_days:
                    lead_days = int(target.lead_days)
        else:
            cache_key = (key[0], key[1], key[2], "")
            forecast = forecast_cache.get(cache_key)
            if forecast is None and cache_key not in forecast_cache:
                forecast = fetch_forecast(latitude=geo.latitude, longitude=geo.longitude, timezone_name=geo.timezone)
                forecast_cache[cache_key] = forecast
            if not forecast:
                continue
            target: Optional[ForecastDay] = None
            for day in forecast:
                if day.day.month == parsed.month and day.day.day == parsed.day:
                    target = day
                    break
            if target is None:
                continue
            temps_f = [float(target.temperature_max_f)]
            lead_days = int(target.lead_days)

        if not temps_f or lead_days is None:
            continue

        mu_f = float(sum(temps_f) / len(temps_f))

        sigma_base_f = float(sigma_f_for_lead_days(lead_days))
        # Treat model disagreement as additional independent uncertainty.
        sigma_model_f = 0.0
        if len(temps_f) > 1:
            sigma_model_f = math.sqrt(sum((x - mu_f) ** 2 for x in temps_f) / float(len(temps_f)))
        sigma_total_f = math.sqrt((sigma_base_f**2) + (sigma_model_f**2))

        mu: float
        sigma: float
        if parsed.unit == "C":
            mu = (float(mu_f) - 32.0) * (5.0 / 9.0)
            sigma = float(sigma_total_f) * (5.0 / 9.0)
        else:
            mu = float(mu_f)
            sigma = float(sigma_total_f)
        p_yes = normal_bucket_probability(mu=mu, sigma=sigma, low=parsed.low, high=parsed.high)
        p_yes = float(_clamp(float(p_yes), float(p_min), float(p_max)))
        if calibration is not None:
            try:
                from tools.probability_calibration import apply_probability_calibration

                p_yes = float(apply_probability_calibration(calibration, float(p_yes)))
            except (OSError, ValueError, TypeError, KeyError):
                pass
            p_yes = float(_clamp(float(p_yes), float(p_min), float(p_max)))
        confidence = float(confidence_for_sigma(sigma_total_f))

        row = {
            "market_id": market_id,
            "p_yes": round(p_yes, 6),
            "confidence": round(confidence, 6),
            "question": question,
            "source": "weather_open_meteo",
            "forecast_temperature_max_f": round(float(mu_f), 3),
            "sigma_f": round(float(sigma_total_f), 3),
            "lead_days": int(lead_days),
        }
        if model_temps:
            row["forecast_models"] = sorted(model_temps.keys())
            row["forecast_model_temps_f"] = {k: round(float(v), 3) for k, v in sorted(model_temps.items())}
            row["sigma_model_f"] = round(float(sigma_model_f), 3)
        out.append(row)
    return out


def render_jsonl(rows: Iterable[Mapping[str, Any]]) -> str:
    return "".join(json.dumps(dict(row), ensure_ascii=False) + "\n" for row in rows)


def main() -> None:
    parser = argparse.ArgumentParser(description="Generate temperature-range weather signals via Open-Meteo")
    parser.add_argument("--gamma-url", default="https://gamma-api.polymarket.com/markets")
    parser.add_argument(
        "--limit",
        type=int,
        default=800,
        help="Fallback market fetch limit (only used if event discovery yields no markets).",
    )
    parser.add_argument("--series-url", default="https://gamma-api.polymarket.com/series")
    parser.add_argument("--events-url", default="https://gamma-api.polymarket.com/events")
    # Legacy flag names kept for compatibility; these now control series discovery paging.
    parser.add_argument("--events-page-size", type=int, default=100)
    parser.add_argument("--events-max-pages", type=int, default=20)
    parser.add_argument("--events-max-count", type=int, default=200)
    parser.add_argument("--events-limit", type=int, default=4)
    parser.add_argument("--max-markets", type=int, default=4000)
    parser.add_argument("--forecast-days", type=int, default=16)
    parser.add_argument(
        "--forecast-model",
        action="append",
        default=[],
        help="Open-Meteo model name (repeatable). If set, forecasts are ensembled across models.",
    )
    parser.add_argument(
        "--calibration-file",
        default="",
        help="Optional probability calibration JSON (see tools/probability_calibration.py).",
    )
    parser.add_argument("--write-jsonl", default="")
    args = parser.parse_args()

    calibration: Optional[Mapping[str, Any]] = None
    if str(args.calibration_file or "").strip():
        try:
            path = Path(str(args.calibration_file)).expanduser()
            payload = json.loads(path.read_text(encoding="utf-8"))
            if isinstance(payload, Mapping):
                from tools.probability_calibration import validate_calibration

                validate_calibration(payload)
                calibration = payload
        except (OSError, ValueError, TypeError):
            calibration = None

    markets = fetch_weather_markets_from_gamma(
        series_url=str(args.series_url),
        events_url=str(args.events_url),
        series_page_size=max(1, int(args.events_page_size)),
        series_max_pages=max(1, int(args.events_max_pages)),
        series_max_count=max(0, int(args.events_max_count)),
        events_limit=max(1, int(args.events_limit)),
        max_markets=max(0, int(args.max_markets)),
    )
    if not markets:
        markets = fetch_gamma_markets(gamma_url=args.gamma_url, limit=max(1, args.limit))
    rows = build_weather_signals_from_markets(
        markets=markets,
        now_utc=datetime.now(timezone.utc),
        geocode_city=lambda city: geocode_city_open_meteo(city=city),
        fetch_forecast=lambda **kwargs: fetch_forecast_open_meteo(
            forecast_days=max(1, int(args.forecast_days)),
            **kwargs,
        ),
        forecast_models=[part.strip() for item in (args.forecast_model or []) for part in str(item).split(",") if part.strip()],
        calibration=calibration,
    )
    text = render_jsonl(rows)
    print(text, end="")
    if args.write_jsonl:
        Path(args.write_jsonl).write_text(text, encoding="utf-8")


if __name__ == "__main__":
    main()
