#!/usr/bin/env python3
"""Backtest Polymarket daily weather markets using deterministic temperature bucket probabilities.

This tool supports two evaluation modes:
1) Trading backtest (requires CLOB public price history; practical lookback is ~30 days)
2) Predictive accuracy backtest (no prices required; can run longer windows)

Notes:
- Polymarket's public CLOB price history endpoint only supports limited lookback windows.
  For older windows, the tool can still score forecast accuracy vs outcomes but cannot
  simulate realistic entry prices without archived snapshots.
"""

from __future__ import annotations

import argparse
import json
import math
import os
import sys
from dataclasses import dataclass
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from typing import Any, Dict, Iterable, List, Mapping, Optional, Sequence, Tuple
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

from tools.polymarket_fetch_settlements import extract_winner_from_market
from tools.polymarket_openclaw_bot import BotConfig, MarketSnapshot, OpenClawSignal, build_trade_plan
from tools.weather_open_meteo_signal import confidence_for_sigma, sigma_f_for_lead_days
from tools.weather_range_model import normal_bucket_probability, parse_temperature_bucket_question


def _clamp(value: float, lo: float, hi: float) -> float:
    return max(lo, min(hi, value))


def _to_float(value: Any) -> Optional[float]:
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def _request_json(url: str) -> Any:
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-polyclaw-weather-backtest/1.0"})
    with urlopen(req, timeout=30) as resp:
        return json.loads(resp.read().decode("utf-8"))


def parse_clob_token_ids(market: Mapping[str, Any]) -> List[str]:
    raw = market.get("clobTokenIds")
    if isinstance(raw, list):
        return [str(item).strip() for item in raw if str(item).strip()]
    if isinstance(raw, str):
        text = raw.strip()
        if not text:
            return []
        try:
            parsed = json.loads(text)
        except json.JSONDecodeError:
            return []
        if isinstance(parsed, list):
            return [str(item).strip() for item in parsed if str(item).strip()]
    return []


def select_price_at_timestamp(*, history: Sequence[Mapping[str, Any]], ts: int) -> Optional[float]:
    """Select an approximate tradeable price at a timestamp from CLOB price history.

    Strategy:
    - Use the last datapoint at or before ts if available
    - Otherwise, use the first datapoint after ts
    """
    if not history:
        return None
    target = int(ts)
    best_before: Optional[float] = None
    best_before_t: Optional[int] = None
    first_after: Optional[float] = None
    first_after_t: Optional[int] = None
    for row in history:
        t = row.get("t")
        p = row.get("p")
        if not isinstance(t, (int, float)):
            continue
        price = _to_float(p)
        if price is None:
            continue
        t_int = int(t)
        if t_int <= target:
            if best_before_t is None or t_int > best_before_t:
                best_before_t = t_int
                best_before = float(price)
        else:
            if first_after_t is None or t_int < first_after_t:
                first_after_t = t_int
                first_after = float(price)
    if best_before is not None:
        return best_before
    return first_after


def fetch_clob_price_history(
    *,
    token_id: str,
    interval: str,
    fidelity: int,
    host: str = "https://clob.polymarket.com",
) -> List[Mapping[str, Any]]:
    token = str(token_id or "").strip()
    if not token:
        return []
    params = urlencode({"market": token, "interval": str(interval), "fidelity": str(int(fidelity))})
    url = host.rstrip("/") + "/prices-history?" + params
    payload = _request_json(url)
    hist = payload.get("history") if isinstance(payload, Mapping) else None
    return [row for row in hist if isinstance(row, Mapping)] if isinstance(hist, list) else []


def fetch_gamma_series(*, series_url: str) -> List[Mapping[str, Any]]:
    out: List[Mapping[str, Any]] = []
    offset = 0
    limit = 100
    while True:
        url = series_url.rstrip("/") + "?" + urlencode(
            {"limit": str(limit), "offset": str(offset), "recurrence": "daily", "active": "true", "closed": "false"}
        )
        payload = _request_json(url)
        rows = [row for row in payload if isinstance(row, Mapping)] if isinstance(payload, list) else []
        out.extend(rows)
        if len(rows) < limit:
            break
        offset += limit
        if offset > 5000:
            break
    # Daily weather series are identified by slug suffix.
    return [row for row in out if str(row.get("slug") or "").lower().endswith("-daily-weather")]


def fetch_gamma_events_for_series(
    *,
    events_url: str,
    series_id: str,
    closed: bool,
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
        params = {
            "series_id": sid,
            "active": "false" if closed else "true",
            "closed": "true" if closed else "false",
            "limit": str(size),
            "offset": str(offset),
        }
        url = events_url.rstrip("/") + "?" + urlencode(params)
        payload = _request_json(url)
        rows = [row for row in payload if isinstance(row, Mapping)] if isinstance(payload, list) else []
        if not rows:
            break
        out.extend(rows)
        if len(rows) < size:
            break
    return out


def parse_iso8601(ts: str) -> Optional[datetime]:
    text = str(ts or "").strip()
    if not text:
        return None
    try:
        return datetime.fromisoformat(text.replace("Z", "+00:00"))
    except ValueError:
        return None


def date_in_range(d: date, start: date, end: date) -> bool:
    return start <= d <= end


@dataclass(frozen=True)
class MarketForecast:
    event_id: str
    city: str
    market_id: str
    question: str
    yes_token_id: str
    no_token_id: str
    event_end_date: date
    entry_time_utc: datetime
    p_yes: float
    confidence: float
    winner: str  # "YES"/"NO"/"" (empty if unresolved)


def _fahrenheit_to_celsius(x_f: float) -> float:
    return (float(x_f) - 32.0) * (5.0 / 9.0)


def build_market_forecasts(
    *,
    events: Sequence[Mapping[str, Any]],
    forecast_temp_max_f_by_city_date: Mapping[Tuple[str, date], float],
    entry_offset_hours: int,
) -> List[MarketForecast]:
    forecasts: List[MarketForecast] = []
    offset_h = max(0, int(entry_offset_hours))
    for ev in events:
        event_id = str(ev.get("id") or "").strip()
        end_dt = parse_iso8601(str(ev.get("endDate") or ""))
        start_dt = parse_iso8601(str(ev.get("startDate") or ""))
        if end_dt is None or start_dt is None:
            continue
        markets = ev.get("markets")
        if not isinstance(markets, list):
            continue
        event_end = end_dt.date()
        entry_time = start_dt.astimezone(timezone.utc) + timedelta(hours=offset_h)
        lead_days = max(0, (event_end - entry_time.date()).days)
        sigma_f = sigma_f_for_lead_days(lead_days)
        conf = confidence_for_sigma(sigma_f)

        for market in markets:
            if not isinstance(market, Mapping):
                continue
            question = str(market.get("question") or market.get("title") or "").strip()
            market_id = str(market.get("id") or market.get("market_id") or market.get("conditionId") or "").strip()
            if not question or not market_id:
                continue
            parsed = parse_temperature_bucket_question(question)
            if parsed is None:
                continue
            # Forecast lookup: keyed by (city string from question, target date).
            mu_f = forecast_temp_max_f_by_city_date.get((parsed.city, date(event_end.year, parsed.month, parsed.day)))
            if mu_f is None:
                continue

            mu: float
            sigma: float
            if parsed.unit == "C":
                mu = _fahrenheit_to_celsius(mu_f)
                sigma = float(sigma_f) * (5.0 / 9.0)
            else:
                mu = float(mu_f)
                sigma = float(sigma_f)

            p_yes = float(normal_bucket_probability(mu=mu, sigma=sigma, low=parsed.low, high=parsed.high))
            p_yes = float(_clamp(p_yes, 0.001, 0.999))

            token_ids = parse_clob_token_ids(market)
            yes_token = token_ids[0] if len(token_ids) >= 2 else ""
            no_token = token_ids[1] if len(token_ids) >= 2 else ""

            winner = extract_winner_from_market(market) or ""
            forecasts.append(
                MarketForecast(
                    event_id=event_id,
                    city=parsed.city,
                    market_id=market_id,
                    question=question,
                    yes_token_id=yes_token,
                    no_token_id=no_token,
                    event_end_date=event_end,
                    entry_time_utc=entry_time,
                    p_yes=p_yes,
                    confidence=float(_clamp(conf, 0.0, 1.0)),
                    winner=winner,
                )
            )
    return forecasts


def summarize_accuracy(rows: Sequence[MarketForecast]) -> Dict[str, Any]:
    brier_sum = 0.0
    brier_n = 0
    logloss_sum = 0.0
    logloss_n = 0
    resolved = 0
    for row in rows:
        if row.winner not in {"YES", "NO"}:
            continue
        resolved += 1
        y = 1.0 if row.winner == "YES" else 0.0
        p = float(_clamp(row.p_yes, 1e-6, 1.0 - 1e-6))
        brier_sum += (p - y) ** 2
        brier_n += 1
        logloss_sum += -(y * math.log(p) + (1.0 - y) * math.log(1.0 - p))
        logloss_n += 1
    return {
        "resolved_markets": resolved,
        "brier": round((brier_sum / brier_n), 6) if brier_n else None,
        "logloss": round((logloss_sum / logloss_n), 6) if logloss_n else None,
    }


def summarize_trading_records(trades: Sequence[Mapping[str, Any]]) -> Dict[str, Any]:
    realized = [t for t in trades if bool(t.get("resolved"))]
    total_stake = sum(float(t.get("stake_usd", 0.0) or 0.0) for t in realized)
    total_ev = sum(float(t.get("expected_value_usd", 0.0) or 0.0) for t in realized)
    total_pnl = sum(float(t.get("net_pnl_usd", 0.0) or 0.0) for t in realized)
    wins = sum(1 for t in realized if float(t.get("net_pnl_usd", 0.0) or 0.0) > 0.0)
    return {
        "trades": len(trades),
        "resolved_trades": len(realized),
        "total_stake_usd": round(total_stake, 4),
        "expected_value_usd": round(total_ev, 4),
        "realized_pnl_usd": round(total_pnl, 4),
        "win_rate": round((wins / len(realized)), 6) if realized else None,
        "return_on_stake": round((total_pnl / total_stake), 6) if total_stake > 0 else None,
    }


def simulate_trading_backtest(
    *,
    rows: Sequence[MarketForecast],
    bot_config: BotConfig,
    price_history_interval: str,
    price_history_fidelity: int,
    fee_bps_per_side: float,
    slippage_bps_per_side: float,
) -> Dict[str, Any]:
    # Group by entry date (UTC) to approximate daily risk budgeting.
    by_day: Dict[str, List[MarketForecast]] = {}
    for row in rows:
        key = row.entry_time_utc.date().isoformat()
        by_day.setdefault(key, []).append(row)

    # For each day, use bot planner to pick trades (with diversification).
    trade_records: List[Dict[str, Any]] = []
    cost_rate = max(0.0, 2.0 * (float(fee_bps_per_side) + float(slippage_bps_per_side)) / 10000.0)

    # Cache token->history to avoid duplicate fetches (rare but cheap).
    history_cache: Dict[str, List[Mapping[str, Any]]] = {}

    for day in sorted(by_day.keys()):
        day_rows = by_day[day]
        # Build candidate markets/signals for the day. We fetch prices only for the YES token and
        # approximate NO price as 1-YES.
        markets: List[MarketSnapshot] = []
        signals: Dict[str, OpenClawSignal] = {}
        for row in day_rows:
            if not row.market_id or not row.yes_token_id:
                continue
            token = row.yes_token_id
            hist = history_cache.get(token)
            if hist is None:
                hist = fetch_clob_price_history(
                    token_id=token,
                    interval=price_history_interval,
                    fidelity=price_history_fidelity,
                )
                history_cache[token] = hist
            price = select_price_at_timestamp(history=hist, ts=int(row.entry_time_utc.timestamp()))
            if price is None:
                continue
            yes_price = float(_clamp(price, 0.001, 0.999))
            no_price = float(_clamp(1.0 - yes_price, 0.001, 0.999))
            markets.append(
                MarketSnapshot(
                    market_id=row.market_id,
                    question=row.question,
                    yes_token_id=row.yes_token_id,
                    no_token_id=row.no_token_id or f"{row.market_id}:NO",
                    yes_price=yes_price,
                    no_price=no_price,
                    liquidity_usd=0.0,
                    volume_usd=0.0,
                )
            )
            signals[row.market_id] = OpenClawSignal(market_id=row.market_id, p_yes=row.p_yes, confidence=row.confidence)

        plan = build_trade_plan(signals=signals, markets=markets, config=bot_config)
        for entry in plan.entries:
            winner = ""
            # Find winner from corresponding forecast row
            for row in day_rows:
                if row.market_id == entry.market_id:
                    winner = row.winner
                    break
            side = str(entry.side).upper()
            is_resolved = winner in {"YES", "NO"}
            is_win = is_resolved and side == winner
            stake = float(entry.stake_usd)
            entry_price = float(entry.entry_price)
            gross_pnl = (stake * ((1.0 / entry_price) - 1.0)) if is_win else (-stake if is_resolved else 0.0)
            net_pnl = gross_pnl - (stake * cost_rate) if is_resolved else 0.0
            trade_records.append(
                {
                    "entry_date": day,
                    "market_id": entry.market_id,
                    "question": entry.question,
                    "side": side,
                    "model_prob": float(entry.model_prob),
                    "entry_price": entry_price,
                    "edge": float(entry.edge),
                    "confidence": float(entry.confidence),
                    "stake_usd": stake,
                    "expected_value_usd": float(entry.expected_value_usd),
                    "winner": winner,
                    "resolved": bool(is_resolved),
                    "net_pnl_usd": round(float(net_pnl), 6),
                }
            )

    realized = [t for t in trade_records if t.get("resolved")]
    total_stake = sum(float(t.get("stake_usd", 0.0) or 0.0) for t in realized)
    total_ev = sum(float(t.get("expected_value_usd", 0.0) or 0.0) for t in realized)
    total_pnl = sum(float(t.get("net_pnl_usd", 0.0) or 0.0) for t in realized)
    wins = sum(1 for t in realized if float(t.get("net_pnl_usd", 0.0) or 0.0) > 0.0)
    return {
        "resolved_trades": len(realized),
        "total_stake_usd": round(total_stake, 4),
        "expected_value_usd": round(total_ev, 4),
        "realized_pnl_usd": round(total_pnl, 4),
        "win_rate": round((wins / len(realized)), 6) if realized else None,
        "return_on_stake": round((total_pnl / total_stake), 6) if total_stake > 0 else None,
        "trades": trade_records,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Backtest Polymarket daily weather markets")
    parser.add_argument("--start-date", default="")
    parser.add_argument("--end-date", default="")
    parser.add_argument(
        "--bot-config-file",
        default=os.getenv("POLYCLAW_CONFIG_FILE", str(BASE_DIR / "tools" / "configs" / "polymarket_openclaw.contrarian.runtime.json")),
        help="polymarket_openclaw bot config JSON (used for stake sizing/filters)",
    )
    parser.add_argument("--series-url", default="https://gamma-api.polymarket.com/series")
    parser.add_argument("--events-url", default="https://gamma-api.polymarket.com/events")
    parser.add_argument("--closed", action="store_true", help="Use closed=true events (recommended for backtests)")
    parser.add_argument("--entry-offset-hours", type=int, default=6)
    parser.add_argument("--candidate-per-event", type=int, default=3)
    parser.add_argument("--max-events-per-series", type=int, default=120)
    parser.add_argument("--price-history-interval", default="1m")
    parser.add_argument("--price-history-fidelity", type=int, default=60)
    parser.add_argument("--fee-bps-per-side", type=float, default=-1.0)
    parser.add_argument("--slippage-bps-per-side", type=float, default=-1.0)
    parser.add_argument("--write-report", default="")
    args = parser.parse_args()

    today = datetime.now(timezone.utc).date()
    end = date.fromisoformat(args.end_date) if args.end_date else today
    start = date.fromisoformat(args.start_date) if args.start_date else (end - timedelta(days=90))

    series = fetch_gamma_series(series_url=str(args.series_url))
    series_ids = [str(row.get("id") or "").strip() for row in series if str(row.get("id") or "").strip()]

    # Fetch events and filter to requested range.
    events: List[Mapping[str, Any]] = []
    for sid in series_ids:
        cap = max(0, int(args.max_events_per_series))
        rows = fetch_gamma_events_for_series(
            events_url=str(args.events_url),
            series_id=sid,
            closed=bool(args.closed),
            limit=200,
            max_pages=20,
        )
        # Keep only events in date range.
        kept: List[Mapping[str, Any]] = []
        for ev in rows:
            end_dt = parse_iso8601(str(ev.get("endDate") or ""))
            if end_dt is None:
                continue
            if date_in_range(end_dt.date(), start, end):
                kept.append(ev)
        if cap > 0 and len(kept) > cap:
            kept = kept[-cap:]
        events.extend(kept)

    # Build temperature forecasts for each unique city in the questions.
    cities: List[str] = []
    for ev in events:
        mkts = ev.get("markets")
        if not isinstance(mkts, list):
            continue
        for m in mkts:
            if not isinstance(m, Mapping):
                continue
            q = str(m.get("question") or m.get("title") or "").strip()
            parsed = parse_temperature_bucket_question(q)
            if parsed is not None:
                cities.append(parsed.city)
    unique_cities = sorted({c for c in cities if c})

    # We import Open-Meteo geocoding/forecast from weather_open_meteo_signal for consistency.
    from tools.weather_open_meteo_signal import geocode_city_open_meteo

    temp_map: Dict[Tuple[str, date], float] = {}
    for city in unique_cities:
        geo = geocode_city_open_meteo(city=city)
        if geo is None:
            continue
        params = {
            "latitude": str(geo.latitude),
            "longitude": str(geo.longitude),
            "daily": "temperature_2m_max",
            "start_date": start.isoformat(),
            "end_date": end.isoformat(),
            "timezone": "UTC",
            "temperature_unit": "fahrenheit",
            "models": "gfs_seamless",
        }
        url = "https://historical-forecast-api.open-meteo.com/v1/forecast?" + urlencode(params)
        payload = _request_json(url)
        daily = payload.get("daily") if isinstance(payload, Mapping) else None
        if not isinstance(daily, Mapping):
            continue
        times = daily.get("time")
        temps = daily.get("temperature_2m_max")
        if not isinstance(times, list) or not isinstance(temps, list):
            continue
        for t, temp in zip(times, temps):
            if not isinstance(t, str):
                continue
            try:
                day = date.fromisoformat(t[:10])
            except ValueError:
                continue
            val = _to_float(temp)
            if val is None:
                continue
            temp_map[(city, day)] = float(val)

    forecasts_all = build_market_forecasts(
        events=events,
        forecast_temp_max_f_by_city_date=temp_map,
        entry_offset_hours=int(args.entry_offset_hours),
    )

    # Candidate reduction per event: keep top N by p_yes.
    # This keeps network cost bounded for trading simulation (prices are only available for ~1 month).
    n_per = max(1, int(args.candidate_per_event))
    grouped: Dict[str, List[MarketForecast]] = {}
    for row in forecasts_all:
        key = row.event_id or f"{row.city}|{row.event_end_date.isoformat()}"
        grouped.setdefault(key, []).append(row)
    forecasts: List[MarketForecast] = []
    for rows in grouped.values():
        rows_sorted = sorted(rows, key=lambda r: r.p_yes, reverse=True)
        forecasts.extend(rows_sorted[:n_per])

    accuracy = summarize_accuracy(forecasts_all)

    bot_config_path = Path(str(args.bot_config_file)).expanduser()
    if not bot_config_path.exists():
        raise SystemExit(f"bot config not found: {bot_config_path}")
    bot_payload = json.loads(bot_config_path.read_text(encoding="utf-8"))
    if not isinstance(bot_payload, Mapping):
        raise SystemExit(f"unsupported bot config payload: {bot_config_path}")
    bot_config = BotConfig(**{k: bot_payload[k] for k in BotConfig.__annotations__.keys() if k in bot_payload})
    fee_bps = float(bot_payload.get("fee_bps_per_side", 0.0)) if args.fee_bps_per_side < 0 else float(args.fee_bps_per_side)
    slippage_bps = (
        float(bot_payload.get("slippage_bps_per_side", 0.0))
        if args.slippage_bps_per_side < 0
        else float(args.slippage_bps_per_side)
    )
    # Ensure the cost params align between planning and realized PnL.
    bot_config = BotConfig(
        **{
            **{k: getattr(bot_config, k) for k in BotConfig.__annotations__.keys()},
            "fee_bps_per_side": fee_bps,
            "slippage_bps_per_side": slippage_bps,
        }
    )
    trading = simulate_trading_backtest(
        rows=forecasts,
        bot_config=bot_config,
        price_history_interval=str(args.price_history_interval),
        price_history_fidelity=int(args.price_history_fidelity),
        fee_bps_per_side=fee_bps,
        slippage_bps_per_side=slippage_bps,
    )

    trades_all = trading.get("trades", [])
    if not isinstance(trades_all, list):
        trades_all = []

    # Windowed summaries for 7/30/90 days ending at `end`.
    windows = [7, 30, 90]
    window_reports: Dict[str, Any] = {}
    for days in windows:
        w_start = end - timedelta(days=max(0, int(days) - 1))
        w_key = f"last_{int(days)}d"
        acc_rows = [r for r in forecasts_all if date_in_range(r.event_end_date, w_start, end)]
        trade_rows = [
            t
            for t in trades_all
            if isinstance(t, Mapping)
            and date_in_range(date.fromisoformat(str(t.get("entry_date", "1970-01-01"))), w_start, end)
        ]
        window_reports[w_key] = {
            "range": {"start": w_start.isoformat(), "end": end.isoformat()},
            "accuracy": summarize_accuracy(acc_rows),
            "trading": summarize_trading_records(trade_rows),
        }

    report: Dict[str, Any] = {
        "range": {"start": start.isoformat(), "end": end.isoformat()},
        "events": len(events),
        "markets_scored": len(forecasts_all),
        "accuracy": accuracy,
        "trading": {k: v for k, v in trading.items() if k != "trades"},
        "windows": window_reports,
    }
    text = json.dumps(report, ensure_ascii=False, indent=2)
    print(text)
    if args.write_report:
        out_path = Path(args.write_report)
        out_path.parent.mkdir(parents=True, exist_ok=True)
        out_path.write_text(text + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
