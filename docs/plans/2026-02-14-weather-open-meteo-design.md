# Weather Signals (Global) via Open-Meteo (No OpenClaw)

## Goal
Replace LLM/OpenClaw-driven weather probabilities with a deterministic, testable pipeline that can cover global temperature-range markets.

## Scope
- Input: Polymarket markets (Gamma API).
- Target questions: `"Will the highest temperature in <City> be between <low>-<high>°F on <Month> <Day>?"`.
- Output: OpenClaw-style JSONL rows (`market_id`, `p_yes`, `confidence`, `source`), written by `tools/openclaw_signal_sync.py` into `data/openclaw/signals.jsonl`.

Non-goals (Phase 1):
- Handling non-Fahrenheit markets, non-"highest temperature" phrasing variants, or non-city locations (airports/stations).
- Complex calibration / backtesting of the temperature distribution parameters.

## Data Sources
- Geocoding: Open-Meteo geocoding API (global).
- Forecast: Open-Meteo forecast API `daily=temperature_2m_max`, `temperature_unit=fahrenheit`, `timezone=auto`.

## Architecture / Data Flow
1. `tools/weather_open_meteo_signal.py`
   - Fetch active markets from Gamma (`--limit` cap).
   - Parse weather questions into `(city, month, day, low_f, high_f)`.
   - Per-city cache: geocode city -> `(lat, lon, tz)`.
   - Per-location cache: fetch daily max forecast list for `forecast_days` and map to target date.
   - Convert forecast max temperature to bucket probability `P(low<=T<=high)` via Gaussian approximation with continuity correction.
   - Emit JSONL rows with `source="weather_open_meteo"`.
2. `tools/openclaw_signal_sync.py`
   - Runs the configured command (now can be non-OpenClaw).
   - Computes `source_counts` from rows.
   - Computes `agent_signal_count/ratio` as "trusted" signals (any `source` except `heuristic_fallback`/`unknown`) to keep safety gates working without LLM.
3. `tools/run_polymarket_openclaw_service.py`
   - Uses `agent_signal_count/ratio` gates to avoid trading on fallback/unknown signals.

## Probability Model (Phase 1)
- Mean (`mu`): forecast daily max temperature (F).
- Stddev (`sigma`): simple lead-time-based uncertainty (`sigma_f_for_lead_days`), clamped.
- Probability: `normal_range_probability(mu, sigma, low_f, high_f)` from `tools/weather_range_model.py`.
- Confidence: monotone function of `sigma` (lower uncertainty => higher confidence).

## Risk Controls
- Require `source` for all deterministic rows; `unknown` and `heuristic_fallback` are treated as untrusted.
- Keep `.env` live execution disabled by default (`POLYCLAW_LIVE_EXECUTION=0`).
- Diversification protection against "bucket stacking" is handled in `tools/polymarket_openclaw_bot.py` via question grouping.

## Upgrade Path (Option 2)
If Phase 1 performance is weak:
- Replace Open-Meteo forecast with NOAA global model ingestion (e.g., GFS) and/or additional forecast ensembles.
- Add calibration for `sigma` by lead-time and location (or learn it from resolved markets).

