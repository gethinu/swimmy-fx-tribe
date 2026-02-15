# Polymarket Weather Snapshots Design

**Goal:** enable realistic (>30d) trading backtests and later calibration by archiving the *exact* market/price/context state at the time we would have entered trades.

**Non-goals (for now):**
- Guarantee profitability (snapshots just make evaluation + iteration honest)
- Any LLM/OpenClaw dependency (signals remain deterministic)

## What “Gamma event/market state” means

Polymarket exposes “Gamma” API objects at three main layers:

- **Series** (`/series`): a collection (e.g., `chicago-daily-weather`)
  - Typical state fields: `active`, `closed`, `archived`, `restricted`, `recurrence`, `updatedAt`, plus volume/liquidity counters.

- **Event** (`/events`): one instance in a series (e.g., “Highest temperature in Chicago on Feb 16?”)
  - Typical state fields: `active`, `closed`, `archived`, `restricted`
  - Lifecycle/ops fields we care about: `startDate`, `endDate`, `closedTime`, `automaticallyResolved`, `resolutionSource`, `pendingDeployment`, `deploying`, plus volume/open-interest counters.

- **Market** (embedded in event, or `/markets/<id>`): one tradeable outcome market (bucket/range)
  - Typical state fields: `active`, `closed`, `archived`, `restricted`
  - Tradeability fields: `acceptingOrders`, `ready`, `enableOrderBook`, `orderMinSize`, `orderPriceMinTickSize`, `spread`, `bestAsk`, `lastTradePrice`
  - Settlement-ish fields: `umaResolutionStatus`, `resolvedBy`, sometimes `winner`/`result`/`resolution`
  - Pricing/token linkage fields: `outcomes`, `outcomePrices`, `clobTokenIds`, `conditionId`

This “state” is exactly what must be snapshotted, because it drives:
- whether we *could* have traded (accepting orders, min size, etc)
- what we would have paid (best ask / orderbook)
- what resolved and how (winner/resolution status)

## Snapshot Strategy (Recommended: Hybrid)

We will store both:
1) **Raw** API payloads (for forensics / schema changes)
2) **Normalized** JSONL records (for efficient backtest + training)

### Data Sources

- Gamma:
  - `GET /series?recurrence=daily&active=true&closed=false`
  - `GET /events?series_id=...&active=true&closed=false&limit=...`
- CLOB:
  - `GET /book?token_id=...` (orderbook + last trade price when available)
- Deterministic signals:
  - run the same command used for production signal generation (currently `tools/weather_open_meteo_signal.py`)

### On-Disk Layout

Base dir: `data/snapshots/polymarket_weather/`

Per snapshot directory (UTC timestamped):
- `manifest.json`
- `gamma_series.json`
- `gamma_events.json`
- `markets.jsonl` (normalized, one line per market; includes `event_id`, `series_id`)
- `signals.jsonl` (raw signal generator output, one line per market id)
- `clob_books.jsonl` (one line per token id; bids/asks trimmed to depth)
- `errors.jsonl` (optional; any fetch/parse failures)

We will also include `schema_version` in `manifest.json` for forward compatibility.

### Orderbook Policy

Default behavior: fetch orderbooks for **all markets present in signals**.

Rationale: right now the active universe is small enough to be feasible, and this gives honest entry-price reconstruction later.

We will still support knobs:
- `--orderbook-mode=all|topk|none`
- `--orderbook-depth=N` (trim to top N bids/asks)
- `--topk=N` (if using `topk`, selected by edge/volume)

## Error Handling

Collector must be best-effort:
- network errors should not crash the whole run
- each failed fetch is recorded to `errors.jsonl`
- `manifest.json` summarizes counts + failures

## Testing

Unit tests will:
- stub Gamma/CLOB responses (no real network)
- verify file outputs exist
- verify orderbook trimming
- verify manifest counts and that failures are recorded (e.g., a 404 “no orderbook” case)

## Running Periodically (Ops)

Recommended cadence: **every 15 minutes**.

Reasoning:
- daily weather markets update frequently enough that entry price realism matters
- 15m is usually enough to reconstruct “as of” entry windows without exploding storage

Retention suggestion:
- keep **at least 90 days** of snapshots if you want honest 3-month PnL backtests
- if storage grows too fast, keep all `manifest.json` + `signals.jsonl` and reduce `clob_books.jsonl` depth or cadence
