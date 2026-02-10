# Legend Gate Comparison Design

**Date:** 2026-02-09

## Goal
Compare baseline Legend strategies vs model-gated versions across USDJPY, EURUSD, GBPUSD using local OHLC CSVs. Apply SL/TP and 0.5 pip slippage. Report IS/OOS metrics (80/20 split).

## Scope
- Strategies: Legend-Golden-Cross-Classic (H1), Legend-RSI2-Reversion (M5)
- Pairs: USDJPY, EURUSD, GBPUSD
- Data: data/historical/{PAIR}_{TF}.csv
- Gate: get-model-prediction logic from src/lisp/core/research-algorithms.lisp (ported for comparison)

## Assumptions
- Long-only signals.
- Entry/exit on next bar open.
- SL/TP evaluated using OHLC; if both SL and TP hit in same bar, assume SL first (conservative).
- Slippage: 0.5 pip per trade.
- IS/OOS split by index (oldest 80% / newest 20%).
- SL/TP are pip-size scaled per pair (JPY pairs: 0.01, others: 0.0001).
  - Golden Cross: SL=100 pips, TP=200 pips
  - RSI2 Reversion: SL=10 pips, TP=10 pips

## Architecture
Canonical comparator is a Guardian-side CLI that uses the same backtester metrics as production:
- Runner: `guardian/src/bin/legend_gate_compare.rs`
- Backtester metrics: daily Sharpe (annualized `sqrt(252)`), PF, Win%, Trades, MaxDD

Local Python comparator remains as a lightweight approximation:
- `tools/legend_gate_compare.py`

## Data Flow
CSV -> candles -> model predictions -> strategy AST signals -> backtest -> IS/OOS metrics.

## Error Handling
- Missing/empty CSV: abort with clear message.
- Insufficient bars: skip case and report zeros.
- No trades: metrics set to zero.

## Validation
- Run Guardian comparator once to capture summary tables for baseline vs gated cases:
  - `cd guardian && cargo run -q --bin legend_gate_compare -- --pairs USDJPY EURUSD GBPUSD 2>/dev/null`
