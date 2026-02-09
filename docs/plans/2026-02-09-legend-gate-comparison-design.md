# Legend Gate Comparison Design

**Date:** 2026-02-09

## Goal
Compare baseline Legend strategies vs model-gated versions across USDJPY, EURUSD, GBPUSD using local OHLC CSVs, with SL/TP and 0.5 pip slippage applied. Report IS/OOS metrics (80/20 split).

## Scope
- Strategies: Legend-Golden-Cross-Classic (H1), Legend-RSI-Reversion-V1 (M5)
- Pairs: USDJPY, EURUSD, GBPUSD
- Data: data/historical/{PAIR}_{TF}.csv
- Gate: get-model-prediction logic from src/lisp/core/research-algorithms.lisp

## Assumptions
- Long-only signals.
- Entry/exit on next bar open.
- SL/TP evaluated using OHLC; if both SL and TP hit in same bar, assume SL first (conservative).
- Slippage: 0.5 pip per trade.
- IS/OOS split by index (oldest 80% / newest 20%).

## Architecture
Single local Python script (standard library only) with modules:
- Loader: read CSV into OHLC arrays.
- Indicators: SMA(50/200), RSI(2), realized volatility(20), dual-trend, Kalman velocity.
- Model gate: volatility switch to kalman/ensemble and gating rules.
- Strategy engine: Golden Cross, RSI Reversion signals.
- Simulator: long-only, next-bar entry/exit, SL/TP, slippage.
- Metrics: Sharpe (per-trade), PF, Win%, Trades, MaxDD.

## Data Flow
CSV -> OHLC arrays -> indicators -> model predictions -> signals -> trades -> IS/OOS metrics.

## Error Handling
- Missing/empty CSV: abort with clear message.
- Insufficient bars: skip case and report zeros.
- No trades: metrics set to zero.

## Validation
- Sanity check for indicator lengths and non-empty predictions.
- Run script once to capture summary tables for baseline vs gated cases.
