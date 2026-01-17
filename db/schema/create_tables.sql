-- Phase 9: Structured Data & DB Schema
-- Based on memo3.txt Section 12

-- 12.1 Ticks
CREATE TABLE IF NOT EXISTS ticks (
  id SERIAL PRIMARY KEY,
  instrument TEXT NOT NULL,
  ts TIMESTAMP WITH TIME ZONE NOT NULL,
  bid NUMERIC NOT NULL,
  ask NUMERIC NOT NULL,
  tick_volume INT,
  source TEXT
);

CREATE INDEX idx_ticks_ts ON ticks(ts);
CREATE INDEX idx_ticks_instrument ON ticks(instrument);

-- 12.2 Bars
CREATE TABLE IF NOT EXISTS bars (
  id SERIAL PRIMARY KEY,
  instrument TEXT NOT NULL,
  timeframe TEXT NOT NULL, -- 'M1', 'H1', 'D1', etc.
  ts TIMESTAMP WITH TIME ZONE NOT NULL,
  open NUMERIC NOT NULL,
  high NUMERIC NOT NULL,
  low NUMERIC NOT NULL,
  close NUMERIC NOT NULL,
  volume NUMERIC
);

CREATE INDEX idx_bars_ts ON bars(ts);
CREATE INDEX idx_bars_instrument_tf ON bars(instrument, timeframe);

-- 12.3 Signals
CREATE TABLE IF NOT EXISTS signals (
  id UUID PRIMARY KEY,
  strategy_id TEXT NOT NULL,
  instrument TEXT NOT NULL,
  timeframe TEXT NOT NULL,
  side TEXT NOT NULL, -- 'BUY', 'SELL', 'EXIT'
  price NUMERIC,
  confidence NUMERIC,
  suggested_lot NUMERIC,
  ts TIMESTAMP WITH TIME ZONE NOT NULL,
  state_vector JSONB,
  reason TEXT -- Added for traceability
);

CREATE INDEX idx_signals_ts ON signals(ts);
CREATE INDEX idx_signals_strategy ON signals(strategy_id);

-- 12.5 Audit Events (Immutable Log)
CREATE TABLE IF NOT EXISTS audit_events (
  id UUID PRIMARY KEY,
  event_type TEXT NOT NULL, -- 'BENCH', 'KILL', 'GUARDIAN_VETO', etc.
  payload JSONB NOT NULL,
  ts TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_audit_ts ON audit_events(ts);
CREATE INDEX idx_audit_type ON audit_events(event_type);
