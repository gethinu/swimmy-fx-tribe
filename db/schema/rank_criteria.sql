-- V48.0: Rank Criteria Database Schema
-- PostgreSQL schema for storing rank promotion criteria

-- Rank Criteria Table
CREATE TABLE IF NOT EXISTS rank_criteria (
    id SERIAL PRIMARY KEY,
    rank VARCHAR(10) NOT NULL UNIQUE,  -- 'B', 'A', 'S', 'legend'
    min_sharpe DECIMAL(5,3) NOT NULL DEFAULT 0.0,
    min_pf DECIMAL(5,3) NOT NULL DEFAULT 0.0,      -- Profit Factor
    min_wr DECIMAL(5,3) NOT NULL DEFAULT 0.0,      -- Win Rate (0-1)
    max_dd DECIMAL(5,3) NOT NULL DEFAULT 1.0,      -- Max Drawdown (0-1)
    cpcv_required BOOLEAN NOT NULL DEFAULT FALSE,
    description TEXT,
    updated_at TIMESTAMP DEFAULT NOW()
);

-- Insert default criteria
INSERT INTO rank_criteria (rank, min_sharpe, min_pf, min_wr, max_dd, cpcv_required, description)
VALUES 
    ('B', 0.1, 0.0, 0.0, 1.0, FALSE, 'Entry level - passed Phase 1 BT'),
    ('A', 0.3, 1.2, 0.40, 0.20, FALSE, 'Professional - OOS validated'),
    ('S', 0.5, 1.5, 0.45, 0.15, TRUE, 'Elite - CPCV validated, live trading permitted'),
    ('legend', 1.0, 2.0, 0.55, 0.10, TRUE, 'Legendary - Exceptional performance')
ON CONFLICT (rank) DO UPDATE SET 
    min_sharpe = EXCLUDED.min_sharpe,
    min_pf = EXCLUDED.min_pf,
    min_wr = EXCLUDED.min_wr,
    max_dd = EXCLUDED.max_dd,
    cpcv_required = EXCLUDED.cpcv_required,
    updated_at = NOW();

-- Create index for fast lookup
CREATE INDEX IF NOT EXISTS idx_rank_criteria_rank ON rank_criteria(rank);

-- CPCV Validation History Table (for auditing)
CREATE TABLE IF NOT EXISTS cpcv_history (
    id SERIAL PRIMARY KEY,
    strategy_name VARCHAR(255) NOT NULL,
    validated_at TIMESTAMP DEFAULT NOW(),
    passed BOOLEAN NOT NULL,
    median_sharpe DECIMAL(5,3),
    paths_passed INTEGER,
    paths_total INTEGER,
    old_rank VARCHAR(10),
    new_rank VARCHAR(10),
    notes TEXT
);

-- Create index for strategy lookup
CREATE INDEX IF NOT EXISTS idx_cpcv_history_strategy ON cpcv_history(strategy_name);
CREATE INDEX IF NOT EXISTS idx_cpcv_history_date ON cpcv_history(validated_at);
