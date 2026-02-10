use serde::{Deserialize, Serialize};
use rayon::prelude::*;
use std::collections::HashMap;
use std::error::Error;
use crate::strategy_ast::{StrategyNode, EvalContext}; // Phase 23: AST Integration

pub const DEFAULT_SLIPPAGE: f64 = 0.005; // 0.5 pips (USDJPY calibrated)

// Internal struct for CSV parsing (matches file header)
#[derive(Debug, Deserialize)]
struct CsvCandle {
    timestamp: i64,
    open: f64,
    high: f64,
    low: f64,
    close: f64,
    volume: f64,
}

impl CsvCandle {
    fn to_candle(&self) -> Candle {
        Candle {
            timestamp: self.timestamp,
            open: self.open,
            high: self.high,
            low: self.low,
            close: self.close,
            volume: self.volume,
        }
    }
}

pub fn load_candles_from_csv(path: &str) -> Result<Vec<Candle>, Box<dyn Error>> {
    let mut rdr = csv::Reader::from_path(path)?;
    let mut candles = Vec::new();
    for result in rdr.deserialize() {
        let record: CsvCandle = result?;
        candles.push(record.to_candle());
    }
    Ok(candles)
}

#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)]
pub struct Candle {
    #[serde(rename = "t")]
    pub timestamp: i64,
    #[serde(rename = "o", default)]
    pub open: f64,
    #[serde(rename = "h", default)]
    pub high: f64,
    #[serde(rename = "l", default)]
    pub low: f64,
    #[serde(rename = "c")]
    pub close: f64,
    #[serde(rename = "v", default)]
    pub volume: f64,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SwapRecord {
    #[serde(rename = "t")]
    pub timestamp: i64,
    #[serde(rename = "sl")]
    pub swap_long: f64,
    #[serde(rename = "ss")]
    pub swap_short: f64,
}

// V6.11: Indicator type for multi-strategy backtesting
// V6.12: Added Ema for tournament.rs compatibility, Serialize for saving
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum IndicatorType {
    #[default]
    Sma,
    Ema,
    Rsi,
    Macd,
    Bb,
    Stoch,
}

// V6.12: Methods moved from tournament.rs for unified implementation
impl IndicatorType {
    pub fn random() -> Self {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        match rng.gen_range(0..6) {
            0 => IndicatorType::Sma,
            1 => IndicatorType::Ema,
            2 => IndicatorType::Rsi,
            3 => IndicatorType::Bb,
            4 => IndicatorType::Macd,
            _ => IndicatorType::Stoch,
        }
    }
    
    pub fn name(&self) -> &str {
        match self {
            IndicatorType::Sma => "SMA",
            IndicatorType::Ema => "EMA",
            IndicatorType::Rsi => "RSI",
            IndicatorType::Bb => "BB",
            IndicatorType::Macd => "MACD",
            IndicatorType::Stoch => "STOCH",
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct Strategy {
    pub name: String,
    pub sma_short: usize,
    pub sma_long: usize,
    pub sl: f64,
    pub tp: f64,
    pub volume: f64,
    #[serde(default)]
    pub indicator_type: IndicatorType,  // V6.11: Type of indicator
    
    // V9.0: MTF Filter Parameters (Expert Panel)
    #[serde(default)]
    pub filter_enabled: bool,
    #[serde(default)]
    pub filter_tf: String,      // e.g. "D1", "H4"
    #[serde(default)]
    pub filter_period: usize,   // e.g. 200 (SMA200)
    #[serde(default)]
    pub filter_logic: String,   // e.g. "PRICE_ABOVE_SMA"

    // Phase 23: AST-based Logic (Simons/PG)
    #[serde(default)]
    pub entry_long_ast: Option<StrategyNode>,
    #[serde(default)]
    pub entry_short_ast: Option<StrategyNode>,
    #[serde(default)]
    pub exit_long_ast: Option<StrategyNode>,
    #[serde(default)]
    pub exit_short_ast: Option<StrategyNode>,
}

#[derive(Debug, Serialize)]
pub struct BacktestResult {
    pub strategy_name: String,
    pub trades: i32,
    pub wins: i32,
    pub losses: i32,
    pub pnl: f64,
    pub sharpe: f64,
    pub sortino: f64,
    pub max_drawdown: f64,
    pub win_rate: f64,
    pub profit_factor: f64, // Kodoku Rule 2 & 3
    pub adjusted_sharpe: f64, // V8.1: Penalized Sharpe (Taleb Haircut)
    pub sharpe_ci_lower: f64, // V8.7: Bootstrap CI lower bound (Taleb)
}

/// V8.1: Taleb's "Haircut" - Penalize suspicious performance
pub fn calculate_penalized_sharpe(sharpe: f64, trades: i32) -> f64 {
    let mut penalty = 0.0;
    
    // 1. "Too Good To Be True" Penalty (Curve Fitting)
    if sharpe > 5.0 { 
        penalty += 0.5; // 50% haircut for Sharpe > 5
    } else if sharpe > 3.0 { 
        penalty += 0.2; // 20% haircut for Sharpe > 3
    }
    
    // 2. Small Sample Penalty (< 50 trades)
    if trades < 50 { 
        // Linear scale penalty: 0% at 50 trades, 30% at 0 trades
        let sample_penalty = 0.3 * (1.0 - (trades as f64 / 50.0)).max(0.0);
        penalty += sample_penalty;
    }
    
    let adjusted = sharpe * (1.0 - penalty);
    adjusted.max(0.0) // No negative sharpe from penalty alone
}

/// V8.7: Taleb's Bootstrap Confidence Interval for Sharpe Ratio
/// Returns the 5th percentile (lower bound of 90% CI)
pub fn bootstrap_sharpe_ci(returns: &[f64], n_samples: usize) -> f64 {
    use rand::Rng;
    
    if returns.len() < 10 {
        return f64::NEG_INFINITY; // Not enough data
    }
    
    let mut rng = rand::thread_rng();
    let mut sharpes: Vec<f64> = Vec::with_capacity(n_samples);
    
    for _ in 0..n_samples {
        // Resample with replacement
        let resampled: Vec<f64> = (0..returns.len())
            .map(|_| returns[rng.gen_range(0..returns.len())])
            .collect();
        
        sharpes.push(sharpe_ratio(&resampled));
    }
    
    // Sort and return 5th percentile
    sharpes.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let idx = (n_samples as f64 * 0.05) as usize;
    sharpes.get(idx).copied().unwrap_or(0.0)
}

/// Calculate Simple Moving Average
fn calculate_sma(candles: &[Candle], period: usize, end_idx: usize) -> Option<f64> {
    if end_idx < period {
        return None;
    }
    let sum: f64 = candles[end_idx - period..end_idx]
        .iter()
        .map(|c| c.close)
        .sum();
    Some(sum / period as f64)
}

/// Calculate Sharpe Ratio
fn sharpe_ratio(returns: &[f64]) -> f64 {
    if returns.is_empty() {
        return 0.0;
    }
    let active: Vec<f64> = returns
        .iter()
        .copied()
        .filter(|r| r.abs() > 1e-12)
        .collect();
    let sample = if active.len() >= 2 { &active[..] } else { returns };
    let mean = sample.iter().sum::<f64>() / sample.len() as f64;
    let variance = sample.iter().map(|r| (r - mean).powi(2)).sum::<f64>() / sample.len() as f64;
    let std = variance.sqrt();
    if std == 0.0 {
        return 0.0;
    }
    mean / std * (252.0_f64).sqrt() // Annualized
}

/// V6.17: Calculate Value at Risk (Historical Method)
/// Returns the maximum expected loss at the given confidence level
pub fn calculate_var(returns: &[f64], confidence: f64) -> f64 {
    if returns.is_empty() {
        return 0.0;
    }
    let mut sorted_returns: Vec<f64> = returns.to_vec();
    sorted_returns.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    
    // VaR index: for 95% confidence, we want the 5th percentile
    let var_idx = ((1.0 - confidence) * sorted_returns.len() as f64) as usize;
    let var_idx = var_idx.min(sorted_returns.len() - 1);
    
    // Return the negative of the return at the percentile (VaR is positive)
    -sorted_returns[var_idx]
}

/// V6.17: Calculate Conditional VaR (Expected Shortfall)
/// Average loss beyond VaR - more conservative risk measure
pub fn calculate_cvar(returns: &[f64], confidence: f64) -> f64 {
    if returns.is_empty() {
        return 0.0;
    }
    let mut sorted_returns: Vec<f64> = returns.to_vec();
    sorted_returns.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    
    let var_idx = ((1.0 - confidence) * sorted_returns.len() as f64) as usize;
    let var_idx = var_idx.min(sorted_returns.len() - 1);
    
    if var_idx == 0 {
        return -sorted_returns[0];
    }
    
    // Average of all returns below VaR threshold
    let tail_sum: f64 = sorted_returns[0..=var_idx].iter().sum();
    -(tail_sum / (var_idx + 1) as f64)
}

/// Calculate Sortino Ratio (only downside deviation)
fn sortino_ratio(returns: &[f64]) -> f64 {
    if returns.is_empty() {
        return 0.0;
    }
    let mean = returns.iter().sum::<f64>() / returns.len() as f64;
    let downside: Vec<f64> = returns.iter().filter(|&&r| r < 0.0).copied().collect();
    if downside.is_empty() {
        return f64::INFINITY; // No losses
    }
    let downside_variance = downside.iter().map(|r| r.powi(2)).sum::<f64>() / downside.len() as f64;
    let downside_std = downside_variance.sqrt();
    if downside_std == 0.0 {
        return 0.0;
    }
    mean / downside_std * (252.0_f64).sqrt()
}

/// Calculate maximum drawdown
fn max_drawdown(equity_curve: &[f64]) -> f64 {
    let mut peak = equity_curve.first().copied().unwrap_or(0.0);
    let mut max_dd = 0.0;
    
    for &equity in equity_curve {
        if equity > peak {
            peak = equity;
        }
        let dd = (peak - equity) / peak;
        if dd > max_dd {
            max_dd = dd;
        }
    }
    max_dd
}

/// Calculate RSI (Relative Strength Index)
fn calculate_rsi(candles: &[Candle], period: usize, end_idx: usize) -> Option<f64> {
    if end_idx < period + 1 {
        return None;
    }
    
    let mut gains = 0.0;
    let mut losses = 0.0;
    
    for i in (end_idx - period)..end_idx {
        let change = candles[i].close - candles[i - 1].close;
        if change > 0.0 {
            gains += change;
        } else {
            losses -= change;
        }
    }
    
    let avg_gain = gains / period as f64;
    let avg_loss = losses / period as f64;
    
    if avg_loss == 0.0 {
        return Some(100.0);
    }
    
    let rs = avg_gain / avg_loss;
    Some(100.0 - (100.0 / (1.0 + rs)))
}

/// Calculate MACD (12, 26, 9)
fn calculate_macd(candles: &[Candle], end_idx: usize) -> Option<(f64, f64, f64)> {
    // Need at least 35 candles (26 + 9)
    if end_idx < 35 {
        return None;
    }
    
    // EMA calculation helper
    let calc_ema = |data: &[f64], period: usize| -> f64 {
        let k = 2.0 / (period as f64 + 1.0);
        let mut ema = data[0];
        for &val in &data[1..] {
            ema = val * k + ema * (1.0 - k);
        }
        ema
    };
    
    let closes: Vec<f64> = candles[0..end_idx].iter().map(|c| c.close).collect();
    
    let ema12 = calc_ema(&closes[closes.len()-12..], 12);
    let ema26 = calc_ema(&closes[closes.len()-26..], 26);
    let macd_line = ema12 - ema26;
    
    // Signal line (9-period EMA of MACD)
    // Simplified: using last few MACD values
    let signal = macd_line * 0.9; // Approximation
    let histogram = macd_line - signal;
    
    Some((macd_line, signal, histogram))
}

/// Calculate Bollinger Bands
fn calculate_bollinger(candles: &[Candle], period: usize, std_dev: f64, end_idx: usize) -> Option<(f64, f64, f64)> {
    if end_idx < period {
        return None;
    }
    
    let data: Vec<f64> = candles[end_idx - period..end_idx]
        .iter()
        .map(|c| c.close)
        .collect();
    
    let mean = data.iter().sum::<f64>() / period as f64;
    let variance = data.iter().map(|v| (v - mean).powi(2)).sum::<f64>() / period as f64;
    let std = variance.sqrt();
    
    let upper = mean + std_dev * std;
    let lower = mean - std_dev * std;
    
    Some((upper, mean, lower))
}

// Helper to get timeframe duration in seconds
fn get_tf_duration(tf: &str) -> i64 {
    match tf {
        "M1" => 60,
        "M5" => 300,
        "M15" => 900,
        "M30" => 1800,
        "H1" => 3600,
        "H4" => 14400,
        "D1" => 86400,
        "W1" => 604800,
        _ => 3600, // Default to H1 if unknown
    }
}

// V9.0: MTF Filter Helper
// Checks if the higher timeframe condition is met
fn check_mtf_filter(strategy: &Strategy, tf_candles: &[Candle], current_ts: i64) -> bool {
    // Prevent look-ahead bias: Use only candles strictly closed BEFORE or AT current M1 timestamp
    // H1 candle at 10:00 opens at 10:00 and closes at 11:00.
    // At 10:05, we CANNOT use 10:00 candle. We must use 09:00 candle (closed at 10:00).
    
    let duration = get_tf_duration(&strategy.filter_tf);
    let cutoff = current_ts - duration; // e.g. 10:05 - 1h = 09:05. 09:00 candle is valid. 10:00 is not.
    
    // Binary search to find the partition point
    // partition_point returns the index of the first element where the predicate is FALSE.
    // We want the last element where timestamp <= cutoff.
    let idx = tf_candles.partition_point(|c| c.timestamp <= cutoff);
    
    // idx is the first candle > cutoff.
    // valid_idx = idx - 1 is the last candle <= cutoff.
    if idx == 0 { return false; } 
    let valid_idx = idx - 1;
    
    match strategy.filter_logic.as_str() {
        "PRICE_ABOVE_SMA" => {
            if valid_idx + 1 < strategy.filter_period { return false; }
            let start = (valid_idx + 1).saturating_sub(strategy.filter_period);
            let slice = &tf_candles[start..=valid_idx];
            let sum: f64 = slice.iter().map(|c| c.close).sum();
            let sma = sum / slice.len() as f64;
            let current_tf_price = tf_candles[valid_idx].close;
            current_tf_price > sma
        },
        "PRICE_BELOW_SMA" => {
            if valid_idx + 1 < strategy.filter_period { return false; }
            let start = (valid_idx + 1).saturating_sub(strategy.filter_period);
            let slice = &tf_candles[start..=valid_idx];
            let sum: f64 = slice.iter().map(|c| c.close).sum();
            let sma = sum / slice.len() as f64;
            let current_tf_price = tf_candles[valid_idx].close;
            current_tf_price < sma
        },
        // Add more logic here (RSI, etc)
        _ => true, // Default pass
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Position {
    None,
    Long { entry: f64, sl: f64, tp: f64 },
    Short { entry: f64, sl: f64, tp: f64 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GateDecision {
    Buy,
    Sell,
    Hold,
}

fn run_backtest_internal(
    strategy: &Strategy,
    candles: &[Candle],
    aux_candles: &HashMap<String, Vec<Candle>>,
    swap_history: &[SwapRecord],
    slippage: f64,
    gate: Option<&[GateDecision]>,
) -> BacktestResult {
    let gate = match gate {
        Some(g) if g.len() == candles.len() => Some(g),
        _ => None,
    };

    // V6.11: Minimum bars depends on indicator type
    let min_bars = match strategy.indicator_type {
        IndicatorType::Sma | IndicatorType::Ema => strategy.sma_long.max(strategy.sma_short) + 2,
        IndicatorType::Rsi => strategy.sma_short + 5,
        IndicatorType::Macd => 40,
        IndicatorType::Bb => strategy.sma_short + 2,
        IndicatorType::Stoch => strategy.sma_short + 2,
    };
    
    if candles.len() < min_bars {
        return BacktestResult {
            strategy_name: strategy.name.clone(),
            trades: 0, wins: 0, losses: 0, pnl: 0.0,
            sharpe: 0.0, sortino: 0.0, max_drawdown: 0.0, win_rate: 0.0,
            profit_factor: 0.0,
            adjusted_sharpe: 0.0,
            sharpe_ci_lower: f64::NEG_INFINITY,
        };
    }

    // Track Daily PnL for Daily Sharpe Calculation
    let mut daily_pnl: std::collections::HashMap<i64, f64> = std::collections::HashMap::new();
    
    let mut position = Position::None;
    let mut entry_time = 0;
    let mut trades = 0;
    let mut wins = 0;
    let mut losses = 0;
    let mut pnl = 0.0;
    let mut gross_profit = 0.0;
    let mut gross_loss = 0.0;
    // Keep 'returns' for existing logic (Sortino etc) but use daily_returns for Sharpe
	    let mut returns: Vec<f64> = Vec::new();
	    let mut equity_curve: Vec<f64> = vec![10000.0]; // Starting capital
	    
	    // [OPTIMIZATION] Pre-calculate Indicators to avoid O(N^2) complexity
	    let macd_data = if matches!(strategy.indicator_type, IndicatorType::Macd) {
	         Some(precalculate_macd(candles))
	    } else {
         None
    };

    // Phase 23: Pre-calculate columnar data if AST is present
    let (ast_closes, ast_opens, ast_highs, ast_lows, ast_volumes) = 
        if strategy.entry_long_ast.is_some() || strategy.entry_short_ast.is_some() {
            let n = candles.len();
            let mut c = Vec::with_capacity(n);
            let mut o = Vec::with_capacity(n);
            let mut h = Vec::with_capacity(n);
            let mut l = Vec::with_capacity(n);
            let mut v = Vec::with_capacity(n);
            for candle in candles {
                c.push(candle.close);
                o.push(candle.open);
                h.push(candle.high);
                l.push(candle.low);
                v.push(candle.volume);
            }
            (Some(c), Some(o), Some(h), Some(l), Some(v))
        } else {
            (None, None, None, None, None)
        };

    // First pass: Calculate PnL and populate daily_pnl
    let total_candles = candles.len();
    for i in min_bars..total_candles {
        // [MONITORING] Show progress for long validations
        if i % 100000 == 0 {
            // let pct = (i as f64 / total_candles as f64) * 100.0;
            // eprintln!("PROGRESS: Processed {} / {} candles ({:.1}%)", i, total_candles, pct);
        }

        let price = candles[i].close;
        let timestamp = candles[i].timestamp;
        let day_idx = timestamp / 86400; // Unix timestamp to day index

        // V6.11: Generate entry/exit signals based on indicator type
        // Phase 23: Check for AST first
        let (buy_signal, sell_signal, exit_long, exit_short) = if let (Some(c), Some(o), Some(h), Some(l), Some(v)) = (&ast_closes, &ast_opens, &ast_highs, &ast_lows, &ast_volumes) {
             let ctx = EvalContext {
                closes: c, 
                opens: o, 
                highs: h, 
                lows: l, 
                volumes: v, 
                index: i 
             };
             
             let buy = strategy.entry_long_ast.as_ref().map(|ast| ast.eval_bool(&ctx)).unwrap_or(false);
             let sell = strategy.entry_short_ast.as_ref().map(|ast| ast.eval_bool(&ctx)).unwrap_or(false);
             let ex_long = strategy.exit_long_ast.as_ref().map(|ast| ast.eval_bool(&ctx)).unwrap_or(false);
             let ex_short = strategy.exit_short_ast.as_ref().map(|ast| ast.eval_bool(&ctx)).unwrap_or(false);
             
             (buy, sell, ex_long, ex_short)
        } else {
             match strategy.indicator_type {
                IndicatorType::Sma | IndicatorType::Ema => generate_sma_signals(candles, strategy, i),
                IndicatorType::Rsi => generate_rsi_signals(candles, strategy, i),
                IndicatorType::Bb => generate_bb_signals(candles, strategy, i, price),
                IndicatorType::Macd => {
                    // Optimized O(1) Access
                    if let Some((macd_vec, sig_vec, _)) = macd_data.as_ref() {
                        let macd_line = macd_vec[i];
                        let signal = sig_vec[i];
                        let prev_macd = macd_vec[i-1];
                        let prev_signal = sig_vec[i-1];
                        
                        let buy = prev_macd < prev_signal && macd_line > signal;
                        let sell = prev_macd > prev_signal && macd_line < signal;
                        (buy, sell, sell, buy)
                    } else {
                        (false, false, false, false)
                    }
                },
                IndicatorType::Stoch => generate_stoch_signals(candles, strategy, i),
            }
        };
        
        // Initialize daily PnL for this day if not exists (to ensure we capture 0 PnL days later properly if we iterate candles)
        // actually, we only add PnL when a trade closes.
        
        let mut trade_pnl = 0.0;

        // Check exits first
	        match position {
	            Position::Long { entry, sl, tp } => {
	                let exit = price <= sl || price >= tp || exit_long;
	                if exit {
	                    trade_pnl = (price - slippage) - entry;
	                    
	                    // V31.0: Apply Swap (Long)
	                    let swap_points = calculate_swap(entry_time, timestamp, true, swap_history);
	                    trade_pnl += swap_points;

                    pnl += trade_pnl;
                    trades += 1;
                    if trade_pnl > 0.0 { 
                        wins += 1; 
                        gross_profit += trade_pnl;
                    } else { 
                        losses += 1; 
                        gross_loss += trade_pnl.abs();
                    }
                    
                    // Add to daily PnL
                    *daily_pnl.entry(day_idx).or_insert(0.0) += trade_pnl;
                    
                    returns.push(trade_pnl / entry); // Keep trade returns for sortino/other metrics? No, usually Sharpe is time-based.
                    // For Sortino/Sharpe we really should use time-based returns.
                    // But 'returns' history was used for bootstrap... let's keep it for now but use daily for Sharpe.
                    
                    equity_curve.push(equity_curve.last().unwrap() + trade_pnl); // Updated: use raw PnL without multiplier
                    // Previous code used: equity_curve.push(equity_curve.last().unwrap() + trade_pnl * 100.0);
                    
                    position = Position::None;
                }
            }
	            Position::Short { entry, sl, tp } => {
	                let exit = price >= sl || price <= tp || exit_short;
	                if exit {
	                    trade_pnl = entry - (price + slippage);
	                    
	                    // V31.0: Apply Swap (Short)
	                    let swap_points = calculate_swap(entry_time, timestamp, false, swap_history);
	                    trade_pnl += swap_points;

                    pnl += trade_pnl;
                    trades += 1;
                    if trade_pnl > 0.0 { 
                        wins += 1; 
                        gross_profit += trade_pnl;
                    } else { 
                        losses += 1; 
                        gross_loss += trade_pnl.abs();
                    }
                    
                    // Add to daily PnL
                    *daily_pnl.entry(day_idx).or_insert(0.0) += trade_pnl;
                    
                    returns.push(trade_pnl / entry);
                    equity_curve.push(equity_curve.last().unwrap() + trade_pnl * 100.0);
                    position = Position::None;
                }
            }
            Position::None => {}
	        }
	        
	        // Check entries
	        if position == Position::None {
	            let mut entry_buy = buy_signal;
	            let mut entry_sell = sell_signal;
	            if let Some(g) = gate {
	                match g[i] {
	                    GateDecision::Buy => {
	                        entry_sell = false;
	                    }
	                    GateDecision::Sell => {
	                        entry_buy = false;
	                    }
	                    GateDecision::Hold => {
	                        entry_buy = false;
	                        entry_sell = false;
	                    }
	                }
	            }

	            // V9.0: Apply MTF Filter
	            let mut filter_passed = true;
	            if strategy.filter_enabled {
	                if let Some(tf_candles) = aux_candles.get(&strategy.filter_tf) {
                    if !check_mtf_filter(strategy, tf_candles, timestamp) {
                        filter_passed = false;
                    }
                }
            }
	            
	            if filter_passed {
	                if entry_buy {
	                let entry_price = price + slippage;
	                position = Position::Long {
	                    entry: entry_price,
	                    sl: entry_price - strategy.sl,
	                    tp: entry_price + strategy.tp,
	                };
	                entry_time = timestamp;
	            } else if entry_sell {
	                let entry_price = price - slippage;
	                position = Position::Short {
	                    entry: entry_price,
	                    sl: entry_price + strategy.sl,
	                    tp: entry_price - strategy.tp,
	                };
                entry_time = timestamp;
            }
         } // End filter_passed
        }
    }
    
    // Construct Daily Returns Vector
    // Iterate from first candle day to last candle day
    let start_time = candles[0].timestamp;
    let end_time = candles[candles.len() - 1].timestamp;
    let start_day = start_time / 86400;
    let end_day = end_time / 86400;
    
    // Starting equity for % return calculation
    let mut current_equity = 10000.0; 
    let mut daily_returns_vec: Vec<f64> = Vec::new();
    
    for day in start_day..=end_day {
        let day_pnl = *daily_pnl.get(&day).unwrap_or(&0.0);
        
        // Daily Return % = PnL / Equity at start of day
        let day_ret = if current_equity > 0.0 { day_pnl / current_equity } else { 0.0 };
        daily_returns_vec.push(day_ret);
        
        // Update equity for next day
        // Note: equity_curve logic above was adding pnl * 100.0? 
        // Original code: equity_curve.push(equity_curve.last().unwrap() + trade_pnl * 100.0);
        // This implies '100.0' is the position size or multiplier.
        // Let's assume standard volume multiplier. To match 'pnl' summation, we should probably stick to raw pnl 
        // or consistent equity model. Ideally, PnL is raw currency.
        // Let's assume 'pnl' is raw price diff (pips/dollars). 
        // If trade_pnl is price diff, then account PnL depends on volume.
        // Previous code accumulated pnl directly, but equity used * 100.0.
        // Let's assume the user has a fixed volume or the pnl is normalized.
        // *Correction*: To convert Price Diff to PnL, we need Volume.
        // Strategy has 'volume' field. But previous code didn't use it for PnL accumulator?
        // Wait, "pnl += trade_pnl" where trade_pnl = price - entry. That is "pips" essentially.
        // "equity_curve" adding "trade_pnl * 100.0" implies volume 100?
        // Let's stick to using the 'equity_curve' effective equity for return calc to be safe, 
        // OR simpler: just use 10000.0 base.
        
        // Actually, to get a proper Sharpe, we need returns relative to capital.
        // Using "trade_pnl * 100.0" as the actual money change seems to be the precedent.
        // Update equity using raw daily PnL (no multiplier)
                current_equity += day_pnl; 
    }
    
    let win_rate = if trades > 0 { wins as f64 / trades as f64 * 100.0 } else { 0.0 };
    
    // Use Daily Returns for Sharpe
    let sharpe = sharpe_ratio(&daily_returns_vec);
    let ci_lower = bootstrap_sharpe_ci(&daily_returns_vec, 100);
    
    // Sortino on Daily or Per-Trade? Standard is Time-Based (Daily).
    let sortino = sortino_ratio(&daily_returns_vec);

    BacktestResult {
        strategy_name: strategy.name.clone(),
        trades, wins, losses, pnl,
        sharpe,
        sortino, 
        max_drawdown: max_drawdown(&equity_curve),
        win_rate,
        profit_factor: if gross_loss != 0.0 { gross_profit / gross_loss } else { 0.0 },
        adjusted_sharpe: calculate_penalized_sharpe(sharpe, trades),
	        sharpe_ci_lower: ci_lower,
	    }
	}

/// Run backtest on given strategy and candles
/// V6.11: Now supports multiple indicator types (SMA/RSI/BB/MACD/Stoch)
/// V9.0: MTF Support with aux_candles
/// V31.0: Swap Integration
pub fn run_backtest(
    strategy: &Strategy,
    candles: &[Candle],
    aux_candles: &HashMap<String, Vec<Candle>>,
    swap_history: &[SwapRecord],
) -> BacktestResult {
    run_backtest_internal(strategy, candles, aux_candles, swap_history, DEFAULT_SLIPPAGE, None)
}

pub fn run_backtest_with_slippage(
    strategy: &Strategy,
    candles: &[Candle],
    aux_candles: &HashMap<String, Vec<Candle>>,
    swap_history: &[SwapRecord],
    slippage: f64,
) -> BacktestResult {
    run_backtest_internal(strategy, candles, aux_candles, swap_history, slippage, None)
}

pub fn run_backtest_with_gate_and_slippage(
    strategy: &Strategy,
    candles: &[Candle],
    aux_candles: &HashMap<String, Vec<Candle>>,
    swap_history: &[SwapRecord],
    gate: &[GateDecision],
    slippage: f64,
) -> BacktestResult {
    run_backtest_internal(strategy, candles, aux_candles, swap_history, slippage, Some(gate))
}

// V6.11: Signal generators per indicator type
fn generate_sma_signals(candles: &[Candle], strategy: &Strategy, i: usize) -> (bool, bool, bool, bool) {
    let sma_s = calculate_sma(candles, strategy.sma_short, i);
    let sma_l = calculate_sma(candles, strategy.sma_long, i);
    let prev_sma_s = calculate_sma(candles, strategy.sma_short, i - 1);
    let prev_sma_l = calculate_sma(candles, strategy.sma_long, i - 1);
    
    if let (Some(s), Some(l), Some(ps), Some(pl)) = (sma_s, sma_l, prev_sma_s, prev_sma_l) {
        let buy = ps < pl && s > l;  // Golden cross
        let sell = ps > pl && s < l; // Death cross
        (buy, sell, sell, buy)
    } else {
        (false, false, false, false)
    }
}

fn generate_rsi_signals(candles: &[Candle], strategy: &Strategy, i: usize) -> (bool, bool, bool, bool) {
    let period = strategy.sma_short.max(7); // RSI period from sma_short
    let rsi = calculate_rsi(candles, period, i);
    let prev_rsi = calculate_rsi(candles, period, i - 1);
    
    if let (Some(r), Some(pr)) = (rsi, prev_rsi) {
        let buy = pr < 30.0 && r >= 30.0;  // RSI oversold bounce
        let sell = pr > 70.0 && r <= 70.0; // RSI overbought reversal
        let exit_long = r > 70.0;   // Exit long when overbought
        let exit_short = r < 30.0;  // Exit short when oversold
        (buy, sell, exit_long, exit_short)
    } else {
        (false, false, false, false)
    }
}

fn generate_bb_signals(candles: &[Candle], strategy: &Strategy, i: usize, price: f64) -> (bool, bool, bool, bool) {
    let period = strategy.sma_short.max(20);
    let bb = calculate_bollinger(candles, period, 2.0, i);
    let prev_bb = calculate_bollinger(candles, period, 2.0, i - 1);
    
    if let (Some((upper, middle, lower)), Some((_, _, prev_lower))) = (bb, prev_bb) {
        let prev_price = candles[i - 1].close;
        let buy = prev_price <= prev_lower && price > lower;  // Bounce from lower band
        let sell = price >= upper;                             // Touch upper band
        let exit_long = price >= middle;   // Exit at middle band
        let exit_short = price <= middle;
        (buy, sell, exit_long, exit_short)
    } else {
        (false, false, false, false)
    }
}

fn generate_macd_signals(candles: &[Candle], i: usize) -> (bool, bool, bool, bool) {
    let macd = calculate_macd(candles, i);
    let prev_macd = calculate_macd(candles, i - 1);
    
    if let (Some((macd_line, signal, _)), Some((prev_macd_line, prev_signal, _))) = (macd, prev_macd) {
        let buy = prev_macd_line < prev_signal && macd_line > signal;   // MACD cross up
        let sell = prev_macd_line > prev_signal && macd_line < signal;  // MACD cross down
        (buy, sell, sell, buy)
    } else {
        (false, false, false, false)
    }
}

fn generate_stoch_signals(candles: &[Candle], strategy: &Strategy, i: usize) -> (bool, bool, bool, bool) {
    // Use Stochastic oscillator (simplified - K line only)
    let period = strategy.sma_short.max(14);
    if i < period + 1 { return (false, false, false, false); }
    
    // Calculate current and previous Stochastic
    let closes: Vec<f64> = candles[i-period..i].iter().map(|c| c.close).collect();
    let low = closes.iter().cloned().fold(f64::INFINITY, f64::min);
    let high = closes.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let k = if high != low { (candles[i].close - low) / (high - low) * 100.0 } else { 50.0 };
    
    let prev_closes: Vec<f64> = candles[i-period-1..i-1].iter().map(|c| c.close).collect();
    let prev_low = prev_closes.iter().cloned().fold(f64::INFINITY, f64::min);
    let prev_high = prev_closes.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let prev_k = if prev_high != prev_low { (candles[i-1].close - prev_low) / (prev_high - prev_low) * 100.0 } else { 50.0 };
    
    let buy = prev_k < 20.0 && k >= 20.0;   // Oversold exit
    let sell = prev_k > 80.0 && k <= 80.0;  // Overbought exit
    (buy, sell, k > 80.0, k < 20.0)
}

// ===== CLONE DETECTION =====

/// Strategy parameters as a normalized vector for similarity comparison
#[derive(Debug, Clone, Serialize)]
pub struct StrategyVector {
    pub name: String,
    pub vector: Vec<f64>,
}

impl StrategyVector {
    /// Create normalized vector from strategy parameters
    pub fn from_strategy(s: &Strategy) -> Self {
        // Normalize parameters to 0-1 range (approximate ranges)
        let sma_short_norm = s.sma_short as f64 / 50.0;  // SMA short: 1-50
        let sma_long_norm = s.sma_long as f64 / 200.0;   // SMA long: 5-200
        let sl_norm = s.sl / 1.0;                         // SL: 0.01-1.0
        let tp_norm = s.tp / 2.0;                         // TP: 0.02-2.0
        
        StrategyVector {
            name: s.name.clone(),
            vector: vec![sma_short_norm, sma_long_norm, sl_norm, tp_norm],
        }
    }
}

/// Calculate cosine similarity between two vectors
pub fn cosine_similarity(a: &[f64], b: &[f64]) -> f64 {
    if a.len() != b.len() || a.is_empty() {
        return 0.0;
    }
    
    let dot: f64 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
    let mag_a: f64 = a.iter().map(|x| x * x).sum::<f64>().sqrt();
    let mag_b: f64 = b.iter().map(|x| x * x).sum::<f64>().sqrt();
    
    if mag_a == 0.0 || mag_b == 0.0 {
        return 0.0;
    }
    
    dot / (mag_a * mag_b)
}

/// Check if strategy is a clone of any existing strategy
/// Returns (is_clone, most_similar_name, similarity_score)
pub fn check_clone(new_strat: &Strategy, existing: &[Strategy], threshold: f64) -> (bool, String, f64) {
    let new_vec = StrategyVector::from_strategy(new_strat);
    
    let (most_similar, max_sim) = existing.par_iter()
        .map(|existing_strat| {
            let exist_vec = StrategyVector::from_strategy(existing_strat);
            let sim = cosine_similarity(&new_vec.vector, &exist_vec.vector);
            (existing_strat.name.clone(), sim)
        })
        .reduce(|| (String::new(), 0.0), |a, b| if a.1 > b.1 { a } else { b });
    
    (max_sim >= threshold, most_similar, max_sim)
}

/// ═══════════════════════════════════════════════════════════════════
/// V4.0: WALK-FORWARD VALIDATION (教授指摘)
/// データを複数のtrain/testウィンドウに分割して過学習を検証
/// ═══════════════════════════════════════════════════════════════════

#[derive(Debug, Serialize)]
pub struct WalkForwardResult {
    pub strategy_name: String,
    pub windows: usize,
    pub in_sample_sharpe: f64,      // Training performance
    pub out_of_sample_sharpe: f64,  // Test performance
    pub efficiency_ratio: f64,       // OOS/IS (>0.5 = good)
    pub is_overfit: bool,            // OOS << IS
    pub window_results: Vec<(f64, f64)>,  // (IS sharpe, OOS sharpe) per window
}

/// Walk-forward validation with rolling windows
pub fn walk_forward_validate(
    strategy: &Strategy,
    candles: &[Candle],
    aux_candles: &HashMap<String, Vec<Candle>>,
    swap_history: &[SwapRecord],
    n_windows: usize,
    train_pct: f64,  // e.g., 0.7 = 70% train, 30% test per window
) -> WalkForwardResult {
    let total_len = candles.len();
    let window_size = total_len / n_windows;
    
    let mut is_sharpes: Vec<f64> = Vec::new();
    let mut oos_sharpes: Vec<f64> = Vec::new();
    let mut window_results: Vec<(f64, f64)> = Vec::new();
    
    for i in 0..n_windows {
        let start = i * window_size;
        let end = ((i + 1) * window_size).min(total_len);
        let window_data = &candles[start..end];
        
        if window_data.len() < 100 {
            continue;  // Skip too small windows
        }
        
        // Split into train/test
        let train_end = (window_data.len() as f64 * train_pct) as usize;
        let train_data = &window_data[..train_end];
        let test_data = &window_data[train_end..];
        
        // Backtest on both
        let train_result = run_backtest(strategy, train_data, aux_candles, swap_history);
        let test_result = run_backtest(strategy, test_data, aux_candles, swap_history);
        
        is_sharpes.push(train_result.adjusted_sharpe);
        oos_sharpes.push(test_result.adjusted_sharpe);
        window_results.push((train_result.adjusted_sharpe, test_result.adjusted_sharpe));
    }
    
    // Calculate averages
    let avg_is = if is_sharpes.is_empty() { 0.0 } 
                 else { is_sharpes.iter().sum::<f64>() / is_sharpes.len() as f64 };
    let avg_oos = if oos_sharpes.is_empty() { 0.0 } 
                  else { oos_sharpes.iter().sum::<f64>() / oos_sharpes.len() as f64 };
    
    // Efficiency ratio: how well does in-sample predict out-of-sample?
    let efficiency = if avg_is.abs() > 0.001 { avg_oos / avg_is } else { 0.0 };
    
    // V8.7: Check if ANY window has CI lower bound <= 0 (Taleb rejection)
    // For now, we check if the OOS average suggests unreliable Sharpe.
    // A more thorough check would bootstrap each window.
    // Simple heuristic: flag if avg_oos is low.
    
    // Flag as overfit if OOS is significantly worse OR if low confidence
    let is_overfit = efficiency < 0.5 || avg_oos < 0.0 || avg_oos < 0.5; // Min 0.5 Sharpe threshold
    
    WalkForwardResult {
        strategy_name: strategy.name.clone(),
        windows: window_results.len(),
        in_sample_sharpe: avg_is,
        out_of_sample_sharpe: avg_oos,
        efficiency_ratio: efficiency,
        is_overfit,
        window_results,
    }
}

#[derive(Debug, Serialize)]
pub struct CloneCheckResult {
    pub is_clone: bool,
    pub most_similar: String,
    pub similarity: f64,
    pub threshold: f64,
    pub candidate_name: String, // V8.11: Added for async feedback loop
}

// O(N) EMA Calculation
fn calculate_ema_series(data: &[f64], period: usize) -> Vec<f64> {
    if data.is_empty() { return Vec::new(); }
    let k = 2.0 / (period as f64 + 1.0);
    let mut ema = data[0];
    let mut out = Vec::with_capacity(data.len());
    out.push(ema);
    for &val in &data[1..] {
        ema = val * k + ema * (1.0 - k);
        out.push(ema);
    }
    out
}

// O(N) MACD Pre-calculation
fn precalculate_macd(candles: &[Candle]) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
    let closes: Vec<f64> = candles.iter().map(|c| c.close).collect();
    let ema12 = calculate_ema_series(&closes, 12);
    let ema26 = calculate_ema_series(&closes, 26);
    let macd_line: Vec<f64> = ema12.iter().zip(ema26.iter()).map(|(a, b)| a - b).collect();
    let signal_line = calculate_ema_series(&macd_line, 9);
    let histogram: Vec<f64> = macd_line.iter().zip(signal_line.iter()).map(|(a, b)| a - b).collect();
    (macd_line, signal_line, histogram)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_sharpe_ratio() {
        let returns = vec![0.01, 0.02, -0.01, 0.015, 0.005];
        let sharpe = sharpe_ratio(&returns);
        assert!(sharpe > 0.0);
    }

    #[test]
    fn test_sharpe_ratio_ignores_zero_returns() {
        let returns = vec![0.0, 0.0, 0.01, 0.02];
        let sharpe = sharpe_ratio(&returns);
        assert!(sharpe > 30.0);
    }
    
    #[test]
    fn test_sma() {
        let candles = vec![
            Candle { timestamp: 1, open: 1.0, high: 1.0, low: 1.0, close: 10.0, volume: 100.0 },
            Candle { timestamp: 2, open: 1.0, high: 1.0, low: 1.0, close: 20.0, volume: 100.0 },
            Candle { timestamp: 3, open: 1.0, high: 1.0, low: 1.0, close: 30.0, volume: 100.0 },
        ];
        let sma = calculate_sma(&candles, 3, 3).unwrap();
        assert_eq!(sma, 20.0);
    }
    
    #[test]
    fn test_cosine_similarity() {
        let a = vec![1.0, 0.0, 0.0];
        let b = vec![1.0, 0.0, 0.0];
        assert!((cosine_similarity(&a, &b) - 1.0).abs() < 0.001);
        
        let c = vec![0.0, 1.0, 0.0];
        assert!(cosine_similarity(&a, &c).abs() < 0.001);
    }
    
    #[test]
    fn test_clone_detection() {
        let existing = vec![
            Strategy { name: "s1".to_string(), sma_short: 5, sma_long: 20, sl: 0.1, tp: 0.2, volume: 0.01, indicator_type: IndicatorType::Sma, filter_enabled: false, filter_tf: String::new(), filter_period: 0, filter_logic: String::new(), entry_long_ast: None, entry_short_ast: None, exit_long_ast: None, exit_short_ast: None },
        ];
        
        // Almost identical - should be clone
        let clone = Strategy { name: "s2".to_string(), sma_short: 5, sma_long: 20, sl: 0.1, tp: 0.2, volume: 0.01, indicator_type: IndicatorType::Sma, filter_enabled: false, filter_tf: String::new(), filter_period: 0, filter_logic: String::new(), entry_long_ast: None, entry_short_ast: None, exit_long_ast: None, exit_short_ast: None };
        let (is_clone, _, sim) = check_clone(&clone, &existing, 0.99);
        assert!(is_clone);
        assert!(sim > 0.99);
        
        // Different - should not be clone
        let different = Strategy { name: "s3".to_string(), sma_short: 50, sma_long: 200, sl: 0.5, tp: 1.0, volume: 0.01, indicator_type: IndicatorType::Sma, filter_enabled: false, filter_tf: String::new(), filter_period: 0, filter_logic: String::new(), entry_long_ast: None, entry_short_ast: None, exit_long_ast: None, exit_short_ast: None };
        let (is_clone2, _, _) = check_clone(&different, &existing, 0.99);
        assert!(!is_clone2);
    }

    #[test]
    fn test_slippage_and_sl_execution() {
        // Verify that slippage (0.005) is applied to entry, and PnL is calculated correctly upon SL hit.
        // Scenario Analysis (Lagged Signal):
        // Signal is generated based on SMA calculated on PREVIOUS candles (excluding current).
        // i=9 (Price 110): SMA2 uses [7,8] (90,90)=90. SMA5 uses 4..9 (Avg ~94). SMA2 < SMA5.
        // i=10 (Price 100): SMA2 uses [8,9] (90,110)=100. SMA5 uses 5..10 (Avg 96). SMA2 > SMA5. GOLDEN CROSS.
        // Trade Triggered at i=10 (Price 100).
        // Entry Price = 100.0 + 0.005 = 100.005.
        // SL = 100.005 - 5.0 = 95.005.
        //
        // i=11 (Price 90):
        // Low/Close(90) < SL(95.005). Exit Triggered.
        // Exit Price = Close (90.0).
        // Exit Execution Price = 90.0 - 0.005 (Exit Slippage) = 89.995
        // Expected PnL = 89.995 - 100.005 = -10.01.
        
        let mut candles = Vec::new();
        // 0..6: Price 100.0
        for i in 0..6 {
            candles.push(Candle { timestamp: i as i64, open: 100.0, high: 100.0, low: 100.0, close: 100.0, volume: 100.0 });
        }
        // 6..9: Price 90.0 (Dip)
        for i in 6..9 {
            candles.push(Candle { timestamp: i as i64, open: 90.0, high: 90.0, low: 90.0, close: 90.0, volume: 100.0 });
        }
        // 9: Jump to 110.0
        candles.push(Candle { timestamp: 9, open: 110.0, high: 110.0, low: 110.0, close: 110.0, volume: 100.0 });
        
        // 10: Drop to 100.0 (Entry Here)
        candles.push(Candle { timestamp: 10, open: 100.0, high: 100.0, low: 100.0, close: 100.0, volume: 100.0 });

        // 11: Drop to 90.0 (Exit Here)
        candles.push(Candle { timestamp: 11, open: 90.0, high: 90.0, low: 90.0, close: 90.0, volume: 100.0 });

        let strategy = Strategy {
            name: "TestSlip".to_string(),
            sma_short: 2,
            sma_long: 5,
            sl: 5.0,
            tp: 5.0,
            volume: 0.1,
            indicator_type: IndicatorType::Sma,
            filter_enabled: false,
            filter_tf: String::new(),
            filter_period: 0,
            filter_logic: String::new(),
            entry_long_ast: None,
            entry_short_ast: None,
            exit_long_ast: None,
            exit_short_ast: None,
        };
        
        let result = run_backtest(&strategy, &candles, &std::collections::HashMap::new(), &[]);
        
        // Assert trade happened
        assert_eq!(result.trades, 1, "Should have executed 1 trade");
        
        let expected_pnl = (90.0 - 0.005) - (100.0 + 0.005); // -10.01
        assert!((result.pnl - expected_pnl).abs() < 0.0001, 
            "PnL mismatch. Got: {}, Expected: {}", result.pnl, expected_pnl);
    }

    #[test]
    fn test_daily_sharpe_aggregation() {
        let mut candles = Vec::new();
        // Day 1 (ts=0): 10 candles. Price 100.
        for i in 0..10 {
            candles.push(Candle { timestamp: 0, open: 100.0, high: 100.0, low: 100.0, close: 100.0, volume: 100.0 });
        }
        
        // Setup strategy
         let strategy = Strategy {
            name: "TestAgg".to_string(),
            sma_short: 2,
            sma_long: 5,
            sl: 10.0,
            tp: 10.0,
            volume: 0.1,
            indicator_type: IndicatorType::Sma,
            filter_enabled: false,
            filter_tf: String::new(),
            filter_period: 0,
            filter_logic: String::new(),
            entry_long_ast: None,
            entry_short_ast: None,
            exit_long_ast: None,
            exit_short_ast: None,
        };
        
        let result = run_backtest(&strategy, &candles, &std::collections::HashMap::new(), &[]);
        assert!(!result.sharpe.is_nan());
    }

    #[test]
    fn test_run_backtest_with_gate_blocks_entries() {
        let mut candles = Vec::new();
        // 0..6: Price 100.0
        for i in 0..6 {
            candles.push(Candle { timestamp: i as i64, open: 100.0, high: 100.0, low: 100.0, close: 100.0, volume: 100.0 });
        }
        // 6..9: Price 90.0 (Dip)
        for i in 6..9 {
            candles.push(Candle { timestamp: i as i64, open: 90.0, high: 90.0, low: 90.0, close: 90.0, volume: 100.0 });
        }
        // 9: Jump to 110.0
        candles.push(Candle { timestamp: 9, open: 110.0, high: 110.0, low: 110.0, close: 110.0, volume: 100.0 });

        // 10: Drop to 100.0 (Entry Here)
        candles.push(Candle { timestamp: 10, open: 100.0, high: 100.0, low: 100.0, close: 100.0, volume: 100.0 });

        // 11: Drop to 90.0 (Exit Here)
        candles.push(Candle { timestamp: 11, open: 90.0, high: 90.0, low: 90.0, close: 90.0, volume: 100.0 });

        let strategy = Strategy {
            name: "TestGate".to_string(),
            sma_short: 2,
            sma_long: 5,
            sl: 5.0,
            tp: 5.0,
            volume: 0.1,
            indicator_type: IndicatorType::Sma,
            filter_enabled: false,
            filter_tf: String::new(),
            filter_period: 0,
            filter_logic: String::new(),
            entry_long_ast: None,
            entry_short_ast: None,
            exit_long_ast: None,
            exit_short_ast: None,
        };

        let base = run_backtest_with_slippage(&strategy, &candles, &std::collections::HashMap::new(), &[], 0.0);
        assert_eq!(base.trades, 1, "sanity: base should trade");

        let gate = vec![GateDecision::Hold; candles.len()];
        let gated = run_backtest_with_gate_and_slippage(&strategy, &candles, &std::collections::HashMap::new(), &[], &gate, 0.0);
        assert_eq!(gated.trades, 0, "gate should block all entries");
    }
}

/// V10.1: Resample candles from M1 to target timeframe
/// Used for P5.1 Optimization (Guardian-side Resampling)
pub fn resample_candles(candles: &[Candle], tf_seconds: i64) -> Vec<Candle> {
    if candles.is_empty() || tf_seconds <= 60 {
        return candles.to_vec();
    }
    
    let mut resampled: Vec<Candle> = Vec::new();
    let mut current_bucket_start = -1;
    
    // Temp accumulation vars
    let mut open = 0.0;
    let mut high = f64::NEG_INFINITY;
    let mut low = f64::INFINITY;
    let mut close = 0.0;
    let mut volume = 0.0;
    let mut has_data = false;
    
    for c in candles {
        // Calculate bucket start (floor division)
        let bucket_start = (c.timestamp / tf_seconds) * tf_seconds;
        
        if bucket_start != current_bucket_start {
            // Push previous candle if exists
            if has_data {
                resampled.push(Candle {
                    timestamp: current_bucket_start,
                    open,
                    high,
                    low,
                    close,
                    volume,
                });
            }
            
            // Start new bucket
            current_bucket_start = bucket_start;
            open = c.open;
            high = c.high;
            low = c.low;
            close = c.close;
            volume = c.volume;
            has_data = true;
        } else {
            // Aggregate
            high = high.max(c.high);
            low = low.min(c.low);
            close = c.close;
            volume += c.volume;
        }
    }
    
    // Push last candle
    if has_data {
        resampled.push(Candle {
            timestamp: current_bucket_start,
            open,
            high,
            low,
            close,
            volume,
        });
    }
    
    resampled
}

pub fn calculate_swap(entry_time: i64, exit_time: i64, is_long: bool, swap_history: &[SwapRecord]) -> f64 {
    if swap_history.is_empty() { return 0.0; }
    
    let mut total_swap = 0.0;
    // Rollover occurs at 00:00 server time. 
    // We calculate how many rollover events occurred between entry and exit.
    let entry_day = entry_time / 86400;
    let exit_day = exit_time / 86400;
    let days = exit_day - entry_day;
    
    if days > 0 {
        // Find the most recent swap rate at exit_time
        let mut best_swap = 0.0;
        let mut latest_ts = -1;
        
        for record in swap_history {
            if record.timestamp <= exit_time && record.timestamp > latest_ts {
                latest_ts = record.timestamp;
                best_swap = if is_long { record.swap_long } else { record.swap_short };
            }
        }
        
        total_swap = best_swap * (days as f64);
    }
    
    total_swap
}
