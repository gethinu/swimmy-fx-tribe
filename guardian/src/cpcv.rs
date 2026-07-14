// guardian/src/cpcv.rs
// ============================================================================
// COMBINATORIAL PURGED CROSS-VALIDATION (CPCV)
// ============================================================================
// V47.0: Owner's Vision - Robust backtest validation
//
// CPCV divides data into N blocks, generates multiple train/test combinations,
// and applies Purging + Embargo to prevent information leakage.
// ============================================================================

use crate::backtester::{BacktestResult, Candle};
use rayon::prelude::*;

/// Default number of blocks to divide data into (Expert Panel V48.2)
const DEFAULT_NUM_BLOCKS: usize = 5;
const MAX_NUM_BLOCKS: usize = 10;

/// Purge period in bars (3 months of M1 = ~131,400) - Expert Panel V47.1
const PURGE_BARS: usize = 131_400;

/// Embargo period in bars (1.5 months of M1 = ~65,700) - Expert Panel V47.1
const EMBARGO_BARS: usize = 65_700;

/// Minimum bars required for any CPCV segment
const MIN_RANGE_BARS: usize = 1000;

/// A single block of time-series data
#[derive(Clone)]
pub struct DataBlock {
    pub index: usize,
    pub start_row: usize,
    pub end_row: usize,
}

/// Result from a single CPCV path
#[derive(Clone, Debug)]
pub struct CpcvPathResult {
    pub train_blocks: Vec<usize>,
    pub test_blocks: Vec<usize>,
    pub sharpe: f64,
    pub profit_factor: f64,
    pub win_rate: f64,
    pub max_dd: f64,
    pub trades: i32,
}

/// Aggregate CPCV result (distribution statistics)
#[derive(Clone, Debug, Default)]
pub struct CpcvAggregateResult {
    pub median_sharpe: f64,
    pub median_pf: f64,
    pub median_wr: f64,
    pub median_maxdd: f64,
    pub std_sharpe: f64,  // Stability measure
    pub path_count: usize,
    pub passed_count: usize,  // Paths meeting minimum criteria
}

/// Create N blocks from data of given length (V48.2: num_blocks is now dynamic)
pub fn create_blocks(total_rows: usize, num_blocks: usize) -> Vec<DataBlock> {
    let block_size = total_rows / num_blocks;
    (0..num_blocks).map(|i| DataBlock {
        index: i,
        start_row: i * block_size,
        end_row: if i == num_blocks - 1 { total_rows } else { (i + 1) * block_size },
    }).collect()
}

/// Helper for generating combinations (N choose K)
fn combinations(n: usize, k: usize) -> Vec<Vec<usize>> {
    let mut results = Vec::new();
    if k == 0 || k > n { return results; }
    let mut combination = (0..k).collect::<Vec<usize>>();
    loop {
        results.push(combination.clone());
        let mut i = k;
        while i > 0 && combination[i - 1] == n - k + i - 1 {
            i -= 1;
        }
        if i == 0 { break; }
        combination[i - 1] += 1;
        for j in i..k {
            combination[j] = combination[j - 1] + 1;
        }
    }
    results
}

/// Generate all valid train/test combinations (Generic Combinatorial V48.2)
/// Test: k blocks, Train: remaining blocks
pub fn generate_cpcv_paths(num_blocks: usize, k: usize) -> Vec<(Vec<usize>, Vec<usize>)> {
    let mut paths = Vec::new();
    let all_indices: Vec<usize> = (0..num_blocks).collect();
    let test_combinations = combinations(num_blocks, k);
    
    for test_indices in test_combinations {
        let train_indices: Vec<usize> = all_indices.iter()
            .filter(|i| !test_indices.contains(i))
            .cloned()
            .collect();
        paths.push((train_indices, test_indices));
    }
    
    paths
}

/// Apply purge: Remove training data that's too close to test period
pub fn apply_purge(train_end: usize, test_start: usize, _purge_bars: usize) -> usize {
    // Return adjusted training end position
    if test_start > train_end + PURGE_BARS {
        train_end
    } else if test_start > train_end {
        test_start.saturating_sub(PURGE_BARS)
    } else {
        train_end  // Wrapped case
    }
}

fn ranges_from_blocks(blocks: &[DataBlock], indices: &[usize]) -> Vec<(usize, usize)> {
    let mut ranges: Vec<(usize, usize)> = indices
        .iter()
        .filter_map(|idx| blocks.get(*idx))
        .map(|b| (b.start_row, b.end_row))
        .collect();
    ranges.sort_by_key(|(start, _)| *start);
    merge_ranges(ranges)
}

fn merge_ranges(mut ranges: Vec<(usize, usize)>) -> Vec<(usize, usize)> {
    ranges.sort_by_key(|(start, _)| *start);
    let mut merged: Vec<(usize, usize)> = Vec::new();
    for (start, end) in ranges {
        if start >= end {
            continue;
        }
        if let Some(last) = merged.last_mut() {
            if start <= last.1 {
                if end > last.1 {
                    last.1 = end;
                }
                continue;
            }
        }
        merged.push((start, end));
    }
    merged
}

fn exclude_range(range: (usize, usize), exclude: (usize, usize)) -> Vec<(usize, usize)> {
    let (start, end) = range;
    let (ex_start, ex_end) = exclude;
    if ex_end <= start || ex_start >= end {
        return vec![(start, end)];
    }
    let mut out = Vec::new();
    if ex_start > start {
        out.push((start, ex_start));
    }
    if ex_end < end {
        out.push((ex_end, end));
    }
    out
}

fn apply_purge_embargo_to_ranges(
    train_ranges: Vec<(usize, usize)>,
    test_ranges: &[(usize, usize)],
    purge_bars: usize,
    embargo_bars: usize,
) -> Vec<(usize, usize)> {
    let mut current = train_ranges;
    for (test_start, test_end) in test_ranges {
        let exclude_start = test_start.saturating_sub(purge_bars);
        let exclude_end = test_end.saturating_add(embargo_bars);
        let exclude = (exclude_start, exclude_end);
        let mut next = Vec::new();
        for range in current {
            next.extend(exclude_range(range, exclude));
        }
        current = next;
    }
    merge_ranges(current)
}

fn combine_results(results: &[BacktestResult]) -> BacktestResult {
    let mut total_trades: i32 = 0;
    let mut total_wins: i32 = 0;
    let mut total_losses: i32 = 0;
    let mut total_pnl: f64 = 0.0;
    let mut weighted_sharpe: f64 = 0.0;
    let mut weighted_sortino: f64 = 0.0;
    let mut weighted_max_dd: f64 = 0.0;
    let mut weighted_pf: f64 = 0.0;
    let mut weighted_adj_sharpe: f64 = 0.0;
    let mut weighted_ci_lower: f64 = 0.0;

    for r in results {
        let w = r.trades.max(1) as f64;
        total_trades += r.trades;
        total_wins += r.wins;
        total_losses += r.losses;
        total_pnl += r.pnl;
        weighted_sharpe += r.sharpe * w;
        weighted_sortino += r.sortino * w;
        weighted_max_dd += r.max_drawdown * w;
        weighted_pf += r.profit_factor * w;
        weighted_adj_sharpe += r.adjusted_sharpe * w;
        weighted_ci_lower += r.sharpe_ci_lower * w;
    }

    let weight = total_trades.max(1) as f64;
    let win_rate = if total_trades > 0 {
        total_wins as f64 / total_trades as f64
    } else {
        0.0
    };

    BacktestResult {
        strategy_name: results
            .first()
            .map(|r| r.strategy_name.clone())
            .unwrap_or_else(|| "cpcv_strat".to_string()),
        trades: total_trades,
        wins: total_wins,
        losses: total_losses,
        pnl: total_pnl,
        sharpe: weighted_sharpe / weight,
        sortino: weighted_sortino / weight,
        max_drawdown: weighted_max_dd / weight,
        win_rate,
        profit_factor: weighted_pf / weight,
        adjusted_sharpe: weighted_adj_sharpe / weight,
        sharpe_ci_lower: weighted_ci_lower / weight,
        trade_list: None,
    }
}

/// Run CPCV validation on a strategy
/// Returns aggregate statistics across all paths
pub fn run_cpcv_validation(
    strategy_name: &str,
    candles_path: &str,
    strategy_params: &serde_json::Value,
) -> Result<CpcvAggregateResult, String> {
    let all_candles = crate::backtester::load_candles_from_csv(candles_path)
        .map_err(|e| format!("Failed to load candles: {}", e))?;
    run_cpcv_validation_with_loaded_candles(strategy_name, &all_candles, strategy_params)
}

fn run_cpcv_validation_with_loaded_candles(
    strategy_name: &str,
    all_candles: &[Candle],
    strategy_params: &serde_json::Value,
) -> Result<CpcvAggregateResult, String> {
    let total_rows = all_candles.len();
    
    // V48.2: Dynamic Block count based on data length (Expert Panel mandate)
    let num_blocks = if total_rows > 2_000_000 { 10 }
                    else if total_rows > 1_000_000 { 8 }
                    else if total_rows > 500_000 { 6 }
                    else { DEFAULT_NUM_BLOCKS };
                    
    if total_rows < num_blocks * 50_000 {
        return Err(format!("Insufficient data for {} blocks: {} rows", num_blocks, total_rows));
    }
    
    let blocks = create_blocks(total_rows, num_blocks);
    // k = 2 for combinations (López de Prado: small k is fine for n=5..10)
    let paths = generate_cpcv_paths(num_blocks, 2);
    
    println!("[CPCV] Data length: {} rows -> Using {} blocks ({} paths) for {}", 
             total_rows, num_blocks, paths.len(), strategy_name);
    
    // Run backtests in parallel for each path
    let results: Vec<CpcvPathResult> = paths.par_iter()
        .filter_map(|(train_idx, test_idx)| {
            let test_ranges = ranges_from_blocks(&blocks, test_idx);
            if test_ranges.is_empty() {
                eprintln!("[CPCV] Path {:?}/{:?} skipped: empty test ranges", train_idx, test_idx);
                return None;
            }

            let train_ranges = ranges_from_blocks(&blocks, train_idx);
            let purged_train = apply_purge_embargo_to_ranges(
                train_ranges,
                &test_ranges,
                PURGE_BARS,
                EMBARGO_BARS,
            );
            let train_bars: usize = purged_train
                .iter()
                .map(|(start, end)| end.saturating_sub(*start))
                .sum();
            if train_bars < MIN_RANGE_BARS {
                eprintln!(
                    "[CPCV] Path {:?}/{:?} skipped: insufficient train bars after purge/embargo ({})",
                    train_idx,
                    test_idx,
                    train_bars
                );
                return None;
            }

            let mut test_results = Vec::new();
                for (start, end) in &test_ranges {
                    let bars = end.saturating_sub(*start);
                    if bars < MIN_RANGE_BARS {
                    eprintln!(
                        "[CPCV] Path {:?}/{:?} skipped: test range too small ({} bars)",
                        train_idx,
                        test_idx,
                        bars
                    );
                    return None;
                    }
                match run_backtest_range_from_loaded_candles(all_candles, strategy_params, *start, *end) {
                    Ok(bt) => test_results.push(bt),
                    Err(e) => {
                        eprintln!("[CPCV] Path {:?}/{:?} failed: {}", train_idx, test_idx, e);
                        return None;
                    }
                }
            }

            if test_results.is_empty() {
                return None;
            }

            let bt = combine_results(&test_results);
            Some(CpcvPathResult {
                train_blocks: train_idx.clone(),
                test_blocks: test_idx.clone(),
                sharpe: bt.sharpe,
                profit_factor: bt.profit_factor,
                win_rate: bt.win_rate,
                max_dd: bt.max_drawdown,
                trades: bt.trades,
            })
        })
        .collect();
    
    if results.is_empty() {
        return Err("All CPCV paths failed".to_string());
    }
    
    Ok(calculate_aggregate(&results))
}

fn run_backtest_range(
    candles_path: &str,
    strategy_params: &serde_json::Value,
    start_row: usize,
    end_row: usize,
) -> Result<BacktestResult, String> {
    let all_candles = crate::backtester::load_candles_from_csv(candles_path)
        .map_err(|e| format!("Failed to load candles: {}", e))?;
    run_backtest_range_from_loaded_candles(&all_candles, strategy_params, start_row, end_row)
}

fn run_backtest_range_from_loaded_candles(
    all_candles: &[Candle],
    strategy_params: &serde_json::Value,
    start_row: usize,
    end_row: usize,
) -> Result<BacktestResult, String> {
    // V47.1: Implement range-based backtest
    use crate::backtester::{run_backtest, resample_candles, Strategy, IndicatorType};

    // Slice to range
    let end_clamped = end_row.min(all_candles.len());
    let start_clamped = start_row.min(end_clamped);
    
    if end_clamped - start_clamped < 1000 {
        return Err(format!("Insufficient data in range: {} bars", end_clamped - start_clamped));
    }
    
    let range_slice = &all_candles[start_clamped..end_clamped];

    let indicator_type = match strategy_params
        .get("indicator_type")
        .and_then(|v| v.as_str())
        .unwrap_or("sma")
        .to_ascii_lowercase()
        .as_str()
    {
        "sma" => IndicatorType::Sma,
        "ema" => IndicatorType::Ema,
        "rsi" => IndicatorType::Rsi,
        "macd" => IndicatorType::Macd,
        "bb" | "bollinger" => IndicatorType::Bb,
        "stoch" | "stochastic" => IndicatorType::Stoch,
        "vwap" => IndicatorType::Vwap,
        "volsma" | "vol_sma" | "volume_sma" => IndicatorType::Volsma,
        "vpoc" | "volume_profile" | "volume-profile" => IndicatorType::Vpoc,
        "vwapvr" | "vwap_volume_ratio" | "vwap-volume-ratio" => IndicatorType::Vwapvr,
        _ => IndicatorType::Sma,
    };

    let filter_enabled = match strategy_params.get("filter_enabled") {
        Some(serde_json::Value::Bool(b)) => *b,
        Some(serde_json::Value::Number(n)) => n.as_i64().unwrap_or(0) != 0,
        Some(serde_json::Value::String(s)) => {
            matches!(s.to_ascii_lowercase().as_str(), "t" | "true" | "1" | "yes" | "y")
        }
        _ => false,
    };

    let filter_tf = strategy_params
        .get("filter_tf")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    let filter_period = strategy_params
        .get("filter_period")
        .and_then(|v| v.as_u64())
        .unwrap_or(0) as usize;

    let filter_logic = strategy_params
        .get("filter_logic")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    let timeframe_min = strategy_params
        .get("timeframe")
        .and_then(|v| v.as_i64())
        .unwrap_or(1);

    // Align CPCV evaluation with normal BACKTEST behavior: optional resampling to strategy timeframe.
    // If the strategy uses MTF filter, derive aux candles from the same M1 slice via resampling.
    let mut aux: std::collections::HashMap<String, Vec<crate::backtester::Candle>> =
        std::collections::HashMap::new();
    if filter_enabled && !filter_tf.trim().is_empty() {
        let tf = filter_tf.trim().to_ascii_uppercase();
        let tf_seconds = crate::backtester::tf_to_seconds(&tf);
        aux.insert(tf.clone(), resample_candles(range_slice, tf_seconds));
    }
    
    // Parse strategy from JSON
    let strategy = Strategy {
        name: strategy_params.get("name").and_then(|v| v.as_str()).unwrap_or("cpcv_strat").to_string(),
        sma_short: strategy_params.get("sma_short").and_then(|v| v.as_u64()).unwrap_or(10) as usize,
        sma_long: strategy_params.get("sma_long").and_then(|v| v.as_u64()).unwrap_or(50) as usize,
        sl: strategy_params.get("sl").and_then(|v| v.as_f64()).unwrap_or(50.0),
        tp: strategy_params.get("tp").and_then(|v| v.as_f64()).unwrap_or(100.0),
        volume: strategy_params.get("volume").and_then(|v| v.as_f64()).unwrap_or(0.01),
        indicator_type,
        filter_enabled,
        filter_tf,
        filter_period,
        filter_logic,
        // Phase 23: AST deserialization
        entry_long_ast: strategy_params.get("entry_long_ast").and_then(|v| serde_json::from_value(v.clone()).ok()),
        entry_short_ast: strategy_params.get("entry_short_ast").and_then(|v| serde_json::from_value(v.clone()).ok()),
        exit_long_ast: strategy_params.get("exit_long_ast").and_then(|v| serde_json::from_value(v.clone()).ok()),
        exit_short_ast: strategy_params.get("exit_short_ast").and_then(|v| serde_json::from_value(v.clone()).ok()),
            band_mult: 2.0,
            atr_period: 14,
            atr_barrier_sl: 0.0,
            atr_barrier_tp: 0.0,
    };
    
    // Run backtest on the range
    let result = if timeframe_min > 1 {
        let working_candles = resample_candles(range_slice, timeframe_min.saturating_mul(60));
        run_backtest(&strategy, &working_candles, &aux, &[])
    } else {
        run_backtest(&strategy, range_slice, &aux, &[])
    };
    
    Ok(result)
}

fn calculate_aggregate(results: &[CpcvPathResult]) -> CpcvAggregateResult {
    let n = results.len();
    if n == 0 {
        return CpcvAggregateResult::default();
    }
    
    let mut sharpes: Vec<f64> = results.iter().map(|r| r.sharpe).collect();
    let mut pfs: Vec<f64> = results.iter().map(|r| r.profit_factor).collect();
    let mut wrs: Vec<f64> = results.iter().map(|r| r.win_rate).collect();
    let mut dds: Vec<f64> = results.iter().map(|r| r.max_dd).collect();
    
    // V50.2: Fix panic sort_by (Total Ordering for NaN)
    let float_cmp = |a: &f64, b: &f64| {
        if a.is_nan() {
            std::cmp::Ordering::Greater // Move NaNs to end
        } else if b.is_nan() {
            std::cmp::Ordering::Less
        } else {
            a.partial_cmp(b).unwrap()
        }
    };

    sharpes.sort_by(float_cmp);
    pfs.sort_by(float_cmp);
    wrs.sort_by(float_cmp);
    dds.sort_by(float_cmp);
    
    let median = |v: &[f64]| -> f64 {
        let mid = v.len() / 2;
        if v.len() % 2 == 0 {
            (v[mid - 1] + v[mid]) / 2.0
        } else {
            v[mid]
        }
    };
    
    let mean_sharpe = sharpes.iter().sum::<f64>() / n as f64;
    let variance = sharpes.iter().map(|s| (s - mean_sharpe).powi(2)).sum::<f64>() / n as f64;
    
    // Count paths passing minimum B-rank criteria
    let passed = results.iter().filter(|r| {
        r.sharpe >= 0.1 && r.profit_factor >= 1.0 && r.win_rate >= 0.30 && r.max_dd < 0.30
    }).count();
    
    CpcvAggregateResult {
        median_sharpe: median(&sharpes),
        median_pf: median(&pfs),
        median_wr: median(&wrs),
        median_maxdd: median(&dds),
        std_sharpe: variance.sqrt(),
        path_count: n,
        passed_count: passed,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::time::{SystemTime, UNIX_EPOCH};
    
    #[test]
    fn test_block_creation() {
        let blocks = create_blocks(1000000, 5);
        assert_eq!(blocks.len(), 5);
        assert_eq!(blocks[0].start_row, 0);
        assert_eq!(blocks[4].end_row, 1000000);
    }
    
    #[test]
    fn test_path_generation() {
        let paths = generate_cpcv_paths(5, 2);
        assert_eq!(paths.len(), 10); // 5C2 = 10
    }

    #[test]
    fn test_run_backtest_range_respects_indicator_type() {
        // Regression: CPCV range backtest must respect `strategy_params.indicator_type`.
        // If hard-coded to SMA, setting `sma_long` huge makes min_bars too large -> no trades.
        let mut path = std::env::temp_dir();
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time went backwards")
            .as_nanos();
        path.push(format!("swimmy-ut-cpcv-indicator-{}.csv", nanos));

        let mut f = std::fs::File::create(&path).expect("create temp csv");
        writeln!(f, "timestamp,open,high,low,close,volume").unwrap();

        // 1200 bars: 600 downtrend then 600 uptrend to trigger RSI oversold-bounce.
        let start = 1000.0_f64;
        for i in 0..600 {
            let close = start - i as f64;
            writeln!(f, "{},{},{},{},{},{}", (i as i64) * 60, close, close, close, close, 1.0).unwrap();
        }
        let bottom = start - 599.0;
        for j in 0..600 {
            let i = 600 + j;
            let close = bottom + j as f64;
            writeln!(f, "{},{},{},{},{},{}", (i as i64) * 60, close, close, close, close, 1.0).unwrap();
        }
        drop(f);

        let params = serde_json::json!({
            "name": "UT-CPCV-IND",
            "sma_short": 7,
            "sma_long": 5000,
            "sl": 1.0,
            "tp": 1.0,
            "volume": 0.01,
            "indicator_type": "rsi"
        });

        let bt = run_backtest_range(path.to_str().unwrap(), &params, 0, 1200)
            .expect("backtest range should succeed");
        let _ = std::fs::remove_file(&path);

        assert!(bt.trades > 0, "expected RSI strategy to close trades; got {}", bt.trades);
    }

    #[test]
    fn test_run_backtest_range_from_loaded_candles_respects_indicator_type() {
        let mut path = std::env::temp_dir();
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time went backwards")
            .as_nanos();
        path.push(format!("swimmy-ut-cpcv-preloaded-{}.csv", nanos));

        let mut f = std::fs::File::create(&path).expect("create temp csv");
        writeln!(f, "timestamp,open,high,low,close,volume").unwrap();

        let start = 1000.0_f64;
        for i in 0..600 {
            let close = start - i as f64;
            writeln!(f, "{},{},{},{},{},{}", (i as i64) * 60, close, close, close, close, 1.0).unwrap();
        }
        let bottom = start - 599.0;
        for j in 0..600 {
            let i = 600 + j;
            let close = bottom + j as f64;
            writeln!(f, "{},{},{},{},{},{}", (i as i64) * 60, close, close, close, close, 1.0).unwrap();
        }
        drop(f);

        let params = serde_json::json!({
            "name": "UT-CPCV-IND-PRELOADED",
            "sma_short": 7,
            "sma_long": 5000,
            "sl": 1.0,
            "tp": 1.0,
            "volume": 0.01,
            "indicator_type": "rsi"
        });

        let candles = crate::backtester::load_candles_from_csv(path.to_str().unwrap())
            .expect("load candles from csv");
        let bt = run_backtest_range_from_loaded_candles(&candles, &params, 0, 1200)
            .expect("preloaded range backtest should succeed");
        let _ = std::fs::remove_file(&path);

        assert!(bt.trades > 0, "expected RSI strategy to close trades; got {}", bt.trades);
    }

    #[test]
    fn test_apply_purge_embargo_trims_train_ranges() {
        let blocks = create_blocks(1000, 5);
        let train_idx = vec![0, 1, 3, 4];
        let test_idx = vec![2];
        let train_ranges = ranges_from_blocks(&blocks, &train_idx);
        let test_ranges = ranges_from_blocks(&blocks, &test_idx);

        let purged = apply_purge_embargo_to_ranges(train_ranges, &test_ranges, 50, 30);

        assert_eq!(purged, vec![(0, 350), (630, 1000)]);
    }

    #[test]
    fn test_apply_purge_embargo_can_empty_train_ranges() {
        let blocks = create_blocks(300, 3);
        let train_idx = vec![0, 2];
        let test_idx = vec![1];
        let train_ranges = ranges_from_blocks(&blocks, &train_idx);
        let test_ranges = ranges_from_blocks(&blocks, &test_idx);

        let purged = apply_purge_embargo_to_ranges(train_ranges, &test_ranges, 150, 150);

        assert!(purged.is_empty());
    }
}
