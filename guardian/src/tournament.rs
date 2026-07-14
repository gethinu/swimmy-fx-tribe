// tournament.rs - Strategy Tournament & Parallel Evolution
// AlphaZero-style self-play for trading strategies

use rayon::prelude::*;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::backtester::{Candle, BacktestResult, IndicatorType};  // V6.12: Unified IndicatorType

// V6.12: IndicatorType impl moved to backtester.rs

/// Strategy with Elo rating for tournament ranking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RatedStrategy {
    pub name: String,
    pub indicator_type: IndicatorType,  // V4.0: Added
    pub param_short: usize,    // Was param_short, now generic
    pub param_long: usize,     // Was param_long, now generic
    pub sl: f64,
    pub tp: f64,
    pub elo: f64,
    pub matches: u32,
    pub wins: u32,
}

impl RatedStrategy {
    pub fn new(name: String, indicator_type: IndicatorType, param_short: usize, param_long: usize, sl: f64, tp: f64) -> Self {
        RatedStrategy {
            name,
            indicator_type,
            param_short,
            param_long,
            sl,
            tp,
            elo: 1000.0,  // Starting Elo
            matches: 0,
            wins: 0,
        }
    }
    
    // V4.0: Create random diverse strategy
    // V6.12: Updated to use lowercase variant names from backtester::IndicatorType
    pub fn random_new(_id: usize) -> Self {
        let mut rng = rand::thread_rng();
        let ind_type = IndicatorType::random();
        let (short, long) = match ind_type {
            IndicatorType::Sma | IndicatorType::Ema => {
                let s = rng.gen_range(5..30);
                let l = rng.gen_range(s + 10..200);
                (s, l)
            }
            IndicatorType::Rsi => (14, rng.gen_range(30..70)),  // period, threshold
            IndicatorType::Bb => (20, rng.gen_range(15..25)),   // period, deviation*10
            IndicatorType::Macd => (12, 26),                     // fast, slow
            IndicatorType::Stoch => (14, rng.gen_range(20..80)), // period, threshold
            IndicatorType::Vwap => (rng.gen_range(10..80), 0),   // period, reserved
            IndicatorType::Volsma => (rng.gen_range(5..40), rng.gen_range(110..220)), // period, spike%
            IndicatorType::Vpoc => (rng.gen_range(20..120), rng.gen_range(8..24)),     // lookback, bins
            IndicatorType::Vwapvr => (rng.gen_range(5..40), rng.gen_range(110..220)),  // period, orange threshold%
            // Unreachable: IndicatorType::random() never yields Keltner, so live breeding
            // cannot produce it. Arm exists only for match exhaustiveness.
            IndicatorType::Keltner => (rng.gen_range(20..60), 20),                      // EMA period, reserved
        };
        let name = format!("{}-{}-{}", ind_type.name(), short, long);
        RatedStrategy::new(name, ind_type, short, long, rng.gen_range(0.1..0.4), rng.gen_range(0.2..0.8))
    }
}

/// Tournament result
#[derive(Debug, Serialize)]
pub struct TournamentResult {
    pub total_matches: u32,
    pub rankings: Vec<(String, f64, u32)>,  // (name, elo, wins)
}

/// Parallel backtest multiple strategies
#[allow(dead_code)]
pub fn parallel_backtest(
    strategies: &[RatedStrategy],
    candles: &[Candle],
) -> Vec<(String, BacktestResult)> {
    strategies
        .par_iter()  // Parallel iterator!
        .map(|strat| {
            let result = crate::backtester::run_backtest(
                &crate::backtester::Strategy {
                    name: strat.name.clone(),
                    sma_short: strat.param_short,
                    sma_long: strat.param_long,
                    sl: strat.sl,
                    tp: strat.tp,
                    volume: 0.01,
                    indicator_type: Default::default(),  // V6.11
                    filter_enabled: false,
                    filter_tf: String::new(),
                    filter_period: 0,
                    filter_logic: String::new(),
                    entry_long_ast: None,
                    entry_short_ast: None,
                    exit_long_ast: None,
                    exit_short_ast: None,
            band_mult: 2.0,
            atr_period: 14,
            atr_barrier_sl: 0.0,
            atr_barrier_tp: 0.0,
                },
                candles,
                &HashMap::new(),
                &[],
            );
            (strat.name.clone(), result)
        })
        .collect()
}

/// Run a match between two strategies
fn run_match(
    strat_a: &RatedStrategy,
    strat_b: &RatedStrategy,
    candles: &[Candle],
) -> (f64, f64) {  // (sharpe_a, sharpe_b)
    let result_a = crate::backtester::run_backtest(
        &crate::backtester::Strategy {
            name: strat_a.name.clone(),
            sma_short: strat_a.param_short,
            sma_long: strat_a.param_long,
            sl: strat_a.sl,
            tp: strat_a.tp,
            volume: 0.01,
            indicator_type: Default::default(),  // V6.11
            filter_enabled: false,
            filter_tf: String::new(),
            filter_period: 0,
            filter_logic: String::new(),
            entry_long_ast: None,
            entry_short_ast: None,
            exit_long_ast: None,
            exit_short_ast: None,
            band_mult: 2.0,
            atr_period: 14,
            atr_barrier_sl: 0.0,
            atr_barrier_tp: 0.0,
        },
        candles,
        &HashMap::new(),
        &[],
    );
    let result_b = crate::backtester::run_backtest(
        &crate::backtester::Strategy {
            name: strat_b.name.clone(),
            sma_short: strat_b.param_short,
            sma_long: strat_b.param_long,
            sl: strat_b.sl,
            tp: strat_b.tp,
            volume: 0.01,
            indicator_type: Default::default(),  // V6.11
            filter_enabled: false,
            filter_tf: String::new(),
            filter_period: 0,
            filter_logic: String::new(),
            entry_long_ast: None,
            entry_short_ast: None,
            exit_long_ast: None,
            exit_short_ast: None,
            band_mult: 2.0,
            atr_period: 14,
            atr_barrier_sl: 0.0,
            atr_barrier_tp: 0.0,
        },
        candles,
        &HashMap::new(),
        &[],
    );
    (result_a.sharpe, result_b.sharpe)
}

/// Update Elo ratings after a match
fn update_elo(winner_elo: f64, loser_elo: f64, k: f64) -> (f64, f64) {
    let expected_winner = 1.0 / (1.0 + 10.0_f64.powf((loser_elo - winner_elo) / 400.0));
    let expected_loser = 1.0 - expected_winner;
    
    let new_winner_elo = winner_elo + k * (1.0 - expected_winner);
    let new_loser_elo = loser_elo + k * (0.0 - expected_loser);
    
    (new_winner_elo, new_loser_elo)
}

/// Run a full tournament with round-robin matches
pub fn run_tournament(
    strategies: &mut [RatedStrategy],
    candles: &[Candle],
    rounds: usize,
) -> TournamentResult {
    let mut rng = rand::thread_rng();
    let n = strategies.len();
    let mut total_matches = 0u32;
    
    println!("🏆 Starting tournament with {} strategies, {} rounds", n, rounds);
    
    for round in 0..rounds {
        // Random pairings for this round
        let mut indices: Vec<usize> = (0..n).collect();
        
        // Fisher-Yates shuffle
        for i in (1..n).rev() {
            let j = rng.gen_range(0..=i);
            indices.swap(i, j);
        }
        
        // Process pairs in parallel
        let matches: Vec<(usize, usize, f64, f64)> = indices
            .chunks(2)
            .filter(|chunk| chunk.len() == 2)
            .collect::<Vec<_>>()
            .par_iter()
            .map(|chunk| {
                let i = chunk[0];
                let j = chunk[1];
                let (sharpe_a, sharpe_b) = run_match(&strategies[i], &strategies[j], candles);
                (i, j, sharpe_a, sharpe_b)
            })
            .collect();
        
        // Update Elo ratings (sequential to avoid race conditions)
        for (i, j, sharpe_a, sharpe_b) in matches {
            total_matches += 1;
            strategies[i].matches += 1;
            strategies[j].matches += 1;
            
            if sharpe_a > sharpe_b {
                strategies[i].wins += 1;
                let (new_a, new_b) = update_elo(strategies[i].elo, strategies[j].elo, 32.0);
                strategies[i].elo = new_a;
                strategies[j].elo = new_b;
            } else if sharpe_b > sharpe_a {
                strategies[j].wins += 1;
                let (new_b, new_a) = update_elo(strategies[j].elo, strategies[i].elo, 32.0);
                strategies[i].elo = new_a;
                strategies[j].elo = new_b;
            }
            // Draw: no Elo change
        }
        
        if (round + 1) % 10 == 0 {
            println!("  Round {}/{} complete", round + 1, rounds);
        }
    }
    
    // Sort by Elo for rankings
    strategies.sort_by(|a, b| b.elo.partial_cmp(&a.elo).unwrap());
    
    let rankings: Vec<(String, f64, u32)> = strategies
        .iter()
        .map(|s| (s.name.clone(), s.elo, s.wins))
        .collect();
    
    println!("🏆 Tournament complete! Top 5:");
    for (i, (name, elo, wins)) in rankings.iter().take(5).enumerate() {
        println!("  {}. {} (Elo: {:.0}, Wins: {})", i + 1, name, elo, wins);
    }
    
    TournamentResult {
        total_matches,
        rankings,
    }
}

/// Generate offspring through crossover
pub fn crossover(parent_a: &RatedStrategy, parent_b: &RatedStrategy) -> RatedStrategy {
    let mut rng = rand::thread_rng();
    
    // Weighted crossover favoring higher Elo parent
    let weight_a = parent_a.elo / (parent_a.elo + parent_b.elo);
    
    // V4.0: Inherit indicator type from higher Elo parent
    let indicator_type = if rng.gen::<f64>() < weight_a {
        parent_a.indicator_type
    } else {
        parent_b.indicator_type
    };
    
    let param_short = if rng.gen::<f64>() < weight_a {
        parent_a.param_short
    } else {
        parent_b.param_short
    };
    
    let param_long = if rng.gen::<f64>() < weight_a {
        parent_a.param_long
    } else {
        parent_b.param_long
    };
    
    // Interpolate SL/TP
    let sl = parent_a.sl * weight_a + parent_b.sl * (1.0 - weight_a);
    let tp = parent_a.tp * weight_a + parent_b.tp * (1.0 - weight_a);
    
    let name = format!("{}-{}-{}", indicator_type.name(), param_short, param_long);
    
    RatedStrategy::new(name, indicator_type, param_short, param_long, sl, tp)
}

/// Mutate a strategy
pub fn mutate(strat: &mut RatedStrategy) {
    let mut rng = rand::thread_rng();
    
    // Mutate SMA periods
    if rng.gen::<f64>() < 0.3 {
        let delta: i32 = rng.gen_range(-5..=5);
        strat.param_short = ((strat.param_short as i32 + delta).max(2) as usize).min(50);
    }
    
    if rng.gen::<f64>() < 0.3 {
        let delta: i32 = rng.gen_range(-10..=10);
        strat.param_long = ((strat.param_long as i32 + delta).max(10) as usize).min(200);
    }
    
    // Ensure ordering only for MA-cross style indicators.
    if matches!(strat.indicator_type, IndicatorType::Sma | IndicatorType::Ema | IndicatorType::Macd)
        && strat.param_short >= strat.param_long
    {
        strat.param_long = strat.param_short + 10;
    }
    
    // Mutate SL/TP
    if rng.gen::<f64>() < 0.2 {
        strat.sl *= 1.0 + rng.gen_range(-0.2..0.2);
        strat.sl = strat.sl.max(0.05).min(0.5);
    }
    
    if rng.gen::<f64>() < 0.2 {
        strat.tp *= 1.0 + rng.gen_range(-0.2..0.2);
        strat.tp = strat.tp.max(0.1).min(1.0);
    }
    
    strat.name = format!("{}-M-{}-{}", strat.indicator_type.name(), strat.param_short, strat.param_long);
}

/// Evolution cycle: Tournament → Selection → Crossover → Mutation
pub fn evolution_cycle(
    population: &mut Vec<RatedStrategy>,
    candles: &[Candle],
    tournament_rounds: usize,
) {
    let pop_size = population.len();
    
    // 1. Run tournament to establish rankings
    run_tournament(population, candles, tournament_rounds);
    
    // 2. Selection: Keep top 25% (P2: Graham's Margin of Safety)
    let survivors = pop_size / 4;
    population.truncate(survivors);
    
    // 3. Crossover: Breed survivors to restore population
    let mut offspring: Vec<RatedStrategy> = Vec::new();
    let mut rng = rand::thread_rng();
    
    while offspring.len() < pop_size - survivors {
        let i = rng.gen_range(0..survivors);
        let j = rng.gen_range(0..survivors);
        if i != j {
            let child = crossover(&population[i], &population[j]);
            offspring.push(child);
        }
    }
    
    // 4. Mutation
    for child in &mut offspring {
        if rng.gen::<f64>() < 0.5 {
            mutate(child);
        }
    }
    
    population.extend(offspring);
    
    println!("🧬 Evolution cycle complete. Population: {}", population.len());
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_elo_update() {
        let (new_winner, new_loser) = update_elo(1000.0, 1000.0, 32.0);
        assert!(new_winner > 1000.0);
        assert!(new_loser < 1000.0);
    }
}
