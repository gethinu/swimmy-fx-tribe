// mcts.rs - Monte Carlo Tree Search for Strategy Parameter Optimization
// AlphaZero-style parameter space exploration

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::backtester::{Candle, Strategy, run_backtest};

/// Node in the MCTS tree
#[derive(Debug, Clone)]
pub struct MctsNode {
    pub params: StrategyParams,
    pub visits: u32,
    pub total_reward: f64,
    pub children: Vec<MctsNode>,
    pub untried_actions: Vec<Action>,
}

/// Strategy parameters to optimize
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct StrategyParams {
    pub sma_short: usize,
    pub sma_long: usize,
    pub sl: f64,
    pub tp: f64,
}

/// Actions to modify parameters
#[derive(Debug, Clone)]
pub enum Action {
    IncreaseSmaShort(i32),
    DecreaseSmaShort(i32),
    IncreaseSmaLong(i32),
    DecreaseSmaLong(i32),
    IncreaseSL(f64),
    DecreaseSL(f64),
    IncreaseTP(f64),
    DecreaseTP(f64),
}

impl StrategyParams {
    pub fn apply_action(&self, action: &Action) -> Self {
        let mut new = self.clone();
        match action {
            Action::IncreaseSmaShort(d) => new.sma_short = ((new.sma_short as i32 + d).max(3) as usize).min(50),
            Action::DecreaseSmaShort(d) => new.sma_short = ((new.sma_short as i32 - d).max(3) as usize).min(50),
            Action::IncreaseSmaLong(d) => new.sma_long = ((new.sma_long as i32 + d).max(10) as usize).min(200),
            Action::DecreaseSmaLong(d) => new.sma_long = ((new.sma_long as i32 - d).max(10) as usize).min(200),
            Action::IncreaseSL(d) => new.sl = (new.sl + d).max(0.05).min(0.50),
            Action::DecreaseSL(d) => new.sl = (new.sl - d).max(0.05).min(0.50),
            Action::IncreaseTP(d) => new.tp = (new.tp + d).max(0.10).min(1.00),
            Action::DecreaseTP(d) => new.tp = (new.tp - d).max(0.10).min(1.00),
        }
        // Ensure sma_short < sma_long
        if new.sma_short >= new.sma_long {
            new.sma_long = new.sma_short + 10;
        }
        new
    }
    
    pub fn to_strategy(&self, name: &str) -> Strategy {
        Strategy {
            name: name.to_string(),
            sma_short: self.sma_short,
            sma_long: self.sma_long,
            sl: self.sl,
            tp: self.tp,
            volume: 0.01,
            indicator_type: Default::default(),  // V6.11
            filter_enabled: false,
            filter_tf: String::new(),
            filter_period: 0,
            filter_logic: String::new(),
        }
    }
}

/// Composite reward function (multi-objective)
#[derive(Debug, Clone, Serialize)]
pub struct CompositeScore {
    pub sharpe: f64,
    pub win_rate: f64,
    pub profit_factor: f64,
    pub max_drawdown: f64,
    pub total_trades: u32,
    pub composite: f64,  // Final combined score
}

impl CompositeScore {
    pub fn calculate(sharpe: f64, win_rate: f64, profit_factor: f64, max_dd: f64, trades: u32) -> Self {
        // Multi-objective optimization weights
        let w_sharpe = 0.35;
        let w_winrate = 0.20;
        let w_pf = 0.20;
        let w_dd = 0.15;
        let w_trades = 0.10;
        
        // Normalize components
        let sharpe_norm = (sharpe / 3.0).max(-1.0).min(1.0);  // Target Sharpe ~3
        let winrate_norm = (win_rate - 0.4) / 0.3;  // Target 40-70%
        let pf_norm = ((profit_factor - 1.0) / 2.0).max(-1.0).min(1.0);  // Target PF ~3
        let dd_norm = 1.0 - (max_dd / 20.0).min(1.0);  // Penalty for DD > 20%
        let trades_norm = ((trades as f64 - 10.0) / 50.0).max(0.0).min(1.0);  // Want 10-60 trades
        
        let composite = w_sharpe * sharpe_norm
            + w_winrate * winrate_norm
            + w_pf * pf_norm
            + w_dd * dd_norm
            + w_trades * trades_norm;
        
        CompositeScore {
            sharpe,
            win_rate,
            profit_factor,
            max_drawdown: max_dd,
            total_trades: trades,
            composite,
        }
    }
}

impl MctsNode {
    pub fn new(params: StrategyParams) -> Self {
        let actions = vec![
            Action::IncreaseSmaShort(2),
            Action::DecreaseSmaShort(2),
            Action::IncreaseSmaLong(5),
            Action::DecreaseSmaLong(5),
            Action::IncreaseSL(0.02),
            Action::DecreaseSL(0.02),
            Action::IncreaseTP(0.05),
            Action::DecreaseTP(0.05),
        ];
        
        MctsNode {
            params,
            visits: 0,
            total_reward: 0.0,
            children: Vec::new(),
            untried_actions: actions,
        }
    }
    
    /// UCB1 selection for child with best exploration-exploitation tradeoff
    pub fn ucb1_select(&self, exploration: f64) -> Option<usize> {
        if self.children.is_empty() {
            return None;
        }
        
        let parent_visits = self.visits as f64;
        let mut best_idx = 0;
        let mut best_ucb = f64::NEG_INFINITY;
        
        for (i, child) in self.children.iter().enumerate() {
            let exploitation = if child.visits > 0 {
                child.total_reward / child.visits as f64
            } else {
                0.0
            };
            
            let exploration_term = if child.visits > 0 {
                exploration * (parent_visits.ln() / child.visits as f64).sqrt()
            } else {
                f64::INFINITY  // Prioritize unvisited
            };
            
            let ucb = exploitation + exploration_term;
            if ucb > best_ucb {
                best_ucb = ucb;
                best_idx = i;
            }
        }
        
        Some(best_idx)
    }
    
    /// Expand node by trying an untried action
    pub fn expand(&mut self) -> Option<usize> {
        if self.untried_actions.is_empty() {
            return None;
        }
        
        let mut rng = rand::thread_rng();
        let idx = rng.gen_range(0..self.untried_actions.len());
        let action = self.untried_actions.remove(idx);
        
        let new_params = self.params.apply_action(&action);
        let child = MctsNode::new(new_params);
        self.children.push(child);
        
        Some(self.children.len() - 1)
    }
    
    /// Check if fully expanded
    pub fn is_fully_expanded(&self) -> bool {
        self.untried_actions.is_empty()
    }
    
    /// Check if terminal (for now, never terminal - could add depth limit)
    #[allow(dead_code)]
    pub fn is_terminal(&self) -> bool {
        false
    }
}

/// MCTS Search
pub struct MctsSearch {
    pub root: MctsNode,
    pub exploration_constant: f64,
    pub iterations: u32,
}

impl MctsSearch {
    pub fn new(initial_params: StrategyParams) -> Self {
        MctsSearch {
            root: MctsNode::new(initial_params),
            exploration_constant: 1.414,  // sqrt(2)
            iterations: 0,
        }
    }
    
    /// Run simulation (rollout) for a node
    #[allow(dead_code)]
    fn simulate(&self, params: &StrategyParams, candles: &[Candle]) -> f64 {
        let strat = params.to_strategy("MCTS-Sim");
        let result = run_backtest(&strat, candles, &HashMap::new());
        
        // Calculate composite score
        let score = CompositeScore::calculate(
            result.sharpe,
            result.win_rate,
            1.0 + result.sharpe.max(0.0),  // Proxy for profit_factor
            result.max_drawdown,
            result.trades as u32,
        );
        
        score.composite
    }
    
    /// Backpropagate reward up the tree
    #[allow(dead_code)]
    fn backpropagate(_path: &mut Vec<usize>, nodes: &mut MctsNode, reward: f64) {
        // Simple version: update root
        nodes.visits += 1;
        nodes.total_reward += reward;
        
        // Also update along path if we stored it properly
        // (simplified implementation)
    }
    
    /// Run one MCTS iteration
    fn run_iteration(&mut self, candles: &[Candle]) {
        let exploration = self.exploration_constant;
        
        // Get info from root without keeping borrow
        let fully_expanded = self.root.is_fully_expanded();
        
        if !fully_expanded {
            // Expand first
            if let Some(child_idx) = self.root.expand() {
                // Clone params before simulate
                let child_params = self.root.children[child_idx].params.clone();
                
                // Simulate (now self is not borrowed mutably)
                let strat = child_params.to_strategy("MCTS-Sim");
                let result = run_backtest(&strat, candles, &HashMap::new());
                let score = CompositeScore::calculate(
                    result.sharpe, result.win_rate, 1.0 + result.sharpe.max(0.0),
                    result.max_drawdown, result.trades as u32
                );
                let reward = score.composite;
                
                // Update
                self.root.children[child_idx].visits += 1;
                self.root.children[child_idx].total_reward += reward;
                self.root.visits += 1;
                self.root.total_reward += reward;
            }
        } else {
            // Select best child
            if let Some(child_idx) = self.root.ucb1_select(exploration) {
                let child_params = self.root.children[child_idx].params.clone();
                
                // Simulate
                let strat = child_params.to_strategy("MCTS-Sim");
                let result = run_backtest(&strat, candles, &HashMap::new());
                let score = CompositeScore::calculate(
                    result.sharpe, result.win_rate, 1.0 + result.sharpe.max(0.0),
                    result.max_drawdown, result.trades as u32
                );
                let reward = score.composite;
                
                // Update
                self.root.children[child_idx].visits += 1;
                self.root.children[child_idx].total_reward += reward;
                self.root.visits += 1;
                self.root.total_reward += reward;
            }
        }
        
        self.iterations += 1;
    }
    
    /// Run MCTS for specified iterations
    pub fn search(&mut self, candles: &[Candle], iterations: u32) -> (StrategyParams, f64) {
        println!("🔍 MCTS: Starting {} iterations...", iterations);
        
        for i in 0..iterations {
            self.run_iteration(candles);
            
            if (i + 1) % 50 == 0 {
                println!("  {} iterations complete", i + 1);
            }
        }
        
        // Find best child
        let best = self.root.children.iter()
            .max_by(|a, b| {
                let score_a = if a.visits > 0 { a.total_reward / a.visits as f64 } else { f64::NEG_INFINITY };
                let score_b = if b.visits > 0 { b.total_reward / b.visits as f64 } else { f64::NEG_INFINITY };
                score_a.partial_cmp(&score_b).unwrap()
            });
        
        if let Some(best_node) = best {
            let avg_reward = best_node.total_reward / best_node.visits.max(1) as f64;
            println!("🏆 MCTS Best: {:?} (score: {:.4}, visits: {})", 
                     best_node.params, avg_reward, best_node.visits);
            (best_node.params.clone(), avg_reward)
        } else {
            (self.root.params.clone(), 0.0)
        }
    }
}

/// Convenience function to optimize a strategy
pub fn optimize_strategy(
    initial: StrategyParams,
    candles: &[Candle],
    iterations: u32,
) -> (StrategyParams, CompositeScore) {
    let mut mcts = MctsSearch::new(initial.clone());
    let (best_params, _) = mcts.search(candles, iterations);
    
    // Get final composite score
    let strat = best_params.to_strategy("MCTS-Optimized");
    let result = run_backtest(&strat, candles, &HashMap::new());
    let score = CompositeScore::calculate(
        result.sharpe,
        result.win_rate,
        1.0 + result.sharpe.max(0.0),  // Proxy for profit_factor
        result.max_drawdown,
        result.trades as u32,
    );
    
    (best_params, score)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_composite_score() {
        let score = CompositeScore::calculate(2.0, 0.55, 1.5, 10.0, 30);
        assert!(score.composite > 0.0);
        assert!(score.composite < 1.0);
    }
    
    #[test]
    fn test_params_apply_action() {
        let params = StrategyParams { sma_short: 10, sma_long: 50, sl: 0.15, tp: 0.30 };
        let new_params = params.apply_action(&Action::IncreaseSmaShort(2));
        assert_eq!(new_params.sma_short, 12);
    }
}
