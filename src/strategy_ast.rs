use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Phase 23: The Foundation Upgrade (Simons/PG)
/// Strategy AST Node
/// This structure replaces the brittle string parsing with a recursive tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StrategyNode {
    // Logic Operators
    And(Box<StrategyNode>, Box<StrategyNode>),
    Or(Box<StrategyNode>, Box<StrategyNode>),
    Not(Box<StrategyNode>),

    // Comparison Operators (Signal Generation)
    Gt(Box<StrategyNode>, Box<StrategyNode>),
    Lt(Box<StrategyNode>, Box<StrategyNode>),
    Cross(Box<StrategyNode>, Box<StrategyNode>),     // Generic Cross
    CrossUp(Box<StrategyNode>, Box<StrategyNode>),   // LHS crosses RHS from below
    CrossDown(Box<StrategyNode>, Box<StrategyNode>), // LHS crosses RHS from above

    // Value Nodes (Indicators & Primitives)
    Constant(f64),
    PriceClose,
    PriceOpen,
    PriceHigh,
    PriceLow,
    Volume,

    // Indicators (Parameterized)
    Sma { period: usize },
    Ema { period: usize },
    Rsi { period: usize },
    Atr { period: usize },
    Macd { fast: usize, slow: usize, signal: usize },
    BollingerUpper { period: usize, std_dev: f64 },
    BollingerLower { period: usize, std_dev: f64 },
}

/// Evaluation Context
/// Passed to the evaluate function to provide market data.
pub struct EvalContext<'a> {
    pub closes: &'a [f64],
    pub opens: &'a [f64],
    pub highs: &'a [f64],
    pub lows: &'a [f64],
    pub volumes: &'a [f64],
    pub index: usize, // Current candle index to evaluate
}

impl StrategyNode {
    /// Evaluates the node to a float value (for indicators) or 1.0/0.0 (for logic).
    /// Returns f64::NAN if insufficient data.
    pub fn eval_float(&self, ctx: &EvalContext) -> f64 {
        if ctx.index >= ctx.closes.len() {
            return f64::NAN;
        }

        match self {
            StrategyNode::Constant(val) => *val,
            StrategyNode::PriceClose => ctx.closes[ctx.index],
            StrategyNode::PriceOpen => ctx.opens[ctx.index],
            StrategyNode::PriceHigh => ctx.highs[ctx.index],
            StrategyNode::PriceLow => ctx.lows[ctx.index],
            StrategyNode::Volume => ctx.volumes[ctx.index],

            // Math/Logic mapping to float (True=1.0, False=0.0)
            StrategyNode::And(l, r) => if self.eval_bool(ctx) { 1.0 } else { 0.0 },
            StrategyNode::Or(l, r) => if self.eval_bool(ctx) { 1.0 } else { 0.0 },
            StrategyNode::Not(n) => if self.eval_bool(ctx) { 1.0 } else { 0.0 },
            
            // Indicators
            // TODO: Implement actual indicator logic here. 
            // For Phase 23 step 1, we map to placeholders.
            // In a real implementation, we would likely pre-calculate indicators 
            // or use a rolling window calculator. 
            // For now, implementing simple SMA as proof of concept.
            StrategyNode::Sma { period } => {
                if ctx.index < period - 1 { return f64::NAN; }
                let start = ctx.index - (period - 1);
                let slice = &ctx.closes[start..=ctx.index];
                slice.iter().sum::<f64>() / *period as f64
            },
            
            // Fallback for unimplemented indicators
            _ => f64::NAN, 
        }
    }

    /// Evaluates the node to a boolean (Signal triggers).
    pub fn eval_bool(&self, ctx: &EvalContext) -> bool {
        match self {
            StrategyNode::And(l, r) => l.eval_bool(ctx) && r.eval_bool(ctx),
            StrategyNode::Or(l, r) => l.eval_bool(ctx) || r.eval_bool(ctx),
            StrategyNode::Not(n) => !n.eval_bool(ctx),

            StrategyNode::Gt(l, r) => l.eval_float(ctx) > r.eval_float(ctx),
            StrategyNode::Lt(l, r) => l.eval_float(ctx) < r.eval_float(ctx),
            
            // Cross Logic looks at previous candle (index - 1)
            StrategyNode::CrossUp(l, r) => {
                if ctx.index == 0 { return false; }
                let prev_ctx = EvalContext { index: ctx.index - 1, ..*ctx };
                
                let l_curr = l.eval_float(ctx);
                let r_curr = r.eval_float(ctx);
                let l_prev = l.eval_float(&prev_ctx);
                let r_prev = r.eval_float(&prev_ctx);

                // Check for NaN
                if l_curr.is_nan() || r_curr.is_nan() || l_prev.is_nan() || r_prev.is_nan() {
                    return false;
                }

                l_prev <= r_prev && l_curr > r_curr
            },
             StrategyNode::CrossDown(l, r) => {
                if ctx.index == 0 { return false; }
                let prev_ctx = EvalContext { index: ctx.index - 1, ..*ctx };
                
                let l_curr = l.eval_float(ctx);
                let r_curr = r.eval_float(ctx);
                let l_prev = l.eval_float(&prev_ctx);
                let r_prev = r.eval_float(&prev_ctx);

                 if l_curr.is_nan() || r_curr.is_nan() || l_prev.is_nan() || r_prev.is_nan() {
                    return false;
                }

                l_prev >= r_prev && l_curr < r_curr
            },
            StrategyNode::Cross(l, r) => {
                 // Either Up or Down
                 // Optimization: reuse CrossUp/Down logic or inline it
                 if ctx.index == 0 { return false; }
                 let prev_ctx = EvalContext { index: ctx.index - 1, ..*ctx };
                 let l_curr = l.eval_float(ctx);
                 let r_curr = r.eval_float(ctx);
                 let l_prev = l.eval_float(&prev_ctx);
                 let r_prev = r.eval_float(&prev_ctx);
                 
                 if l_curr.is_nan() || r_curr.is_nan() || l_prev.is_nan() || r_prev.is_nan() {
                    return false;
                 }
                 
                 (l_prev <= r_prev && l_curr > r_curr) || (l_prev >= r_prev && l_curr < r_curr)
            },

            // Default fallback for Value nodes used in Boolean context
            _ => self.eval_float(ctx) != 0.0,
        }
    }
}
