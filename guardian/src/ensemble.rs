// ensemble.rs - V8.10: Ensemble Predictions & A/B Shadow Testing (Naval P3)
// Combines NN, LSTM, PPO predictions for robust decision making
// Tracks shadow predictions for A/B testing without live execution

use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;
use chrono::Utc;

const SHADOW_LOG_PATH: &str = "shadow_predictions.json";

/// Ensemble prediction combining multiple models
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnsemblePrediction {
    pub action: usize,           // 0=BUY, 1=SELL, 2=HOLD
    pub confidence: f64,         // Combined confidence
    pub nn_action: usize,
    pub lstm_action: usize,
    pub ppo_action: usize,
    pub nn_confidence: f64,
    pub lstm_confidence: f64,
    pub ppo_confidence: f64,
    pub agreement: f64,          // How much models agree (0-1)
}

/// Shadow test record for A/B testing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShadowTestRecord {
    pub timestamp: String,
    pub symbol: String,
    pub ensemble_action: usize,
    pub actual_action: usize,    // What was actually executed
    pub price_at_decision: f64,
    pub price_after: Option<f64>,  // Filled in later for P&L comparison
    pub pnl_ensemble: Option<f64>,
    pub pnl_actual: Option<f64>,
}

/// Shadow testing tracker
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ShadowTracker {
    pub records: Vec<ShadowTestRecord>,
    pub ensemble_total_pnl: f64,
    pub actual_total_pnl: f64,
    pub matches: usize,
    pub mismatches: usize,
}

impl ShadowTracker {
    pub fn new() -> Self {
        Self::load_or_new()
    }

    pub fn load_or_new() -> Self {
        if Path::new(SHADOW_LOG_PATH).exists() {
            if let Ok(json) = fs::read_to_string(SHADOW_LOG_PATH) { if let Ok(tracker) = serde_json::from_str(&json) { return tracker } }
        }
        ShadowTracker::default()
    }

    pub fn save(&self) -> Result<(), std::io::Error> {
        let json = serde_json::to_string_pretty(self)?;
        fs::write(SHADOW_LOG_PATH, json)?;
        Ok(())
    }

    pub fn record_decision(
        &mut self,
        symbol: &str,
        ensemble_action: usize,
        actual_action: usize,
        price: f64,
    ) {
        let record = ShadowTestRecord {
            timestamp: Utc::now().to_rfc3339(),
            symbol: symbol.to_string(),
            ensemble_action,
            actual_action,
            price_at_decision: price,
            price_after: None,
            pnl_ensemble: None,
            pnl_actual: None,
        };

        if ensemble_action == actual_action {
            self.matches += 1;
        } else {
            self.mismatches += 1;
        }

        self.records.push(record);
        
        // Keep only last 500 records
        if self.records.len() > 500 {
            self.records.remove(0);
        }
        
        let _ = self.save();
    }

    pub fn update_pnl(&mut self, symbol: &str, new_price: f64) {
        // Update most recent unfilled record for this symbol
        for record in self.records.iter_mut().rev() {
            if record.symbol == symbol && record.price_after.is_none() {
                let price_change = new_price - record.price_at_decision;
                
                // Calculate what each would have made
                record.price_after = Some(new_price);
                record.pnl_ensemble = Some(match record.ensemble_action {
                    0 => price_change,   // BUY profits if price up
                    1 => -price_change,  // SELL profits if price down
                    _ => 0.0,            // HOLD
                });
                record.pnl_actual = Some(match record.actual_action {
                    0 => price_change,
                    1 => -price_change,
                    _ => 0.0,
                });
                
                self.ensemble_total_pnl += record.pnl_ensemble.unwrap_or(0.0);
                self.actual_total_pnl += record.pnl_actual.unwrap_or(0.0);
                
                let _ = self.save();
                break;
            }
        }
    }

    pub fn get_performance_summary(&self) -> (f64, f64, f64) {
        let agreement_rate = if self.matches + self.mismatches > 0 {
            self.matches as f64 / (self.matches + self.mismatches) as f64
        } else {
            0.0
        };
        (self.ensemble_total_pnl, self.actual_total_pnl, agreement_rate)
    }
}

/// Ensemble predictor combining NN, LSTM, PPO
pub struct EnsemblePredictor;

impl EnsemblePredictor {
    /// Combine predictions from multiple models
    /// nn_probs: [P(UP), P(DOWN), P(FLAT)]
    /// lstm_probs: [P(UP), P(DOWN), P(FLAT)]
    /// ppo_probs: [P(BUY), P(SELL), P(HOLD)]
    pub fn predict(
        nn_probs: [f64; 3],
        lstm_probs: [f64; 3],
        ppo_probs: [f64; 3],
    ) -> EnsemblePrediction {
        // Weighted average (NN and LSTM are price predictors, PPO is action)
        // Weights: NN=0.35, LSTM=0.35, PPO=0.30
        let combined = [
            nn_probs[0] * 0.35 + lstm_probs[0] * 0.35 + ppo_probs[0] * 0.30,
            nn_probs[1] * 0.35 + lstm_probs[1] * 0.35 + ppo_probs[1] * 0.30,
            nn_probs[2] * 0.35 + lstm_probs[2] * 0.35 + ppo_probs[2] * 0.30,
        ];

        // Find best action
        let mut best_action = 0;
        let mut best_conf = combined[0];
        for (i, &p) in combined.iter().enumerate() {
            if p > best_conf {
                best_action = i;
                best_conf = p;
            }
        }

        // Individual model actions
        let nn_action = nn_probs.iter().enumerate()
            .max_by(|a, b| a.1.partial_cmp(b.1).unwrap())
            .map(|(i, _)| i).unwrap_or(2);
        let lstm_action = lstm_probs.iter().enumerate()
            .max_by(|a, b| a.1.partial_cmp(b.1).unwrap())
            .map(|(i, _)| i).unwrap_or(2);
        let ppo_action = ppo_probs.iter().enumerate()
            .max_by(|a, b| a.1.partial_cmp(b.1).unwrap())
            .map(|(i, _)| i).unwrap_or(2);

        // Agreement: count how many agree with final decision
        let mut agree_count = 0;
        if nn_action == best_action { agree_count += 1; }
        if lstm_action == best_action { agree_count += 1; }
        if ppo_action == best_action { agree_count += 1; }
        let agreement = agree_count as f64 / 3.0;

        EnsemblePrediction {
            action: best_action,
            confidence: best_conf,
            nn_action,
            lstm_action,
            ppo_action,
            nn_confidence: nn_probs[nn_action],
            lstm_confidence: lstm_probs[lstm_action],
            ppo_confidence: ppo_probs[ppo_action],
            agreement,
        }
    }

    /// Should we trust this prediction enough to act?
    /// Requires high confidence AND high agreement
    pub fn should_act(pred: &EnsemblePrediction) -> bool {
        pred.confidence > 0.45 && pred.agreement >= 0.67
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ensemble_prediction() {
        let nn_probs = [0.6, 0.3, 0.1];    // NN says BUY
        let lstm_probs = [0.5, 0.4, 0.1];  // LSTM leans BUY
        let ppo_probs = [0.7, 0.2, 0.1];   // PPO says BUY
        
        let pred = EnsemblePredictor::predict(nn_probs, lstm_probs, ppo_probs);
        
        assert_eq!(pred.action, 0); // Should be BUY
        assert!(pred.confidence > 0.5);
        assert!(pred.agreement > 0.9); // All agree
    }

    #[test]
    fn test_ensemble_disagreement() {
        let nn_probs = [0.7, 0.2, 0.1];    // NN says BUY
        let lstm_probs = [0.2, 0.7, 0.1];  // LSTM says SELL
        let ppo_probs = [0.1, 0.2, 0.7];   // PPO says HOLD
        
        let pred = EnsemblePredictor::predict(nn_probs, lstm_probs, ppo_probs);
        
        // With such disagreement, confidence should be lower
        assert!(pred.confidence < 0.5);
        assert!(pred.agreement < 0.5); // Models disagree
    }

    #[test]
    fn test_shadow_tracker() {
        let mut tracker = ShadowTracker::default();
        
        tracker.record_decision("USDJPY", 0, 1, 150.0);
        assert_eq!(tracker.mismatches, 1);
        
        tracker.record_decision("USDJPY", 0, 0, 150.5);
        assert_eq!(tracker.matches, 1);
        
        let (_, _, agreement) = tracker.get_performance_summary();
        assert!((agreement - 0.5).abs() < 0.01);
    }
}
