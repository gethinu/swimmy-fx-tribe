// guardian/src/kalman.rs
// ============================================================================
// KALMAN FILTER FOR STRATEGY PARAMETER ESTIMATION
// ============================================================================
// V47.0: Owner's Vision - Dynamic parameter adaptation
//
// Uses Kalman filtering to:
// 1. Estimate "true" strategy performance (filtering out noise)
// 2. Track optimal parameter drift over time
// 3. Provide confidence intervals for performance metrics
// ============================================================================

use nalgebra::{Matrix2, Vector2, Matrix2x1};

/// Kalman Filter state for a single strategy
#[derive(Clone, Debug)]
pub struct KalmanState {
    /// State vector: [estimated_sharpe, sharpe_velocity]
    pub x: Vector2<f64>,
    /// State covariance matrix
    pub p: Matrix2<f64>,
    /// Process noise covariance
    pub q: Matrix2<f64>,
    /// Measurement noise variance
    pub r: f64,
    /// Observation count
    pub n: u32,
}

impl Default for KalmanState {
    fn default() -> Self {
        Self::new(0.0, 0.1, 0.01)
    }
}

impl KalmanState {
    /// Create new Kalman filter state
    /// - initial_estimate: Initial Sharpe estimate
    /// - process_noise: How much we expect true performance to change
    /// - measurement_noise: How noisy individual trade results are
    pub fn new(initial_estimate: f64, process_noise: f64, measurement_noise: f64) -> Self {
        KalmanState {
            x: Vector2::new(initial_estimate, 0.0), // [value, velocity]
            p: Matrix2::identity() * 1.0,  // Initial uncertainty
            q: Matrix2::new(
                process_noise, 0.0,
                0.0, process_noise * 0.1
            ),
            r: measurement_noise,
            n: 0,
        }
    }
    
    /// Predict step: Project state forward
    pub fn predict(&mut self) {
        // State transition: x_k = A * x_{k-1}
        // A = [[1, dt], [0, 1]] with dt=1
        let a = Matrix2::new(
            1.0, 1.0,
            0.0, 1.0
        );
        
        self.x = a * self.x;
        self.p = a * self.p * a.transpose() + self.q;
    }
    
    /// Update step: Incorporate new observation
    /// Returns innovation (difference between expected and actual)
    pub fn update(&mut self, observation: f64) -> f64 {
        // Observation matrix: we only observe the value, not velocity
        let h = Matrix2x1::new(1.0, 0.0);
        
        // Innovation (measurement residual)
        let y = observation - (h.transpose() * self.x)[0];
        
        // Innovation covariance
        let s = (h.transpose() * self.p * h)[0] + self.r;
        
        // Kalman gain
        let k = self.p * h * (1.0 / s);
        
        // Update state
        self.x = self.x + k * y;
        
        // Update covariance
        let i = Matrix2::identity();
        self.p = (i - k * h.transpose()) * self.p;
        
        self.n += 1;
        
        y // Return innovation for diagnostics
    }
    
    /// Get current filtered estimate
    pub fn estimate(&self) -> f64 {
        self.x[0]
    }
    
    /// Get current velocity (trend)
    pub fn velocity(&self) -> f64 {
        self.x[1]
    }
    
    /// Get uncertainty (standard deviation of estimate)
    pub fn uncertainty(&self) -> f64 {
        self.p[(0, 0)].sqrt()
    }
    
    /// Check if estimate is stable (low uncertainty, many observations)
    pub fn is_stable(&self) -> bool {
        self.n >= 10 && self.uncertainty() < 1.0
    }
    
    /// Get confidence interval (estimate ± 2*sigma)
    pub fn confidence_interval(&self) -> (f64, f64) {
        let sigma = self.uncertainty();
        (self.estimate() - 2.0 * sigma, self.estimate() + 2.0 * sigma)
    }
}

/// Kalman filter for multiple metrics
#[derive(Clone, Debug, Default)]
pub struct StrategyKalman {
    pub sharpe: KalmanState,
    pub profit_factor: KalmanState,
    pub win_rate: KalmanState,
}

impl StrategyKalman {
    pub fn new() -> Self {
        Self {
            sharpe: KalmanState::new(0.0, 0.1, 0.5),
            profit_factor: KalmanState::new(1.0, 0.05, 0.3),
            win_rate: KalmanState::new(0.5, 0.02, 0.1),
        }
    }
    
    /// Update all filters with new trade result
    pub fn update(&mut self, trade_sharpe: f64, trade_pf: f64, trade_won: bool) {
        self.sharpe.predict();
        self.sharpe.update(trade_sharpe);
        
        self.profit_factor.predict();
        self.profit_factor.update(trade_pf);
        
        self.win_rate.predict();
        self.win_rate.update(if trade_won { 1.0 } else { 0.0 });
    }
    
    /// Get filtered performance estimates
    pub fn get_estimates(&self) -> (f64, f64, f64) {
        (
            self.sharpe.estimate(),
            self.profit_factor.estimate(),
            self.win_rate.estimate()
        )
    }
    
    /// Check if all metrics are stable
    pub fn is_stable(&self) -> bool {
        self.sharpe.is_stable() && self.profit_factor.is_stable() && self.win_rate.is_stable()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_kalman_convergence() {
        let mut kf = KalmanState::new(0.0, 0.1, 0.5);
        
        // Simulate noisy observations around true value of 0.5
        let true_value = 0.5;
        for i in 0..50 {
            let noise = (i % 5) as f64 * 0.1 - 0.2;  // Noise between -0.2 and 0.2
            kf.predict();
            kf.update(true_value + noise);
        }
        
        // Should converge close to true value
        assert!((kf.estimate() - true_value).abs() < 0.2, "Estimate: {}", kf.estimate());
        assert!(kf.is_stable(), "Should be stable after 50 observations");
    }
    
    #[test]
    fn test_strategy_kalman() {
        let mut sk = StrategyKalman::new();
        
        for _ in 0..20 {
            sk.update(0.3, 1.5, true);
        }
        
        let (s, pf, wr) = sk.get_estimates();
        assert!(s > 0.2 && s < 0.4, "Sharpe should converge near 0.3");
        assert!(pf > 1.3 && pf < 1.7, "PF should converge near 1.5");
        assert!(wr > 0.8, "WinRate should converge near 1.0 for all wins");
    }
    
    // V47.2: Edge case tests per Uncle Bob's recommendation
    
    #[test]
    fn test_kalman_negative_values() {
        let mut kf = KalmanState::new(-1.0, 0.1, 0.5);
        
        // Track negative Sharpe (losing strategy)
        for _ in 0..30 {
            kf.predict();
            kf.update(-0.5);
        }
        
        assert!(kf.estimate() < 0.0, "Should track negative values");
        assert!(kf.estimate() > -1.0, "Should not diverge below -1");
    }
    
    #[test]
    fn test_kalman_zero_observations() {
        let mut kf = KalmanState::new(0.0, 0.1, 0.5);
        
        // All zeros
        for _ in 0..20 {
            kf.predict();
            kf.update(0.0);
        }
        
        // Should stay near zero, not crash or NaN
        assert!(kf.estimate().is_finite(), "Estimate should be finite");
        assert!(kf.estimate().abs() < 0.1, "Should stay near zero");
    }
    
    #[test]
    fn test_kalman_extreme_noise() {
        let mut kf = KalmanState::new(0.0, 0.01, 10.0);  // Very high measurement noise
        
        // Wild observations
        let observations = [100.0, -100.0, 50.0, -50.0, 0.0];
        for &obs in &observations {
            kf.predict();
            kf.update(obs);
        }
        
        // Should not crash, estimate should be finite
        assert!(kf.estimate().is_finite(), "Should handle extreme noise");
        assert!(kf.uncertainty() > 0.0, "Uncertainty should be positive");
    }
    
    #[test]
    fn test_kalman_confidence_interval() {
        let mut kf = KalmanState::new(0.5, 0.1, 0.3);
        
        for _ in 0..30 {
            kf.predict();
            kf.update(0.5);
        }
        
        let (lower, upper) = kf.confidence_interval();
        assert!(lower < kf.estimate(), "Lower bound < estimate");
        assert!(upper > kf.estimate(), "Upper bound > estimate");
        assert!(lower < upper, "Proper interval ordering");
    }
}

