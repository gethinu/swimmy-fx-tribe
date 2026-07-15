// lstm.rs - LSTM Network for Sequence Prediction
// Deep learning for time series pattern recognition

use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

const LSTM_MODEL_PATH: &str = "lstm_model.json";

// LSTM dimensions
const INPUT_SIZE: usize = 10;   // Features per timestep
const HIDDEN_SIZE: usize = 32;  // LSTM hidden state size
const SEQ_LEN: usize = 20;      // Sequence length (20 candles)
const OUTPUT_SIZE: usize = 3;   // UP, DOWN, FLAT

/// LSTM Cell with forget, input, output gates
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LstmCell {
    // Forget gate: Wf, Uf, bf
    wf: [[f64; INPUT_SIZE]; HIDDEN_SIZE],
    uf: [[f64; HIDDEN_SIZE]; HIDDEN_SIZE],
    bf: [f64; HIDDEN_SIZE],
    
    // Input gate: Wi, Ui, bi
    wi: [[f64; INPUT_SIZE]; HIDDEN_SIZE],
    ui: [[f64; HIDDEN_SIZE]; HIDDEN_SIZE],
    bi: [f64; HIDDEN_SIZE],
    
    // Cell candidate: Wc, Uc, bc
    wc: [[f64; INPUT_SIZE]; HIDDEN_SIZE],
    uc: [[f64; HIDDEN_SIZE]; HIDDEN_SIZE],
    bc: [f64; HIDDEN_SIZE],
    
    // Output gate: Wo, Uo, bo
    wo: [[f64; INPUT_SIZE]; HIDDEN_SIZE],
    uo: [[f64; HIDDEN_SIZE]; HIDDEN_SIZE],
    bo: [f64; HIDDEN_SIZE],
}

/// Full LSTM Network
#[derive(Debug, Serialize, Deserialize)]
pub struct LstmNetwork {
    cell: LstmCell,
    // Output layer: hidden -> output
    w_out: [[f64; HIDDEN_SIZE]; OUTPUT_SIZE],
    b_out: [f64; OUTPUT_SIZE],
    // State
    pub train_count: usize,
}

#[derive(Debug, Serialize)]
pub struct LstmPrediction {
    pub up_prob: f64,
    pub down_prob: f64,
    pub flat_prob: f64,
    pub signal: String,
    pub confidence: f64,
    pub hidden_state: Vec<f64>,  // For visualization
}

impl LstmCell {
    fn new() -> Self {
        let init = |i: usize, j: usize, scale: f64| -> f64 {
            let seed = ((i * 17 + j * 31) as f64 * 0.618033988749895) % 1.0;
            (seed - 0.5) * scale
        };
        
        let scale_i = (2.0 / INPUT_SIZE as f64).sqrt();
        let scale_h = (2.0 / HIDDEN_SIZE as f64).sqrt();
        
        let mut wf = [[0.0; INPUT_SIZE]; HIDDEN_SIZE];
        let mut uf = [[0.0; HIDDEN_SIZE]; HIDDEN_SIZE];
        let mut wi = [[0.0; INPUT_SIZE]; HIDDEN_SIZE];
        let mut ui = [[0.0; HIDDEN_SIZE]; HIDDEN_SIZE];
        let mut wc = [[0.0; INPUT_SIZE]; HIDDEN_SIZE];
        let mut uc = [[0.0; HIDDEN_SIZE]; HIDDEN_SIZE];
        let mut wo = [[0.0; INPUT_SIZE]; HIDDEN_SIZE];
        let mut uo = [[0.0; HIDDEN_SIZE]; HIDDEN_SIZE];
        
        for i in 0..HIDDEN_SIZE {
            for j in 0..INPUT_SIZE {
                wf[i][j] = init(i, j, scale_i);
                wi[i][j] = init(i + 100, j, scale_i);
                wc[i][j] = init(i + 200, j, scale_i);
                wo[i][j] = init(i + 300, j, scale_i);
            }
            for j in 0..HIDDEN_SIZE {
                uf[i][j] = init(i + 400, j, scale_h);
                ui[i][j] = init(i + 500, j, scale_h);
                uc[i][j] = init(i + 600, j, scale_h);
                uo[i][j] = init(i + 700, j, scale_h);
            }
        }
        
        // Forget gate bias initialized to 1.0 (common practice)
        let bf = [1.0; HIDDEN_SIZE];
        let bi = [0.0; HIDDEN_SIZE];
        let bc = [0.0; HIDDEN_SIZE];
        let bo = [0.0; HIDDEN_SIZE];
        
        LstmCell { wf, uf, bf, wi, ui, bi, wc, uc, bc, wo, uo, bo }
    }
}

fn sigmoid(x: f64) -> f64 {
    1.0 / (1.0 + (-x).exp())
}

fn tanh_act(x: f64) -> f64 {
    x.tanh()
}

fn softmax(x: &[f64; OUTPUT_SIZE]) -> [f64; OUTPUT_SIZE] {
    let max_x = x.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let exp_x: Vec<f64> = x.iter().map(|&v| (v - max_x).exp()).collect();
    let sum: f64 = exp_x.iter().sum();
    [exp_x[0] / sum, exp_x[1] / sum, exp_x[2] / sum]
}

impl LstmNetwork {
    pub fn new() -> Self {
        let cell = LstmCell::new();
        
        let init = |i: usize, j: usize, scale: f64| -> f64 {
            let seed = ((i * 23 + j * 37) as f64 * 0.618033988749895) % 1.0;
            (seed - 0.5) * scale
        };
        
        let scale = (2.0 / HIDDEN_SIZE as f64).sqrt();
        let mut w_out = [[0.0; HIDDEN_SIZE]; OUTPUT_SIZE];
        
        for i in 0..OUTPUT_SIZE {
            for j in 0..HIDDEN_SIZE {
                w_out[i][j] = init(i, j, scale);
            }
        }
        
        LstmNetwork {
            cell,
            w_out,
            b_out: [0.0; OUTPUT_SIZE],
            train_count: 0,
        }
    }
    
    /// Forward pass through LSTM cell for one timestep
    fn cell_forward(
        &self,
        x: &[f64; INPUT_SIZE],
        h_prev: &[f64; HIDDEN_SIZE],
        c_prev: &[f64; HIDDEN_SIZE],
    ) -> ([f64; HIDDEN_SIZE], [f64; HIDDEN_SIZE]) {
        let mut f = [0.0; HIDDEN_SIZE];  // Forget gate
        let mut i = [0.0; HIDDEN_SIZE];  // Input gate
        let mut c_tilde = [0.0; HIDDEN_SIZE];  // Cell candidate
        let mut o = [0.0; HIDDEN_SIZE];  // Output gate
        
        for h in 0..HIDDEN_SIZE {
            // Forget gate
            let mut sum_f = self.cell.bf[h];
            for k in 0..INPUT_SIZE { sum_f += self.cell.wf[h][k] * x[k]; }
            for k in 0..HIDDEN_SIZE { sum_f += self.cell.uf[h][k] * h_prev[k]; }
            f[h] = sigmoid(sum_f);
            
            // Input gate
            let mut sum_i = self.cell.bi[h];
            for k in 0..INPUT_SIZE { sum_i += self.cell.wi[h][k] * x[k]; }
            for k in 0..HIDDEN_SIZE { sum_i += self.cell.ui[h][k] * h_prev[k]; }
            i[h] = sigmoid(sum_i);
            
            // Cell candidate
            let mut sum_c = self.cell.bc[h];
            for k in 0..INPUT_SIZE { sum_c += self.cell.wc[h][k] * x[k]; }
            for k in 0..HIDDEN_SIZE { sum_c += self.cell.uc[h][k] * h_prev[k]; }
            c_tilde[h] = tanh_act(sum_c);
            
            // Output gate
            let mut sum_o = self.cell.bo[h];
            for k in 0..INPUT_SIZE { sum_o += self.cell.wo[h][k] * x[k]; }
            for k in 0..HIDDEN_SIZE { sum_o += self.cell.uo[h][k] * h_prev[k]; }
            o[h] = sigmoid(sum_o);
        }
        
        // New cell state: c = f * c_prev + i * c_tilde
        let mut c = [0.0; HIDDEN_SIZE];
        for h in 0..HIDDEN_SIZE {
            c[h] = f[h] * c_prev[h] + i[h] * c_tilde[h];
        }
        
        // New hidden state: h = o * tanh(c)
        let mut h = [0.0; HIDDEN_SIZE];
        for h_idx in 0..HIDDEN_SIZE {
            h[h_idx] = o[h_idx] * tanh_act(c[h_idx]);
        }
        
        (h, c)
    }
    
    /// Forward pass through entire sequence
    pub fn predict(&self, sequence: &[[f64; INPUT_SIZE]; SEQ_LEN]) -> LstmPrediction {
        let mut h = [0.0; HIDDEN_SIZE];
        let mut c = [0.0; HIDDEN_SIZE];
        
        // Process each timestep
        for t in 0..SEQ_LEN {
            let (h_new, c_new) = self.cell_forward(&sequence[t], &h, &c);
            h = h_new;
            c = c_new;
        }
        
        // Output layer
        let mut out = [0.0; OUTPUT_SIZE];
        for i in 0..OUTPUT_SIZE {
            out[i] = self.b_out[i];
            for j in 0..HIDDEN_SIZE {
                out[i] += self.w_out[i][j] * h[j];
            }
        }
        
        let probs = softmax(&out);
        
        let (signal, confidence) = if probs[0] > probs[1] && probs[0] > probs[2] {
            ("BUY".to_string(), probs[0])
        } else if probs[1] > probs[0] && probs[1] > probs[2] {
            ("SELL".to_string(), probs[1])
        } else {
            ("HOLD".to_string(), probs[2])
        };
        
        LstmPrediction {
            up_prob: probs[0],
            down_prob: probs[1],
            flat_prob: probs[2],
            signal,
            confidence,
            hidden_state: h.to_vec(),
        }
    }
    
    /// Extract features from candles for LSTM input
    pub fn extract_sequence(candles: &[super::backtester::Candle]) -> Option<[[f64; INPUT_SIZE]; SEQ_LEN]> {
        if candles.len() < SEQ_LEN + 5 {
            return None;
        }
        
        let mut sequence = [[0.0; INPUT_SIZE]; SEQ_LEN];
        
        for t in 0..SEQ_LEN {
            let idx = t;
            let c = &candles[idx];
            let c_prev = &candles[idx + 1];
            
            // Feature extraction for each timestep
            let returns = (c.close - c_prev.close) / c_prev.close;
            let range = (c.high - c.low) / c.close;
            let body = (c.close - c.open).abs() / c.close;
            let upper_wick = (c.high - c.close.max(c.open)) / c.close;
            let lower_wick = (c.close.min(c.open) - c.low) / c.close;
            
            // SMA features
            let sma5: f64 = candles[idx..idx+5].iter().map(|x| x.close).sum::<f64>() / 5.0;
            let sma_diff = (c.close - sma5) / sma5;
            
            // Momentum
            let momentum_3 = (c.close - candles[idx + 3].close) / candles[idx + 3].close;
            let momentum_5 = (c.close - candles[idx + 5].close) / candles[idx + 5].close;
            
            // Volume normalized (simplified)
            let vol_ratio = 1.0;  // Placeholder
            
            // Direction
            let direction = if c.close > c.open { 1.0 } else { -1.0 };
            
            sequence[t] = [
                returns * 100.0,
                range * 100.0,
                body * 100.0,
                upper_wick * 100.0,
                lower_wick * 100.0,
                sma_diff * 100.0,
                momentum_3 * 100.0,
                momentum_5 * 100.0,
                vol_ratio,
                direction,
            ];
        }
        
        Some(sequence)
    }
    
    /// V6.19: Simplified LSTM training (output layer + hidden state nudging)
    /// This is a simplified version that doesn't do full BPTT but provides
    /// meaningful learning by adjusting output weights and biasing cell gates
    #[allow(dead_code)]
    pub fn train_simple(&mut self, sequence: &[[f64; INPUT_SIZE]; SEQ_LEN], target: usize, lr: f64) {
        // Forward pass to get prediction
        let mut h = [0.0; HIDDEN_SIZE];
        let mut c = [0.0; HIDDEN_SIZE];
        
        for t in 0..SEQ_LEN {
            let (h_new, c_new) = self.cell_forward(&sequence[t], &h, &c);
            h = h_new;
            c = c_new;
        }
        
        // Compute output
        let mut out = [0.0; OUTPUT_SIZE];
        for i in 0..OUTPUT_SIZE {
            out[i] = self.b_out[i];
            for j in 0..HIDDEN_SIZE {
                out[i] += self.w_out[i][j] * h[j];
            }
        }
        
        let probs = softmax(&out);
        
        // Compute cross-entropy gradient
        let mut d_out = probs;
        d_out[target] -= 1.0;  // Gradient of cross-entropy loss
        
        // Update output layer weights
        for i in 0..OUTPUT_SIZE {
            self.b_out[i] -= lr * d_out[i];
            for j in 0..HIDDEN_SIZE {
                self.w_out[i][j] -= lr * d_out[i] * h[j];
            }
        }
        
        // Simplified hidden state update: nudge gate biases
        // based on prediction error direction
        let error_magnitude = d_out.iter().map(|x| x.abs()).sum::<f64>() / OUTPUT_SIZE as f64;
        let nudge = lr * error_magnitude * 0.1;  // Small nudge to gates
        
        // If wrong prediction, slightly increase forget gate bias (more memory)
        // and decrease input gate bias (more selective)
        if target != probs.iter().enumerate().max_by(|a, b| a.1.partial_cmp(b.1).unwrap()).unwrap().0 {
            for h_idx in 0..HIDDEN_SIZE {
                self.cell.bf[h_idx] += nudge;  // Remember more
                self.cell.bi[h_idx] -= nudge * 0.5;  // Be more selective
            }
        }
        
        self.train_count += 1;
        
        // Auto-save every 100 training samples
        if self.train_count.is_multiple_of(100) {
            let _ = self.save();
        }
        
        println!("🎓 LSTM TRAIN #{}: target={} pred={} error={:.4}", 
                 self.train_count, target,
                 probs.iter().enumerate().max_by(|a, b| a.1.partial_cmp(b.1).unwrap()).unwrap().0,
                 error_magnitude);
    }
    
    /// V6.22: Full BPTT (Backpropagation Through Time) Training
    /// Proper gradient computation through all timesteps for LSTM learning
    pub fn train_bptt(&mut self, sequence: &[[f64; INPUT_SIZE]; SEQ_LEN], target: usize, lr: f64) {
        // ====== FORWARD PASS WITH CACHING ======
        // Cache all intermediate values needed for backward pass
        let mut h_cache: [[f64; HIDDEN_SIZE]; SEQ_LEN + 1] = [[0.0; HIDDEN_SIZE]; SEQ_LEN + 1];
        let mut c_cache: [[f64; HIDDEN_SIZE]; SEQ_LEN + 1] = [[0.0; HIDDEN_SIZE]; SEQ_LEN + 1];
        let mut f_cache: [[f64; HIDDEN_SIZE]; SEQ_LEN] = [[0.0; HIDDEN_SIZE]; SEQ_LEN];
        let mut i_cache: [[f64; HIDDEN_SIZE]; SEQ_LEN] = [[0.0; HIDDEN_SIZE]; SEQ_LEN];
        let mut c_tilde_cache: [[f64; HIDDEN_SIZE]; SEQ_LEN] = [[0.0; HIDDEN_SIZE]; SEQ_LEN];
        let mut o_cache: [[f64; HIDDEN_SIZE]; SEQ_LEN] = [[0.0; HIDDEN_SIZE]; SEQ_LEN];
        
        // Forward pass with caching
        for t in 0..SEQ_LEN {
            let x = &sequence[t];
            // Copy previous values to avoid borrow issues
            let h_prev_copy = h_cache[t];
            let c_prev_copy = c_cache[t];
            
            for h in 0..HIDDEN_SIZE {
                // Forget gate
                let mut sum_f = self.cell.bf[h];
                for k in 0..INPUT_SIZE { sum_f += self.cell.wf[h][k] * x[k]; }
                for k in 0..HIDDEN_SIZE { sum_f += self.cell.uf[h][k] * h_prev_copy[k]; }
                f_cache[t][h] = sigmoid(sum_f);
                
                // Input gate
                let mut sum_i = self.cell.bi[h];
                for k in 0..INPUT_SIZE { sum_i += self.cell.wi[h][k] * x[k]; }
                for k in 0..HIDDEN_SIZE { sum_i += self.cell.ui[h][k] * h_prev_copy[k]; }
                i_cache[t][h] = sigmoid(sum_i);
                
                // Cell candidate
                let mut sum_c = self.cell.bc[h];
                for k in 0..INPUT_SIZE { sum_c += self.cell.wc[h][k] * x[k]; }
                for k in 0..HIDDEN_SIZE { sum_c += self.cell.uc[h][k] * h_prev_copy[k]; }
                c_tilde_cache[t][h] = tanh_act(sum_c);
                
                // Output gate
                let mut sum_o = self.cell.bo[h];
                for k in 0..INPUT_SIZE { sum_o += self.cell.wo[h][k] * x[k]; }
                for k in 0..HIDDEN_SIZE { sum_o += self.cell.uo[h][k] * h_prev_copy[k]; }
                o_cache[t][h] = sigmoid(sum_o);
                
                // Cell state and hidden state
                c_cache[t + 1][h] = f_cache[t][h] * c_prev_copy[h] + i_cache[t][h] * c_tilde_cache[t][h];
                h_cache[t + 1][h] = o_cache[t][h] * tanh_act(c_cache[t + 1][h]);
            }
        }
        
        // Output layer
        let final_h = &h_cache[SEQ_LEN];
        let mut out = [0.0; OUTPUT_SIZE];
        for i in 0..OUTPUT_SIZE {
            out[i] = self.b_out[i];
            for j in 0..HIDDEN_SIZE {
                out[i] += self.w_out[i][j] * final_h[j];
            }
        }
        let probs = softmax(&out);
        
        // ====== BACKWARD PASS ======
        // Output layer gradient (cross-entropy loss)
        let mut d_out = probs;
        d_out[target] -= 1.0;
        
        // Gradient for output weights
        for i in 0..OUTPUT_SIZE {
            self.b_out[i] -= lr * d_out[i];
            for j in 0..HIDDEN_SIZE {
                self.w_out[i][j] -= lr * d_out[i] * final_h[j];
            }
        }
        
        // Gradient flowing into final hidden state
        let mut d_h = [0.0; HIDDEN_SIZE];
        for j in 0..HIDDEN_SIZE {
            for i in 0..OUTPUT_SIZE {
                d_h[j] += d_out[i] * self.w_out[i][j];
            }
        }
        
        // BPTT: Backpropagate through time
        let mut d_c_next = [0.0; HIDDEN_SIZE];
        let clip_val = 5.0;  // Gradient clipping
        
        for t in (0..SEQ_LEN).rev() {
            let x = &sequence[t];
            let h_prev = &h_cache[t];
            let c_prev = &c_cache[t];
            let c = &c_cache[t + 1];
            
            // Gradients for gates
            let mut d_o = [0.0; HIDDEN_SIZE];
            let mut d_f = [0.0; HIDDEN_SIZE];
            let mut d_i = [0.0; HIDDEN_SIZE];
            let mut d_c_tilde = [0.0; HIDDEN_SIZE];
            let mut d_c = [0.0; HIDDEN_SIZE];
            
            for h in 0..HIDDEN_SIZE {
                // d_h flows from above
                let tanh_c = tanh_act(c[h]);
                
                // Output gate: d_o = d_h * tanh(c) * sigmoid'(o)
                d_o[h] = d_h[h] * tanh_c * o_cache[t][h] * (1.0 - o_cache[t][h]);
                
                // Cell state: d_c = d_h * o * (1 - tanh^2(c)) + d_c_next
                d_c[h] = d_h[h] * o_cache[t][h] * (1.0 - tanh_c * tanh_c) + d_c_next[h];
                
                // Forget gate: d_f = d_c * c_prev * sigmoid'(f)
                d_f[h] = d_c[h] * c_prev[h] * f_cache[t][h] * (1.0 - f_cache[t][h]);
                
                // Input gate: d_i = d_c * c_tilde * sigmoid'(i)
                d_i[h] = d_c[h] * c_tilde_cache[t][h] * i_cache[t][h] * (1.0 - i_cache[t][h]);
                
                // Cell candidate: d_c_tilde = d_c * i * tanh'(c_tilde)
                d_c_tilde[h] = d_c[h] * i_cache[t][h] * (1.0 - c_tilde_cache[t][h] * c_tilde_cache[t][h]);
                
                // Gradient clipping
                d_o[h] = d_o[h].max(-clip_val).min(clip_val);
                d_f[h] = d_f[h].max(-clip_val).min(clip_val);
                d_i[h] = d_i[h].max(-clip_val).min(clip_val);
                d_c_tilde[h] = d_c_tilde[h].max(-clip_val).min(clip_val);
                
                // d_c_next for previous timestep
                d_c_next[h] = d_c[h] * f_cache[t][h];
            }
            
            // Update weights (with learning rate)
            for h in 0..HIDDEN_SIZE {
                // Bias updates
                self.cell.bf[h] -= lr * d_f[h];
                self.cell.bi[h] -= lr * d_i[h];
                self.cell.bc[h] -= lr * d_c_tilde[h];
                self.cell.bo[h] -= lr * d_o[h];
                
                // Input weight updates
                for k in 0..INPUT_SIZE {
                    self.cell.wf[h][k] -= lr * d_f[h] * x[k];
                    self.cell.wi[h][k] -= lr * d_i[h] * x[k];
                    self.cell.wc[h][k] -= lr * d_c_tilde[h] * x[k];
                    self.cell.wo[h][k] -= lr * d_o[h] * x[k];
                }
                
                // Hidden weight updates
                for k in 0..HIDDEN_SIZE {
                    self.cell.uf[h][k] -= lr * d_f[h] * h_prev[k];
                    self.cell.ui[h][k] -= lr * d_i[h] * h_prev[k];
                    self.cell.uc[h][k] -= lr * d_c_tilde[h] * h_prev[k];
                    self.cell.uo[h][k] -= lr * d_o[h] * h_prev[k];
                }
            }
            
            // Compute d_h for previous timestep
            let mut d_h_prev = [0.0; HIDDEN_SIZE];
            for k in 0..HIDDEN_SIZE {
                for h in 0..HIDDEN_SIZE {
                    d_h_prev[k] += d_f[h] * self.cell.uf[h][k];
                    d_h_prev[k] += d_i[h] * self.cell.ui[h][k];
                    d_h_prev[k] += d_c_tilde[h] * self.cell.uc[h][k];
                    d_h_prev[k] += d_o[h] * self.cell.uo[h][k];
                }
            }
            d_h = d_h_prev;
        }
        
        self.train_count += 1;
        
        // Calculate loss for logging
        let loss = -probs[target].max(1e-10).ln();
        let pred = probs.iter().enumerate().max_by(|a, b| a.1.partial_cmp(b.1).unwrap()).unwrap().0;
        
        if self.train_count.is_multiple_of(50) {
            let _ = self.save();
        }
        
        println!("🧠 BPTT #{}: target={} pred={} loss={:.4} correct={}", 
                 self.train_count, target, pred, loss, if target == pred { "✓" } else { "✗" });
    }
    
    /// Save model to file
    pub fn save(&self) -> Result<(), std::io::Error> {
        let json = serde_json::to_string_pretty(self)?;
        fs::write(LSTM_MODEL_PATH, json)?;
        println!("💾 LSTM Model saved (train_count: {})", self.train_count);
        Ok(())
    }
    
    /// Load or create new
    pub fn load_or_new() -> Self {
        if Path::new(LSTM_MODEL_PATH).exists() {
            match fs::read_to_string(LSTM_MODEL_PATH) {
                Ok(json) => match serde_json::from_str(&json) {
                    Ok(net) => {
                        println!("📂 LSTM Model loaded");
                        return net;
                    }
                    Err(e) => println!("⚠️ LSTM parse error: {}", e),
                },
                Err(e) => println!("⚠️ LSTM read error: {}", e),
            }
        }
        println!("🆕 Creating new LSTM model");
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_lstm_forward() {
        let net = LstmNetwork::new();
        let sequence = [[0.1; INPUT_SIZE]; SEQ_LEN];
        let pred = net.predict(&sequence);
        
        let sum = pred.up_prob + pred.down_prob + pred.flat_prob;
        assert!((sum - 1.0).abs() < 0.01);
    }
}
