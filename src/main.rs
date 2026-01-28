// guardian/src/main.rs
// Full Duplex Version: MT5 <-> Rust <-> Lisp + Backtester + Neural Net + Tournament + LSTM + MCTS + PPO
mod backtester;
mod neural;
mod tournament;
mod lstm;
mod mcts;
mod ppo;  // V6.20: PPO Agent
mod ensemble;  // V8.10: Ensemble Predictions & A/B Testing
mod audit; // Phase 8: Immutable Audit Logging
mod cpcv;   // V47.0: CPCV Backtest Validation
mod kalman; // V47.0: Kalman Filter for Parameter Estimation

use zmq::{Context, POLLIN};
use serde::{Deserialize, Serialize};
use std::sync::Mutex;
use std::collections::HashMap;
use chrono::{Datelike, Timelike}; // V9.1: Fix for weekday() and hour()
use std::fs::File;
use std::io::{BufReader, Write};

// Global neural network instance (loads from file if exists)
lazy_static::lazy_static! {
    static ref NEURAL_NET: Mutex<neural::NeuralNet> = Mutex::new(neural::NeuralNet::load_or_new());
    // V7.2: PPO Agent Integration (Naval's Fix)
    static ref PPO_AGENT: Mutex<ppo::PPOAgent> = Mutex::new(ppo::PPOAgent::new());
    // V7.8+++: LSTM for sequence-based prediction
    static ref LSTM_NET: Mutex<lstm::LstmNetwork> = Mutex::new(lstm::LstmNetwork::new());
    // V8.10: A/B Shadow Testing Tracker
    static ref SHADOW_TRACKER: Mutex<ensemble::ShadowTracker> = Mutex::new(ensemble::ShadowTracker::new());
    // V10.0: Backtest Data Cache (Musk's Optimization)
    static ref BACKTEST_DATA_CACHE: Mutex<HashMap<String, (Vec<backtester::Candle>, HashMap<String, Vec<backtester::Candle>>)>> = Mutex::new(HashMap::new());
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct BacktestRequest {
    action: String,
    strategy: backtester::Strategy,
    // V10.0: Data Caching optimization
    #[serde(default)]
    data_id: Option<String>,
    // V10.1: Target Timeframe (Minutes) for Resampling
    #[serde(default)]
    timeframe: Option<i64>,
    #[serde(default)]
    candles: Vec<backtester::Candle>,
    #[serde(default)]
    aux_candles: HashMap<String, Vec<backtester::Candle>>,
    // V9.1: File-based data loading to prevent OOM
    #[serde(default)]
    candles_file: Option<String>,
    #[serde(default)]
    aux_candles_files: Option<HashMap<String, String>>,
}

// V11.0: P7 Load CSV Command
#[derive(Debug, Deserialize)]
struct LoadCsvRequest {
    path: String,
    data_id: String,
}

// V10.0: New command to cache data
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct CacheDataRequest {
    action: String,
    data_id: String,
    candles: Vec<backtester::Candle>,
    #[serde(default)]
    aux_candles: HashMap<String, Vec<backtester::Candle>>,
}

// Helper to load candles from CSV
fn load_candles_from_csv(path: &str) -> std::io::Result<Vec<backtester::Candle>> {
    let file = std::fs::File::open(path)?;
    let mut rdr = csv::ReaderBuilder::new()
        .has_headers(false) // Assuming standard headerless format or checking logic
        .from_reader(file);
        
    let mut candles = Vec::new();
    
    for result in rdr.records() {
        let record = result?;
        // Expected format: timestamp,open,high,low,close,volume,...
        if record.len() < 6 { continue; }
        
        // Skip header if parse fails (try f64 for robustness against "123.0")
        if record[0].parse::<f64>().is_err() { continue; }
        
        // Parse as f64 then cast to i64 to handle "12345.0"
        let ts_val = record[0].parse::<f64>().unwrap_or(0.0);
        
        let candle = backtester::Candle {
            timestamp: ts_val as i64,
            open: record[1].parse().unwrap_or(0.0),
            high: record[2].parse().unwrap_or(0.0),
            low: record[3].parse().unwrap_or(0.0),
            close: record[4].parse().unwrap_or(0.0),
            volume: record[5].parse().unwrap_or(0.0),
        };
        candles.push(candle);
    }
    
    Ok(candles)
}


#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct CloneCheckRequest {
    action: String,
    new_strategy: backtester::Strategy,
    existing_strategies: Vec<backtester::Strategy>,
    threshold: f64,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct PredictRequest {
    action: String,
    candles: Vec<backtester::Candle>,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct TrainRequest {
    action: String,
    candles: Vec<backtester::Candle>,
    target: usize,  // 0=UP, 1=DOWN, 2=FLAT
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct EvolveRequest {
    action: String,
    candles: Vec<backtester::Candle>,
    rounds: usize,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct MctsRequest {
    action: String,
    candles: Vec<backtester::Candle>,
    iterations: u32,
    sma_short: usize,
    sma_long: usize,
    sl: f64,
    tp: f64,
}

// P12: CPCV Validation Request from Lisp
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct CpcvRequest {
    action: String,
    strategy_name: String,
    symbol: String,
    candles_file: String,
    strategy_params: serde_json::Value,
}

// ═══════════════════════════════════════════════════
// V8.0: RISK GATE - Guardian's Veto Power (Graham)
// Brain's orders must pass through this gate before MT5
// ═══════════════════════════════════════════════════

#[derive(Debug, Clone)]
enum VetoReason {
    DailyLimitExceeded,
    RiskPerTradeExceeded,
    BrainTimeout,
    MalformedOrder,
    InvalidLotSize,
    ConcentrationLimitExceeded, // V8.6
    MarketClosed, // V9.1
}

impl std::fmt::Display for VetoReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VetoReason::DailyLimitExceeded => write!(f, "Daily loss limit exceeded"),
            VetoReason::RiskPerTradeExceeded => write!(f, "Risk per trade too high"),
            VetoReason::BrainTimeout => write!(f, "Brain unresponsive"),
            VetoReason::MalformedOrder => write!(f, "Malformed order message"),
            VetoReason::InvalidLotSize => write!(f, "Invalid lot size"),
            VetoReason::ConcentrationLimitExceeded => write!(f, "Daily symbol concentration limit exceeded"),
            VetoReason::MarketClosed => write!(f, "Market Closed"),
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Clone)]
#[allow(dead_code)]
struct TradeOrder {
    action: String,
    symbol: String,
    #[serde(default)]
    lot: f64,
    #[serde(default)]
    sl: Option<f64>,
    #[serde(default)]
    tp: Option<f64>,
    #[serde(default)]
    magic: Option<i32>, // V44.0: Added Magic Number support
    #[serde(default)]
    comment: Option<String>, // V44.1: Added Comment support for Strategy Name
    #[serde(default)]
    reason: Option<String>,
    #[serde(default)]
    estimated_loss: Option<f64>, // Phase 8: Risk Per Trade (Worst Case)
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct RiskGate {
    daily_pnl: f64,
    max_daily_loss: f64,
    max_lot_size: f64,
    min_lot_size: f64,
    brain_connected: bool,
    wins: u32,
    losses: u32,
    total_win_amt: f64,  // V8.4: Track amounts for True Kelly
    total_loss_amt: f64, // V8.4: Track amounts for True Kelly
    consecutive_losses: u32, // V8.5: Loss Streak Circuit Breaker (Graham)
    daily_symbol_volume: HashMap<String, f64>, // V8.6: Concentration Limit
    open_positions: HashMap<String, f64>, // V8.9: Track open positions for correlation
    vix_level: f64, // V8.9: VIX (Renamed from mock_vix in P1)
    
    #[serde(skip)]
    test_mode: bool, // Prevent disk IO during tests
}

impl RiskGate {
    fn new() -> Self {
        // P0: Try load from disk first
        if let Ok(gate) = RiskGate::load_from_disk() {
            println!("[GUARDIAN] 🛡️ RiskGate State Restored: Daily PnL={:.2}, VIX={:.2}", gate.daily_pnl, gate.vix_level);
            return gate;
        }

        RiskGate {
            daily_pnl: 0.0,
            max_daily_loss: 5000.0,  // -¥5000 daily limit
            max_lot_size: 0.1,       // Max 0.1 lot
            min_lot_size: 0.01,      // Min 0.01 lot
            brain_connected: true,
            wins: 0,
            losses: 0,
            total_win_amt: 0.0,
            total_loss_amt: 0.0,
            consecutive_losses: 0,
            daily_symbol_volume: HashMap::new(),
            open_positions: HashMap::new(),
            vix_level: 30.0, // P1: Default to Cautious (30.0) instead of Fake (20.0)
            test_mode: false,
        }
    }

    fn for_testing() -> Self {
        RiskGate {
            daily_pnl: 0.0,
            max_daily_loss: 5000.0,
            max_lot_size: 0.1,
            min_lot_size: 0.01,
            brain_connected: true,
            wins: 0,
            losses: 0,
            total_win_amt: 0.0,
            total_loss_amt: 0.0,
            consecutive_losses: 0,
            daily_symbol_volume: HashMap::new(),
            open_positions: HashMap::new(),
            vix_level: 30.0,
            test_mode: true,
        }
    }

    fn load_from_disk() -> std::io::Result<Self> {
        let file = File::open("data/memory/risk_state.json")?;
        let reader = BufReader::new(file);
        let gate = serde_json::from_reader(reader)?;
        Ok(gate)
    }

    fn save_to_disk(&self) {
        if self.test_mode { return; }
        
        // Ensure directory exists (best effort)
        let _ = std::fs::create_dir_all("data/memory");
        
        if let Ok(file) = File::create("data/memory/risk_state.json") {
            let _ = serde_json::to_writer_pretty(file, self);
        }
    }

    /// Phase 8: Maximum Risk Per Trade (Percent of Equity)
    /// Hardcap: 2% (Graham)
    fn check_max_risk_per_trade(&self, estimated_loss: f64) -> bool {
         // Assuming max_daily_loss is effectively 5% or similar, 
         // risk per trade typically max 1-2%.
         // Let's use hard limit for now: 2000 JPY (conservative) or relative?
         // Since safe_limit=5000 is daily limit.
         // Let's say max individual loss = 2000 (40% of daily buffer).
         estimated_loss <= 2000.0
    }

    /// Phase 8: Check Liquidity (Lot Hard Cap)
    fn check_liquidity(&self, lot: f64) -> bool {
         lot <= 5.0
    }

    /// V8.9: Update VIX (Naval's external data placeholder)
    fn update_vix(&mut self, vix_level: f64) {
        self.vix_level = vix_level;
        self.save_to_disk(); // Persist update
    }

    /// V8.9: Track open position for correlation penalty (Graham)
    fn add_position(&mut self, symbol: &str, lot: f64) {
        *self.open_positions.entry(symbol.to_string()).or_insert(0.0) += lot;
    }

    fn close_position(&mut self, symbol: &str, lot: f64) {
        if let Some(pos) = self.open_positions.get_mut(symbol) {
            *pos -= lot;
            if *pos <= 0.0 {
                self.open_positions.remove(symbol);
            }
            self.save_to_disk();
        }
    }

    /// V8.9: Calculate correlation penalty for correlated pairs
    fn correlation_penalty(&self, symbol: &str) -> f64 {
        // Define highly correlated pairs
        let correlated_pairs = [
            ("EURUSD", "GBPUSD"),
            ("EURUSD", "EURGBP"),
            ("USDJPY", "EURJPY"),
            ("AUDUSD", "NZDUSD"),
        ];

        let mut penalty = 1.0;
        for (sym1, sym2) in &correlated_pairs {
            if symbol == *sym1 && self.open_positions.contains_key(*sym2) {
                penalty *= 0.7; // 30% reduction
            } else if symbol == *sym2 && self.open_positions.contains_key(*sym1) {
                penalty *= 0.7;
            }
        }
        penalty
    }

    /// V8.9: VIX-based risk adjustment (Naval)
    fn vix_adjustment(&self) -> f64 {
        // Higher VIX = lower risk. Normal VIX ~15-20.
        // VIX > 30: High volatility, cut risk by 50%
        // VIX < 15: Low volatility, allow full risk
        if self.vix_level > 30.0 { 0.5 }
        else if self.vix_level > 25.0 { 0.7 }
        else { 1.0 }
    }
    
    fn update_pnl(&mut self, pnl: f64) {
        self.daily_pnl += pnl;
        if pnl > 0.0 { 
            self.wins += 1;
            self.total_win_amt += pnl;
            self.consecutive_losses = 0; // Reset streak
        }
        else if pnl < 0.0 { 
            self.losses += 1;
            self.total_loss_amt += pnl.abs();
            self.consecutive_losses += 1; // Increment streak
        }
        self.save_to_disk(); // Persist update
    }
    
    fn reset_daily(&mut self) {
        self.daily_pnl = 0.0;
        self.daily_symbol_volume.clear(); // Reset daily limits
        // Don't reset statistics (Kelly) or streak?
        // Streak is a "current state" thing, usually persists across days until a win?
        // Let's reset daily PnL but keep long-term stats.
        // But consecutive_losses technically might be better resetting daily?
        // Graham: "If 3 consecutive losses". Usually means "in a row".
        // Let's keep it persistent for now.
        
        self.save_to_disk(); // Persist daily reset
    }
    
    /// V8.5: Advisor P1 Refinements (Circuit Breaker)
    fn calculate_safe_lot(&self) -> f64 {
        // 1. Loss Streak Circuit Breaker (Graham)
        // If 3 or more consecutive losses, cut risk by 50%
        let streak_penalty = if self.consecutive_losses >= 3 { 0.5 } else { 1.0 };
        
        // 2. Drawdown Dampener (Taleb/Survival)
        // As daily_pnl approaches max_daily_loss, reduce risk towards zero.
        // E.g., if loss is -400 / -500 (80% used), dampener = 0.2.
        // If loss is -490 / -500 (98% used), dampener = 0.02.
        let loss_room = (self.max_daily_loss + self.daily_pnl).max(0.0);
        let dd_dampener = if loss_room < (self.max_daily_loss * 0.5) {
             loss_room / (self.max_daily_loss * 0.5)
        } else {
             1.0
        };

        if self.wins + self.losses < 10 {
             return self.max_lot_size * dd_dampener * streak_penalty; // Not enough sample, rely on dampener
        }
        
        // 3. True Half-Kelly Criterion (Graham/Edge)
        let total_trades = (self.wins + self.losses) as f64;
        let win_prob = self.wins as f64 / total_trades;
        let loss_prob = 1.0 - win_prob;
        
        let avg_win = if self.wins > 0 { self.total_win_amt / self.wins as f64 } else { 0.0 };
        let avg_loss = if self.losses > 0 { self.total_loss_amt / self.losses as f64 } else { 1.0 }; // Avoid div0
        
        let payoff_ratio = if avg_loss > 0.0 { avg_win / avg_loss } else { 1.0 };
        
        // Kelly = (bp - q) / b
        // b = payoff_ratio
        // p = win_prob
        // q = loss_prob
        let kelly = if payoff_ratio > 0.0 {
            (payoff_ratio * win_prob - loss_prob) / payoff_ratio
        } else {
            0.0
        };
        
        let half_kelly = kelly * 0.5;
        
        if half_kelly <= 0.0 {
            return self.min_lot_size; // No edge
        }
        
        // Simplify: Map Kelly % to Lot Size fraction of Max Lot
        // If Half-Kelly suggests risking 5% of bankroll, and Max Lot is ~1% risk,
        // we can probably go full Max Lot.
        // Let's constrain: Half-Kelly 10% = Max Lot. Linear scale below that.
        
        let kelly_factor = (half_kelly * 10.0).min(1.0).max(0.1);
        
        let safe_lot = self.max_lot_size * kelly_factor * dd_dampener * streak_penalty; // Apply streak penalty
        safe_lot.max(self.min_lot_size)
    }

    /// Returns Ok(approved_lot_size) or Err(VetoReason)
    /// Also updates internal state if approved? No, state update (PnL) happens on CLOSE.
    /// Does this update volume tracking? 
    /// Yes, checking an order implies "intent to execute". 
    /// But check_order is const &self... wait.
    /// I need &mut self to update daily_symbol_volume.
    /// Check the signature.
    /// Original check_order was &self. 
    /// I need to change check_order to &mut self OR use RefCell/Mutex inside (RiskGate is inside Mutex in main).
    /// In main: `match risk_gate.check_order(&order)`
    /// `risk_gate` is a `std::sync::MutexGuard`. MutexGuard allows &mut access.
    /// So risk_gate.check_order can be &mut self.
    fn check_order(&mut self, order: &TradeOrder) -> Result<f64, VetoReason> {
        // Phase 8: Liquidity Check (Hard Cap)
        if !self.check_liquidity(order.lot) {
            return Err(VetoReason::InvalidLotSize);
        }

        // Phase 8: Worst Case Loss Check
        if let Some(est_loss) = order.estimated_loss {
             if !self.check_max_risk_per_trade(est_loss) {
                 return Err(VetoReason::RiskPerTradeExceeded);
             }
        }
        // 0. Market Hours Check (Saturday/Sunday protection)
        // Skip this check during unit tests to ensure they pass regardless of when they run
        #[cfg(not(test))]
        {
            let now_local = chrono::Local::now();
            let weekday = now_local.weekday();
            // Close is Sat 06:50 JST. Open is Mon 06:00 JST.
            // Block if Sat > 07:00 OR Sunday OR Mon < 05:00
            if weekday == chrono::Weekday::Sun || 
               (weekday == chrono::Weekday::Sat && now_local.hour() >= 7) ||
               (weekday == chrono::Weekday::Mon && now_local.hour() < 5) { // Early Mon buffer
                return Err(VetoReason::MarketClosed);
            }
        }

        // 1. Brain must be connected
        if !self.brain_connected {
            return Err(VetoReason::BrainTimeout);
        }

        // 2. Daily Loss Limit (Graham)
        if self.daily_pnl <= -self.max_daily_loss {
             // Allow closing trades even if limit exceeded? 
             // Current TradeOrder types are BUY/SELL (entries). Exits are separate string cmds.
             // checks only valid for BUY/SELL actions
             if order.action == "BUY" || order.action == "SELL" {
                 return Err(VetoReason::DailyLimitExceeded);
             }
        }

        // 3. Lot Size Validations
        if order.lot < self.min_lot_size || order.lot > self.max_lot_size {
            return Err(VetoReason::InvalidLotSize);
        }
        
        // 4. Per-Symbol Concentration Limit (V8.6 -> V44.7: Relaxed for Sharpe-Top1)
        // Limit: Max 2.0 lot per symbol per day (was 0.5)
        let max_daily_symbol_lot = 2.0;
        let current_vol = *self.daily_symbol_volume.get(&order.symbol).unwrap_or(&0.0);
        
        if current_vol + order.lot > max_daily_symbol_lot {
            return Err(VetoReason::ConcentrationLimitExceeded);
        }
        
        // 5. Kelly Criterion Cap (Graham)
        let safe_cap = self.calculate_safe_lot();
        
        // 6. VIX Adjustment (Naval P2)
        let vix_adjusted_cap = safe_cap * self.vix_adjustment();
        
        // 7. Correlation Penalty (Graham P2)
        let correlation_adjusted_cap = vix_adjusted_cap * self.correlation_penalty(&order.symbol);
        
        let approved_lot = if order.lot > correlation_adjusted_cap {
            correlation_adjusted_cap
        } else {
            order.lot
        };
        
        // Approved! Update volume tracking
        // Note: This assumes every check_order results in a placed trade.
        // In main loop logic, if Ok() is returned, it sends to MT5.
        // However, MT5 might fail. But for risk accounting, we assume intent = exposure.
        // Also, if we capped the lot, we should track the CAPPED lot, not requested.
        
        // Re-check concentration with approved lot
        if current_vol + approved_lot > max_daily_symbol_lot {
             // If capping reduces it enough to pass?
             // Unlikely if limit is 0.5 and order is 0.1 but we are at 0.49.
             return Err(VetoReason::ConcentrationLimitExceeded);
        }
        
        *self.daily_symbol_volume.entry(order.symbol.clone()).or_insert(0.0) += approved_lot;
        
        // V8.9: Track open position for correlation
        self.add_position(&order.symbol, approved_lot);
        
        self.save_to_disk(); // Persist exposure update
        
        Ok(approved_lot)
    }
}

// Global tournament population
lazy_static::lazy_static! {
    static ref POPULATION: Mutex<Vec<tournament::RatedStrategy>> = {
        let mut pop = Vec::new();
        // V4.0: Initialize with DIVERSE strategies (not just SMA)
        for i in 0..61 {
            pop.push(tournament::RatedStrategy::random_new(i));
        }
        println!("🧬 Initial population: {} diverse strategies", pop.len());
        Mutex::new(pop)
    };
}

fn main() {
    // V9.0: Naval's Modularization - Standalone Backtest Mode
    let args: Vec<String> = std::env::args().collect();
    if args.contains(&"--backtest-only".to_string()) {
        run_backtest_only_mode();
        return;
    }

    println!("🦀 Guardian (Full Duplex + Backtester + Neural Net) is activating...");

    let context = Context::new();
    let _target_ip = "172.18.192.1"; // Windows IP

    // --- 1. DATA FLOW (MT5 -> Rust) ---
    // V15: bind instead of connect - allows multiple EAs to connect
    let sub_market = context.socket(zmq::SUB).unwrap();
    let addr_market = "tcp://*:5557";
    println!("🔌 Binding Market Data: {}", addr_market);
    sub_market.bind(addr_market).expect("Fail to bind MT5 Data");
    sub_market.set_subscribe(b"").unwrap();

    // --- 2. NERVOUS SYSTEM (Rust <-> Lisp) ---
    // [AFFERENT] Rust -> Lisp (Sensory Input)
    let push_to_brain = context.socket(zmq::PUSH).unwrap();
    push_to_brain.connect("tcp://localhost:5555").expect("Fail to connect Brain Sensory Nerve");
    println!("🧠 Connected to Brain Sensory Nerve (PUSH 5555)");

    // [EFFERENT] Lisp -> Rust (Motor Output)
    let sub_from_brain = context.socket(zmq::SUB).unwrap();
    sub_from_brain.connect("tcp://localhost:5556").expect("Fail to connect Brain Motor Nerve");
    sub_from_brain.set_subscribe(b"").unwrap();
    println!("👂 Listening to Brain Motor Nerve (SUB 5556)");

    // [EXTERNAL] Legacy Command Input (Port 5559)
    let sub_cmd = context.socket(zmq::SUB).unwrap();
    sub_cmd.bind("tcp://*:5559").expect("Fail to bind External Command");
    sub_cmd.set_subscribe(b"").unwrap();
    println!("👂 Listening for External Commands on port 5559");

    // [PUB] MT5へ命令を転送 (5560)
    let pub_to_mt5 = context.socket(zmq::PUB).unwrap();
    pub_to_mt5.bind("tcp://*:5560").expect("Fail to bind MT5 Command");
    println!("👊 Ready to punch commands to MT5 on port 5560");
    
    // [PUSH] Notifier Service (5562) - V15.3: Dead Man's Switch Notification
    let push_to_notifier = context.socket(zmq::PUSH).unwrap();
    push_to_notifier.connect("tcp://localhost:5562").expect("Fail to connect Notifier");
    println!("📢 Connected to Notifier Service (PUSH 5562)");

    println!("🧠 Neural Network: READY");

    println!("🚀 System All Green. Loop Start.");

    // V5.0: Heartbeat timer (every 10 seconds)
    let mut last_heartbeat = std::time::Instant::now();
    const HEARTBEAT_INTERVAL_SECS: u64 = 10;
    
    // V7.1: Dead Man's Switch (Fail-safe)
    let mut last_brain_msg = std::time::Instant::now();
    const BRAIN_TIMEOUT_SECS: u64 = 120;
    let mut brain_connected = true;

    // V7.2: Reflex Arc (Taleb's Spinal Cord)
    // Autonomous reaction to extreme volatility
    // V7.7++: Multi-timeframe support (Taleb's recommendation)
    struct Reflex {
        last_price: f64,
        last_tick_time: std::time::Instant,
        velocity_buffer: Vec<f64>,      // Tick-level (1-second)
        velocity_5min: Vec<f64>,        // 5-minute aggregated velocities
        velocity_1hour: Vec<f64>,       // 1-hour aggregated velocities
        tick_count: u64,                // For timeframe aggregation
        rally_confidence: f64,          // Accumulated confidence for aggressive action
    }
    let mut reflex = Reflex { 
        last_price: 0.0, 
        last_tick_time: std::time::Instant::now(),
        velocity_buffer: Vec::new(),
        velocity_5min: Vec::new(),
        velocity_1hour: Vec::new(),
        tick_count: 0,
        rally_confidence: 0.0,
    };
    const MAX_VELOCITY_THRESHOLD: f64 = 0.50; // JPY per second? 50 pips/sec is crash
    const PANIC_COOLDOWN_SECS: u64 = 300;
    const RALLY_CONFIDENCE_THRESHOLD: f64 = 3.0; // Need 3 confirmations for aggressive action
    let mut last_panic = std::time::Instant::now() - std::time::Duration::from_secs(PANIC_COOLDOWN_SECS);

    // V7.8: Naval Critique #1 - AutoLearner (Continuous Real-time Learning)
    // V7.8+: Naval improvements - Adaptive LR, Accuracy Tracking, Dynamic Ensemble
    struct AutoLearner {
        price_history: Vec<f64>,       // Last N prices for feature extraction
        last_train_tick: u64,          // Last tick we trained on
        micro_train_interval: u64,     // Train every N ticks
        total_micro_trains: u64,       // Counter for logging
        // V7.8+ Naval improvements
        current_volatility: f64,       // For adaptive learning rate
        last_predictions: Vec<(usize, usize)>, // (predicted, actual) pairs for accuracy
        accuracy_window: usize,        // Window for rolling accuracy
        // Naval Critique #2: Dynamic Ensemble Weights (Meritocracy)
        nn_accuracy: f64,              // Rolling accuracy for NN
        lstm_accuracy: f64,            // Rolling accuracy for LSTM
        ppo_accuracy: f64,             // Rolling accuracy for PPO
        nn_correct: u64,
        lstm_correct: u64,
        ppo_correct: u64,
        total_ensemble_votes: u64,
        // V7.8+++: LSTM sequence accumulation (Naval tech debt fix)
        lstm_sequence: Vec<[f64; 10]>, // Accumulate 20x10 sequence for LSTM
    }
    let mut auto_learner = AutoLearner {
        price_history: Vec::new(),
        last_train_tick: 0,
        micro_train_interval: 60,      // Micro-train every 60 ticks (~1 minute)
        total_micro_trains: 0,
        current_volatility: 1.0,
        last_predictions: Vec::new(),
        accuracy_window: 100,
        nn_accuracy: 0.5,              // Start with 50% (random)
        lstm_accuracy: 0.5,
        ppo_accuracy: 0.5,
        nn_correct: 0,
        lstm_correct: 0,
        ppo_correct: 0,
        total_ensemble_votes: 0,
        lstm_sequence: Vec::new(),
    };
    const AUTO_LEARN_HISTORY_SIZE: usize = 25;
    const LSTM_SEQ_LEN: usize = 20;

    // V8.0: Risk Gate - Guardian's Veto Power
    let mut risk_gate = RiskGate::new();
    let mut emergency_close_sent = false;  // Track if we already sent CLOSE_ALL

    // Phase 8: Audit Logger
    let audit_logger = audit::AuditLogger::new("guardian_audit.jsonl");

    // V15.4: Revival Loop Prevention (Taleb P2)
    // Track deaths to prevent infinite restart loops
    struct RevivalTracker {
        death_count: u32,
        first_death_time: std::time::Instant,
        revival_halted: bool, // If true, stop attempting auto-revival
    }
    let mut revival_tracker = RevivalTracker {
        death_count: 0,
        first_death_time: std::time::Instant::now(),
        revival_halted: false,
    };
    const REVIVAL_WINDOW_SECS: u64 = 300; // 5 minutes
    const MAX_DEATHS_IN_WINDOW: u32 = 2;  // 2 deaths = halt revival

    loop {
        let mut items = [
            sub_market.as_poll_item(POLLIN),
            sub_cmd.as_poll_item(POLLIN),
            sub_from_brain.as_poll_item(POLLIN),
        ];

        zmq::poll(&mut items, 10).unwrap();

        // V8.0: Saturday Auto-Close (Taleb)
        // Close all positions before weekend (06:50 JST Saturday)
        use chrono::Timelike;
        use chrono::Datelike;
        let now_local = chrono::Local::now();
        
        if now_local.weekday() == chrono::Weekday::Sat && 
           now_local.hour() == 6 && 
           now_local.minute() >= 50 && now_local.minute() < 55 {
               if !emergency_close_sent {
                   println!("🚨 WEEKEND CLOSE: Closing positions H4 and below (D1+ protected)");
                   let _ = pub_to_mt5.send("CLOSE_SHORT_TF", 0);
                   let _ = pub_to_mt5.send("CANCEL_ALL", 0);
                   emergency_close_sent = true;
                   
                   // Notify Brain
                   let veto_msg = serde_json::json!({
                       "type": "GUARDIAN_VETO",
                       "reason": "Weekend Auto-Close",
                       "action": "CLOSE_ALL"
                   });
                   let _ = push_to_brain.send(&veto_msg.to_string(), 0);
               }
        } else if !brain_connected {
            // Only reset flag if brain is connected OR if it's no longer the close window
            // If brain is disconnected (dead man switch), emergency_close_sent stays true
        } else {
             // Reset flag outside the window so it works next week
             if now_local.weekday() != chrono::Weekday::Sat || now_local.hour() != 6 {
                 // Only reset if we are NOT in emergency mode from Dead Man's Switch
                 if brain_connected {
                     emergency_close_sent = false;
                 }
             }
        }

        // 0. Fail-safe Check (Taleb) - V8.0 Enhanced with Auto-Close
        if last_brain_msg.elapsed().as_secs() > BRAIN_TIMEOUT_SECS {
            if brain_connected {
                println!("💀 CRITICAL: Brain Silence Detected (>{}s). Engaging Dead Man's Switch.", BRAIN_TIMEOUT_SECS);
                println!("🛑 SYSTEM STATUS: EMERGENCY MODE (New Entries Blocked)");
                brain_connected = false;
                risk_gate.brain_connected = false;
                
                // V15.3: ACTION 1 - Notify Discord (via Notifier Service)
                let webhook_url = std::env::var("SWIMMY_DISCORD_ALERTS").unwrap_or_default();
                if !webhook_url.is_empty() {
                    let alert_msg = serde_json::json!({
                        "webhook": webhook_url,
                        "data": {
                            "content": "@here", 
                            "embeds": [{
                                "title": "💀 BRAIN DEAD DETECTED 💀",
                                "description": format!("Brain silence > {}s. Engaging Emergency Protocols.", BRAIN_TIMEOUT_SECS),
                                "color": 16711680 // Red
                            }]
                        }
                    });
                    let _ = push_to_notifier.send(&alert_msg.to_string(), 0);
                    println!("📢 Alert sent to Discord: BRAIN DEAD");
                } else {
                    println!("⚠️ No SWIMMY_DISCORD_ALERTS set. Notification skipped.");
                }

                // V15.4: Auto-Revival with Loop Prevention (Graham P1, Taleb P2, Uncle Bob P3)
                // Track death count within 5-minute window
                let now = std::time::Instant::now();
                if now.duration_since(revival_tracker.first_death_time).as_secs() > REVIVAL_WINDOW_SECS {
                    // Window expired, reset counter
                    revival_tracker.death_count = 0;
                    revival_tracker.first_death_time = now;
                    revival_tracker.revival_halted = false;
                }
                revival_tracker.death_count += 1;

                if revival_tracker.death_count >= MAX_DEATHS_IN_WINDOW {
                    // Too many deaths, halt revival
                    if !revival_tracker.revival_halted {
                        revival_tracker.revival_halted = true;
                        println!("🛑 REVIVAL HALTED: {} deaths in {}s window. Manual intervention required.", 
                            revival_tracker.death_count, REVIVAL_WINDOW_SECS);
                        
                        // Send critical alert
                        if !webhook_url.is_empty() {
                            let halt_msg = serde_json::json!({
                                "webhook": webhook_url,
                                "data": {
                                    "content": "@here ⚠️ MANUAL INTERVENTION REQUIRED",
                                    "embeds": [{
                                        "title": "🛑 AUTO-REVIVAL HALTED",
                                        "description": format!("Brain died {} times in {} seconds. Auto-revival disabled to prevent loop. Please check manually.", 
                                            revival_tracker.death_count, REVIVAL_WINDOW_SECS),
                                        "color": 16711680 // Red
                                    }]
                                }
                            });
                            let _ = push_to_notifier.send(&halt_msg.to_string(), 0);
                        }
                    }
                } else {
                    // Safe to attempt revival
                    println!("🧟 Attempting to Revive Brain (death {}/{} in window)...", 
                        revival_tracker.death_count, MAX_DEATHS_IN_WINDOW);
                    
                    // V15.4 P1 (Graham): Use systemctl instead of pkill for safer lifecycle management
                    let restart_result = std::process::Command::new("systemctl")
                        .arg("--user")
                        .arg("restart")
                        .arg("swimmy-brain")
                        .output();
                    
                    // V15.4 P3 (Uncle Bob): Log the result
                    match restart_result {
                        Ok(output) => {
                            if output.status.success() {
                                println!("✅ systemctl restart swimmy-brain: SUCCESS");
                            } else {
                                println!("❌ systemctl restart failed: exit code {:?}", output.status.code());
                                println!("   stderr: {}", String::from_utf8_lossy(&output.stderr));
                            }
                        },
                        Err(e) => {
                            println!("❌ Failed to execute systemctl: {}", e);
                        }
                    }
                }
                
                // V8.0: Auto-close all positions on Brain timeout (Taleb's recommendation)
                if !emergency_close_sent {
                    println!("🚨 EMERGENCY CLOSE: Closing H4 and below (D1+ protected)");
                    let _ = pub_to_mt5.send("CLOSE_SHORT_TF", 0);
                    let _ = pub_to_mt5.send("CANCEL_ALL", 0);
                    emergency_close_sent = true;
                    
                    // Notify Brain (if it comes back instantly or via restart)
                    let veto_msg = serde_json::json!({
                        "type": "GUARDIAN_VETO",
                        "reason": "Brain timeout - emergency close executed",
                        "action": "CLOSE_ALL"
                    });
                    let _ = push_to_brain.send(&veto_msg.to_string(), 0);
                }
            }
        } else if !brain_connected {
            // If it was dead, and we received a message, it means it's ALIVE again
            println!("🧠 Brain signal restored. System Normal.");
            brain_connected = true;
            risk_gate.brain_connected = true;
            emergency_close_sent = false;  // Reset for next timeout
            
            // Notify recovery
            let webhook_url = std::env::var("SWIMMY_DISCORD_ALERTS").unwrap_or_default();
            if !webhook_url.is_empty() {
                let alert_msg = serde_json::json!({
                    "webhook": webhook_url,
                    "data": {
                        "embeds": [{
                            "title": "🧠 BRAIN REVIVED",
                            "description": "Signal restored. Resume normal operation.",
                            "color": 65280 // Green
                        }]
                    }
                });
                let _ = push_to_notifier.send(&alert_msg.to_string(), 0);
            }
        }

        // V5.0: Send heartbeat every 10 seconds
        if last_heartbeat.elapsed().as_secs() >= HEARTBEAT_INTERVAL_SECS {
            let hb_msg = r#"{"type":"HEARTBEAT","guardian":"alive"}"#;
            // push_to_brain.send(hb_msg, 0).unwrap(); // Use push now
            let _ = push_to_brain.send(hb_msg, 0); // Ignore error (e.g. if Brain down)
            last_heartbeat = std::time::Instant::now();
        }

        // 1. Market data (MT5 -> Lisp)
        if items[0].is_readable() {
            let msg = sub_market.recv_string(0).unwrap().unwrap();
            
            // Forward to Brain via PUSH
            let _ = push_to_brain.send(&msg, 0);

            // V8.0: PnL Integration (Graham)
            // Parse TRADE_CLOSED to update Daily PnL
            if msg.contains("TRADE_CLOSED") {
                 if let Ok(json) = serde_json::from_str::<serde_json::Value>(&msg) {
                     // Accept "profit" OR "pnl" (MT5 sends "pnl")
                     let profit_opt = json.get("profit").and_then(|v| v.as_f64())
                                      .or_else(|| json.get("pnl").and_then(|v| v.as_f64()));
                     
                     if let Some(profit) = profit_opt {
                         risk_gate.update_pnl(profit);
                         println!("💰 PNL UPDATE: {:.2} (Daily Total: {:.2})", profit, risk_gate.daily_pnl);
                         
                         // V8.9 FIX: Close position for correlation tracking
                         if let Some(symbol) = json.get("symbol").and_then(|v| v.as_str()) {
                             if let Some(lot) = json.get("lot").and_then(|v| v.as_f64()) {
                                 risk_gate.close_position(symbol, lot);
                                 println!("📊 Position closed: {} {:.2} lot", symbol, lot);
                             }
                         }
                     }
                 }
            }

            // V7.2: Reflex Arc Analysis
            // Try to parse JSON to get "close" price
            if let Ok(json) = serde_json::from_str::<serde_json::Value>(&msg) {
                if let Some(price) = json.get("close").and_then(|v| v.as_f64()) {
                    let now = std::time::Instant::now();
                    let dt = now.duration_since(reflex.last_tick_time).as_secs_f64();
                    
                    if dt > 0.0 && reflex.last_price > 0.0 {
                        // V8.10 FIX: GAP PROTECTION (Musk's Blind Spot)
                        // If dt > 60s (e.g. Weekend Gap), ignore the velocity spike and reset.
                        if dt > 60.0 {
                            println!("⚠️ GAP DETECTED: Time delta {:.1}s > 60s. Resetting Reflex Arc.", dt);
                            reflex.velocity_buffer.clear();
                            reflex.rally_confidence = 0.0;
                            // Update state without calculating velocity (prevent Panic Buy/Sell)
                            reflex.last_price = price;
                            reflex.last_tick_time = now;
                            continue;
                        }

                        let dp = price - reflex.last_price;  // Keep sign for direction
                        let velocity = dp / dt; // price change per second (signed)
                        
                        // V7.7+: Use DIRECTIONAL velocity for bidirectional detection
                        reflex.velocity_buffer.push(velocity);
                        if reflex.velocity_buffer.len() > 5 {
                            reflex.velocity_buffer.remove(0);
                        }
                        
                        // Calculate average directional velocity over buffer
                        let avg_velocity: f64 = if reflex.velocity_buffer.is_empty() {
                            0.0
                        } else {
                            reflex.velocity_buffer.iter().sum::<f64>() / reflex.velocity_buffer.len() as f64
                        };
                        
                        // V8.9 FIX: Update VIX from velocity variance (higher variance = higher VIX)
                        if reflex.velocity_buffer.len() >= 3 {
                            let variance: f64 = reflex.velocity_buffer.iter()
                                .map(|v| (v - avg_velocity).powi(2))
                                .sum::<f64>() / reflex.velocity_buffer.len() as f64;
                            // Map variance to VIX-like scale: low variance = 15, high = 40
                            let mock_vix = 15.0 + (variance.abs() * 1000.0).min(25.0);
                            risk_gate.update_vix(mock_vix);
                        }
                        
                        // V7.7++: Multi-timeframe aggregation
                        reflex.tick_count += 1;
                        
                        // Every 300 ticks (~5 minutes at 1 tick/sec), aggregate to 5min buffer
                        if reflex.tick_count.is_multiple_of(300) {
                            reflex.velocity_5min.push(avg_velocity);
                            if reflex.velocity_5min.len() > 12 { // Keep last hour of 5min data
                                reflex.velocity_5min.remove(0);
                            }
                        }
                        
                        // Every 3600 ticks (~1 hour), aggregate to 1hour buffer
                        if reflex.tick_count.is_multiple_of(3600) {
                            let avg_5min: f64 = if reflex.velocity_5min.is_empty() { 0.0 } 
                                else { reflex.velocity_5min.iter().sum::<f64>() / reflex.velocity_5min.len() as f64 };
                            reflex.velocity_1hour.push(avg_5min);
                            if reflex.velocity_1hour.len() > 24 { // Keep last day of hourly data
                                reflex.velocity_1hour.remove(0);
                            }
                        }
                        
                        // Check for SUSTAINED Crash (not just a single spike)
                        if avg_velocity < -MAX_VELOCITY_THRESHOLD && reflex.velocity_buffer.len() >= 3
                            && last_panic.elapsed().as_secs() > PANIC_COOLDOWN_SECS {
                                println!("⚡ REFLEX ARC (CRASH): Avg Velocity {:.4} < -{} (Sustained {} ticks)", 
                                    avg_velocity, MAX_VELOCITY_THRESHOLD, reflex.velocity_buffer.len());
                                println!("🚨 EMERGENCY STOP: Sustained Downward Volatility Detected");
                                let _ = pub_to_mt5.send("CLOSE_ALL", 0); 
                                let _ = pub_to_mt5.send("CANCEL_ALL", 0);
                                last_panic = now;
                                reflex.velocity_buffer.clear();
                                reflex.rally_confidence = 0.0; // Reset confidence on crash
                            }
                        
                        // V7.7++: Taleb Critique #3 - AGGRESSIVE RALLY with confidence accumulation
                        if avg_velocity > MAX_VELOCITY_THRESHOLD && reflex.velocity_buffer.len() >= 3 {
                            // Accumulate confidence with each sustained rally tick
                            reflex.rally_confidence += 1.0;
                            
                            println!("🚀 RALLY DETECTED: Avg Velocity {:.4} > {} (Confidence: {:.1}/{:.1})", 
                                avg_velocity, MAX_VELOCITY_THRESHOLD, reflex.rally_confidence, RALLY_CONFIDENCE_THRESHOLD);
                            
                            // When confidence threshold reached, take AGGRESSIVE action
                            if reflex.rally_confidence >= RALLY_CONFIDENCE_THRESHOLD
                                && last_panic.elapsed().as_secs() > PANIC_COOLDOWN_SECS {
                                    println!("🔥 REFLEX ARC (AGGRESSIVE RALLY): Confidence {:.1} reached!", reflex.rally_confidence);
                                    println!("📈 EXECUTING BUY: Strong upward momentum confirmed across {} ticks", reflex.velocity_buffer.len());
                                    
                                    // Send ACTUAL BUY command to MT5
                                    let buy_cmd = serde_json::json!({
                                        "action": "BUY",
                                        "symbol": "USDJPY",
                                        "lot": 0.01,
                                        "reason": "REFLEX_RALLY",
                                        "confidence": reflex.rally_confidence,
                                        "avg_velocity": avg_velocity
                                    });
                                    let _ = pub_to_mt5.send(&buy_cmd.to_string(), 0);
                                    
                                    // Also notify Brain
                                    let signal = serde_json::json!({
                                        "type": "REFLEX_RALLY_EXECUTED",
                                        "action": "BUY",
                                        "confidence": reflex.rally_confidence,
                                        "avg_velocity": avg_velocity,
                                        "price": price
                                    });
                                    let _ = push_to_brain.send(&signal.to_string(), 0);
                                    
                                    last_panic = now;
                                    reflex.velocity_buffer.clear();
                                    reflex.rally_confidence = 0.0;
                                }
                        } else {
                            // Decay confidence when rally not sustained
                            reflex.rally_confidence = (reflex.rally_confidence - 0.5).max(0.0);
                        }
                    }
                    reflex.last_price = price;
                    reflex.last_tick_time = now;
                    
                    // V7.8: Naval Critique #1 - Continuous Micro-Learning
                    // Automatically learn from each market tick
                    auto_learner.price_history.push(price);
                    if auto_learner.price_history.len() > AUTO_LEARN_HISTORY_SIZE {
                        auto_learner.price_history.remove(0);
                    }
                    
                    // V7.8++: Update volatility for adaptive learning rate
                    if auto_learner.price_history.len() >= 10 {
                        let returns: Vec<f64> = auto_learner.price_history.windows(2)
                            .map(|w| (w[1] - w[0]).abs() / w[0])
                            .collect();
                        let avg_return = returns.iter().sum::<f64>() / returns.len() as f64;
                        auto_learner.current_volatility = (avg_return * 1000.0).max(0.1).min(5.0);
                    }
                    
                    // Micro-train every N ticks when we have enough history
                    if reflex.tick_count - auto_learner.last_train_tick >= auto_learner.micro_train_interval 
                       && auto_learner.price_history.len() >= AUTO_LEARN_HISTORY_SIZE {
                        
                        // Extract features from price history
                        if let Some(features) = neural::extract_features_from_prices(&auto_learner.price_history) {
                            // Determine target from recent price movement
                            let recent_change = price - auto_learner.price_history[AUTO_LEARN_HISTORY_SIZE - 5];
                            let target = if recent_change > 0.05 { 0 }      // UP
                                         else if recent_change < -0.05 { 1 } // DOWN
                                         else { 2 };                         // FLAT
                            
                            // V7.8++: Adaptive learning rate based on volatility
                            // High volatility = lower LR (cautious), Low volatility = higher LR (confident)
                            let adaptive_lr = (0.02 / auto_learner.current_volatility).max(0.001).min(0.05);
                            
                            // Get NN prediction BEFORE training (for accuracy tracking)
                            let nn_pred = NEURAL_NET.lock().unwrap().predict(&features);
                            let nn_predicted = if nn_pred.up_prob > nn_pred.down_prob && nn_pred.up_prob > nn_pred.flat_prob { 0 }
                                              else if nn_pred.down_prob > nn_pred.flat_prob { 1 }
                                              else { 2 };
                            
                            // V7.8+++: LSTM sequence accumulation (Naval tech debt fix)
                            // Extend 5 features to 10 features for LSTM input
                            let extended_features: [f64; 10] = [
                                features[0], features[1], features[2], features[3], features[4],
                                auto_learner.current_volatility, 
                                if features[0] > 0.0 { 1.0 } else { -1.0 },  // trend direction
                                (reflex.tick_count % 100) as f64 / 100.0,   // normalized tick
                                auto_learner.nn_accuracy,                    // ensemble confidence
                                price / 150.0 - 1.0,                         // normalized price
                            ];
                            auto_learner.lstm_sequence.push(extended_features);
                            if auto_learner.lstm_sequence.len() > LSTM_SEQ_LEN {
                                auto_learner.lstm_sequence.remove(0);
                            }
                            
                            // Get LSTM prediction when we have full sequence
                            let lstm_predicted = if auto_learner.lstm_sequence.len() >= LSTM_SEQ_LEN {
                                let mut seq_array: [[f64; 10]; 20] = [[0.0; 10]; 20];
                                for (i, row) in auto_learner.lstm_sequence.iter().take(20).enumerate() {
                                    seq_array[i] = *row;
                                }
                                let lstm_pred = LSTM_NET.lock().unwrap().predict(&seq_array);
                                if lstm_pred.up_prob > lstm_pred.down_prob && lstm_pred.up_prob > lstm_pred.flat_prob { 0 }
                                else if lstm_pred.down_prob > lstm_pred.flat_prob { 1 }
                                else { 2 }
                            } else {
                                // Use hash until sequence is full
                                let hash: u64 = features.iter().fold(0u64, |acc, &f| acc.wrapping_add((f * 1000.0) as u64));
                                (hash % 3) as usize
                            };
                            
                            // V7.8+++: Get PPO prediction (for full ensemble tracking)
                            let ppo_predicted = {
                                let state: Vec<f64> = features.to_vec();
                                let (action, _) = PPO_AGENT.lock().unwrap().select_action(&state);
                                action
                            };
                            
                            // Train with adaptive learning rate
                            NEURAL_NET.lock().unwrap().train(&features, target, adaptive_lr);
                            
                            // V7.8+++: Track accuracy for ALL models
                            if nn_predicted == target { auto_learner.nn_correct += 1; }
                            if lstm_predicted == target { auto_learner.lstm_correct += 1; }
                            if ppo_predicted == target { auto_learner.ppo_correct += 1; }
                            auto_learner.total_ensemble_votes += 1;
                            
                            // V7.8+++: Configurable update frequency (every 10 samples for faster adaptation)
                            const ENSEMBLE_UPDATE_FREQ: u64 = 10;
                            if auto_learner.total_ensemble_votes.is_multiple_of(ENSEMBLE_UPDATE_FREQ) {
                                let votes = auto_learner.total_ensemble_votes as f64;
                                auto_learner.nn_accuracy = auto_learner.nn_correct as f64 / votes;
                                auto_learner.lstm_accuracy = auto_learner.lstm_correct as f64 / votes;
                                auto_learner.ppo_accuracy = auto_learner.ppo_correct as f64 / votes;
                                
                                // Calculate ensemble weights (Naval Critique #2: Meritocracy)
                                let total_acc = (auto_learner.nn_accuracy + auto_learner.lstm_accuracy + auto_learner.ppo_accuracy).max(0.01);
                                let nn_weight = auto_learner.nn_accuracy / total_acc;
                                let lstm_weight = auto_learner.lstm_accuracy / total_acc;
                                let ppo_weight = auto_learner.ppo_accuracy / total_acc;
                                
                                // Log every 50 updates (500 samples)
                                if auto_learner.total_ensemble_votes.is_multiple_of(50) {
                                    println!("📊 ENSEMBLE (Meritocracy): NN={:.0}%({:.0}%) LSTM={:.0}%({:.0}%) PPO={:.0}%({:.0}%)",
                                        nn_weight * 100.0, auto_learner.nn_accuracy * 100.0,
                                        lstm_weight * 100.0, auto_learner.lstm_accuracy * 100.0,
                                        ppo_weight * 100.0, auto_learner.ppo_accuracy * 100.0);
                                }
                            }
                            
                            auto_learner.last_train_tick = reflex.tick_count;
                            auto_learner.total_micro_trains += 1;
                            
                            // Log every 100 micro-trains with accuracy
                            if auto_learner.total_micro_trains.is_multiple_of(100) {
                                println!("🧠 AUTO-LEARN #{}: Accuracy={:.1}% Vol={:.2} LR={:.4}",
                                    auto_learner.total_micro_trains, 
                                    auto_learner.nn_accuracy * 100.0,
                                    auto_learner.current_volatility,
                                    adaptive_lr);
                            }
                        }
                    }
                }
            }
        }

        // 2. Existing Commands (External -> Rust) - Port 5559
        if items[1].is_readable() {
            let msg = sub_cmd.recv_string(0).unwrap().unwrap();

            
            if msg.contains("BACKTEST") || msg.starts_with('(') {
                let req_res: Result<BacktestRequest, String> = if msg.starts_with('(') {
                    serde_lexpr::from_str(&msg).map_err(|e| format!("S-Exp Parse Error: {}", e))
                } else {
                    serde_json::from_str(&msg).map_err(|e| format!("JSON Parse Error: {}", e))
                };

                match req_res {
                    Ok(req) => {
                        println!("📊 BACKTEST Request Received ({})", if msg.starts_with('(') { "S-Exp" } else { "JSON" });
                        
                        let (final_candles, final_aux) = if req.candles.is_empty() {
                            if let Some(path) = &req.candles_file {
                                match backtester::load_candles_from_csv(path) {
                                    Ok(c) => (c, req.aux_candles.clone()),
                                    Err(e) => {
                                        println!("❌ CSV Load Error (5559): {}", e);
                                        (Vec::new(), req.aux_candles.clone())
                                    }
                                }
                            } else {
                                (Vec::new(), req.aux_candles.clone())
                            }
                        } else {
                            (req.candles.clone(), req.aux_candles.clone())
                        };

                        let result = backtester::run_backtest(&req.strategy, &final_candles, &final_aux);
                        
                        let response_str = if msg.starts_with('(') {
                            format!("((type . backtest-result) (result . {}))", 
                                serde_lexpr::to_string(&result).unwrap())
                        } else {
                            serde_json::json!({
                                "type": "BACKTEST_RESULT",
                                "result": result
                            }).to_string()
                        };
                        
                        println!("📈 Result: {} ({}): Sharpe={:.2} (Adj:{:.2}), Trades={}, PnL={:.2}", 
                            req.strategy.name, 
                            if req.candles_file.is_some() { "CSV" } else { "Direct" },
                            result.sharpe, result.adjusted_sharpe, result.trades, result.pnl);
                        
                        let _ = push_to_brain.send(&response_str, 0);
                    }
                    Err(e) => {
                        if msg.contains("BACKTEST") || msg.starts_with('(') {
                            println!("❌ Command parse error: {}", e);
                        }
                    }
                }
            } 
            // P1: UPDATE_VIX command
            else if msg.contains("\"action\":\"UPDATE_VIX\"") {
                #[derive(Debug, Deserialize)]
                struct UpdateVixRequest {
                    action: String,
                    vix: f64,
                }
                match serde_json::from_str::<UpdateVixRequest>(&msg) {
                     Ok(req) => {
                         risk_gate.update_vix(req.vix);
                         println!("🛡️ VIX Updated to {:.2}", req.vix);
                     }
                     Err(e) => println!("❌ VIX update parse error: {}", e),
                }
            }
            // V5.0: WALK_FORWARD validation command
            else if msg.contains("\"action\":\"WALK_FORWARD\"") {
                match serde_json::from_str::<BacktestRequest>(&msg) {
                    Ok(req) => {
                        let strategy = backtester::Strategy {
                            name: req.strategy.name.clone(),
                            sma_short: req.strategy.sma_short,
                            sma_long: req.strategy.sma_long,
                            sl: req.strategy.sl,
                            tp: req.strategy.tp,
                            volume: req.strategy.volume,
                            indicator_type: req.strategy.indicator_type,  // V6.11
                            filter_enabled: false,
                            filter_tf: String::new(),
                            filter_period: 0,
                            filter_logic: String::new(),
                        };
                        let result = backtester::walk_forward_validate(&strategy, &req.candles, &req.aux_candles, 3, 0.7);
                        let response = serde_json::json!({
                            "type": "WALK_FORWARD_RESULT",
                            "result": result
                        });
                        
                        if result.is_overfit {
                            println!("⚠️ OVERFIT: {} (IS:{:.2} OOS:{:.2} Eff:{:.1}%)", 
                                result.strategy_name, result.in_sample_sharpe, 
                                result.out_of_sample_sharpe, result.efficiency_ratio * 100.0);
                        } else {
                            println!("✅ ROBUST: {} (Eff:{:.1}%)", result.strategy_name, result.efficiency_ratio * 100.0);
                        }
                        
                        let _ = push_to_brain.send(&response.to_string(), 0);
                    }
                    Err(e) => println!("❌ Walk-forward parse error: {}", e),
                }
            }
            // CLONE CHECK command
            else if msg.contains("\"action\":\"CHECK_CLONE\"") {
                println!("🧬 CLONE CHECK REQUEST RECEIVED");
                match serde_json::from_str::<CloneCheckRequest>(&msg) {
                    Ok(req) => {
                        let (is_clone, similar, sim) = backtester::check_clone(
                            &req.new_strategy, 
                            &req.existing_strategies, 
                            req.threshold
                        );
                        
                        let result = backtester::CloneCheckResult {
                            is_clone,
                            most_similar: similar.clone(),
                            similarity: sim,
                            threshold: req.threshold,
                            candidate_name: req.new_strategy.name.clone(),
                        };
                        
                        if is_clone {
                            println!("  🚫 CLONE detected! Similar to '{}' ({:.1}%)", similar, sim * 100.0);
                        } else {
                            println!("  ✅ UNIQUE strategy (max sim: {:.1}%)", sim * 100.0);
                        }
                        
                        let response = serde_json::json!({
                            "type": "CLONE_CHECK_RESULT",
                            "result": result
                        });
                        let _ = push_to_brain.send(&response.to_string(), 0);
                    }
                    Err(e) => println!("❌ Clone check parse error: {}", e),
                }
            }
            // PREDICT command (Neural Net + LSTM Ensemble)
            // V6.12: Added LSTM ensemble for improved prediction
            else if msg.contains("\"action\":\"PREDICT\"") {
                match serde_json::from_str::<PredictRequest>(&msg) {
                    Ok(req) => {
                        // Neural Net prediction
                        let nn_pred = if let Some(features) = neural::extract_features(&req.candles) {
                            let nn = NEURAL_NET.lock().unwrap();
                            Some(nn.predict(&features))
                        } else {
                            None
                        };
                        
                        // V6.12: LSTM prediction for ensemble
                        let lstm_pred = if let Some(sequence) = lstm::LstmNetwork::extract_sequence(&req.candles) {
                            let lstm_net = lstm::LstmNetwork::load_or_new();
                            Some(lstm_net.predict(&sequence))
                        } else {
                            None
                        };

                        // V7.2: PPO prediction (Naval's Fix: Integrate Zombie PPO)
                        let ppo_probs = if let Some(features) = neural::extract_features(&req.candles) {
                            let ppo = PPO_AGENT.lock().unwrap();
                            Some(ppo.get_action_probs(&features))
                        } else {
                            None
                        };
                        
                        // V7.2: Grand Ensemble (NN + LSTM + PPO)
                        if let Some(nn) = nn_pred {
                            let (mut up, mut down, mut flat) = (nn.up_prob, nn.down_prob, nn.flat_prob);
                            let mut voters = 1.0;
                            
                            // 1. Add LSTM Vote
                            if let Some(ref lstm) = lstm_pred {
                                up += lstm.up_prob;
                                down += lstm.down_prob;
                                flat += lstm.flat_prob;
                                voters += 1.0;
                            }
                            
                            // 2. Add PPO Vote
                            if let Some(ref ppo) = ppo_probs {
                                up += ppo[0];   // 0=BUY
                                down += ppo[1]; // 1=SELL
                                flat += ppo[2]; // 2=HOLD
                                voters += 1.0;
                            }
                            
                            // Average
                            up /= voters;
                            down /= voters;
                            flat /= voters;
                            
                            let (signal, conf) = if up > down && up > flat {
                                ("BUY", up)
                            } else if down > up && down > flat {
                                ("SELL", down)
                            } else {
                                ("HOLD", flat)
                            };
                            
                            println!("🧠 PREDICT (V7.2 Ensemble): {} ({:.1}%) | Voters: {} (NN, LSTM, PPO)",
                                signal, conf * 100.0, voters);
                                
                            let response = serde_json::json!({
                                "type": "PREDICTION",
                                "result": {
                                    "signal": signal,
                                    "confidence": conf,
                                    "up_prob": up,
                                    "down_prob": down,
                                    "flat_prob": flat,
                                    "ensemble_count": voters
                                }
                            });
                            let _ = push_to_brain.send(&response.to_string(), 0);
                        }
                    }
                    Err(e) => println!("❌ Predict parse error: {}", e),
                }
            }
            // TRAIN command (Neural Net online learning)
            else if msg.contains("\"action\":\"TRAIN\"") {
                match serde_json::from_str::<TrainRequest>(&msg) {
                    Ok(req) => {
                        if let Some(features) = neural::extract_features(&req.candles) {
                            // 1. Train Neural Net
                            let mut nn = NEURAL_NET.lock().unwrap();
                            nn.train(&features, req.target, 0.01);

                            // 2. Train PPO Agent (Naval's Fix: No more zombie code)
                            let mut ppo = PPO_AGENT.lock().unwrap();
                            // Supervised PPO Update:
                            // We treat the "target" (0,1,2) as the action we *should* have taken.
                            // We create a fake experience where taking this action led to positive reward.
                            
                            // 0=UP(BUY), 1=DOWN(SELL), 2=FLAT(HOLD)
                            let reward = 1.0; 
                            
                            // Need next_state (for now use current state, assuming Markov property holds enough for simple gradient)
                            let state = features.to_vec();
                            let next_state = features.to_vec();
                            
                            let exp = ppo::Experience {
                                state,
                                action: req.target,
                                reward,
                                next_state,
                                done: false,
                            };
                            
                            ppo.store_experience(exp);
                            ppo.update(); // Trigger update if buffer full (batch > 32)

                            println!("🎓 TRAIN (NN+PPO): target={} (0=UP,1=DOWN,2=FLAT)", req.target);
                        }
                    }
                    Err(e) => println!("❌ Train parse error: {}", e),
                }
            }
            // EVOLVE command (Tournament evolution)
            else if msg.contains("\"action\":\"EVOLVE\"") {
                match serde_json::from_str::<EvolveRequest>(&msg) {
                    Ok(req) => {
                        println!("🧬 EVOLVE REQUEST: {} rounds", req.rounds);
                        let mut pop = POPULATION.lock().unwrap();
                        
                        // Run evolution cycle
                        tournament::evolution_cycle(&mut pop, &req.candles, req.rounds);
                        
                        // Get top 10 strategies
                        let top: Vec<_> = pop.iter()
                            .take(10)
                            .map(|s| serde_json::json!({
                                "name": s.name,
                                "elo": s.elo,
                                "wins": s.wins,
                                "indicator_type": format!("{:?}", s.indicator_type),
                                "param_short": s.param_short,
                                "param_long": s.param_long,
                            }))
                            .collect();
                        
                        let response = serde_json::json!({
                            "type": "EVOLVE_RESULT",
                            "population_size": pop.len(),
                            "top_strategies": top
                        });
                        let _ = push_to_brain.send(&response.to_string(), 0);
                    }
                    Err(e) => println!("❌ Evolve parse error: {}", e),
                }
            }
            // MCTS command (Parameter optimization)
            else if msg.contains("\"action\":\"MCTS\"") {
                match serde_json::from_str::<MctsRequest>(&msg) {
                    Ok(req) => {
                        println!("🔍 MCTS REQUEST: {} iterations", req.iterations);
                        
                        let initial = mcts::StrategyParams {
                            sma_short: req.sma_short,
                            sma_long: req.sma_long,
                            sl: req.sl,
                            tp: req.tp,
                        };
                        
                        let (best_params, score) = mcts::optimize_strategy(
                            initial,
                            &req.candles,
                            req.iterations,
                        );
                        
                        let response = serde_json::json!({
                            "type": "MCTS_RESULT",
                            "best": {
                                "sma_short": best_params.sma_short,
                                "sma_long": best_params.sma_long,
                                "sl": best_params.sl,
                                "tp": best_params.tp,
                            },
                            "score": {
                                "composite": score.composite,
                                "sharpe": score.sharpe,
                                "win_rate": score.win_rate,
                                "profit_factor": score.profit_factor,
                                "max_drawdown": score.max_drawdown,
                                "trades": score.total_trades,
                            }
                        });
                        let _ = push_to_brain.send(&response.to_string(), 0);
                    }
                    Err(e) => println!("❌ MCTS parse error: {}", e),
                }
            }
            // V7.6: PRUNE command - Equilibrium-driven NN sparsification
            else if msg.contains("\"action\":\"PRUNE\"") {
                let mut nn = NEURAL_NET.lock().unwrap();
                let threshold = 0.01; // Default threshold
                let pruned = nn.prune_equilibrium(threshold);
                let sparsity = nn.get_sparsity();
                
                let response = serde_json::json!({
                    "type": "PRUNE_RESULT",
                    "pruned_weights": pruned,
                    "sparsity_percent": sparsity
                });
                let _ = push_to_brain.send(&response.to_string(), 0);
            }
            // V7.6: VAR command - Calculate Value at Risk
            else if msg.contains("\"action\":\"VAR\"") {
                match serde_json::from_str::<PredictRequest>(&msg) {
                    Ok(req) => {
                        let returns: Vec<f64> = req.candles.windows(2)
                            .map(|w| (w[0].close - w[1].close) / w[1].close)
                            .collect();
                        
                        let var_95 = backtester::calculate_var(&returns, 0.95);
                        let cvar_95 = backtester::calculate_cvar(&returns, 0.95);
                        
                        let response = serde_json::json!({
                            "type": "VAR_RESULT",
                            "var_95": var_95,
                            "cvar_95": cvar_95,
                            "sample_size": returns.len()
                        });
                        let _ = push_to_brain.send(&response.to_string(), 0);
                    }
                    Err(e) => println!("❌ VAR parse error: {}", e),
                }
            }
            // P12: CPCV_VALIDATE command - Run Combinatorial Purged Cross-Validation
            else if msg.contains("\"action\":\"CPCV_VALIDATE\"") || (msg.starts_with('(') && msg.contains("CPCV_VALIDATE")) {
                let req_res: Result<CpcvRequest, String> = if msg.starts_with('(') {
                    serde_lexpr::from_str(&msg).map_err(|e| format!("S-Exp Parse Error: {}", e))
                } else {
                    serde_json::from_str(&msg).map_err(|e| format!("JSON Parse Error: {}", e))
                };

                match req_res {
                    Ok(req) => {
                        println!("🔬 CPCV_VALIDATE: {} on {} ({})", req.strategy_name, req.symbol, if msg.starts_with('(') { "S-Exp" } else { "JSON" });
                        
                        // Call CPCV validation with file path
                        match cpcv::run_cpcv_validation(
                            &req.strategy_name,
                            &req.candles_file,
                            &req.strategy_params,
                        ) {
                            Ok(result) => {
                                // Determine pass/fail based on median Sharpe >= 0.5 (S-RANK)
                                let passed = result.median_sharpe >= 0.5 
                                    && result.passed_count >= (result.path_count / 2);
                                
                                let response_str = if msg.starts_with('(') {
                                    format!("((type . cpcv-result) (passed . {}) (median_sharpe . {:.3}) (median_pf . {:.3}) (median_wr . {:.3}) (median_maxdd . {:.3}) (std_sharpe . {:.3}) (path_count . {}) (passed_count . {}))", 
                                        passed, result.median_sharpe, result.median_pf, result.median_wr, result.median_maxdd, result.std_sharpe, result.path_count, result.passed_count)
                                } else {
                                    serde_json::json!({
                                        "type": "CPCV_RESULT",
                                        "strategy": req.strategy_name,
                                        "passed": passed,
                                        "median_sharpe": result.median_sharpe,
                                        "median_pf": result.median_pf,
                                        "median_wr": result.median_wr,
                                        "median_maxdd": result.median_maxdd,
                                        "std_sharpe": result.std_sharpe,
                                        "path_count": result.path_count,
                                        "passed_count": result.passed_count
                                    }).to_string()
                                };
                                println!("🔬 CPCV: {} => passed={} (sharpe={:.3})", 
                                    req.strategy_name, passed, result.median_sharpe);
                                let _ = push_to_brain.send(&response_str, 0);
                            }
                            Err(e) => {
                                let err_msg = format!("Validation failed: {}", e);
                                println!("❌ CPCV: {}", err_msg);
                                let response_str = if msg.starts_with('(') {
                                    format!("((type . cpcv-result) (passed . nil) (error . \"{}\"))", err_msg)
                                } else {
                                    serde_json::json!({
                                        "type": "CPCV_RESULT",
                                        "strategy": req.strategy_name,
                                        "passed": false,
                                        "error": e.to_string()
                                    }).to_string()
                                };
                                let _ = push_to_brain.send(&response_str, 0);
                            }
                        }
                    }
                    Err(e) => println!("❌ CPCV parse error: {}", e),
                }
            }
            // V7.6: FORGET command - Clear traumatic PPO experiences
            else if msg.contains("\"action\":\"FORGET\"") {
                let mut ppo = PPO_AGENT.lock().unwrap();
                let forgotten = ppo.forget_traumatic(-5.0, 500);
                let (len, avg, min) = ppo.get_buffer_stats();
                
                let response = serde_json::json!({
                    "type": "FORGET_RESULT",
                    "forgotten_experiences": forgotten,
                    "buffer_size": len,
                    "avg_reward": avg,
                    "min_reward": min
                });
                let _ = push_to_brain.send(&response.to_string(), 0);
            }
            // V7.6: LSTM_TRAIN command - Train LSTM network
            else if msg.contains("\"action\":\"LSTM_TRAIN\"") {
                match serde_json::from_str::<TrainRequest>(&msg) {
                    Ok(req) => {
                        if let Some(sequence) = lstm::LstmNetwork::extract_sequence(&req.candles) {
                            let mut lstm_net = lstm::LstmNetwork::load_or_new();
                            lstm_net.train_bptt(&sequence, req.target, 0.001);
                            let _ = lstm_net.save();
                            
                            println!("🧠 LSTM_TRAIN: BPTT complete, target={}", req.target);
                            
                            let response = serde_json::json!({
                                "type": "LSTM_TRAIN_RESULT",
                                "status": "ok",
                                "target": req.target
                            });
                            let _ = push_to_brain.send(&response.to_string(), 0);
                        }
                    }
                    Err(e) => println!("❌ LSTM_TRAIN parse error: {}", e),
                }
            }
            else {
                // Normal command - forward to MT5
                println!("⚡ COMMAND DETECTED: {}", msg);
                pub_to_mt5.send(&msg, 0).unwrap();
            }
        }
        // 3. Brain Signals (Lisp -> Rust) - Port 5556
        if items[2].is_readable() {
            // Heartbeat received or command received
            last_brain_msg = std::time::Instant::now();
            
            if let Ok(Ok(msg)) = sub_from_brain.recv_string(0) {
                // V7.13: S-Expression Support for Brain Commands
                let is_sexp = msg.starts_with('(');
                
                if msg.contains("REJECTED") {
                    println!("🛑 BRAIN VETO: Trade Rejected by Constitution!");
                    let _ = pub_to_mt5.send("CANCEL_LAST", 0);
                } else if msg.contains("\"action\":\"CACHE_DATA\"") || (is_sexp && msg.contains("CACHE_DATA")) {
                     let req_res: Result<CacheDataRequest, String> = if is_sexp {
                         serde_lexpr::from_str(&msg).map_err(|e| e.to_string())
                     } else {
                         serde_json::from_str(&msg).map_err(|e| e.to_string())
                     };
                     if let Ok(req) = req_res {
                         let mut cache = BACKTEST_DATA_CACHE.lock().unwrap();
                         cache.insert(req.data_id.clone(), (req.candles, req.aux_candles));
                         println!("✅ Data Cached: {} (S-Exp: {})", req.data_id, is_sexp);
                     }
                } else if msg.contains("\"action\":\"BACKTEST\"") || (is_sexp && msg.contains("BACKTEST")) {
                     let req_res: Result<BacktestRequest, String> = if is_sexp {
                         serde_lexpr::from_str(&msg).map_err(|e| e.to_string())
                     } else {
                         serde_json::from_str(&msg).map_err(|e| e.to_string())
                     };
                     
                     if let Ok(req) = req_res {
                         let run_logic = |candles: &Vec<backtester::Candle>, aux: &HashMap<String, Vec<backtester::Candle>>| {
                              let result = backtester::run_backtest(&req.strategy, candles, aux);
                              let response_str = if is_sexp {
                                  format!("((type . backtest-result) (result . {}))", 
                                      serde_lexpr::to_string(&result).unwrap())
                              } else {
                                  serde_json::json!({"type": "BACKTEST_RESULT", "result": result}).to_string()
                              };
                              let _ = push_to_brain.send(&response_str, 0);
                         };

                        if let Some(id) = &req.data_id {
                             let cache = BACKTEST_DATA_CACHE.lock().unwrap();
                             if let Some((c, a)) = cache.get(id) {
                                 if let Some(tf_min) = req.timeframe {
                                     let resampled = backtester::resample_candles(c, tf_min * 60);
                                     run_logic(&resampled, a);
                                 } else {
                                     run_logic(c, a);
                                 }
                             }
                        } else if let Some(path) = &req.candles_file {
                             if let Ok(c) = backtester::load_candles_from_csv(path) {
                                 run_logic(&c, &req.aux_candles);
                             }
                        } else if !req.candles.is_empty() {
                             run_logic(&req.candles, &req.aux_candles);
                        }
                     }
                } else if msg.contains("\"action\":\"CHECK_CLONE\"") {
                     // V7.10: Route CHECK_CLONE from Brain to backtester
                     println!("🧬 CLONE CHECK (from Brain)");
                     match serde_json::from_str::<CloneCheckRequest>(&msg) {
                         Ok(req) => {
                             let (is_clone, similar, sim) = backtester::check_clone(
                                 &req.new_strategy, 
                                 &req.existing_strategies, 
                                 req.threshold
                             );
                             
                             let result = backtester::CloneCheckResult {
                                 is_clone,
                                 most_similar: similar.clone(),
                                 similarity: sim,
                                 threshold: req.threshold,
                                 candidate_name: req.new_strategy.name.clone(),
                             };
                             
                             if is_clone {
                                 println!("🚫 CLONE: {} is ~{:.0}% similar to {}", 
                                     req.new_strategy.name, sim * 100.0, similar);
                             } else {
                                 println!("✅ UNIQUE: {} (most similar: {} at {:.0}%)", 
                                     req.new_strategy.name, similar, sim * 100.0);
                             }
                             
                             let response = serde_json::json!({
                                 "type": "CLONE_CHECK_RESULT",
                                 "result": result
                             });
                             let _ = push_to_brain.send(&response.to_string(), 0);
                         }
                         Err(e) => println!("❌ Clone check parse error: {}", e),
                     }
                } else if msg.contains("\"action\":\"LOAD_CSV\"") {
                     // V11.0: P7 Load CSV Command
                     println!("📂 LOAD_CSV Request");
                     match serde_json::from_str::<LoadCsvRequest>(&msg) {
                         Ok(req) => {
                             match backtester::load_candles_from_csv(&req.path) {
                                 Ok(candles) => {
                                      let count = candles.len();
                                      let mut cache = BACKTEST_DATA_CACHE.lock().unwrap();
                                      cache.insert(req.data_id.clone(), (candles, HashMap::new()));
                                      println!("✅ Loaded CSV: {} ({} bars) -> ID: {}", req.path, count, req.data_id);
                                 }
                                 Err(e) => println!("❌ CSV Load Error: {}", e),
                             }
                         }
                         Err(e) => println!("❌ Load CSV parse error: {}", e),
                     }
                } else if msg.contains("\"action\":\"PREDICT\"") {
                     // V7.10: Route PREDICT from Brain (already handled in items[1], forward for consistency)
                     // Neural Net prediction - delegate to existing handler logic
                     match serde_json::from_str::<PredictRequest>(&msg) {
                         Ok(req) => {
                             let nn_pred = if let Some(features) = neural::extract_features(&req.candles) {
                                 let nn = NEURAL_NET.lock().unwrap();
                                 Some(nn.predict(&features))
                             } else { None };
                             
                             if let Some(nn) = nn_pred {
                                 let response = serde_json::json!({
                                     "type": "PREDICTION",
                                     "up_prob": nn.up_prob,
                                     "down_prob": nn.down_prob,
                                     "flat_prob": nn.flat_prob,
                                     "signal": if nn.up_prob > nn.down_prob && nn.up_prob > nn.flat_prob { "BUY" }
                                               else if nn.down_prob > nn.up_prob && nn.down_prob > nn.flat_prob { "SELL" }
                                               else { "HOLD" }
                                 });
                                 let _ = push_to_brain.send(&response.to_string(), 0);
                             }
                         }
                         Err(e) => println!("❌ Predict parse error: {}", e),
                     }
                } else if msg.contains("\"action\":") {
                     // V8.0: RiskGate validation for all orders
                     // Parse as TradeOrder to validate, then forward if approved
                     if msg.contains("\"action\":\"BUY\"") || msg.contains("\"action\":\"SELL\"") {
                         match serde_json::from_str::<TradeOrder>(&msg) {
                             Ok(order) => {
                                match risk_gate.check_order(&order) {
                                    Ok(approved_lot) => {
                                        if approved_lot < order.lot {
                                            println!("⚠️ GUARDIAN OVERRIDE: Kelly Cap {:.2} -> {:.2}", order.lot, approved_lot);
                                            
                                            // Phase 8: Log Resized
                                            audit_logger.log(
                                                &format!("{} {} Req:{:.2} App:{:.2}", order.action, order.symbol, order.lot, approved_lot),
                                                "ADJUST",
                                                "Kelly Criterion Cap",
                                                &format!("DPnL:{:.2} Vol:{:.2}", risk_gate.daily_pnl, risk_gate.daily_symbol_volume.get(&order.symbol).unwrap_or(&0.0))
                                            );

                                            // Create modified order
                                            let mut modified_order = order.clone();
                                            modified_order.lot = approved_lot;
                                            
                                            // Serialize and send
                                            if let Ok(new_msg) = serde_json::to_string(&modified_order) {
                                                let _ = pub_to_mt5.send(&new_msg, 0);
                                                
                                                // Notify Brain
                                                let veto_msg = serde_json::json!({
                                                    "type": "GUARDIAN_OVERRIDE",
                                                    "original_lot": order.lot,
                                                    "new_lot": approved_lot,
                                                    "reason": "Kelly Criterion Cap"
                                                });
                                                let _ = push_to_brain.send(&veto_msg.to_string(), 0);
                                            }
                                        } else {
                                            // Order approved as is
                                            println!("✅ GUARDIAN APPROVED: {} {} lot={:.2}", order.action, order.symbol, order.lot);
                                            
                                            // Phase 8: Log Pass
                                            audit_logger.log(
                                                &format!("{} {} Lot:{:.2}", order.action, order.symbol, order.lot),
                                                "PASS",
                                                "Risk Checks Passed",
                                                 &format!("DPnL:{:.2}", risk_gate.daily_pnl)
                                            );
                                            
                                            let _ = pub_to_mt5.send(&msg, 0);
                                            
                                            // V8.10: Record for A/B testing
                                            let action_idx = match order.action.as_str() {
                                                "BUY" => 0,
                                                "SELL" => 1,
                                                _ => 2,
                                            };
                                            if let Ok(mut tracker) = SHADOW_TRACKER.lock() {
                                                // For now, ensemble and actual are same (no shadow predictions yet)
                                                // Future: compare with EnsemblePredictor suggestion
                                                tracker.record_decision(&order.symbol, action_idx, action_idx, 0.0);
                                            }
                                        }
                                    }
                                     Err(reason) => {
                                         // Order REJECTED by Guardian
                                         println!("🛑 GUARDIAN VETO: {} - Reason: {}", order.action, reason);
                                         
                                         // Phase 8: Log Veto
                                         audit_logger.log(
                                            &format!("{} {} Lot:{:.2}", order.action, order.symbol, order.lot),
                                            "VETO",
                                            &reason.to_string(),
                                            &format!("DPnL:{:.2}", risk_gate.daily_pnl)
                                         );
                                         
                                         // Notify Brain of rejection
                                         let veto_msg = serde_json::json!({
                                             "type": "GUARDIAN_VETO",
                                             "rejected_action": order.action,
                                             "symbol": order.symbol,
                                             "lot": order.lot,
                                             "reason": reason.to_string()
                                         });
                                         let _ = push_to_brain.send(&veto_msg.to_string(), 0);
                                     }
                                 }
                             }
                             Err(e) => {
                                 // Malformed order - reject
                                 println!("🛑 GUARDIAN VETO: Malformed order - {}", e);
                                 let veto_msg = serde_json::json!({
                                     "type": "GUARDIAN_VETO",
                                     "reason": format!("Malformed order: {}", e)
                                 });
                                 let _ = push_to_brain.send(&veto_msg.to_string(), 0);
                             }
                         }
                     } else {
                         // Non-trading actions (REQ_HISTORY, CLOSE, etc.) - forward directly
                         println!("🧠 BRAIN ACTION: {}", msg);
                         let _ = pub_to_mt5.send(&msg, 0);
                     }
                }
            }
        }
    }
}

// ═══════════════════════════════════════════════════
// V8.0: UNIT TESTS (Naval's Requirement)
// ═══════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_riskgate_init() {
        let gate = RiskGate::for_testing();
        assert_eq!(gate.daily_pnl, 0.0);
        assert_eq!(gate.brain_connected, true);
        assert_eq!(gate.total_win_amt, 0.0);
    }

    #[test]
    fn test_daily_limit_rejection() {
        let mut gate = RiskGate::for_testing();
        gate.daily_pnl = -5001.0; // Exceeds -5000 limit

        let order = TradeOrder {
            action: "BUY".to_string(),
            symbol: "USDJPY".to_string(),
            lot: 0.01,
            sl: None,
            tp: None,
            magic: None,
            comment: None,
            reason: None,
            estimated_loss: None,
        };

        match gate.check_order(&order) {
            Err(VetoReason::DailyLimitExceeded) => assert!(true),
            _ => panic!("Should reject due to daily limit"),
        }
    }

    #[test]
    fn test_lot_size_limit() {
        let mut gate = RiskGate::for_testing();
        
        let order_too_big = TradeOrder {
            action: "SELL".to_string(),
            symbol: "EURUSD".to_string(),
            lot: 0.2, // > 0.1 max
            sl: None,
            tp: None,
            magic: None,
            comment: None,
            reason: None,
            estimated_loss: None,
        };

        match gate.check_order(&order_too_big) {
            Err(VetoReason::InvalidLotSize) => assert!(true),
            _ => panic!("Should reject big lot"),
        }
    }

    #[test]
    fn test_brain_timeout_rejection() {
        let mut gate = RiskGate::new();
        gate.brain_connected = false;

        let order = TradeOrder {
            action: "BUY".to_string(),
            symbol: "USDJPY".to_string(),
            lot: 0.01,
            sl: None,
            tp: None,
            magic: None,
            comment: None,
            reason: None,
            estimated_loss: None,
        };

        match gate.check_order(&order) {
            Err(VetoReason::BrainTimeout) => assert!(true),
            _ => panic!("Should reject when brain disconnected"),
        }
    }

    // V8.4: Test True Kelly Capping with R:R
    #[test]
    fn test_kelly_capping() {
        let mut gate = RiskGate::for_testing();
        // Simulate bad performance: 10% win rate, but also poor R:R
        gate.wins = 1;
        gate.total_win_amt = 1000.0; // Avg Win = 1000
        gate.losses = 9; 
        gate.total_loss_amt = 9000.0; // Avg Loss = 1000. R:R = 1:1
        
        let safe_lot = gate.calculate_safe_lot();
        assert_eq!(safe_lot, gate.min_lot_size); // Should be min lot
        
        // Simulate good performance: 60% win rate, 2:1 R:R
        gate.wins = 6;
        gate.total_win_amt = 12000.0; // Avg Win = 2000
        gate.losses = 4;
        gate.total_loss_amt = 4000.0; // Avg Loss = 1000. R:R = 2:1
        // Payoff b = 2.0. Win p = 0.6. q = 0.4.
        // Kelly = (2 * 0.6 - 0.4) / 2 = 0.8 / 2 = 0.4 (40%)
        // Half-Kelly = 20%
        // Max lot is 0.1. 
        // We expect it to allow close to max lot.
        
        let safe_lot_good = gate.calculate_safe_lot();
        assert!(safe_lot_good > gate.min_lot_size);
    }
    
    // V8.4: Test Max Drawdown Override
    #[test]
    fn test_drawdown_override() {
        let mut gate = RiskGate::for_testing();
        // Good performance stats to justify high lot
        gate.wins = 60;
        gate.losses = 40;
        gate.total_win_amt = 60000.0;
        gate.total_loss_amt = 40000.0;
        
        // But Daily PnL is bad (-4500/5000 = 90% drawdown)
        gate.daily_pnl = -4500.0;
        
        let safe_lot = gate.calculate_safe_lot();
        
        // Should be dampened significantly. 
        // Loss room = 50. Buffer = 250.
        // Dampener = 50 / 250 = 0.2
        assert!(safe_lot < gate.max_lot_size * 0.3); // Expect < 30% of max
    }

    // V8.5: Test Loss Streak Circuit Breaker
    #[test]
    fn test_loss_streak_circuit_breaker() {
        let mut gate = RiskGate::for_testing();
        // 3 consecutive losses
        gate.update_pnl(-10.0);
        gate.update_pnl(-10.0);
        gate.update_pnl(-10.0);
        assert_eq!(gate.consecutive_losses, 3);
        
        // Even with good stats otherwise? (Hard to simulate good stats with 3 losses if start fresh)
        // Let's set stats manually to justify max lot
        gate.wins = 20;
        gate.losses = 3;  // The 3 we just added
        gate.total_win_amt = 40000.0; // Avg win 2000
        gate.total_loss_amt = 30.0;    // Avg loss 10. Payoff huge.
        
        // Without penalty, Kelly would be huge and limited by max_lot.
        // With penalty, should be 50% of whatever calculated cap, which is max_lot.
        
        let safe_lot = gate.calculate_safe_lot();
        assert!((safe_lot - gate.max_lot_size * 0.5).abs() < 0.001);
        
        // Single win resets streak
        gate.update_pnl(10.0);
        assert_eq!(gate.consecutive_losses, 0);
        let safe_lot_reset = gate.calculate_safe_lot();
        assert!((safe_lot_reset - gate.max_lot_size).abs() < 0.001);
    }

    // V8.6: Test Per-Symbol Concentration Limit (V44.7: updated to 2.0 lot)
    #[test]
    fn test_concentration_limit() {
        let mut gate = RiskGate::for_testing();
        // Max limit is 2.0 lot/day. Using 0.01 lot.
        // 199 orders = 1.99 lot (just under limit)
        let order = TradeOrder {
            action: "BUY".to_string(),
            symbol: "USDJPY".to_string(),
            lot: 0.01,
            sl: None, tp: None, magic: None, comment: None, reason: None, estimated_loss: None,
        };
        
        for i in 0..199 {
            match gate.check_order(&order) {
                Ok(_) => {},
                Err(e) => panic!("Order {} failed inside limit, got {}", i, e),
            }
        }
        
        // At 1.99 lot, next order (2.00) should fail
        match gate.check_order(&order) {
            Err(VetoReason::ConcentrationLimitExceeded) => {},
            Ok(_) => panic!("Should reject order at 2.0 lot"),
            Err(e) => panic!("Wrong error type: {}", e),
        }
        
        // Different symbol should pass
        let order_eur = TradeOrder {
            action: "BUY".to_string(),
            symbol: "EURUSD".to_string(),
            lot: 0.1, 
            sl: None, tp: None, magic: None, comment: None, reason: None, estimated_loss: None,
        };
        match gate.check_order(&order_eur) {
            Ok(_) => assert!(true),
            Err(e) => panic!("Different symbol should be allowed: {}", e),
        }
    }

    // Phase 10 P0: Dead-Man Heartbeat Timeout Test (Taleb)
    #[test]
    fn test_dead_man_timeout_logic() {
        // Verify that RiskGate correctly blocks orders when brain disconnected
        let mut gate = RiskGate::for_testing();
        
        // Initial state: brain connected
        assert!(gate.brain_connected);
        
        // Simulate Brain timeout
        gate.brain_connected = false;
        
        let order = TradeOrder {
            action: "BUY".to_string(),
            symbol: "USDJPY".to_string(),
            lot: 0.01,
            sl: None, tp: None, magic: None, comment: None, reason: None, estimated_loss: None,
        };
        
        // Order should be rejected due to Brain timeout
        match gate.check_order(&order) {
            Err(VetoReason::BrainTimeout) => assert!(true, "Correctly vetoed on Brain timeout"),
            Ok(_) => panic!("Should NOT pass order when Brain is disconnected"),
            Err(e) => panic!("Wrong veto reason: {}", e),
        }
        
        // Restore connection
        gate.brain_connected = true;
        
        // Order should now pass
        match gate.check_order(&order) {
            Ok(_) => assert!(true, "Order passes when Brain reconnected"),
            Err(e) => panic!("Should allow order after Brain reconnects: {}", e),
        }
    }
}

// ═══════════════════════════════════════════════════════════════
// V9.0: Naval's Modularization - Standalone Backtest Mode
// V10.3: Persistent Process with Cache (Expert Panel 2026-01-18)
// ═══════════════════════════════════════════════════════════════
fn run_backtest_only_mode() {
    use std::io::{self, BufRead, Write};
    use std::collections::HashMap;

    // V13: Native S-Expression Protocol Response
    #[derive(Serialize)]
    struct BacktestResponse {
        #[serde(rename = "type")]
        msg_type: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        result: Option<backtester::BacktestResult>,
        #[serde(skip_serializing_if = "Option::is_none")]
        error: Option<String>,
    }

    // In-memory cache for CSV data: Path -> Candles
    let mut candle_cache: HashMap<String, Vec<backtester::Candle>> = HashMap::new();
    
    eprintln!("🚀 Guardian Backtest Mode (S-Expression Protocol) Started");
    
    let stdin = io::stdin();
    for line_result in stdin.lock().lines() {
        let input = match line_result {
            Ok(line) => line,
            Err(_) => break, // EOF or error
        };
        
        if input.trim().is_empty() {
            continue;
        }

        // Parse Request directly into updated BacktestRequest
        // Lisp side should send: ((action . "BACKTEST") (strategy . (...)) ...)
        let request: BacktestRequest = match serde_lexpr::from_str(&input) {
            Ok(v) => v,
            Err(e) => {
                let resp = BacktestResponse {
                    msg_type: "ERROR".to_string(),
                    result: None,
                    error: Some(format!("S-Exp parse error: {}", e)),
                };
                println!("{}", serde_lexpr::to_string(&resp).unwrap());
                continue;
            }
        };

        // Resolve Candles (Cache Check)
        let mut raw_candles: Vec<backtester::Candle> = Vec::new();
        
        if let Some(path) = &request.candles_file {
            // Check cache first
            if let Some(cached) = candle_cache.get(path) {
                raw_candles = cached.clone();
            } else {
                // Load from file
                match load_candles_from_csv(path) {
                    Ok(c) => {
                        eprintln!("📂 Loaded/Cached {} candles from {}", c.len(), path);
                        candle_cache.insert(path.to_string(), c.clone());
                        raw_candles = c;
                    },
                    Err(e) => {
                         let resp = BacktestResponse {
                            msg_type: "ERROR".to_string(),
                            result: None,
                            error: Some(format!("CSV load error: {}", e)),
                        };
                        println!("{}", serde_lexpr::to_string(&resp).unwrap());
                        continue;
                    }
                }
            }
        } else if !request.candles.is_empty() {
             raw_candles = request.candles.clone();
        } else {
             // Error
            let resp = BacktestResponse {
                msg_type: "ERROR".to_string(),
                result: None,
                error: Some("No candles provided (file or raw)".to_string()),
            };
            println!("{}", serde_lexpr::to_string(&resp).unwrap());
            continue;
        }

        if raw_candles.is_empty() {
            let resp = BacktestResponse {
                msg_type: "ERROR".to_string(),
                result: None,
                error: Some("Empty candles".to_string()),
            };
            println!("{}", serde_lexpr::to_string(&resp).unwrap());
            continue;
        }

        // V9.2: MTF Resampling
        let timeframe = request.timeframe.unwrap_or(1) as u32;
        let mut working_candles = raw_candles;

        if timeframe > 1 {
            let tf_seconds = (timeframe as i64) * 60;
            working_candles = backtester::resample_candles(&working_candles, tf_seconds);
        }

        // Aux Candles
        let aux_candles = request.aux_candles.clone();

        // Run Backtest
        let result = backtester::run_backtest(&request.strategy, &working_candles, &aux_candles);

        // Output Result as S-Exp
        let output = BacktestResponse {
            msg_type: "BACKTEST_RESULT".to_string(),
            result: Some(result),
            error: None,
        };
        
        // Use println! to send valid S-Exp
        match serde_lexpr::to_string(&output) {
            Ok(s) => println!("{}", s),
            Err(e) => eprintln!("❌ Serialization Error: {}", e),
        }
        
        io::stdout().flush().unwrap(); // CRITICAL: Flush
    }
}