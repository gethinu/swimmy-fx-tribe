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
mod strategy_ast; // Phase 23: Native AST Protocol
mod cmd_route; // S-expression command routing (Brain -> MT5 vs internal)

use zmq::{Context, POLLIN};
use serde::{Deserialize, Serialize};
use std::sync::Mutex;
use std::collections::HashMap;
use chrono::{Datelike, Timelike}; // V9.1: Fix for weekday() and hour()
use std::fs::File;
use std::io::{BufReader, Write};

const ENDPOINT_MARKET_DATA: &str = "tcp://*:5557";
const ENDPOINT_BRAIN_PUSH: &str = "tcp://localhost:5555";
const ENDPOINT_BRAIN_SUB: &str = "tcp://localhost:5556";
const ENDPOINT_EXTERNAL_CMD: &str = "tcp://*:5559";
const ENDPOINT_MT5_CMD: &str = "tcp://*:5560";
const ENDPOINT_NOTIFIER: &str = "tcp://localhost:5562";

fn guardian_heartbeat_message() -> String {
    r#"((type . "HEARTBEAT") (source . "GUARDIAN") (status . "OK"))"#.to_string()
}

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
    // V20.0: Date Range Support
    #[serde(default)]
    start_time: Option<i64>,
    #[serde(default)]
    end_time: Option<i64>,
    // V9.1: File-based data loading to prevent OOM
    #[serde(default)]
    candles_file: Option<String>,
    #[serde(default)]
    aux_candles_files: Option<HashMap<String, String>>,
    // V31.0: Swap History
    #[serde(default)]
    pub swap_history: Vec<backtester::SwapRecord>,
    // When true, return per-trade PnL logs as `trade_list` in the backtest result.
    #[serde(default)]
    include_trades: bool,
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
    #[serde(default)]
    request_id: Option<String>,
}

#[derive(Debug, Serialize)]
struct CpcvResultPayload {
    strategy_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    request_id: Option<String>,
    median_sharpe: f64,
    median_pf: f64,
    median_wr: f64,
    median_maxdd: f64,
    path_count: usize,
    passed_count: usize,
    failed_count: usize,
    pass_rate: f64,
    is_passed: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

#[derive(Debug, Serialize)]
struct CpcvResponse {
    #[serde(rename = "type")]
    msg_type: String,
    result: CpcvResultPayload,
}

fn sexp_escape_string(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 2);
    out.push('"');
    for ch in input.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn cpcv_result_to_sexp(result: &CpcvResultPayload) -> String {
    let mut parts = Vec::new();
    parts.push(format!(
        "(strategy_name . {})",
        sexp_escape_string(&result.strategy_name)
    ));
    if let Some(request_id) = &result.request_id {
        parts.push(format!(
            "(request_id . {})",
            sexp_escape_string(request_id)
        ));
    }
    parts.push(format!("(median_sharpe . {})", result.median_sharpe));
    parts.push(format!("(median_pf . {})", result.median_pf));
    parts.push(format!("(median_wr . {})", result.median_wr));
    parts.push(format!("(median_maxdd . {})", result.median_maxdd));
    parts.push(format!("(path_count . {})", result.path_count));
    parts.push(format!("(passed_count . {})", result.passed_count));
    parts.push(format!("(failed_count . {})", result.failed_count));
    parts.push(format!("(pass_rate . {})", result.pass_rate));
    parts.push(format!(
        "(is_passed . {})",
        if result.is_passed { "t" } else { "nil" }
    ));
    if let Some(err) = &result.error {
        parts.push(format!("(error . {})", sexp_escape_string(err)));
    }
    format!("({})", parts.join(" "))
}

fn normalize_sexp_key(raw: &str) -> String {
    let mut key = raw.trim();
    if let Some((_, tail)) = key.rsplit_once("::") {
        key = tail;
    } else if let Some((_, tail)) = key.rsplit_once(':') {
        key = tail;
    }
    key.trim_start_matches(':').to_ascii_lowercase()
}

fn lexpr_key_to_string(val: &lexpr::Value) -> Option<String> {
    match val {
        lexpr::Value::Symbol(s) => Some(normalize_sexp_key(&s.to_string())),
        lexpr::Value::Keyword(s) => Some(normalize_sexp_key(&s.to_string())),
        lexpr::Value::String(s) => Some(normalize_sexp_key(s)),
        _ => None,
    }
}

fn lexpr_to_string(val: &lexpr::Value) -> Option<String> {
    match val {
        lexpr::Value::String(s) => Some(s.to_string()),
        lexpr::Value::Symbol(s) => Some(s.to_string()),
        lexpr::Value::Keyword(s) => Some(s.to_string()),
        _ => None,
    }
}

fn lexpr_to_json(val: &lexpr::Value) -> serde_json::Value {
    use lexpr::Value as L;
    match val {
        L::Null | L::Nil => serde_json::Value::Null,
        L::Bool(b) => serde_json::Value::Bool(*b),
        L::Number(n) => {
            if let Some(i) = n.as_i64() {
                serde_json::Value::Number(serde_json::Number::from(i))
            } else if let Some(u) = n.as_u64() {
                serde_json::Value::Number(serde_json::Number::from(u))
            } else if let Some(f) = n.as_f64() {
                serde_json::Number::from_f64(f)
                    .map(serde_json::Value::Number)
                    .unwrap_or(serde_json::Value::Null)
            } else {
                serde_json::Value::Null
            }
        }
        L::Char(c) => serde_json::Value::String(c.to_string()),
        L::String(s) => serde_json::Value::String(s.to_string()),
        L::Symbol(s) => serde_json::Value::String(s.to_string()),
        L::Keyword(s) => serde_json::Value::String(s.to_string()),
        L::Bytes(bytes) => serde_json::Value::Array(
            bytes.iter().map(|b| serde_json::Value::Number((*b as u64).into())).collect(),
        ),
        L::Vector(items) => serde_json::Value::Array(items.iter().map(lexpr_to_json).collect()),
        L::Cons(_) => {
            if let Some(items) = val.to_vec() {
                let all_cons = items.iter().all(|v| v.is_cons());
                if all_cons {
                    let mut map = serde_json::Map::new();
                    for item in items {
                        if let Some((car, cdr)) = item.as_pair() {
                            if let Some(key) = lexpr_key_to_string(car) {
                                map.insert(key, lexpr_to_json(cdr));
                            }
                        }
                    }
                    serde_json::Value::Object(map)
                } else {
                    serde_json::Value::Array(items.iter().map(lexpr_to_json).collect())
                }
            } else if let Some((car, cdr)) = val.as_pair() {
                serde_json::Value::Array(vec![lexpr_to_json(car), lexpr_to_json(cdr)])
            } else {
                serde_json::Value::Null
            }
        }
    }
}

fn lexpr_alist_to_map(val: &lexpr::Value) -> Result<std::collections::HashMap<String, lexpr::Value>, String> {
    let mut out = std::collections::HashMap::new();
    if matches!(val, lexpr::Value::Nil) {
        return Ok(out);
    }
    let list = val.to_vec().ok_or_else(|| "expected list for alist".to_string())?;
    for item in list {
        if let Some((car, cdr)) = item.as_pair() {
            if let Some(key) = lexpr_key_to_string(car) {
                out.insert(key, cdr.clone());
            }
        } else {
            return Err("expected cons cell in alist".to_string());
        }
    }
    Ok(out)
}

fn parse_cpcv_request_sexp(msg: &str) -> Result<CpcvRequest, String> {
    let val: lexpr::Value = lexpr::from_str(msg).map_err(|e| e.to_string())?;
    let map = lexpr_alist_to_map(&val)?;

    let action = map
        .get("action")
        .and_then(lexpr_to_string)
        .ok_or_else(|| "missing action".to_string())?;
    let strategy_name = map
        .get("strategy_name")
        .and_then(lexpr_to_string)
        .ok_or_else(|| "missing strategy_name".to_string())?;
    let symbol = map
        .get("symbol")
        .and_then(lexpr_to_string)
        .ok_or_else(|| "missing symbol".to_string())?;
    let candles_file = map
        .get("candles_file")
        .and_then(lexpr_to_string)
        .ok_or_else(|| "missing candles_file".to_string())?;
    let strategy_params = map
        .get("strategy_params")
        .map(lexpr_to_json)
        .unwrap_or(serde_json::Value::Null);
    let request_id = map.get("request_id").and_then(lexpr_to_string);

    Ok(CpcvRequest {
        action,
        strategy_name,
        symbol,
        candles_file,
        strategy_params,
        request_id,
    })
}

fn build_cpcv_response_str(msg: &str, is_sexp: bool) -> Result<String, String> {
    let req: CpcvRequest = if is_sexp {
        parse_cpcv_request_sexp(msg)?
    } else {
        serde_json::from_str(msg).map_err(|e| e.to_string())?
    };
    let result = build_cpcv_result(&req);
    if is_sexp {
        let result_sexp = cpcv_result_to_sexp(&result);
        Ok(format!("((type . \"CPCV_RESULT\") (result . {}))", result_sexp))
    } else {
        let response = CpcvResponse {
            msg_type: "CPCV_RESULT".to_string(),
            result,
        };
        serde_json::to_string(&response).map_err(|e| e.to_string())
    }
}

fn cpcv_payload_from_aggregate(
    strategy_name: &str,
    request_id: Option<String>,
    agg: &cpcv::CpcvAggregateResult,
) -> CpcvResultPayload {
    let path_count = agg.path_count;
    let passed_count = agg.passed_count;
    let failed_count = path_count.saturating_sub(passed_count);
    let pass_rate = if path_count > 0 {
        passed_count as f64 / path_count as f64
    } else {
        0.0
    };
    let is_passed = agg.median_sharpe >= 0.5 && pass_rate >= 0.5;

    CpcvResultPayload {
        strategy_name: strategy_name.to_string(),
        request_id,
        median_sharpe: agg.median_sharpe,
        median_pf: agg.median_pf,
        median_wr: agg.median_wr,
        median_maxdd: agg.median_maxdd,
        path_count,
        passed_count,
        failed_count,
        pass_rate,
        is_passed,
        error: None,
    }
}

fn build_cpcv_result(_req: &CpcvRequest) -> CpcvResultPayload {
    let request_id = _req.request_id.clone();
    match cpcv::run_cpcv_validation(&_req.strategy_name, &_req.candles_file, &_req.strategy_params) {
        Ok(agg) => cpcv_payload_from_aggregate(&_req.strategy_name, request_id, &agg),
        Err(e) => CpcvResultPayload {
            strategy_name: _req.strategy_name.clone(),
            request_id,
            median_sharpe: 0.0,
            median_pf: 0.0,
            median_wr: 0.0,
            median_maxdd: 0.0,
            path_count: 0,
            passed_count: 0,
            failed_count: 0,
            pass_rate: 0.0,
            is_passed: false,
            error: Some(e),
        },
    }
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
    let addr_market = ENDPOINT_MARKET_DATA;
    println!("🔌 Binding Market Data: {}", addr_market);
    sub_market.bind(addr_market).expect("Fail to bind MT5 Data");
    sub_market.set_subscribe(b"").unwrap();

    // --- 2. NERVOUS SYSTEM (Rust <-> Lisp) ---
    // [AFFERENT] Rust -> Lisp (Sensory Input)
    let push_to_brain = context.socket(zmq::PUSH).unwrap();
    push_to_brain.connect(ENDPOINT_BRAIN_PUSH).expect("Fail to connect Brain Sensory Nerve");
    println!("🧠 Connected to Brain Sensory Nerve (PUSH 5555)");

    // [EFFERENT] Lisp -> Rust (Motor Output)
    let sub_from_brain = context.socket(zmq::SUB).unwrap();
    sub_from_brain.connect(ENDPOINT_BRAIN_SUB).expect("Fail to connect Brain Motor Nerve");
    sub_from_brain.set_subscribe(b"").unwrap();
    println!("👂 Listening to Brain Motor Nerve (SUB 5556)");

    // [EXTERNAL] Legacy Command Input (Port 5559)
    let sub_cmd = context.socket(zmq::SUB).unwrap();
    sub_cmd.bind(ENDPOINT_EXTERNAL_CMD).expect("Fail to bind External Command");
    sub_cmd.set_subscribe(b"").unwrap();
    println!("👂 Listening for External Commands on port 5559");

    // [PUB] MT5へ命令を転送 (5560)
    let pub_to_mt5 = context.socket(zmq::PUB).unwrap();
    pub_to_mt5.bind(ENDPOINT_MT5_CMD).expect("Fail to bind MT5 Command");
    println!("👊 Ready to punch commands to MT5 on port 5560");
    
    // [PUSH] Notifier Service (5562) - V15.3: Dead Man's Switch Notification
    let push_to_notifier = context.socket(zmq::PUSH).unwrap();
    push_to_notifier.connect(ENDPOINT_NOTIFIER).expect("Fail to connect Notifier");
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
                    let payload = serde_json::json!({
                        "content": "@here",
                        "embeds": [{
                            "title": "💀 BRAIN DEAD DETECTED 💀",
                            "description": format!("Brain silence > {}s. Engaging Emergency Protocols.", BRAIN_TIMEOUT_SECS),
                            "color": 16711680 // Red
                        }]
                    });
                    let alert_msg = serde_json::json!({
                        "type": "NOTIFIER",
                        "schema_version": 1,
                        "action": "SEND",
                        "webhook": webhook_url,
                        "payload": payload
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
                            let payload = serde_json::json!({
                                "content": "@here ⚠️ MANUAL INTERVENTION REQUIRED",
                                "embeds": [{
                                    "title": "🛑 AUTO-REVIVAL HALTED",
                                    "description": format!("Brain died {} times in {} seconds. Auto-revival disabled to prevent loop. Please check manually.",
                                        revival_tracker.death_count, REVIVAL_WINDOW_SECS),
                                    "color": 16711680 // Red
                                }]
                            });
                            let halt_msg = serde_json::json!({
                                "type": "NOTIFIER",
                                "schema_version": 1,
                                "action": "SEND",
                                "webhook": webhook_url,
                                "payload": payload
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
                        .arg("restart")
                        .arg("swimmy-brain")
                        .output();
                    
                    // V15.4 P3 (Uncle Bob): Log the result
                    let restart_ok = match restart_result {
                        Ok(output) => {
                            if output.status.success() {
                                println!("✅ systemctl restart swimmy-brain: SUCCESS");
                                true
                            } else {
                                println!("❌ systemctl restart failed: exit code {:?}", output.status.code());
                                println!("   stderr: {}", String::from_utf8_lossy(&output.stderr));
                                false
                            }
                        },
                        Err(e) => {
                            println!("❌ Failed to execute systemctl: {}", e);
                            false
                        }
                    };

                    if !restart_ok {
                        let pid_result = std::process::Command::new("systemctl")
                            .arg("show")
                            .arg("-p")
                            .arg("MainPID")
                            .arg("--value")
                            .arg("swimmy-brain")
                            .output();

                        match pid_result {
                            Ok(output) => {
                                if output.status.success() {
                                    let pid_raw = String::from_utf8_lossy(&output.stdout);
                                    match pid_raw.trim().parse::<i32>() {
                                        Ok(pid) if pid > 0 => {
                                            println!("🪓 Fallback revive: SIGTERM MainPID={}", pid);
                                            let _ = std::process::Command::new("kill")
                                                .arg("-TERM")
                                                .arg(pid.to_string())
                                                .output();

                                            std::thread::sleep(std::time::Duration::from_secs(3));

                                            let still_alive = std::process::Command::new("kill")
                                                .arg("-0")
                                                .arg(pid.to_string())
                                                .status()
                                                .map(|s| s.success())
                                                .unwrap_or(false);

                                            if still_alive {
                                                println!("🪓 Fallback revive: MainPID still alive, SIGKILL MainPID={}", pid);
                                                let _ = std::process::Command::new("kill")
                                                    .arg("-KILL")
                                                    .arg(pid.to_string())
                                                    .output();
                                            }
                                        }
                                        _ => {
                                            println!("❌ Fallback revive: invalid MainPID value: {:?}", pid_raw.trim());
                                        }
                                    }
                                } else {
                                    println!("❌ Fallback revive: systemctl show MainPID failed: exit code {:?}", output.status.code());
                                    println!("   stderr: {}", String::from_utf8_lossy(&output.stderr));
                                }
                            }
                            Err(e) => {
                                println!("❌ Fallback revive: failed to execute systemctl show MainPID: {}", e);
                            }
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
                let payload = serde_json::json!({
                    "embeds": [{
                        "title": "🧠 BRAIN REVIVED",
                        "description": "Signal restored. Resume normal operation.",
                        "color": 65280 // Green
                    }]
                });
                let alert_msg = serde_json::json!({
                    "type": "NOTIFIER",
                    "schema_version": 1,
                    "action": "SEND",
                    "webhook": webhook_url,
                    "payload": payload
                });
                let _ = push_to_notifier.send(&alert_msg.to_string(), 0);
            }
        }

        // V5.0: Send heartbeat every 10 seconds
        if last_heartbeat.elapsed().as_secs() >= HEARTBEAT_INTERVAL_SECS {
            // push_to_brain.send(hb_msg, 0).unwrap(); // Use push now
            let hb_msg = guardian_heartbeat_message();
            let _ = push_to_brain.send(&hb_msg, 0); // Ignore error (e.g. if Brain down)
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

        // 2. Iron Rule SXP Protocol (Port 5559)
        if items[1].is_readable() {
            let msg = sub_cmd.recv_string(0).unwrap().unwrap();

            // 🛑 IRON RULE: ABOLISH JSON
            if !msg.starts_with('(') {
                println!("❌ Protocol Violation: JSON Rejected (Iron Rule). Msg: {:.50}...", msg);
                continue;
            }

            // MT5 execution protocol uses S-expressions (e.g., ORDER_OPEN/REQ_HISTORY).
            // If an MT5 command arrives on 5559, forward it directly to the EA bridge.
            if matches!(
                cmd_route::route_sexp_message(&msg),
                cmd_route::SexpRoute::ForwardToMt5
            ) {
                let _ = pub_to_mt5.send(&msg, 0);
                continue;
            }

            // SXP Dispatcher
            if msg.contains("CPCV_VALIDATE") {
                println!("🧪 CPCV_VALIDATE received (SXP)");
                match build_cpcv_response_str(&msg, true) {
                    Ok(response_str) => {
                        if !response_str.is_empty() {
                            match push_to_brain.send(&response_str, 0) {
                                Ok(_) => println!(
                                    "🧪 CPCV_RESULT sent to brain (SXP): {:.140}...",
                                    response_str
                                ),
                                Err(e) => println!("❌ CPCV_RESULT send error (SXP): {}", e),
                            }
                        }
                    }
                    Err(e) => println!("❌ CPCV_VALIDATE parse/serialize error: {}", e),
                }
            } else if msg.contains("\"BACKTEST\"") {
                match serde_lexpr::from_str::<BacktestRequest>(&msg) {
                    Ok(req) => {
                        println!("📊 BACKTEST Request (SXP/Zero-Copy)");

                        let (final_candles, final_aux) = if let Some(id) = &req.data_id {
                            // Phase 32: Zero-Copy Cache Logic
                            let mut cache = BACKTEST_DATA_CACHE.lock().unwrap();
                            if let Some((c, a)) = cache.get(id) {
                                println!("⚡ Cache HIT: {}", id);
                                (c.clone(), a.clone())
                            } else {
                                println!("🐢 Cache MISS: {}", id);
                                let (loaded_c, loaded_a) = if let Some(path) = &req.candles_file {
                                    match backtester::load_candles_from_csv(path) {
                                        Ok(c) => (c, req.aux_candles.clone()),
                                        Err(e) => {
                                            println!("❌ CSV Load Error: {}", e);
                                            (Vec::new(), req.aux_candles.clone())
                                        }
                                    }
                                } else {
                                    (req.candles.clone(), req.aux_candles.clone())
                                };

                                if !loaded_c.is_empty() {
                                    cache.insert(id.clone(), (loaded_c.clone(), loaded_a.clone()));
                                    println!("💾 Cached Data: {} ({} bars)", id, loaded_c.len());
                                }
                                (loaded_c, loaded_a)
                            }
                        } else {
                             // Legacy Fallback (SXP with inline candles)
                             (req.candles.clone(), req.aux_candles.clone())
                        };

                        let (working_candles, working_aux) = if let Some(tf_min) = req.timeframe {
                            if tf_min > 1 {
                                let resampled = backtester::resample_candles(&final_candles, tf_min * 60);
                                (resampled, final_aux)
                            } else {
                                (final_candles, final_aux)
                            }
                        } else {
                            (final_candles, final_aux)
                        };

                        let result = if req.include_trades {
                            backtester::run_backtest_with_trade_list(&req.strategy, &working_candles, &working_aux, &req.swap_history)
                        } else {
                            backtester::run_backtest(&req.strategy, &working_candles, &working_aux, &req.swap_history)
                        };
                        
                        // Response is ALWAYS SXP
                        let response_str = format!("((type . backtest-result) (result . {}))", 
                                serde_lexpr::to_string(&result).unwrap());
                        
                        println!("📈 Result: {} ({}): Sharpe={:.2} (Adj:{:.2}), Trades={}, PnL={:.2}", 
                            req.strategy.name, 
                            if req.data_id.is_some() { "Cached/File" } else { "Inline" },
                            result.sharpe, result.adjusted_sharpe, result.trades, result.pnl);
                        
                        let _ = push_to_brain.send(&response_str, 0);
                    }
                    Err(e) => println!("❌ Backtest SXP Error: {}", e),
                }

            } else if msg.contains("\"CHECK_CLONE\"") {
                println!("🧬 CLONE CHECK REQUEST (SXP)");
                match serde_lexpr::from_str::<CloneCheckRequest>(&msg) {
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
                        
                        let response_str = format!("((type . clone-check-result) (result . {}))", 
                                serde_lexpr::to_string(&result).unwrap());
                        let _ = push_to_brain.send(&response_str, 0);
                    }
                    Err(e) => println!("❌ Clone Check SXP Error: {}", e),
                }

            } else if msg.contains("\"UPDATE_CACHE\"") {
                 #[derive(Deserialize)]
                 struct UpdateCacheRequest {
                     action: String,
                     data_id: String
                 }
                 match serde_lexpr::from_str::<UpdateCacheRequest>(&msg) {
                     Ok(req) => {
                         let mut cache = BACKTEST_DATA_CACHE.lock().unwrap();
                         if cache.remove(&req.data_id).is_some() {
                             println!("🧹 Cache Cleared: {}", req.data_id);
                         } else {
                             println!("⚠️ Cache Key Not Found: {}", req.data_id);
                         }
                     },
                     Err(e) => println!("❌ Update Cache parse error: {}", e),
                 }

            } else if msg.contains("\"TRAIN\"") {
                 // SXP TRAIN Handler
                 match serde_lexpr::from_str::<TrainRequest>(&msg) {
                     Ok(req) => {
                         if let Some(features) = neural::extract_features(&req.candles) {
                             let mut nn = NEURAL_NET.lock().unwrap();
                             nn.train(&features, req.target, 0.01);

                             let mut ppo = PPO_AGENT.lock().unwrap();
                             let state = features.to_vec();
                             let next_state = features.to_vec(); // Simplified
                             let exp = ppo::Experience {
                                 state, action: req.target, reward: 1.0, next_state, done: false,
                             };
                             ppo.store_experience(exp);
                             ppo.update();
                             println!("🎓 TRAIN (SXP): target={}", req.target);
                         }
                     },
                     Err(e) => println!("❌ Train SXP Error: {}", e),
                 }

            } else if msg.contains("\"EVOLVE\"") {
                 // SXP EVOLVE Handler
                 match serde_lexpr::from_str::<EvolveRequest>(&msg) {
                     Ok(req) => {
                         let mut pop = POPULATION.lock().unwrap();
                         tournament::evolution_cycle(&mut pop, &req.candles, req.rounds);
                         let top: Vec<_> = pop.iter().take(10).map(|s| s.clone()).collect(); 
                         // Note: Cloning strategy for print/serialize might require Clone trait or manual construction
                         // Assuming we just print/ack for now to satisfy Iron Rule
                         println!("🧬 EVOLUTION COMPLETE (SXP)");
                         
                         // We need to match the expected return format if Lisp expects one
                         // Legacy sent "MCTS_RESULT" (unrelated?) or just updated population?
                         // Legacy sent push_to_brain at the end.
                     },
                     Err(e) => println!("❌ Evolve SXP Error: {}", e),
                 }
            } else if msg.contains("\"PREDICT\"") {
                // SXP PREDICT Handler
                match serde_lexpr::from_str::<PredictRequest>(&msg) {
                    Ok(req) => {
                        // Neural Net prediction
                        let nn_pred = if let Some(features) = neural::extract_features(&req.candles) {
                            let nn = NEURAL_NET.lock().unwrap();
                            Some(nn.predict(&features))
                        } else {
                            None
                        };
                        
                        // LSTM prediction
                        let lstm_pred = if let Some(sequence) = lstm::LstmNetwork::extract_sequence(&req.candles) {
                            let lstm_net = lstm::LstmNetwork::load_or_new();
                            Some(lstm_net.predict(&sequence))
                        } else {
                            None
                        };

                        // PPO prediction
                        let ppo_probs = if let Some(features) = neural::extract_features(&req.candles) {
                            let ppo = PPO_AGENT.lock().unwrap();
                            Some(ppo.get_action_probs(&features))
                        } else {
                            None
                        };
                        
                        // Grand Ensemble Logic
                        if let Some(nn) = nn_pred {
                            let (mut up, mut down, mut flat) = (nn.up_prob, nn.down_prob, nn.flat_prob);
                            let mut voters = 1.0;
                            
                            if let Some(ref lstm) = lstm_pred {
                                up += lstm.up_prob; down += lstm.down_prob; flat += lstm.flat_prob; voters += 1.0;
                            }
                            if let Some(ref ppo) = ppo_probs {
                                up += ppo[0]; down += ppo[1]; flat += ppo[2]; voters += 1.0;
                            }
                            up /= voters; down /= voters; flat /= voters;
                            
                            let (signal, conf) = if up > down && up > flat { ("BUY", up) } 
                                                 else if down > up && down > flat { ("SELL", down) } 
                                                 else { ("HOLD", flat) };
                            
                            println!("🧠 PREDICT (SXP): {} ({:.1}%) | Voters: {}", signal, conf * 100.0, voters);

                            // SXP Response
                            let response_str = format!("((type . prediction) (result . ((signal . \"{}\") (confidence . {:.4}) (voters . {}))))", 
                                signal, conf, voters);
                            let _ = push_to_brain.send(&response_str, 0);
                        }
                    },
                    Err(e) => println!("❌ Predict SXP Error: {}", e),
                }

            } else {
                println!("❌ Unknown/Unsupported SXP Command: {:.50}...", msg);
            }
        }


        // 3. Brain Signals (Lisp -> Rust) - Port 5556
        if items[2].is_readable() {
            // Heartbeat received or command received
            last_brain_msg = std::time::Instant::now();
            
            if let Ok(Ok(msg)) = sub_from_brain.recv_string(0) {
                // V7.13: S-Expression Support for Brain Commands
                let is_sexp = msg.starts_with('(');

                // Brain -> MT5 uses S-expression protocol; forward MT5 commands as-is.
                if is_sexp {
                    if let cmd_route::SexpRoute::ForwardToMt5 =
                        cmd_route::route_sexp_message(&msg)
                    {
                        let _ = pub_to_mt5.send(&msg, 0);
                        continue;
                    }
                }
                
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
                } else if msg.contains("\"action\":\"CPCV_VALIDATE\"") || (is_sexp && msg.contains("CPCV_VALIDATE")) {
                     match build_cpcv_response_str(&msg, is_sexp) {
                         Ok(response_str) => {
                             if !response_str.is_empty() {
                                 let _ = push_to_brain.send(&response_str, 0);
                             }
                         }
                         Err(e) => println!("❌ CPCV_VALIDATE parse/serialize error: {}", e),
                     }
                } else if msg.contains("\"action\":\"BACKTEST\"") || (is_sexp && msg.contains("BACKTEST")) {
                     let req_res: Result<BacktestRequest, String> = if is_sexp {
                         serde_lexpr::from_str(&msg).map_err(|e| e.to_string())
                     } else {
                        serde_json::from_str(&msg).map_err(|e| e.to_string())
                     };
                     
                     if let Ok(req) = req_res {
                          let run_logic = |candles: &Vec<backtester::Candle>, aux: &HashMap<String, Vec<backtester::Candle>>| {
                               let (working_candles, working_aux) = if let Some(tf_min) = req.timeframe {
                                   if tf_min > 1 {
                                       (backtester::resample_candles(candles, tf_min * 60), aux)
                                   } else {
                                       (candles.clone(), aux)
                                   }
                               } else {
                                   (candles.clone(), aux)
                               };

                               // V20.0: Date Range Slicing (Zero-Copy)
                               let start = req.start_time.unwrap_or(0);
                               let end = req.end_time.unwrap_or(i64::MAX);
                               
                               let start_idx = working_candles.partition_point(|c| c.timestamp < start);
                               let end_idx = working_candles.partition_point(|c| c.timestamp <= end);
                               
                               let slice = if start_idx < end_idx && end_idx <= working_candles.len() {
                                   &working_candles[start_idx..end_idx]
                               } else {
                                   &[]
                               };

                               let result = if req.include_trades {
                                   backtester::run_backtest_with_trade_list(&req.strategy, slice, working_aux, &req.swap_history)
                               } else {
                                   backtester::run_backtest(&req.strategy, slice, working_aux, &req.swap_history)
                               };
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
                                 run_logic(c, a);
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
    fn test_cpcv_validate_handler_error_for_missing_file() {
        let req = CpcvRequest {
            action: "CPCV_VALIDATE".to_string(),
            strategy_name: "UT-CPCV".to_string(),
            symbol: "USDJPY".to_string(),
            candles_file: "/tmp/does-not-exist.csv".to_string(),
            strategy_params: serde_json::json!({}),
            request_id: None,
        };

        let result = build_cpcv_result(&req);

        assert_eq!(result.strategy_name, "UT-CPCV");
        assert_eq!(result.path_count, 0);
        assert_eq!(result.passed_count, 0);
        assert!(!result.is_passed);
        assert!(result.error.is_some());
    }

    #[test]
    fn test_cpcv_payload_success() {
        let agg = cpcv::CpcvAggregateResult {
            median_sharpe: 0.75,
            median_pf: 1.3,
            median_wr: 55.0,
            median_maxdd: 0.12,
            std_sharpe: 0.2,
            path_count: 10,
            passed_count: 6,
        };

        let payload = cpcv_payload_from_aggregate("UT-CPCV-SUCCESS", None, &agg);

        assert_eq!(payload.strategy_name, "UT-CPCV-SUCCESS");
        assert!((payload.median_pf - agg.median_pf).abs() < 1e-9);
        assert!((payload.median_wr - agg.median_wr).abs() < 1e-9);
        assert!((payload.median_maxdd - agg.median_maxdd).abs() < 1e-9);
        assert_eq!(payload.path_count, 10);
        assert_eq!(payload.passed_count, 6);
        assert_eq!(payload.failed_count, 4);
        assert!((payload.pass_rate - 0.6).abs() < 1e-9);
        assert!(payload.is_passed);
        assert!(payload.error.is_none());
    }

    #[test]
    fn test_cpcv_result_sexp_includes_medians() {
        let payload = CpcvResultPayload {
            strategy_name: "UT-CPCV-MEDIAN".to_string(),
            request_id: Some("RID-UT".to_string()),
            median_sharpe: 0.55,
            median_pf: 1.6,
            median_wr: 0.52,
            median_maxdd: 0.11,
            path_count: 5,
            passed_count: 3,
            failed_count: 2,
            pass_rate: 0.6,
            is_passed: true,
            error: None,
        };

        let sexp = cpcv_result_to_sexp(&payload);

        assert!(sexp.contains("median_pf"), "sexp should include median_pf");
        assert!(sexp.contains("median_wr"), "sexp should include median_wr");
        assert!(sexp.contains("median_maxdd"), "sexp should include median_maxdd");
    }

    #[test]
    fn test_build_cpcv_response_str_sexp_missing_file() {
        let msg = r#"((action . "CPCV_VALIDATE")
                      (strategy_name . "UT-CPCV-EXT")
                      (symbol . "USDJPY")
                      (candles_file . "/tmp/does-not-exist.csv")
                      (strategy_params . NIL))"#;

        let response = build_cpcv_response_str(msg, true).expect("response should serialize");

        assert!(response.contains("CPCV_RESULT"), "response should be CPCV_RESULT");
        assert!(response.contains("UT-CPCV-EXT"), "response should include strategy name");
    }

    #[test]
    fn test_build_cpcv_response_str_sexp_includes_request_id() {
        let msg = r#"((action . "CPCV_VALIDATE")
                      (strategy_name . "UT-CPCV-REQ")
                      (symbol . "USDJPY")
                      (candles_file . "/tmp/does-not-exist.csv")
                      (request_id . "RID-123")
                      (strategy_params . NIL))"#;

        let response = build_cpcv_response_str(msg, true).expect("response should serialize");

        assert!(response.contains("CPCV_RESULT"), "response should be CPCV_RESULT");
        assert!(response.contains("RID-123"), "response should include request_id");
    }

    #[test]
    fn test_build_cpcv_response_str_sexp_accepts_package_qualified_keys() {
        let msg = r#"((swimmy.school::action . "CPCV_VALIDATE")
                      (swimmy.school::strategy_name . "UT-CPCV-PKG")
                      (swimmy.school::symbol . "USDJPY")
                      (swimmy.school::candles_file . "/tmp/does-not-exist.csv")
                      (swimmy.school::request_id . "RID-PKG")
                      (swimmy.school::strategy_params . NIL))"#;

        let response = build_cpcv_response_str(msg, true).expect("response should serialize");

        assert!(
            response.contains("CPCV_RESULT"),
            "response should be CPCV_RESULT"
        );
        assert!(
            response.contains("UT-CPCV-PKG"),
            "response should include strategy name"
        );
        assert!(
            response.contains("RID-PKG"),
            "response should include request_id"
        );
    }

    #[test]
    fn heartbeat_is_sexp() {
        let msg = guardian_heartbeat_message();
        assert!(msg.starts_with('('), "heartbeat must be S-expression");
        assert!(msg.contains("(type . \"HEARTBEAT\")"), "missing type field");
        assert!(msg.contains("(source . \"GUARDIAN\")"), "missing source field");
        assert!(msg.contains("(status . \"OK\")"), "missing status field");
    }

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

    #[test]
    fn test_revival_does_not_use_systemctl_user_mode() {
        // In this deployment, Swimmy services run under system (not user) systemd.
        // `systemctl --user` is expected to fail (no user bus), so guard against regressions.
        let src_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("main.rs");
        let src = std::fs::read_to_string(&src_path)
            .unwrap_or_else(|e| panic!("failed to read {}: {}", src_path.display(), e));

        let needle = format!(".arg(\"{}\")", "--user");
        assert!(
            !src.contains(&needle),
            "guardian must not call systemctl in --user mode in auto-revival"
        );
    }

    #[test]
    fn test_revival_has_mainpid_kill_fallback() {
        // Auto-revival should tolerate systemctl restart failures (polkit/no tty)
        // by falling back to MainPID-based kill so systemd Restart= can relaunch.
        let src_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("main.rs");
        let src = std::fs::read_to_string(&src_path)
            .unwrap_or_else(|e| panic!("failed to read {}: {}", src_path.display(), e));

        let start = src.find("Attempting to Revive Brain")
            .expect("revival block start marker must exist");
        let end = src[start..]
            .find("EMERGENCY CLOSE")
            .map(|i| start + i)
            .expect("revival block end marker must exist");
        let revival_block = &src[start..end];

        assert!(
            revival_block.contains("show") && revival_block.contains("MainPID") && revival_block.contains("--value"),
            "guardian auto-revival must query MainPID via systemctl show"
        );
        assert!(
            revival_block.contains("-TERM"),
            "guardian auto-revival must send SIGTERM fallback to MainPID"
        );
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
        let result = if request.include_trades {
            backtester::run_backtest_with_trade_list(&request.strategy, &working_candles, &aux_candles, &request.swap_history)
        } else {
            backtester::run_backtest(&request.strategy, &working_candles, &aux_candles, &request.swap_history)
        };

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
