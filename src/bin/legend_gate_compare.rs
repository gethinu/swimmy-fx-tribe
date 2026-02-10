#[path = "../backtester.rs"]
mod backtester;
#[path = "../strategy_ast.rs"]
mod strategy_ast;

use backtester::{
    load_candles_from_csv, run_backtest_with_gate_and_slippage, run_backtest_with_slippage,
    GateDecision, IndicatorType, Strategy,
};
use strategy_ast::StrategyNode;

use std::collections::HashMap;
use std::path::Path;

const SHORT_TREND: usize = 5;
const LONG_TREND: usize = 50;
const BLEND_ALPHA: f64 = 0.7;
const VOL_PERIOD: usize = 20;
const VOL_LOW: f64 = 0.5;
const KALMAN_Q: f64 = 0.01;
const KALMAN_R: f64 = 0.1;
const KALMAN_VEL_ADJ: f64 = 0.1;
const KALMAN_VEL_DECAY: f64 = 0.95;
const KALMAN_THRESH: f64 = 0.0001;

fn pip_size(pair: &str) -> f64 {
    if pair.ends_with("JPY") {
        0.01
    } else {
        0.0001
    }
}

fn resolve_data_dir(arg: Option<&str>) -> String {
    if let Some(v) = arg {
        return v.to_string();
    }
    if Path::new("data/historical").is_dir() {
        "data/historical".to_string()
    } else {
        "../data/historical".to_string()
    }
}

fn kalman_velocity_series(closes: &[f64]) -> Vec<f64> {
    let n = closes.len();
    let mut vels = vec![0.0; n];
    if n == 0 {
        return vels;
    }
    let mut x = closes[0];
    let mut v = 0.0;
    let mut p_x = 1.0;
    let mut p_v = 1.0;
    for i in 1..n {
        let z = closes[i];
        let x_pred = x + v;
        let v_pred = v;
        let p_x_pred = p_x + p_v + KALMAN_Q;
        let p_v_pred = p_v + KALMAN_Q;
        let k_x = p_x_pred / (p_x_pred + KALMAN_R);
        let innovation = z - x_pred;
        x = x_pred + k_x * innovation;
        v = v_pred + KALMAN_VEL_ADJ * innovation;
        p_x = (1.0 - k_x) * p_x_pred;
        p_v = KALMAN_VEL_DECAY * p_v_pred;
        vels[i] = v;
    }
    vels
}

fn trend_strength_series(closes: &[f64], period: usize) -> Vec<Option<f64>> {
    let n = closes.len();
    let mut out = vec![None; n];
    if period <= 1 {
        return out;
    }
    for i in (period - 1)..n {
        let first_price = closes[i + 1 - period];
        let last_price = closes[i];
        let change = last_price - first_price;
        let avg_price = (first_price + last_price) / 2.0;
        if avg_price > 0.0 {
            let mut val = change / (avg_price * 0.1);
            if val > 1.0 {
                val = 1.0;
            }
            if val < -1.0 {
                val = -1.0;
            }
            out[i] = Some(val);
        } else {
            out[i] = Some(0.0);
        }
    }
    out
}

fn realized_volatility_series(closes: &[f64], period: usize) -> Vec<Option<f64>> {
    let n = closes.len();
    let mut out = vec![None; n];
    if n < 2 || period == 0 {
        return out;
    }
    let mut returns = vec![0.0; n];
    for i in 1..n {
        let prev = closes[i - 1];
        returns[i] = if prev == 0.0 { 0.0 } else { 100.0 * (closes[i] - prev) / prev };
    }
    for i in period..n {
        let start = i + 1 - period;
        let window = &returns[start..=i];
        let mean = window.iter().sum::<f64>() / period as f64;
        let var = window.iter().map(|r| (r - mean).powi(2)).sum::<f64>() / period as f64;
        out[i] = Some(var.sqrt());
    }
    out
}

fn model_gate_predict(candles: &[backtester::Candle]) -> Vec<GateDecision> {
    let closes: Vec<f64> = candles.iter().map(|c| c.close).collect();
    let n = closes.len();
    let mut preds = vec![GateDecision::Hold; n];
    if n == 0 {
        return preds;
    }

    let vels = kalman_velocity_series(&closes);
    let mut kalman_dir = vec![0i8; n]; // -1 DOWN, 0 FLAT, +1 UP
    for (i, v) in vels.iter().enumerate() {
        if *v > KALMAN_THRESH {
            kalman_dir[i] = 1;
        } else if *v < -KALMAN_THRESH {
            kalman_dir[i] = -1;
        }
    }

    let short_ts = trend_strength_series(&closes, SHORT_TREND);
    let long_ts = trend_strength_series(&closes, LONG_TREND);
    let vols = realized_volatility_series(&closes, VOL_PERIOD);

    for i in 0..n {
        let vol = vols[i];
        let mode_kalman = matches!(vol, Some(v) if v < VOL_LOW);

        if mode_kalman {
            preds[i] = match kalman_dir[i] {
                1 => GateDecision::Buy,
                -1 => GateDecision::Sell,
                _ => GateDecision::Hold,
            };
            continue;
        }

        let (st, lt) = match (short_ts[i], long_ts[i]) {
            (Some(st), Some(lt)) => (st, lt),
            _ => continue,
        };
        let blended = BLEND_ALPHA * st + (1.0 - BLEND_ALPHA) * lt;
        let dual_dir = if blended > 0.1 {
            1i8
        } else if blended < -0.1 {
            -1i8
        } else {
            0i8
        };
        let aligned = (st > 0.0 && lt > 0.0) || (st < 0.0 && lt < 0.0);

        if aligned && kalman_dir[i] == dual_dir {
            preds[i] = match kalman_dir[i] {
                1 => GateDecision::Buy,
                -1 => GateDecision::Sell,
                _ => GateDecision::Hold,
            };
        }
    }

    preds
}

fn make_golden_cross_strategy(pair: &str) -> Strategy {
    let pip = pip_size(pair);
    let sl = 100.0 * pip;
    let tp = 200.0 * pip;
    Strategy {
        name: "Legend-Golden-Cross-Classic".to_string(),
        sma_short: 50,
        sma_long: 200,
        sl,
        tp,
        volume: 0.1,
        indicator_type: IndicatorType::Sma,
        filter_enabled: false,
        filter_tf: String::new(),
        filter_period: 0,
        filter_logic: String::new(),
        entry_long_ast: Some(StrategyNode::CrossUp(
            Box::new(StrategyNode::Sma { period: 50 }),
            Box::new(StrategyNode::Sma { period: 200 }),
        )),
        entry_short_ast: None,
        exit_long_ast: Some(StrategyNode::CrossDown(
            Box::new(StrategyNode::Sma { period: 50 }),
            Box::new(StrategyNode::Sma { period: 200 }),
        )),
        exit_short_ast: None,
    }
}

fn make_rsi2_reversion_strategy(pair: &str) -> Strategy {
    let pip = pip_size(pair);
    let sl = 10.0 * pip;
    let tp = 10.0 * pip;
    Strategy {
        name: "Legend-RSI2-Reversion".to_string(),
        sma_short: 2,
        sma_long: 14,
        sl,
        tp,
        volume: 0.1,
        indicator_type: IndicatorType::Rsi,
        filter_enabled: false,
        filter_tf: String::new(),
        filter_period: 0,
        filter_logic: String::new(),
        entry_long_ast: Some(StrategyNode::Lt(
            Box::new(StrategyNode::Rsi { period: 2 }),
            Box::new(StrategyNode::Constant(10.0)),
        )),
        entry_short_ast: None,
        exit_long_ast: Some(StrategyNode::Gt(
            Box::new(StrategyNode::Rsi { period: 2 }),
            Box::new(StrategyNode::Constant(90.0)),
        )),
        exit_short_ast: None,
    }
}

fn fmt(v: f64) -> String {
    if v.is_infinite() {
        "inf".to_string()
    } else {
        format!("{:.2}", v)
    }
}

fn print_table(title: &str, rows: &[(String, backtester::BacktestResult, backtester::BacktestResult)]) {
    println!("{title}");
    println!(
        "Pair/Strategy | Base DailySharpe | Base PF | Base Win% | Base Trades | Base MaxDD | Gate DailySharpe | Gate PF | Gate Win% | Gate Trades | Gate MaxDD"
    );
    for (label, base, gate) in rows {
        println!(
            "{label} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {}",
            fmt(base.sharpe),
            fmt(base.profit_factor),
            fmt(base.win_rate),
            base.trades,
            fmt(base.max_drawdown * 100.0),
            fmt(gate.sharpe),
            fmt(gate.profit_factor),
            fmt(gate.win_rate),
            gate.trades,
            fmt(gate.max_drawdown * 100.0),
        );
    }
}

fn parse_pairs(args: &[String]) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    let mut i = 0;
    while i < args.len() {
        if args[i] == "--pairs" {
            i += 1;
            while i < args.len() && !args[i].starts_with("--") {
                out.push(args[i].clone());
                i += 1;
            }
            continue;
        }
        i += 1;
    }
    if out.is_empty() {
        vec!["USDJPY".to_string(), "EURUSD".to_string(), "GBPUSD".to_string()]
    } else {
        out
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let pairs = parse_pairs(&args);
    let data_dir = {
        let mut dd: Option<&str> = None;
        for i in 0..args.len() {
            if args[i] == "--data-dir" {
                dd = args.get(i + 1).map(|s| s.as_str());
            }
        }
        resolve_data_dir(dd)
    };

    let mut is_rows: Vec<(String, backtester::BacktestResult, backtester::BacktestResult)> = Vec::new();
    let mut oos_rows: Vec<(String, backtester::BacktestResult, backtester::BacktestResult)> = Vec::new();

    for pair in pairs {
        let slippage = 0.5 * pip_size(&pair);

        for (tf, strat_builder) in [
            ("H1", make_golden_cross_strategy as fn(&str) -> Strategy),
            ("M5", make_rsi2_reversion_strategy as fn(&str) -> Strategy),
        ] {
            let path = format!("{data_dir}/{pair}_{tf}.csv");
            let candles = load_candles_from_csv(&path)?;
            if candles.is_empty() {
                continue;
            }
            let split_idx = (candles.len() as f64 * 0.8) as usize;

            let strat = strat_builder(&pair);

            let aux: HashMap<String, Vec<backtester::Candle>> = HashMap::new();
            let swap: Vec<backtester::SwapRecord> = Vec::new();

            let is_slice = &candles[..split_idx];
            let oos_slice = &candles[split_idx..];

            let base_is = run_backtest_with_slippage(&strat, is_slice, &aux, &swap, slippage);
            let gate_is_preds = model_gate_predict(is_slice);
            let gate_is = run_backtest_with_gate_and_slippage(&strat, is_slice, &aux, &swap, &gate_is_preds, slippage);

            let base_oos = run_backtest_with_slippage(&strat, oos_slice, &aux, &swap, slippage);
            let gate_oos_preds = model_gate_predict(oos_slice);
            let gate_oos = run_backtest_with_gate_and_slippage(&strat, oos_slice, &aux, &swap, &gate_oos_preds, slippage);

            let label = format!("{pair}/{name}", name = strat.name);
            is_rows.push((label.clone(), base_is, gate_is));
            oos_rows.push((label, base_oos, gate_oos));
        }
    }

    print_table("IS Metrics (80% oldest)", &is_rows);
    println!();
    print_table("OOS Metrics (newest 20%)", &oos_rows);
    Ok(())
}

