// primitive_scan.rs — expanded primitive/barrier forward-robust search (offline / paper only).
//
// SELF-CONTAINED research harness. It touches NO backbone: it reuses ONLY the pure,
// read-only helpers load_candles_from_csv / resample_candles / calculate_penalized_sharpe
// from backtester.rs, and re-implements a backtest loop that faithfully mirrors
// run_backtest_internal's semantics (close-based SL/TP, slippage on BOTH sides =>
// 2*slippage round-trip, one position, daily-returns Sharpe annualized by sqrt(252)).
// A --validate mode cross-checks that faithfulness against the kill_oos_cpcv numbers.
//
// Adds the primitives the live engine does NOT have, so we can ask (cheaply, honestly)
// whether an expanded space contains a FORWARD-ROBUST non-TREND/non-USDJPY edge:
//   * bb        : Bollinger mean-reversion with VARIABLE std_dev (engine hardcodes 2.0)
//   * donchian  : channel breakout (momentum; regime :BREAKOUT)
//   * keltner   : EMA +/- mult*ATR channel mean-reversion
//   * sma/rsi/stoch: replicated for validation / comparison
// Barriers: fixed-pip OR per-trade ATR-normalized (sl/tp as ATR multiples at entry).
// Optional max_hold time-stop. Same OOS/CPCV/holdout window machinery as kill_oos_cpcv.
//
// Volume in this dataset is synthetic (constant 1.0), so volume-based primitives are
// intentionally NOT offered here — they would be degenerate.

#[path = "../backtester.rs"]
mod backtester;
#[path = "../strategy_ast.rs"]
mod strategy_ast;

use backtester::{calculate_penalized_sharpe, load_candles_from_csv, resample_candles, Candle};
use serde::{Deserialize, Serialize};

// UTC epoch bounds (match kill_oos_cpcv)
const TS_2015: i64 = 1_420_070_400;
const TS_2021: i64 = 1_609_459_200;
const TS_2025: i64 = 1_735_689_600;
const CPCV_BLOCKS: usize = 10;
const OOS_MIN_TRADES: i32 = 200;
const PF_GATE: f64 = 1.10;
const PEN_SHARPE_GATE: f64 = 0.30;
const CPCV_FOLD_MIN_TRADES: i32 = 20;
const CPCV_PASS_RATE_GATE: f64 = 0.60;
const CPCV_MEDIAN_SHARPE_MAX: f64 = 2.0;

#[derive(Deserialize, Clone)]
struct ManifestEntry {
    name: String,
    symbol: String,
    #[serde(default = "def_regime")]
    regime: String,
    prim: String,
    period: usize,
    #[serde(default = "def_dev")]
    dev: f64,
    #[serde(default = "def_atr_period")]
    atr_period: usize,
    tf_seconds: i64,
    #[serde(default = "def_barrier_mode")]
    barrier_mode: String, // "pip" | "atr"
    sl: f64,              // pip mode: price distance; atr mode: ATR multiple
    tp: f64,
    #[serde(default)]
    max_hold: i64, // 0 = no time stop
}
fn def_regime() -> String { ":REVERSION".into() }
fn def_dev() -> f64 { 2.0 }
fn def_atr_period() -> usize { 14 }
fn def_barrier_mode() -> String { "pip".into() }

#[derive(Serialize, Clone)]
struct RunMetrics {
    trades: i32,
    pf: f64,
    sharpe: f64,
    penalized_sharpe: f64,
}

#[derive(Serialize, Clone)]
struct FoldMetrics { block: usize, trades: i32, pf: f64, penalized_sharpe: f64, pass: bool }

#[derive(Serialize, Clone)]
struct CpcvSummary {
    valid_folds: usize,
    passing_folds: usize,
    pass_rate: f64,
    median_sharpe: f64,
    folds: Vec<FoldMetrics>,
}

#[derive(Serialize)]
struct StratResult {
    name: String,
    symbol: String,
    regime: String,
    prim: String,
    period: usize,
    dev: f64,
    timeframe_min: i64,
    barrier_mode: String,
    sl: f64,
    tp: f64,
    oos: RunMetrics,
    cpcv: CpcvSummary,
    oos_qualified: bool,
    cpcv_ok: bool,
    diversity_ok: bool,
}

// ---------- indicators (self-contained) ----------
// Window convention MATCHES backtester.rs's calculate_* helpers: the `period` bars
// ENDING BEFORE the current bar i, i.e. slice [i-period, i) (current bar excluded).
// Verified against the engine on SMA/BB/RSI (see --validate cross-check).
fn sma(c: &[Candle], p: usize, i: usize) -> Option<f64> {
    if p == 0 || i < p { return None; }
    let s: f64 = c[i - p..i].iter().map(|x| x.close).sum();
    Some(s / p as f64)
}
// EMA excludes the current bar too (bands compared against the current close).
// New primitive (Keltner) — the engine has no EMA, so no fidelity constraint.
fn ema(c: &[Candle], p: usize, i: usize) -> Option<f64> {
    if p == 0 || i < p { return None; }
    let k = 2.0 / (p as f64 + 1.0);
    let lo = if i >= 4 * p { i - 4 * p } else { 0 };
    let mut e = c[lo].close;
    for j in lo + 1..i {
        e = c[j].close * k + e * (1.0 - k);
    }
    Some(e)
}
// population std, matches calculate_bollinger (÷period)
fn bollinger(c: &[Candle], p: usize, dev: f64, i: usize) -> Option<(f64, f64, f64)> {
    if p == 0 || i < p { return None; }
    let slice = &c[i - p..i];
    let mean: f64 = slice.iter().map(|x| x.close).sum::<f64>() / p as f64;
    let var: f64 = slice.iter().map(|x| (x.close - mean).powi(2)).sum::<f64>() / p as f64;
    let sd = var.sqrt();
    Some((mean + dev * sd, mean, mean - dev * sd))
}
// matches calculate_rsi: simple (non-Wilder) average of gains/losses over
// changes in [i-period, i); needs one extra bar of history for the diff.
fn rsi(c: &[Candle], p: usize, i: usize) -> Option<f64> {
    if p == 0 || i < p + 1 { return None; }
    let (mut g, mut l) = (0.0, 0.0);
    for j in (i - p)..i {
        let ch = c[j].close - c[j - 1].close;
        if ch > 0.0 { g += ch; } else { l -= ch; }
    }
    let (ag, al) = (g / p as f64, l / p as f64);
    if al == 0.0 { return Some(100.0); }
    let rs = ag / al;
    Some(100.0 - 100.0 / (1.0 + rs))
}
fn atr(c: &[Candle], p: usize, i: usize) -> Option<f64> {
    if p == 0 || i < p + 1 { return None; }
    let mut s = 0.0;
    for j in (i - p)..i {
        let tr = (c[j].high - c[j].low)
            .max((c[j].high - c[j - 1].close).abs())
            .max((c[j].low - c[j - 1].close).abs());
        s += tr;
    }
    Some(s / p as f64)
}
fn highest_high(c: &[Candle], p: usize, i: usize) -> Option<f64> {
    if i < p { return None; }
    Some(c[i - p..i].iter().map(|x| x.high).fold(f64::NEG_INFINITY, f64::max))
}
fn lowest_low(c: &[Candle], p: usize, i: usize) -> Option<f64> {
    if i < p { return None; }
    Some(c[i - p..i].iter().map(|x| x.low).fold(f64::INFINITY, f64::min))
}
fn stoch_k(c: &[Candle], p: usize, i: usize) -> Option<f64> {
    if i < p + 1 { return None; }
    let win = &c[i - p..i];
    let lo = win.iter().map(|x| x.close).fold(f64::INFINITY, f64::min);
    let hi = win.iter().map(|x| x.close).fold(f64::NEG_INFINITY, f64::max);
    Some(if hi != lo { (c[i].close - lo) / (hi - lo) * 100.0 } else { 50.0 })
}

// returns (buy, sell, exit_long, exit_short)
fn signals(m: &ManifestEntry, c: &[Candle], i: usize) -> (bool, bool, bool, bool) {
    let price = c[i].close;
    match m.prim.as_str() {
        "sma" => {
            let (p_s, p_l) = (m.period, m.period * 2);
            if let (Some(s), Some(l), Some(ps), Some(pl)) =
                (sma(c, p_s, i), sma(c, p_l, i), sma(c, p_s, i - 1), sma(c, p_l, i - 1))
            {
                let buy = ps < pl && s > l;
                let sell = ps > pl && s < l;
                (buy, sell, sell, buy)
            } else { (false, false, false, false) }
        }
        "rsi" => {
            let p = m.period.max(7);
            if let (Some(r), Some(pr)) = (rsi(c, p, i), rsi(c, p, i - 1)) {
                (pr < 30.0 && r >= 30.0, pr > 70.0 && r <= 70.0, r > 70.0, r < 30.0)
            } else { (false, false, false, false) }
        }
        "stoch" => {
            let p = m.period.max(14);
            if let (Some(k), Some(pk)) = (stoch_k(c, p, i), stoch_k(c, p, i - 1)) {
                (pk < 20.0 && k >= 20.0, pk > 80.0 && k <= 80.0, k > 80.0, k < 20.0)
            } else { (false, false, false, false) }
        }
        "bb" => {
            // variable dev; mean-reversion (bounce off lower / touch upper, exit at middle)
            let p = m.period.max(20);
            if let (Some((up, mid, lo)), Some((_, _, plo))) =
                (bollinger(c, p, m.dev, i), bollinger(c, p, m.dev, i - 1))
            {
                let pc = c[i - 1].close;
                (pc <= plo && price > lo, price >= up, price >= mid, price <= mid)
            } else { (false, false, false, false) }
        }
        "keltner" => {
            // EMA +/- mult*ATR mean-reversion (mult carried in `dev`)
            let p = m.period.max(2);
            if let (Some(e), Some(a), Some(pe), Some(pa)) =
                (ema(c, p, i), atr(c, m.atr_period, i), ema(c, p, i - 1), atr(c, m.atr_period, i - 1))
            {
                let (up, lo) = (e + m.dev * a, e - m.dev * a);
                let plo = pe - m.dev * pa;
                let pc = c[i - 1].close;
                (pc <= plo && price > lo, price >= up, price >= e, price <= e)
            } else { (false, false, false, false) }
        }
        "donchian" => {
            // channel breakout (momentum); exit on opposite channel
            if let (Some(hh), Some(ll)) = (highest_high(c, m.period, i), lowest_low(c, m.period, i)) {
                (price > hh, price < ll, price < ll, price > hh)
            } else { (false, false, false, false) }
        }
        _ => (false, false, false, false),
    }
}

fn min_bars(m: &ManifestEntry) -> usize {
    let base = match m.prim.as_str() {
        "sma" => m.period * 2 + 2,
        "rsi" => m.period.max(7) + 5,
        "stoch" => m.period.max(14) + 2,
        "bb" => m.period.max(20) + 2,
        "keltner" => m.period.max(2).max(m.atr_period) + 2,
        "donchian" => m.period + 2,
        _ => m.period + 2,
    };
    base.max(m.atr_period + 2)
}

enum Pos { None, Long { entry: f64, sl: f64, tp: f64, bar: i64 }, Short { entry: f64, sl: f64, tp: f64, bar: i64 } }

// Faithful mirror of run_backtest_internal (offline, swap=0).
fn backtest(m: &ManifestEntry, c: &[Candle], slip: f64) -> RunMetrics {
    let mb = min_bars(m);
    if c.len() <= mb {
        return RunMetrics { trades: 0, pf: 0.0, sharpe: 0.0, penalized_sharpe: 0.0 };
    }
    let mut pos = Pos::None;
    let (mut trades, mut gp, mut gl) = (0i32, 0.0f64, 0.0f64);
    let mut daily: std::collections::HashMap<i64, f64> = std::collections::HashMap::new();
    let mut bar_ct: i64 = 0;
    for i in mb..c.len() {
        let price = c[i].close;
        let day = c[i].timestamp / 86400;
        let (buy, sell, ex_long, ex_short) = signals(m, c, i);
        // exits first (close-based). A trade that closes at exactly 0 PnL still counts,
        // mirroring run_backtest_internal (trades += 1 unconditionally on exit).
        let (mut did_close, mut closed) = (false, 0.0);
        match pos {
            Pos::Long { entry, sl, tp, bar } => {
                let held = bar_ct - bar;
                let timeout = m.max_hold > 0 && held >= m.max_hold;
                if price <= sl || price >= tp || ex_long || timeout {
                    closed = (price - slip) - entry;
                    did_close = true;
                    pos = Pos::None;
                }
            }
            Pos::Short { entry, sl, tp, bar } => {
                let held = bar_ct - bar;
                let timeout = m.max_hold > 0 && held >= m.max_hold;
                if price >= sl || price <= tp || ex_short || timeout {
                    closed = entry - (price + slip);
                    did_close = true;
                    pos = Pos::None;
                }
            }
            Pos::None => {}
        }
        if did_close {
            trades += 1;
            if closed > 0.0 { gp += closed; } else { gl += closed.abs(); }
            *daily.entry(day).or_insert(0.0) += closed;
        }
        // entries when flat
        if matches!(pos, Pos::None) {
            let (sl_d, tp_d) = barrier_dist(m, c, i);
            if buy {
                let e = price + slip;
                pos = Pos::Long { entry: e, sl: e - sl_d, tp: e + tp_d, bar: bar_ct };
            } else if sell {
                let e = price - slip;
                pos = Pos::Short { entry: e, sl: e + sl_d, tp: e - tp_d, bar: bar_ct };
            }
        }
        bar_ct += 1;
    }
    // daily-returns sharpe (mirror)
    let start_day = c[0].timestamp / 86400;
    let end_day = c[c.len() - 1].timestamp / 86400;
    let mut eq = 10000.0f64;
    let mut rets: Vec<f64> = Vec::new();
    for d in start_day..=end_day {
        let dp = *daily.get(&d).unwrap_or(&0.0);
        rets.push(if eq > 0.0 { dp / eq } else { 0.0 });
        eq += dp;
    }
    let sharpe = sharpe_ratio(&rets);
    RunMetrics {
        trades,
        pf: if gl != 0.0 { gp / gl } else { 0.0 },
        sharpe,
        penalized_sharpe: calculate_penalized_sharpe(sharpe, trades),
    }
}

fn barrier_dist(m: &ManifestEntry, c: &[Candle], i: usize) -> (f64, f64) {
    if m.barrier_mode == "atr" {
        let a = atr(c, m.atr_period, i).unwrap_or(0.0);
        (m.sl * a, m.tp * a)
    } else {
        (m.sl, m.tp)
    }
}

fn sharpe_ratio(r: &[f64]) -> f64 {
    if r.is_empty() { return 0.0; }
    let mean = r.iter().sum::<f64>() / r.len() as f64;
    let var = r.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / r.len() as f64;
    let sd = var.sqrt();
    if sd == 0.0 { return 0.0; }
    mean / sd * (252.0f64).sqrt()
}

fn median(v: &mut [f64]) -> f64 {
    if v.is_empty() { return 0.0; }
    v.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let mid = v.len() / 2;
    if v.len() % 2 == 0 { (v[mid - 1] + v[mid]) / 2.0 } else { v[mid] }
}

fn run_on(m: &ManifestEntry, m1: &[Candle], slip: f64) -> RunMetrics {
    if m.tf_seconds > 60 {
        let tf = resample_candles(m1, m.tf_seconds);
        backtest(m, &tf, slip)
    } else {
        backtest(m, m1, slip)
    }
}

fn run_cpcv(m: &ManifestEntry, full: &[Candle], slip: f64) -> CpcvSummary {
    let n = full.len();
    let (mut folds, mut sharpes) = (Vec::new(), Vec::new());
    let (mut passing, mut valid) = (0usize, 0usize);
    for b in 0..CPCV_BLOCKS {
        let start = b * n / CPCV_BLOCKS;
        let end = if b == CPCV_BLOCKS - 1 { n } else { (b + 1) * n / CPCV_BLOCKS };
        let r = run_on(m, &full[start..end], slip);
        valid += 1;
        sharpes.push(r.sharpe);
        let pass = r.trades >= CPCV_FOLD_MIN_TRADES && r.pf >= PF_GATE && r.penalized_sharpe >= PEN_SHARPE_GATE;
        if pass { passing += 1; }
        folds.push(FoldMetrics { block: b, trades: r.trades, pf: r.pf, penalized_sharpe: r.penalized_sharpe, pass });
    }
    CpcvSummary {
        valid_folds: valid,
        passing_folds: passing,
        pass_rate: if valid > 0 { passing as f64 / valid as f64 } else { 0.0 },
        median_sharpe: median(&mut sharpes.clone()),
        folds,
    }
}

fn slice_range<'a>(c: &'a [Candle], lo: i64, hi: i64) -> &'a [Candle] {
    let s = c.partition_point(|x| x.timestamp < lo);
    let e = c.partition_point(|x| x.timestamp < hi);
    &c[s..e]
}
fn arg(a: &[String], k: &str) -> Option<String> {
    a.iter().position(|x| x == k).and_then(|i| a.get(i + 1)).cloned()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let data = arg(&args, "--data").expect("--data <csv>");
    let out_path = arg(&args, "--out").expect("--out <json>");
    let slip: f64 = arg(&args, "--slippage").and_then(|s| s.parse().ok()).unwrap_or(0.01);

    eprintln!("[scan] loading {}", data);
    let all = load_candles_from_csv(&data)?;
    let all_lo = all.first().map(|c| c.timestamp).unwrap_or(0);
    let all_hi = all.last().map(|c| c.timestamp).unwrap_or(0) + 1;
    let pt = |k: &str, d: i64| arg(&args, k).and_then(|s| s.parse().ok()).unwrap_or(d);
    let oos_start = pt("--oos-start", TS_2021);
    let oos_end = pt("--oos-end", TS_2025);
    let cpcv_start = pt("--cpcv-start", all_lo);
    let cpcv_end = pt("--cpcv-end", all_hi);
    let _ = TS_2015;
    let oos_slice = slice_range(&all, oos_start, oos_end);
    let cpcv_slice = slice_range(&all, cpcv_start, cpcv_end);
    eprintln!("[scan] OOS[{oos_start}..{oos_end}]={} CPCV[{cpcv_start}..{cpcv_end}]={}", oos_slice.len(), cpcv_slice.len());

    let manifest_path = arg(&args, "--manifest").expect("--manifest <json>");
    let manifest: Vec<ManifestEntry> = serde_json::from_str(&std::fs::read_to_string(&manifest_path)?)?;
    eprintln!("[scan] {} configs", manifest.len());

    let mut results = Vec::new();
    for (i, m) in manifest.iter().enumerate() {
        let oos = run_on(m, oos_slice, slip);
        let cpcv = run_cpcv(m, cpcv_slice, slip);
        let oos_qualified = oos.trades >= OOS_MIN_TRADES && oos.pf >= PF_GATE && oos.penalized_sharpe >= PEN_SHARPE_GATE;
        let cpcv_ok = cpcv.pass_rate >= CPCV_PASS_RATE_GATE && cpcv.median_sharpe < CPCV_MEDIAN_SHARPE_MAX;
        let regime_norm = m.regime.trim_start_matches(':').to_uppercase();
        let diversity_ok = m.symbol != "USDJPY" || regime_norm != "TREND";
        eprintln!(
            "[{:3}/{}] {:40} {:8} tf={:4}m | OOS(t={},pf={:.3},pen={:.3}) CPCV(pr={:.2},med={:.2}) {}",
            i + 1, manifest.len(), &m.name, m.prim, m.tf_seconds / 60,
            oos.trades, oos.pf, oos.penalized_sharpe, cpcv.pass_rate, cpcv.median_sharpe,
            if oos_qualified { "QUALIFIED" } else { "" }
        );
        results.push(StratResult {
            name: m.name.clone(), symbol: m.symbol.clone(), regime: m.regime.clone(),
            prim: m.prim.clone(), period: m.period, dev: m.dev, timeframe_min: m.tf_seconds / 60,
            barrier_mode: m.barrier_mode.clone(), sl: m.sl, tp: m.tp,
            oos, cpcv, oos_qualified, cpcv_ok, diversity_ok,
        });
    }
    let qualified = results.iter().filter(|r| r.oos_qualified).count();
    let qual_cpcv = results.iter().filter(|r| r.oos_qualified && r.cpcv_ok).count();
    let qual_div = results.iter().filter(|r| r.oos_qualified && r.diversity_ok).count();
    let verdict = serde_json::json!({
        "summary": {"n": results.len(), "oos_qualified": qualified,
            "oos_qualified_and_cpcv_ok": qual_cpcv, "oos_qualified_and_diverse": qual_div},
        "strategies": results,
    });
    std::fs::write(&out_path, serde_json::to_string_pretty(&verdict)?)?;
    eprintln!("[scan] oos_qualified={qualified}  +cpcv_ok={qual_cpcv}  +diverse={qual_div}  -> {}", out_path);
    Ok(())
}
