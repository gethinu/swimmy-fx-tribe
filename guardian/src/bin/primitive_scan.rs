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
    // --- YARDSTICK AXIS (2026-07-21): hold-to-barrier / "let-winners-run" EXIT mode. The one
    // representational gap that directly increases per-trade thickness: the backtest exit is
    // otherwise UNCONDITIONALLY `sl || tp || indicator-exit || timeout`, so every mean-reversion
    // trade is force-closed at the mid regardless of how wide tp is set — capping the per-trade
    // edge at band->mid and pinning fat trades to high TF. hold_mode="barrier" DROPS the
    // indicator-exit term so a winner rides to the (wide, ATR-multiple) tp or the (multi-day)
    // time-stop. Default "" behaves exactly like "signal" (indicator-exit ON) => a manifest
    // WITHOUT this key is byte-identical to the pre-hold engine (proven by diff).
    #[serde(default)]
    hold_mode: String, // "" | "signal" (default: indicator-exit ON) | "barrier" (indicator-exit OFF)
    // --- YARDSTICK AXIS (2026-07-19): session / time-of-day ENTRY gate. Pure function of the
    // bar's UTC timestamp — orthogonal to every price-shape primitive. When all three fields
    // hold their defaults (sess_start=0, sess_end=24, dow_mask=0) the gate is a no-op, so a
    // manifest WITHOUT these keys is byte-identical to the pre-session engine (proven by diff).
    #[serde(default)]
    sess_start: i64, // entry-allowed window [start,end) in UTC hours, 0..24
    #[serde(default = "def_sess_end")]
    sess_end: i64,
    #[serde(default)]
    dow_mask: u8, // day-of-week bitmask (bit d, 0=Sun..6=Sat); 0 => all days allowed
    #[serde(default)]
    session: String, // human label only (e.g. "LONDON"); never read by the backtest logic
    // --- YARDSTICK AXIS (2026-07-21b): multi-TF CONFLUENCE entry gate (Route C). A long entry is
    // allowed only when a higher-timeframe SMA trend agrees (htf_close > htf_sma), short only when it
    // disagrees (htf_close < htf_sma). Orthogonal to the trade-TF price shape: it conditions on a
    // SLOWER horizon's regime, attacking 2-window robustness (drop counter-regime whipsaws that differ
    // between 2015-21 and 2021-25). Off unless htf_seconds>0 && htf_period>0 && htf_logic!="" => a
    // manifest without these keys is byte-identical to the pre-confluence engine (proven by diff).
    #[serde(default)]
    htf_seconds: i64, // higher-TF bar size in seconds (e.g. 86400=D1 filter for an H4 trade); 0 => off
    #[serde(default)]
    htf_period: usize, // SMA period on the HTF; 0 => off
    #[serde(default)]
    htf_logic: String, // "" => off; "trend" => long iff htf uptrend, short iff htf downtrend
    // --- YARDSTICK AXIS (2026-07-21b): volatility-REGIME entry gate (Route B). Enter only when the
    // trailing ATR percentile (over vol_win prior bars) is in [vol_lo, vol_hi). Conditions entries on a
    // regime-invariant vol band rather than a price level. Off unless vol_hi>0.0 => a manifest without
    // these keys is byte-identical to the pre-gate engine (proven by diff).
    #[serde(default)]
    vol_lo: f64, // lower percentile bound (0..1)
    #[serde(default)]
    vol_hi: f64, // upper percentile bound (0..1); <=0.0 => gate OFF
    #[serde(default)]
    vol_win: usize, // trailing window of ATR values for the percentile; 0 => default 200
    // --- YARDSTICK AXIS (2026-07-21c): SINGLE-LEG relative-value divergence signal (Route D). The
    // natural resolution of Route A's 2-leg cost wall. prim="rvspread" trades ONE real pair (the --data
    // leg, so it pays SINGLE-leg cost) but its ENTRY is driven by the z-score of the cross-pair spread
    // (leg - rv_beta*aux), aux being a SECOND series loaded via --aux. Long the leg when the spread is
    // cheap (z < -rv_dev), short when rich (z > rv_dev), exit when it reverts to the mean (z crosses 0).
    // Off unless prim=="rvspread" AND --aux is given => any manifest that never names "rvspread" is
    // byte-identical to the pre-RV engine (existing prims never touch aux). Attacks the joint box:
    // capture the 2-window-robust spread mechanism at single-leg (not doubled) cost.
    #[serde(default)]
    rv_beta: f64, // hedge ratio in spread = leg - rv_beta*aux
    #[serde(default)]
    rv_period: usize, // z-score lookback (bars at the trade TF)
    #[serde(default = "def_rv_dev")]
    rv_dev: f64, // z entry threshold (|z| > rv_dev to enter)
}
fn def_regime() -> String { ":REVERSION".into() }
fn def_dev() -> f64 { 2.0 }
fn def_atr_period() -> usize { 14 }
fn def_barrier_mode() -> String { "pip".into() }
fn def_sess_end() -> i64 { 24 }
fn def_rv_dev() -> f64 { 2.0 }

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
    // Session axis (placed last, omitted when empty) so a no-session scan serializes
    // byte-for-byte identically to the pre-session engine.
    #[serde(skip_serializing_if = "String::is_empty")]
    session: String,
    // Hold-to-barrier axis (also omitted when empty) — same byte-identity discipline: a scan
    // with no hold_mode key serializes exactly as the pre-hold engine did.
    #[serde(skip_serializing_if = "String::is_empty")]
    hold_mode: String,
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
        "rvspread" => m.rv_period + 2,
        _ => m.period + 2,
    };
    base.max(m.atr_period + 2)
}

// Session/time-of-day ENTRY gate (yardstick axis). A pure function of the bar timestamp,
// so it multiplies the existing price-shape signal by a clock mask without touching the
// signal itself. Default (sess_start=0, sess_end=24, dow_mask=0) => always true, i.e. a
// manifest without session fields takes every entry it took before (byte-identical).
fn in_session(m: &ManifestEntry, ts: i64) -> bool {
    if m.sess_start == 0 && m.sess_end == 24 && m.dow_mask == 0 {
        return true; // no gate — pre-session behavior
    }
    if m.dow_mask != 0 {
        // epoch day 0 (1970-01-01) was a Thursday => (day + 4) mod 7 gives 0=Sun..6=Sat.
        let dow = ((ts.div_euclid(86400) + 4).rem_euclid(7)) as u8;
        if (m.dow_mask & (1u8 << dow)) == 0 { return false; }
    }
    let h = ts.rem_euclid(86400) / 3600; // UTC hour, 0..23
    if m.sess_start <= m.sess_end {
        h >= m.sess_start && h < m.sess_end
    } else {
        h >= m.sess_start || h < m.sess_end // window wraps past midnight (e.g. 22..6)
    }
}

// Multi-TF CONFLUENCE gate (Route C). Returns, per trade-TF candle, the SLOWER horizon's SMA-trend
// state (+1 up / -1 down / 0 = no completed HTF bar yet), using only HTF bars that CLOSED strictly
// before the current HTF bucket => no look-ahead. None when the gate is off (byte-identical path,
// no resampling done). The SMA window [j-p, j) excludes the current bar, matching the engine convention.
fn build_htf(m: &ManifestEntry, m1: &[Candle], tf: &[Candle]) -> Option<Vec<i8>> {
    if m.htf_seconds <= 0 || m.htf_period == 0 || m.htf_logic.is_empty() { return None; }
    let hs = m.htf_seconds;
    let htf = if hs > 60 { resample_candles(m1, hs) } else { m1.to_vec() };
    let p = m.htf_period;
    let mut states: Vec<(i64, i8)> = Vec::with_capacity(htf.len().saturating_sub(p));
    for j in p..htf.len() {
        let sma: f64 = htf[j - p..j].iter().map(|x| x.close).sum::<f64>() / p as f64;
        let st: i8 = if htf[j].close > sma { 1 } else if htf[j].close < sma { -1 } else { 0 };
        states.push((htf[j].timestamp.div_euclid(hs), st));
    }
    let mut out = vec![0i8; tf.len()];
    let (mut ptr, mut last) = (0usize, 0i8);
    for i in 0..tf.len() {
        let tb = tf[i].timestamp.div_euclid(hs);
        while ptr < states.len() && states[ptr].0 < tb { last = states[ptr].1; ptr += 1; }
        out[i] = last;
    }
    Some(out)
}

// Volatility-REGIME gate (Route B). Per candle: is the trailing ATR percentile (fraction of the prior
// vol_win ATR values below the current ATR) inside [vol_lo, vol_hi)? None when off (byte-identical path).
fn build_vol_gate(m: &ManifestEntry, c: &[Candle]) -> Option<Vec<bool>> {
    if m.vol_hi <= 0.0 { return None; }
    let win = if m.vol_win > 0 { m.vol_win } else { 200 };
    let atrv: Vec<f64> = (0..c.len()).map(|i| atr(c, m.atr_period, i).unwrap_or(f64::NAN)).collect();
    let mut vp = vec![false; c.len()];
    for i in 0..c.len() {
        if atrv[i].is_nan() { continue; }
        let lo = if i > win { i - win } else { 0 };
        let (mut cnt, mut below) = (0.0f64, 0.0f64);
        for j in lo..i {
            if !atrv[j].is_nan() { cnt += 1.0; if atrv[j] < atrv[i] { below += 1.0; } }
        }
        let pct = if cnt > 0.0 { below / cnt } else { 0.5 };
        vp[i] = pct >= m.vol_lo && pct < m.vol_hi;
    }
    Some(vp)
}

// Route D: align an auxiliary leg's close to every trade-TF bar of the primary, by resampling the aux
// to the same TF and matching on the timestamp bucket (exact — both grids share the tf_sec boundary).
// Bars with no matching aux resample to NaN => the RV spread is NaN there => no trade.
fn align_aux(aux: &[Candle], tf: &[Candle], tf_sec: i64) -> Vec<f64> {
    let aux_tf = if tf_sec > 60 { resample_candles(aux, tf_sec) } else { aux.to_vec() };
    let mut map: std::collections::HashMap<i64, f64> = std::collections::HashMap::with_capacity(aux_tf.len());
    for a in &aux_tf { map.insert(a.timestamp.div_euclid(tf_sec), a.close); }
    tf.iter().map(|b| *map.get(&b.timestamp.div_euclid(tf_sec)).unwrap_or(&f64::NAN)).collect()
}

// Route D: RV-divergence signal from the precomputed spread vector. Long the traded leg when the spread
// is cheap (z < -dev), short when rich (z > dev), exit_long when the spread has reverted up to its mean
// (z >= 0) and exit_short when reverted down (z <= 0). Window [i-p, i) excludes the current bar.
fn rv_signal(spread: &[f64], p: usize, dev: f64, i: usize) -> (bool, bool, bool, bool) {
    if p == 0 || i < p + 1 { return (false, false, false, false); }
    let win = &spread[i - p..i];
    let mean: f64 = win.iter().sum::<f64>() / p as f64;
    let var: f64 = win.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / p as f64;
    let sd = var.sqrt();
    if !(sd > 0.0) || !spread[i].is_finite() { return (false, false, false, false); }
    let z = (spread[i] - mean) / sd;
    (z < -dev, z > dev, z >= 0.0, z <= 0.0)
}

enum Pos { None, Long { entry: f64, sl: f64, tp: f64, bar: i64 }, Short { entry: f64, sl: f64, tp: f64, bar: i64 } }

// Faithful mirror of run_backtest_internal (offline, swap=0). `htf` is the precomputed multi-TF
// confluence state aligned to `c`; `aux_c` is the Route-D aligned auxiliary leg close (both None =>
// off, byte-identical for every existing primitive).
// `daily_sink` (VERIFICATION-ONLY, ⑫): when Some, the sorted per-UTC-day realized PnL is copied out for
// return-stream correlation / regime analysis. None on every normal path => zero behavior change, and the
// --out serialization is untouched, so a scan run without --dump-daily is byte-identical to the pre-⑫ engine.
fn backtest(m: &ManifestEntry, c: &[Candle], slip: f64, htf: Option<&[i8]>, aux_c: Option<&[f64]>,
            daily_sink: Option<&mut Vec<(i64, f64)>>) -> RunMetrics {
    let mb = min_bars(m);
    if c.len() <= mb {
        return RunMetrics { trades: 0, pf: 0.0, sharpe: 0.0, penalized_sharpe: 0.0 };
    }
    let mut pos = Pos::None;
    let (mut trades, mut gp, mut gl) = (0i32, 0.0f64, 0.0f64);
    let mut daily: std::collections::HashMap<i64, f64> = std::collections::HashMap::new();
    let mut bar_ct: i64 = 0;
    // hold-to-barrier axis: in "barrier" mode the indicator-driven exit is disabled so a
    // winner rides to sl/tp/timeout only. "" and "signal" preserve the pre-hold behavior.
    let hold_barrier = m.hold_mode == "barrier";
    // vol-regime gate (Route B): None => off, unchanged behavior.
    let vol_gate = build_vol_gate(m, c);
    // RV-divergence spread (Route D): only when prim=="rvspread" AND an aux leg was aligned.
    let rv_spread: Option<Vec<f64>> = if m.prim == "rvspread" {
        aux_c.map(|a| (0..c.len()).map(|i| c[i].close - m.rv_beta * a[i]).collect())
    } else { None };
    for i in mb..c.len() {
        let price = c[i].close;
        let day = c[i].timestamp / 86400;
        let (buy, sell, ex_long, ex_short) = match rv_spread.as_deref() {
            Some(sp) => rv_signal(sp, m.rv_period, m.rv_dev, i),
            None => signals(m, c, i),
        };
        // exits first (close-based). A trade that closes at exactly 0 PnL still counts,
        // mirroring run_backtest_internal (trades += 1 unconditionally on exit).
        let (mut did_close, mut closed) = (false, 0.0);
        match pos {
            Pos::Long { entry, sl, tp, bar } => {
                let held = bar_ct - bar;
                let timeout = m.max_hold > 0 && held >= m.max_hold;
                if price <= sl || price >= tp || (ex_long && !hold_barrier) || timeout {
                    closed = (price - slip) - entry;
                    did_close = true;
                    pos = Pos::None;
                }
            }
            Pos::Short { entry, sl, tp, bar } => {
                let held = bar_ct - bar;
                let timeout = m.max_hold > 0 && held >= m.max_hold;
                if price >= sl || price <= tp || (ex_short && !hold_barrier) || timeout {
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
        // entries when flat. Gated by session mask (⑨) AND vol-regime band (Route B, direction-agnostic)
        // AND multi-TF confluence (Route C, direction-specific). All default to no-op => unchanged.
        if matches!(pos, Pos::None) {
            let base_allowed = in_session(m, c[i].timestamp)
                && vol_gate.as_ref().map_or(true, |v| v[i]);
            let long_ok = htf.map_or(true, |h| h[i] == 1);
            let short_ok = htf.map_or(true, |h| h[i] == -1);
            let (sl_d, tp_d) = barrier_dist(m, c, i);
            if buy && base_allowed && long_ok {
                let e = price + slip;
                pos = Pos::Long { entry: e, sl: e - sl_d, tp: e + tp_d, bar: bar_ct };
            } else if sell && base_allowed && short_ok {
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
    if let Some(sink) = daily_sink {
        let mut v: Vec<(i64, f64)> = daily.iter().map(|(&d, &p)| (d, p)).collect();
        v.sort_by_key(|(d, _)| *d);
        *sink = v;
    }
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

fn run_on(m: &ManifestEntry, m1: &[Candle], slip: f64, aux: Option<&[Candle]>,
          daily_sink: Option<&mut Vec<(i64, f64)>>) -> RunMetrics {
    if m.tf_seconds > 60 {
        let tf = resample_candles(m1, m.tf_seconds);
        let htf = build_htf(m, m1, &tf); // None when off => no resample, byte-identical
        let aux_c = aux.map(|a| align_aux(a, &tf, m.tf_seconds));
        backtest(m, &tf, slip, htf.as_deref(), aux_c.as_deref(), daily_sink)
    } else {
        let htf = build_htf(m, m1, m1);
        let aux_c = aux.map(|a| align_aux(a, m1, m.tf_seconds));
        backtest(m, m1, slip, htf.as_deref(), aux_c.as_deref(), daily_sink)
    }
}

fn run_cpcv(m: &ManifestEntry, full: &[Candle], slip: f64, aux: Option<&[Candle]>) -> CpcvSummary {
    let n = full.len();
    let (mut folds, mut sharpes) = (Vec::new(), Vec::new());
    let (mut passing, mut valid) = (0usize, 0usize);
    for b in 0..CPCV_BLOCKS {
        let start = b * n / CPCV_BLOCKS;
        let end = if b == CPCV_BLOCKS - 1 { n } else { (b + 1) * n / CPCV_BLOCKS };
        let block = &full[start..end];
        // slice aux to the same timestamp range as this block (index ranges differ between series)
        let aux_block = aux.map(|a| {
            let lo = block.first().map(|c| c.timestamp).unwrap_or(0);
            let hi = block.last().map(|c| c.timestamp).unwrap_or(0) + 1;
            slice_range(a, lo, hi)
        });
        let r = run_on(m, block, slip, aux_block, None);
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

    // Route D: optional auxiliary leg for rvspread. Absent => aux None => every existing prim unchanged.
    let aux_all: Option<Vec<Candle>> = match arg(&args, "--aux") {
        Some(p) => { eprintln!("[scan] loading aux {}", p); Some(load_candles_from_csv(&p)?) }
        None => None,
    };
    let aux_ref = aux_all.as_deref();
    let oos_aux = aux_ref.map(|a| slice_range(a, oos_start, oos_end));
    let cpcv_aux = aux_ref.map(|a| slice_range(a, cpcv_start, cpcv_end));

    let manifest_path = arg(&args, "--manifest").expect("--manifest <json>");
    let manifest: Vec<ManifestEntry> = serde_json::from_str(&std::fs::read_to_string(&manifest_path)?)?;
    eprintln!("[scan] {} configs", manifest.len());

    // VERIFICATION-ONLY (⑫): --dump-daily <path> appends per-config OOS daily-PnL JSONL for return-stream
    // correlation / regime analysis. Absent => daily_sink is None everywhere => byte-identical scan output.
    let dump_daily = arg(&args, "--dump-daily");
    if let Some(p) = &dump_daily { let _ = std::fs::write(p, ""); eprintln!("[scan] dumping daily PnL -> {p}"); }

    let mut results = Vec::new();
    for (i, m) in manifest.iter().enumerate() {
        let mut daily_vec: Vec<(i64, f64)> = Vec::new();
        let oos = run_on(m, oos_slice, slip, oos_aux,
                         if dump_daily.is_some() { Some(&mut daily_vec) } else { None });
        if let Some(p) = &dump_daily {
            use std::io::Write;
            let line = serde_json::json!({"name": m.name, "daily": daily_vec}).to_string();
            let mut f = std::fs::OpenOptions::new().append(true).create(true).open(p)?;
            writeln!(f, "{line}")?;
        }
        let cpcv = run_cpcv(m, cpcv_slice, slip, cpcv_aux);
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
            session: m.session.clone(),
            hold_mode: m.hold_mode.clone(),
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
