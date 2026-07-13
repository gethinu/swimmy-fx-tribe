// kill_oos_cpcv.rs — KILL_CRITERIA §4 decisive experiment (offline / paper only).
//
// Runs real-cost OOS + corrected CPCV on the 61 PROVISIONAL strategies, reproducing the
// guardian's ACTUAL backtest semantics: strategy-to-alist emits NO entry/exit AST, so the
// live path is an SMA(sma_short)/SMA(sma_long) golden/death cross with SL/TP barriers. This
// bin builds the same Strategy from a manifest (extracted from the library .lisp files) and
// evaluates it with the guardian's own backtester functions.
//
// Corrections vs guardian cpcv.rs (per handbook "CPCV 3-fix"):
//   1. purge/embargo TF-scaled: each fold is a contiguous block; run_backtest's resample +
//      min_bars(sma_long+2 TF-candles) warmup provides a TF-scaled purge at each fold start
//      (== (sma_long+2)*tf_min M1 bars), so no cross-fold leakage. No fixed M1 constant.
//   2. path pass-criterion uses PENALIZED sharpe (Taleb haircut), not raw.
//   3. slippage threaded into every CPCV fold backtest (2-pip round-trip conservative).
//
// Cost model (verified in backtester.rs): slippage is applied on BOTH entry and exit, so
// round-trip cost = 2*slippage. USDJPY pip = 0.01 -> 2-pip round-trip => slippage = 0.01.
// Guardian DEFAULT_SLIPPAGE = 0.005 => 1-pip round-trip (used only for the fidelity check).
//
// Usage: kill_oos_cpcv --data <USDJPY_M1.csv> --manifest <manifest.json> --out <results.json>

#[path = "../backtester.rs"]
mod backtester;
#[path = "../strategy_ast.rs"]
mod strategy_ast;

use backtester::{
    calculate_penalized_sharpe, load_candles_from_csv, resample_candles, run_backtest_with_slippage,
    BacktestResult, Candle, IndicatorType, Strategy, SwapRecord,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// --- constants -----------------------------------------------------------
const USDJPY_PIP: f64 = 0.01;
const COST_2PIP: f64 = 0.01; // per-side => 2*0.01 = 0.02 = 2-pip round-trip
const COST_DEFAULT: f64 = 0.005; // guardian default, 1-pip round-trip (fidelity only)

// UTC epoch bounds
const TS_2015: i64 = 1_420_070_400; // 2015-01-01
const TS_2021: i64 = 1_609_459_200; // 2021-01-01
const TS_2025: i64 = 1_735_689_600; // 2025-01-01

const CPCV_BLOCKS: usize = 10; // matches guardian dynamic rule for >2M M1 rows
const OOS_MIN_TRADES: i32 = 200;
const PF_GATE: f64 = 1.10;
const PEN_SHARPE_GATE: f64 = 0.30;
const CPCV_FOLD_MIN_TRADES: i32 = 20;
const CPCV_PASS_RATE_GATE: f64 = 0.60;
const CPCV_MEDIAN_SHARPE_MAX: f64 = 2.0;

#[derive(Deserialize)]
struct ManifestEntry {
    name: String,
    symbol: String,
    category: String,
    indicator_type: String,
    timeframe_min: i64,
    tf_seconds: i64,
    sma_short: usize,
    sma_long: usize,
    sl: f64,
    tp: f64,
    volume: f64,
    #[serde(default)]
    is_trades: Option<i64>,
    #[serde(default)]
    is_pf: Option<f64>,
    #[serde(default)]
    is_sharpe: Option<f64>,
}

#[derive(Serialize, Clone)]
struct RunMetrics {
    trades: i32,
    pf: f64,
    sharpe: f64,
    penalized_sharpe: f64,
    max_dd: f64,
    win_rate: f64,
}

impl RunMetrics {
    fn from(r: &BacktestResult) -> Self {
        RunMetrics {
            trades: r.trades,
            pf: r.profit_factor,
            sharpe: r.sharpe,
            penalized_sharpe: calculate_penalized_sharpe(r.sharpe, r.trades),
            max_dd: r.max_drawdown,
            win_rate: r.win_rate,
        }
    }
}

#[derive(Serialize)]
struct FoldMetrics {
    block: usize,
    trades: i32,
    pf: f64,
    sharpe: f64,
    penalized_sharpe: f64,
    pass: bool,
}

#[derive(Serialize)]
struct CpcvSummary {
    valid_folds: usize,
    passing_folds: usize,
    pass_rate: f64,
    median_sharpe: f64,
    median_penalized_sharpe: f64,
    median_pf: f64,
    folds: Vec<FoldMetrics>,
}

#[derive(Serialize)]
struct StratResult {
    name: String,
    symbol: String,
    category: String,
    indicator_type: String,
    timeframe_min: i64,
    sma_short: usize,
    sma_long: usize,
    sl: f64,
    tp: f64,
    // reported IS (from library) for fidelity cross-check
    reported_is_trades: Option<i64>,
    reported_is_pf: Option<f64>,
    reported_is_sharpe: Option<f64>,
    // my reproduction of IS-like window (2015-2020) at guardian default cost
    fidelity_is: RunMetrics,
    // real-cost OOS (2021-2024, 2-pip)
    oos: RunMetrics,
    cpcv: CpcvSummary,
    oos_qualified: bool, // trades>=200 & PF>=1.10 & penalized_sharpe>=0.3
    cpcv_ok: bool,       // pass_rate>=0.6 & median_sharpe<2.0
    diversity_ok: bool,  // non-USDJPY OR non-TREND
}

fn build_strategy(m: &ManifestEntry) -> Strategy {
    let it = match m.indicator_type.to_lowercase().as_str() {
        "ema" => IndicatorType::Ema,
        _ => IndicatorType::Sma,
    };
    Strategy {
        name: m.name.clone(),
        sma_short: m.sma_short,
        sma_long: m.sma_long,
        sl: m.sl,
        tp: m.tp,
        volume: m.volume,
        indicator_type: it,
        filter_enabled: false,
        filter_tf: String::new(),
        filter_period: 0,
        filter_logic: String::new(),
        entry_long_ast: None,
        entry_short_ast: None,
        exit_long_ast: None,
        exit_short_ast: None,
    }
}

// ts in [lo, hi)
fn slice_range<'a>(c: &'a [Candle], lo: i64, hi: i64) -> &'a [Candle] {
    let start = c.partition_point(|x| x.timestamp < lo);
    let end = c.partition_point(|x| x.timestamp < hi);
    &c[start..end]
}

fn run_on(strat: &Strategy, m1: &[Candle], tf_seconds: i64, slippage: f64) -> BacktestResult {
    let aux: HashMap<String, Vec<Candle>> = HashMap::new();
    let swap: Vec<SwapRecord> = Vec::new();
    if tf_seconds > 60 {
        let tf = resample_candles(m1, tf_seconds);
        run_backtest_with_slippage(strat, &tf, &aux, &swap, slippage)
    } else {
        run_backtest_with_slippage(strat, m1, &aux, &swap, slippage)
    }
}

fn median(v: &mut Vec<f64>) -> f64 {
    if v.is_empty() {
        return 0.0;
    }
    v.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let mid = v.len() / 2;
    if v.len() % 2 == 0 {
        (v[mid - 1] + v[mid]) / 2.0
    } else {
        v[mid]
    }
}

fn run_cpcv(strat: &Strategy, full: &[Candle], tf_seconds: i64, slippage: f64) -> CpcvSummary {
    let n = full.len();
    let mut folds: Vec<FoldMetrics> = Vec::new();
    let mut sharpes: Vec<f64> = Vec::new();
    let mut pens: Vec<f64> = Vec::new();
    let mut pfs: Vec<f64> = Vec::new();
    let mut passing = 0usize;
    let mut valid = 0usize;
    for b in 0..CPCV_BLOCKS {
        let start = b * n / CPCV_BLOCKS;
        let end = if b == CPCV_BLOCKS - 1 { n } else { (b + 1) * n / CPCV_BLOCKS };
        let block = &full[start..end];
        let r = run_on(strat, block, tf_seconds, slippage);
        if r.trades == 0 {
            // still record as a (failing) fold for transparency, but it is a valid fold
            folds.push(FoldMetrics { block: b, trades: 0, pf: 0.0, sharpe: 0.0, penalized_sharpe: 0.0, pass: false });
            valid += 1;
            sharpes.push(0.0);
            pens.push(0.0);
            pfs.push(0.0);
            continue;
        }
        let pen = calculate_penalized_sharpe(r.sharpe, r.trades);
        let pass = r.trades >= CPCV_FOLD_MIN_TRADES && r.profit_factor >= PF_GATE && pen >= PEN_SHARPE_GATE;
        if pass {
            passing += 1;
        }
        valid += 1;
        sharpes.push(r.sharpe);
        pens.push(pen);
        pfs.push(r.profit_factor);
        folds.push(FoldMetrics {
            block: b,
            trades: r.trades,
            pf: r.profit_factor,
            sharpe: r.sharpe,
            penalized_sharpe: pen,
            pass,
        });
    }
    let pass_rate = if valid > 0 { passing as f64 / valid as f64 } else { 0.0 };
    CpcvSummary {
        valid_folds: valid,
        passing_folds: passing,
        pass_rate,
        median_sharpe: median(&mut sharpes.clone()),
        median_penalized_sharpe: median(&mut pens.clone()),
        median_pf: median(&mut pfs.clone()),
        folds,
    }
}

fn arg(args: &[String], key: &str) -> Option<String> {
    args.iter().position(|a| a == key).and_then(|i| args.get(i + 1)).cloned()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let data = arg(&args, "--data").expect("--data <csv> required");
    let manifest_path = arg(&args, "--manifest").expect("--manifest <json> required");
    let out_path = arg(&args, "--out").expect("--out <json> required");

    eprintln!("[kill] loading candles {}", data);
    let all = load_candles_from_csv(&data)?;
    eprintln!("[kill] {} M1 candles loaded ({}..{})", all.len(), all.first().map(|c| c.timestamp).unwrap_or(0), all.last().map(|c| c.timestamp).unwrap_or(0));

    let manifest: Vec<ManifestEntry> = serde_json::from_str(&std::fs::read_to_string(&manifest_path)?)?;
    eprintln!("[kill] {} strategies in manifest", manifest.len());

    let is_slice = slice_range(&all, TS_2015, TS_2021); // 2015-2020 fidelity window
    let oos_slice = slice_range(&all, TS_2021, TS_2025); // 2021-2024 OOS
    eprintln!("[kill] IS(2015-2020)={} M1  OOS(2021-2024)={} M1", is_slice.len(), oos_slice.len());

    let mut results: Vec<StratResult> = Vec::new();
    for (i, m) in manifest.iter().enumerate() {
        let strat = build_strategy(m);
        let fidelity = run_on(&strat, is_slice, m.tf_seconds, COST_DEFAULT);
        let oos = run_on(&strat, oos_slice, m.tf_seconds, COST_2PIP);
        let cpcv = run_cpcv(&strat, &all, m.tf_seconds, COST_2PIP);

        let oos_pen = calculate_penalized_sharpe(oos.sharpe, oos.trades);
        let oos_qualified =
            oos.trades >= OOS_MIN_TRADES && oos.profit_factor >= PF_GATE && oos_pen >= PEN_SHARPE_GATE;
        let cpcv_ok = cpcv.pass_rate >= CPCV_PASS_RATE_GATE && cpcv.median_sharpe < CPCV_MEDIAN_SHARPE_MAX;
        // NB: `category` may arrive with or without a leading colon depending on the
        // manifest source (SQL columns store "TREND"; .lisp-derived manifests store
        // ":TREND"). Normalise before the diversity test — comparing raw against
        // ":TREND" only was a false-positive bug that flagged every USDJPY/TREND clone
        // as "diverse" and produced a spurious §4 SURVIVE (2026-07-13 go/no-go).
        let cat_norm = m.category.trim_start_matches(':');
        let diversity_ok = m.symbol != "USDJPY" || cat_norm != "TREND";

        eprintln!(
            "[{:2}/{}] {:42} tf={:4}m sma={}/{} | IS(t={},pf={:.3}) OOS(t={},pf={:.3},pen={:.3}) CPCV(pr={:.2},med={:.2}) {}",
            i + 1, manifest.len(), &m.name, m.timeframe_min, m.sma_short, m.sma_long,
            fidelity.trades, fidelity.profit_factor,
            oos.trades, oos.profit_factor, oos_pen,
            cpcv.pass_rate, cpcv.median_sharpe,
            if oos_qualified { "QUALIFIED" } else { "" }
        );

        results.push(StratResult {
            name: m.name.clone(),
            symbol: m.symbol.clone(),
            category: m.category.clone(),
            indicator_type: m.indicator_type.clone(),
            timeframe_min: m.timeframe_min,
            sma_short: m.sma_short,
            sma_long: m.sma_long,
            sl: m.sl,
            tp: m.tp,
            reported_is_trades: m.is_trades,
            reported_is_pf: m.is_pf,
            reported_is_sharpe: m.is_sharpe,
            fidelity_is: RunMetrics::from(&fidelity),
            oos: RunMetrics::from(&oos),
            cpcv,
            oos_qualified,
            cpcv_ok,
            diversity_ok,
        });
    }

    // ---- §4 verdict ----
    let qualified: Vec<&StratResult> = results.iter().filter(|r| r.oos_qualified).collect();
    let qualified_cpcv: Vec<&StratResult> =
        qualified.iter().cloned().filter(|r| r.cpcv_ok).collect();
    let diverse = qualified.iter().any(|r| r.diversity_ok);

    let s4_part1 = qualified.len() >= 3;
    let s4_part2 = diverse;
    let s4_part3 = !qualified_cpcv.is_empty(); // at least the qualified ones also clear CPCV
    let s4_pass = s4_part1 && s4_part2 && s4_part3;

    let verdict = serde_json::json!({
        "experiment": "KILL_CRITERIA §4 decisive OOS+CPCV",
        "date": "2026-07-08",
        "population": {
            "n": results.len(),
            "all_usdjpy": results.iter().all(|r| r.symbol == "USDJPY"),
            "all_trend": results.iter().all(|r| r.category == ":TREND"),
        },
        "cost_model": "round-trip 2 pip (slippage=0.01 per side, applied entry+exit); sharpe annualized daily x sqrt(252); penalized = Taleb haircut",
        "gates": {
            "oos_min_trades": OOS_MIN_TRADES,
            "pf_gate": PF_GATE,
            "penalized_sharpe_gate": PEN_SHARPE_GATE,
            "cpcv_pass_rate_gate": CPCV_PASS_RATE_GATE,
            "cpcv_median_sharpe_max": CPCV_MEDIAN_SHARPE_MAX
        },
        "results_count": {
            "oos_qualified": qualified.len(),
            "oos_qualified_and_cpcv_ok": qualified_cpcv.len(),
            "any_diverse_among_qualified": diverse
        },
        "s4": {
            "part1_ge3_qualified": s4_part1,
            "part2_diversity": s4_part2,
            "part3_cpcv": s4_part3,
            "PASS": s4_pass
        },
        "kill_recommendation": if s4_pass { "SURVIVE (A promotion)" } else { "KILL per §5 (real-cost OOS produced no qualifying diverse survivors)" },
        "strategies": results
    });

    std::fs::write(&out_path, serde_json::to_string_pretty(&verdict)?)?;
    eprintln!("\n=== §4 VERDICT ===");
    eprintln!("OOS-qualified (trades>=200 & PF>=1.10 & penalized_sharpe>=0.3): {}", qualified.len());
    eprintln!("  ...of which CPCV-ok (pass_rate>=0.6 & median_sharpe<2.0): {}", qualified_cpcv.len());
    eprintln!("  ...any diverse (non-USDJPY or non-TREND): {}", diverse);
    eprintln!("§4 PASS = {}  =>  {}", s4_pass, if s4_pass { "SURVIVE" } else { "KILL per §5" });
    eprintln!("wrote {}", out_path);
    Ok(())
}
