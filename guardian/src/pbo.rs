// guardian/src/pbo.rs
// ============================================================================
// PBO — PROBABILITY OF BACKTEST OVERFITTING via CSCV
// ============================================================================
// V-methodology (2026-08-11): Combinatorially-Symmetric Cross-Validation
// (Bailey, Borwein, Lopez de Prado, Zhu — "The Probability of Backtest
// Overfitting", 2017).
//
// PBO answers: "given a *set of candidate configurations* selected on in-sample
// performance, how often does the in-sample winner underperform the median of the
// rest out-of-sample?" It is a population-level overfit probability, and is the
// honest replacement/companion for a fixed pass-rate "floor".
//
// This module is a PURE numeric engine (std-only, no I/O, no backtester coupling).
// It is inert unless a caller opts in — nothing here runs on any default path.
// ============================================================================

/// Result of a CSCV / PBO computation.
#[allow(dead_code)] // diagnostic fields are read by tests / optional callers
#[derive(Clone, Debug, Default, PartialEq)]
pub struct PboResult {
    /// Probability of Backtest Overfitting in [0,1] — P(logit < 0).
    pub pbo: f64,
    /// Number of candidate configurations (columns) considered.
    pub n_strategies: usize,
    /// Number of disjoint time blocks S actually used (even).
    pub n_blocks: usize,
    /// Number of C(S, S/2) IS/OOS splits evaluated.
    pub n_splits: usize,
    /// Mean logit lambda across splits (diagnostic; >0 favourable, <0 overfit).
    pub mean_logit: f64,
}

/// All k-subsets of `0..n` (indices), strictly increasing.
fn combinations(n: usize, k: usize) -> Vec<Vec<usize>> {
    let mut results = Vec::new();
    if k == 0 || k > n {
        return results;
    }
    let mut combo: Vec<usize> = (0..k).collect();
    loop {
        results.push(combo.clone());
        let mut i = k;
        while i > 0 && combo[i - 1] == n - k + i - 1 {
            i -= 1;
        }
        if i == 0 {
            break;
        }
        combo[i - 1] += 1;
        for j in i..k {
            combo[j] = combo[j - 1] + 1;
        }
    }
    results
}

/// Average (fractional) rank of `value` within `all`, ascending so the worst gets
/// rank 1 and the best gets rank N. Ties share their averaged rank.
fn average_rank(value: f64, all: &[f64]) -> f64 {
    let mut less = 0usize;
    let mut equal = 0usize;
    for &v in all {
        if v < value {
            less += 1;
        } else if (v - value).abs() <= f64::EPSILON * value.abs().max(1.0) {
            equal += 1;
        }
    }
    // rank = (#strictly-less) + (#equal + 1)/2   -> average rank of the tie group
    less as f64 + (equal as f64 + 1.0) / 2.0
}

/// Compute PBO from a block-performance matrix.
///
/// `block_perf[b][n]` is the performance metric (e.g. per-block Sharpe) of
/// configuration `n` in time block `b`. Rows = S disjoint time blocks, columns =
/// N candidate configurations.
///
/// Returns `None` when the matrix is too small to form a symmetric split
/// (need N >= 2 and S >= 2). If S is odd the last block is dropped so that the
/// IS/OOS halves are symmetric (a standard CSCV convention).
pub fn compute_pbo(block_perf: &[Vec<f64>]) -> Option<PboResult> {
    let mut s = block_perf.len();
    if s < 2 {
        return None;
    }
    let n = block_perf[0].len();
    if n < 2 || block_perf.iter().any(|row| row.len() != n) {
        return None;
    }
    // Symmetric split needs an even number of blocks.
    if s % 2 == 1 {
        s -= 1;
    }
    let half = s / 2;
    let block_perf = &block_perf[..s];

    let is_sets = combinations(s, half);
    if is_sets.is_empty() {
        return None;
    }

    let mut overfit = 0usize;
    let mut logit_sum = 0.0f64;
    let mut splits = 0usize;

    for is_blocks in &is_sets {
        let mut in_is = vec![false; s];
        for &b in is_blocks {
            in_is[b] = true;
        }

        // Aggregate each config's performance over IS blocks and OOS blocks.
        let mut r_is = vec![0.0f64; n];
        let mut r_oos = vec![0.0f64; n];
        for b in 0..s {
            for cfg in 0..n {
                if in_is[b] {
                    r_is[cfg] += block_perf[b][cfg];
                } else {
                    r_oos[cfg] += block_perf[b][cfg];
                }
            }
        }

        // Best configuration in-sample (argmax; ties -> lowest index, deterministic).
        let mut best = 0usize;
        for cfg in 1..n {
            if r_is[cfg] > r_is[best] {
                best = cfg;
            }
        }

        // Relative OOS rank of the IS winner.
        let rank = average_rank(r_oos[best], &r_oos);
        let omega = rank / (n as f64 + 1.0); // in (0,1)
        // Guard the logit against exact 0/1.
        let omega = omega.clamp(1e-12, 1.0 - 1e-12);
        let lambda = (omega / (1.0 - omega)).ln();
        logit_sum += lambda;
        if lambda < 0.0 {
            overfit += 1;
        }
        splits += 1;
    }

    Some(PboResult {
        pbo: overfit as f64 / splits as f64,
        n_strategies: n,
        n_blocks: s,
        n_splits: splits,
        mean_logit: logit_sum / splits as f64,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn combinations_are_symmetric_and_counted() {
        assert_eq!(combinations(4, 2).len(), 6); // 4C2
        assert_eq!(combinations(6, 3).len(), 20); // 6C3
        assert_eq!(combinations(2, 1).len(), 2);
        for c in combinations(6, 3) {
            assert!(c.windows(2).all(|w| w[0] < w[1]));
        }
    }

    #[test]
    fn pbo_too_small_returns_none() {
        assert!(compute_pbo(&[]).is_none());
        assert!(compute_pbo(&[vec![1.0, 2.0]]).is_none()); // only 1 block
        assert!(compute_pbo(&[vec![1.0], vec![2.0]]).is_none()); // only 1 config
    }

    #[test]
    fn pbo_dominant_config_is_zero() {
        // Config A is strictly best in every block -> IS winner is always OOS best
        // -> logit > 0 everywhere -> PBO = 0.
        let m = vec![
            vec![3.0, 2.0, 1.0],
            vec![3.0, 2.0, 1.0],
            vec![3.0, 2.0, 1.0],
            vec![3.0, 2.0, 1.0],
        ];
        let r = compute_pbo(&m).unwrap();
        assert_eq!(r.n_strategies, 3);
        assert_eq!(r.n_blocks, 4);
        assert!((r.pbo - 0.0).abs() < 1e-12, "dominant config must give PBO=0, got {}", r.pbo);
        assert!(r.mean_logit > 0.0);
    }

    #[test]
    fn pbo_perfectly_overfit_is_one() {
        // Anti-correlated blocks: whichever config wins IS is worst OOS.
        // Two blocks, C(2,1)=2 splits, both overfit -> PBO = 1.
        let m = vec![
            vec![3.0, 2.0, 1.0], // block 0: A>B>C
            vec![1.0, 2.0, 3.0], // block 1: C>B>A
        ];
        let r = compute_pbo(&m).unwrap();
        assert_eq!(r.n_splits, 2);
        assert!((r.pbo - 1.0).abs() < 1e-12, "anti-correlated must give PBO=1, got {}", r.pbo);
        assert!(r.mean_logit < 0.0);
    }

    #[test]
    fn pbo_partial_overfit_is_strictly_between_0_and_1() {
        // A dominates the first half, C the second, B is always middle. Half the
        // symmetric splits are overfit (IS-winner worst OOS), half are neutral ties.
        // Expected PBO = 2/6 ≈ 0.3333 — proves PBO produces a real interior value,
        // not just the 0/1 corner cases.
        let m = vec![
            vec![3.0, 2.0, 1.0],
            vec![3.0, 2.0, 1.0],
            vec![1.0, 2.0, 3.0],
            vec![1.0, 2.0, 3.0],
        ];
        let r = compute_pbo(&m).unwrap();
        assert_eq!(r.n_splits, 6);
        assert!((r.pbo - (2.0 / 6.0)).abs() < 1e-12, "expected PBO=1/3, got {}", r.pbo);
        assert!(r.pbo > 0.0 && r.pbo < 1.0);
    }

    #[test]
    fn pbo_odd_blocks_drops_last_for_symmetry() {
        // 3 blocks -> uses 2 (even); should behave like the 2-block overfit case.
        let m = vec![
            vec![3.0, 2.0, 1.0],
            vec![1.0, 2.0, 3.0],
            vec![9.0, 9.0, 9.0], // dropped
        ];
        let r = compute_pbo(&m).unwrap();
        assert_eq!(r.n_blocks, 2);
        assert!((r.pbo - 1.0).abs() < 1e-12);
    }

    #[test]
    fn average_rank_orders_worst_to_best() {
        let all = vec![1.0, 2.0, 3.0];
        assert!((average_rank(1.0, &all) - 1.0).abs() < 1e-12); // worst
        assert!((average_rank(3.0, &all) - 3.0).abs() < 1e-12); // best
        // all tied -> average rank (N+1)/2 = 2
        let tied = vec![5.0, 5.0, 5.0];
        assert!((average_rank(5.0, &tied) - 2.0).abs() < 1e-12);
    }
}
