# Multi-Metric Scoring Design

**Goal:** Replace Sharpe-only ranking decisions with a unified composite score that uses PF/WR/MaxDD alongside Sharpe, with stage-specific metric sources (IS/OOS/CPCV).

## Architecture
We introduce a single scoring function that accepts a normalized metrics bundle and returns a scalar score. The same function is used across B-rank culling, breeder selection, and promotion/demotion logic, but the metric sources are stage-specific:
- **B/C candidates:** IS metrics (Sharpe, PF, WR, MaxDD)
- **A evaluation:** OOS metrics (oos_sharpe / oos_pf / oos_wr / oos_maxdd)
- **S evaluation:** CPCV medians (cpcv_median_sharpe / pf / wr / maxdd)

A lightweight “stage selector” chooses which fields to feed into scoring. Missing metrics for the required stage cause the score check to be skipped (no promotion on partial data).

## Scoring Formula
Composite score (higher is better):

`score = w_sharpe*norm_sharpe + w_pf*norm_pf + w_wr*norm_wr - w_dd*norm_maxdd`

Normalization ranges (clamped):
- Sharpe: 0–2
- PF: 1–2
- WR: 0.40–0.70
- MaxDD: 0–0.20 (inverted penalty)

Initial weights (tunable via defparameter):
- `*score-weight-sharpe* = 0.45`
- `*score-weight-pf* = 0.25`
- `*score-weight-wr* = 0.20`
- `*score-weight-maxdd* = 0.10`

## Targeted Changes
- `src/lisp/school/school-ranking.lisp`
  - `cull-rank-b-pool` sorting: Sharpe → composite score (IS)
- `src/lisp/school/school-breeder.lisp`
  - Parent selection and pool culling: Sharpe → composite score (IS)
- `src/lisp/school/school-strategy.lisp`
  - `check-promotion`: Sharpe thresholds → composite score thresholds (IS)
- `src/lisp/school/school-rank-system.lisp`
  - Use composite score for A/S evaluation (OOS/CPCV sources)

## Error Handling / Safety
- If required metrics are missing for a stage, skip score-based promotion and retain current rank.
- Keep existing absolute minimum gates where already enforced (e.g., CPCV pass-rate).

## Tests (TDD)
Add minimal unit tests to ensure:
1. Composite score favors PF/WR when Sharpe is lower but DD is controlled.
2. Composite score penalizes high MaxDD even with strong Sharpe.
3. `cull-rank-b-pool` uses composite score ordering.
4. `check-promotion` no longer depends on Sharpe alone.

## Success Criteria
- Sharpe-only sorting removed from targeted flows.
- Stage-specific metric sources are respected.
- Tests validate score behavior and selection logic changes.
