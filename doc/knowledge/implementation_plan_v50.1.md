# 🦅 Implementation Plan V50.1: The Pivot (Quant & Sniper)

**Status**: Planning
**Version**: V50.1
**Theme**: "Less, but Better" (Subtractive Improvement)

---

## 🚀 Goal
Pivot away from Swarm (Complexity/Fragility) towards **Quantitative Precision** (Simons) and **Manual Excellence** (Paul Graham).
Simplify the codebase by removing dormant Swarm code.

---

## 📋 V50.1 Roadmap

### Phase 16: The Great Filter (Cleanup)
> *Advisor: Naval Ravikant*
- [ ] **DELETE** `school-swarm-core.lisp` (Dead Code).
- [ ] **DELETE** `school-factory.lisp` (Dead Code).
- [ ] Remove `initialize-project-haystack` calls/references.

### Phase 17: The Renaissance (Quant Infrastructure)
> *Advisor: Jim Simons*
- [ ] **CREATE** `src/lisp/school/school-quant.lisp`.
    - Statistical Primitives: Mean, Variance, Z-Score, Correlation (Pearson).
    - Pattern Primitives: Simple "Regime Detection" via Volatility Clustering.

### Phase 18: The Simons Class (Quant Specialists)
> *Advisor: Jim Simons*
- [ ] **CREATE** `school-quant-specialist.lisp`.
- [ ] Implement `quant-z-score-reversion` (Stat Arb).
- [ ] Implement `quant-volatility-clustering-breakout` (Breakout).

### Phase 19: The Sniper (Manual Tool)
> *Advisor: Paul Graham*
- [ ] **CREATE** `tools/sniper.lisp`.
    - A CLI tool to "Do things that don't scale".
    - Allows user to inject a *single, hand-crafted* strategy into the running system via ZMQ.
    - Usage: `./sniper "MY-STRAT" "M5" "(> rsi 80)"`

---

## 🛠️ Technical Details

### 1. Quant Library (`school-quant.lisp`)
```lisp
(defun calculate-z-score (series window)
  (let ((mean (calculate-sma series window))
        (std  (calculate-stddev series window)))
    (if (> std 0)
        (/ (- (first series) mean) std)
        0.0)))
```

### 2. Sniper Tool (`tools/sniper.lisp`)
- Wraps `sbcl` to load `swimmy.asd`.
- Construct a `make-strategy` form dynamically.
- Sends `FORCE_RECRUIT` command to port 5556.

---

## 🧪 Verification Plan

### Automated
1.  **Unit Test**: `test-school-quant.lisp` (Verify math accuracy).
2.  **Integration**: Verify `tools/sniper.lisp` successfully injects a strategy into the running brain.

### Manual
1.  Run `./sniper "TEST-SNIPER" ...` and check Discord/Logs for "Recruited".
2.  Observe "Simons Class" strategies in `school-db` (they should be generated on startup).
