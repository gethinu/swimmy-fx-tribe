# 🧬 Implementation Plan V50.2: The "Design Image" (Evolution 2.0)

**Date**: 2026-01-28
**Status**: Planning
**Goal**: Implement the User's specific "Design Image" for Strategy Lifecycle, Ranking, and Breeding.

---

## 1. 🏗️ Core Architecture (The Foundation)

### Timeframes & Universes
*   **Timeframes**: `M5`, `M15`, `H1`, `H4`, `D1`, `W1`, `MN1` (Month).
*   **Pairs**: `USDJPY`, `EURUSD`, `GBPUSD`.
*   **Backtest Range 1 (Screening)**: 2006-01-01 ~ 2020-12-31 (15 Years).
*   **Backtest Range 2 (Validation)**: 2021-01-01 ~ Present (~5 Years).

### The Ranking Ladder
1.  **Newborn**: Created via Breeding, Hunting, or Mutation.
2.  **Phase 1 Screening (Backtest 1)**:
    *   Test on all 3 pairs (USDJPY, EURUSD, GBPUSD).
    *   **Selection**: Pick the BEST performing pair.
    *   **Metrics**: Sharpe, PF, Win-Rate.
    *   **Verdict**:
        *   PASS -> Promote to **Rank B**.
        *   FAIL -> **Graveyard** (Discard, save parameters to DB for avoidance).
3.  **Rank B (The Pool)**:
    *   Wait until **100 Strategies** accumulate per Timeframe (e.g., 100 H1 Strategies).
    *   **Culling Event**: Once 100 reached, compare Sharpe/PF.
    *   **Promotion**: **Top 2** move to **Rank A**.
    *   **Discard**: Bottom 98 (or subset) remain or die? (Assume standard culling/recycling).
4.  **Rank A (The Candidates)**:
    *   **Phase 2 Validation (Backtest 2)**: Test on 2021-Present (Out-of-Sample).
    *   **Re-Measure**: Check Sharpe, PF, WR against Standard Criteria.
    *   **Verdict**:
        *   PASS -> Promote to **Rank S**.
        *   FAIL -> Graveyard (Discard).
5.  **Rank S (The Gladiators)**:
    *   **Active Trading**: Allowed to execute trades in MT5.
    *   **Learning**: Results (Win/Loss) accumulated for RL feedback.

---

## 2. 🧬 Breeding Logic (Evolution)

### Competitive Breeding (Parent vs Child)
When a strategy breeds:
1.  **Comparison**: Compare Parent vs Child performance (Metrics).
2.  **Outcome**:
    *   **Child > Parent**: Parent -> **Graveyard**.
    *   **Parent > Child**: Child -> **Graveyard**.
3.  **Aging**: A strategy can breed max **3 times**. After 3rd breed -> **Graveyard** (Old Age).

### Legendary Exception
*   **Legends**: The original 61 "Golden" strategies.
*   **Rules**:
    *   **Immortal**: Never sent to Graveyard by aging or competition.
    *   **Studs**: Periodically breed with random **Rank B** strategies to inject quality DNA.

---

## 3. 📋 Implementation Phases

### Phase 20: Architecture Upgrade (Data & Backtest)
- [ ] **Mod**: Update `school-constants.lisp` with new Timeframes/Pairs.
- [ ] **New**: `school-backtest-v2.lisp` implementing the 2-Stage Range logic.
- [ ] **New**: `school-ranking.lisp` implementing the B->A->S logic.

### Phase 21: Breeding & Competition
- [ ] **Mod**: `school-breeder.lisp` to implement Parent vs Child logic.
- [ ] **Mod**: Add `age` slot to strategy struct (track breed count).
- [ ] **Mod**: Implement Legendary Exception logic.

### Phase 22: Migration & Cleanup
- [ ] **Migrate**: Convert existing strategies to fit new scheme (or purge if incompatible).
- [ ] **Verify**: Run full lifecycle test (Breed -> BT1 -> Rank B -> Cull -> BT2 -> Rank S).

---

## 4. 📉 Standard Criteria (Baseline)

| Metric | Rank B Threshold | Rank A/S Threshold |
|:-------|:-----------------|:-------------------|
| Sharpe | >= 0.1           | >= 0.5             |
| PF     | >= 1.0           | >= 1.5             |
| WinRate| >= 30%           | >= 45%             |

*(Note: User said "Sharpe, PF, WR as criteria". I used V49.8 values as defaults, adjustable via Config)*
