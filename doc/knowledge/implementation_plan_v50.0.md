# 🐝 Implementation Plan V50.0: Project Haystack (Swarm Activation)

**Status**: Planning
**Version**: V50.0
**Theme**: "The Power of the Many" (Quantity has a Quality all its own)

---

## 🚀 Goal
Activate the dormant **Project Haystack** components (`school-swarm-core`, `school-factory`).
Transition Swimmy from a "Hero Strategy" model (searching for one perfect algo) to a "Swarm Intelligence" model (aggregating thousands of weak predictors).

---

## 📋 V50.0 Roadmap

### Phase 13: The Awakening (Activation)
> *Existing code integration*
- [ ] Hook `initialize-project-haystack` into `start-evolution-service` (`school-connector.lisp`).
- [ ] Ensure Swarm strategies are correctly registered in the SQL DB (`upsert-strategy`).
- [ ] Verify Swarm execution in `school-execution.lisp` (Does it respect the `-1/0/1` signal?).

### Phase 14: The Hive View (Visualization)
> *Observability*
- [ ] Add Swarm Status to Discord Reports (`notify-discord-status`).
- [ ] Show "Consensus Strength" in logs.
- [ ] Export consensus Dataframe for analysis.

### Phase 15: The Expansion (Predictor Factory 2.0)
> *Logic Diversity*
- [ ] Add `candlestick-patterns` to `generate-combinatorial-library`.
- [ ] Add `multi-timeframe` predictors (e.g. H1 trend confirmation for M5 swarm).
- [ ] Implement `prune-weak-predictors` (Survival of the fittest within the swarm).

---

## 🛠️ Technical Details

### 1. Integration Point
**File**: `src/lisp/school/school-connector.lisp`
**Change**:
```lisp
(defun start-evolution-service ()
  ...
  (format t "[MAIN] 🐝 Initializing Project Haystack...~%")
  (swimmy.school:initialize-project-haystack) ;; <--- NEW
  ...)
```

### 2. Execution Logic
**File**: `src/lisp/school/school-execution.lisp`
**Verify**:
- Does `get-signal` handle the `(values signal strength)` return from `convene-swarm-voting`?
- Currently `get-signal` expects a simple logic evaluation. Swarm needs special handling or a wrapper.

---

## 🧪 Verification Plan

### Automated
1.  **Unit Test**: `test-swarm-voting.lisp` - Mock history, ensure 10 predictors voting results in correct consensus.
2.  **Integration**: Run `(initialize-project-haystack :force t)` and check `*strategy-knowledge-base*`.

### Manual
1.  **Backtest**: Run a Phase 1 BT on the new `Swarm-USDJPY` strategy.
2.  **Live**: Watch logs for `[SWARM] Convening voting... Consensus=0.8` messages.

---

## 🔒 Owner's Safety Guidelines
- **Risk**: Swarm is creating a strategy with *many* moving parts.
- **Mitigation**: Swarm strategies start at **BATTLEFIELD** tier (0.01 lots) regardless of backtest results until proven.
