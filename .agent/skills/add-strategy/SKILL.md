---
name: add-strategy
description: Adds a new trading strategy to the system.
---

# Add Strategy Skill

Helper for generating and registering new trading strategies.

## Steps

### 1. Understand Requirements
Analyze the user's request for the strategy (e.g., "RSI trend follower", "Bollinger Band breakout").

### 2. Locate Targets
Check `src/lisp/strategies.lisp`. This is the main registry for strategies.
(Note: If the system is modularized, check `src/lisp/school/` as well).

### 3. Generate Code
Create a new strategy definition using the `defstrategy` macro.

Format:
```lisp
(defstrategy "Strategy-Name"
  :indicators ((sma 10) (rsi 14))
  :entry (and (cross-above close sma-10) (> rsi 50))
  :exit (cross-below close sma-10)
  :timeframe 5  ;; M5
  :category :trend)
```

### 4. Implement
Append the code to `src/lisp/strategies.lisp`.

### 5. Hot Reload (Optional)
If Supported, send a reload command. Otherwise, use the `deploy` skill to restart the Brain.
