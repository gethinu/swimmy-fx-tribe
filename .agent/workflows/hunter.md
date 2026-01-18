---
description: Real Strategy Hunter (Web Search -> Injection)
---
# Real Strategy Hunt Workflow
This workflow instructs the Agent to search the web for trading strategies, extracting logic, and injecting it into the system.

## 1. Search for Strategies
- Use `search_web` to find "Forex trading strategies PineScript MQL4 2024 2025" or specific technical indicators.
- **Criteria**: Look for concrete logic (Entry, Exit, SL/TP). Avoid generic advice.
- **Target**: Find at least 1 distinct strategy logic (e.g., "RSI + EMA Crossover", "Bollinger Squeeze with MACD").

## 2. Extract Logic & Format
- Extract the core rules:
    - **Indicators**: What indicators with what parameters? (e.g., RSI 14, EMA 50)
    - **Entry**: Condition to buy/sell (e.g., Close > EMA 50 AND RSI < 30)
    - **Exit**: Condition to close (SL, TP, or indicator based)
- Convert to Swimmy Lisp `def-founder` format.
    - Use `src/lisp/school/school-hunter.lisp` as a reference for syntax.
    - Ensure unique name: `Hunted-[Name]-[Timestamp]`

## 3. Inject Strategy
- Append the `def-founder` code block to `src/lisp/school/school-hunter.lisp`.
- **Validation**: Ensure parentheses are balanced.

## 4. Notify System
- The system automatically detects the new strategy on restart or reload.
- (Optional) Run `make run` if system is not running.

## Example Code Block
```lisp
(def-founder :hunted-web-rsi-macd "Hunted-Web-RSI-MACD-Gen0"
  "Web Source: TradingView (RSI+MACD Trend)"
  (make-strategy
   :name "Hunted-Web-RSI-MACD-Gen0"
   :category :trend
   :timeframe "H1"
   :generation 0
   :indicators '((rsi 14) (macd 12 26 9))
   :entry '(and (> macd-main macd-signal) (> rsi 50))
   :exit '(or (< macd-main macd-signal) (> pnl tp) (< pnl (- sl)))))
```
