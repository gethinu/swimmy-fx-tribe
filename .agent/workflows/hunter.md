---
description: Automated Strategy Hunter (Web -> Code)
---
# Strategy Hunter Protocol

This workflow allows the agent to autonomously hunt for new trading strategies from the web and inject them into Swimmy.

## Usage
`/hunter [keywords]` (e.g., `/hunter "RSI divergence"`, `/hunter "Mean Reversion PineScript"`)

## Steps

1.  **Search Phase**
    *   Use `search_web` to find high-quality strategy logic.
    *   Queries: `TradingView [keywords] strategy code`, `PineScript [keywords] algorithm`, `Best forex [keywords] strategy logic`.
    *   Target: Look for clear algorithmic rules (Entry, Exit, Stop Loss, Take Profit).

2.  **Analysis Phase**
    *   Read the content of promising search results.
    *   Extract the core logic.
    *   Discard vague or "black box" strategies.
    *   Ensure the logic relies on available indicators (SMA, EMA, RSI, BB, MACD, Stoch, Session) or simple math.

3.  **Implementation Phase**
    *   Convert the extracted logic into a `def-founder` Lisp form.
    *   Use a unique keyword (e.g., `:hunted-rsi-div`) and name (e.g., `Hunted-RSI-Div-Gen0`).
    *   **CRITICAL**: Use the `def-founder` macro syntax.
    *   Ensure param types are correct (e.g., integers for periods, floats for prices).

4.  **Injection Phase**
    *   Append the new `def-founder` block to `src/lisp/school-founders.lisp` (at the end, before the Census section if possible, or just at the very end).
    *   **Note**: Ensure `(immigration-census)` or explicit `(recruit-founder :key)` is called to activate it.

5.  **Verification Phase**
    *   Run `make lineage` to verify compilation.
    *   If successful, notify the user.

## Example
`/hunter "Heikin Ashi Scalp"` -> Agent searches, finds a logic, implements `Hunted-HA-Scalp-Gen0`, injects it, validates.
