---
name: troubleshoot
description: Known issues and solutions for Swimmy system. Reference before debugging.
---

# Swimmy Troubleshooting Guide

## ⚠️ READ THIS FIRST
Before debugging ANY issue, check this list. Many problems have been solved before.

---

## 🔴 Backtest System Issues

### Backtest Requests Not Sent
**Symptoms**: No `📊 Requesting backtest` logs, cache not growing

**Cause**: `*candle-history*` is nil (legacy variable)

**Solution**: Use `*candle-histories*` hash table:
```lisp
(let ((snapshot (or *candle-history* 
                    (gethash "USDJPY" *candle-histories*))))
```

### Backtest Parse Errors
**Symptoms**: `❌ Backtest parse error: invalid type` in guardian.log

**Known Type Mismatches**:
| Lisp Field | Must Be | Common Mistake |
|------------|---------|----------------|
| `filter_enabled` | `:true` / `:false` | Using `nil` → JSON null |
| `sma_short/long` | Integer only | Float like BB deviation 2.0 |
| `filter_period` | Integer | nil → JSON null |

**Solution**: Fix `strategy-to-json` in `school-backtest.lisp`

### Cache Size Too Small
**Symptoms**: Cache has 6 entries but 292 strategies exist

**Cause**: Parse errors cause silent failures

**Check**: `tail logs/guardian.log | grep "parse error"`

---

## 🔴 Strategy Classification Issues

### All Strategies in Same Category
**Symptoms**: All 61 strategies classified as TREND

**Cause**: Categorization logic not running on startup

**Solution**: Force re-classification in startup sequence
```lisp
(dolist (s *strategy-knowledge-base*)
  (setf (strategy-category s) (categorize-strategy s)))
```
*Commits: 8549d9c, f47b591, 2760ae2*

### Symbol Mismatch
**Symptoms**: EURUSD strategy executing on GBPUSD

**Cause**: `check-symbol-mismatch` not called or returning nil

**Solution**: Verify `strategy-for-symbol` field or derive from name

---

## 🔴 Market Hours / Weekend Issues

### Phantom Notifications on Weekend
**Symptoms**: Trade notifications when market is closed

**Solution**: Add `market-open-p` check before notification
*Commit: 203bad3*

### D1+ Positions Closed on Restart
**Cause**: `CLOSE_ALL` sent during Dead Man's Switch

**Solution**: Use `CLOSE_SHORT_TF` command (protects D1/W1/MN)
- Lisp: Add TF to order comment: `|D1`, `|W1`, `|MN`
- MT5: `CloseShortTimeframePositions()` filters by comment

---

## 🔴 Sharpe Ratio Bugs

### Sharpe Shows Wrong Value (-0.19 bug)
**Cause**: Missing `pnl-history` slot, ratio→float conversion error

**Solution**: Ensure `(float sharpe)` is used in display
*Commit: 04a0311, da06d79*

### All Strategies Have Sharpe 0.00
**Cause**: Backtest not running (see Backtest System Issues above)

---

## 🔴 Safety Cap / Lot Size Issues

### JPY Lots Too Small
**Cause**: Safety cap formula not accounting for JPY scale

**Solution**: Use `/100` scaling for JPY symbols
*Commit: 92b095d, c90f4a6*

### Min Lot Blocking All Trades
**Cause**: `min-lot` set too high (>0.01)

**Solution**: Set `*base-lot-size*` to 0.01 minimum
*Commit: 04b3837*

---

## 🔴 Magic Number / Allocation Issues

### Duplicate Magic Numbers
**Symptoms**: Position sync fails, wrong positions closed

**Cause**: Magic generation not unique per slot

**Solution**: Use deterministic format: `1[CatID][Slot]` (e.g., 110001)
*Commit: 553078f*

---

## 🔴 Notification Issues

### Apex Webhook Not Firing
**Cause**: Package not exported, boundp check failing

**Solution**: Verify export in `packages.lisp`, check webhook URL
*Commits: 0e7561c, c303fcf*

---

## 🔴 Variable Name Confusion

| ✅ Correct | ❌ Wrong | Purpose |
|-----------|----------|---------|
| `*candle-histories*` | `*candle-history*` | Hash: symbol→candles |
| `*strategy-knowledge-base*` | `*evolved-strategies*` | Main strategy list |
| `:true` / `:false` | `t` / `nil` | JSON boolean (jsown) |

---

## How to Use This Skill

1. **Before debugging**: Search this file for symptoms
2. **If found**: Apply documented solution
3. **If new issue**: Add to this file after solving
4. **Commit message**: Include relevant commit hash when updating

