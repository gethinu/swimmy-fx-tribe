# MT5 Followup Validation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Validate the short-window MT5 inventory winners over longer windows and audit the two worst-performing Legend ports without touching the frozen Swimmy runtime.

**Architecture:** Reuse the isolated portable MT5 tester runner to execute the same candidate set over yearly and fixed half-year windows. Parse the generated manifests and HTML reports to separate stable candidates from window-dependent ones, then write the findings into a single audit note.

**Tech Stack:** `python3`, `tools/mt5_inventory_tester.py`, MT5 Strategy Tester, Markdown docs

---

### Task 1: Run yearly validation windows

**Files:**
- Use: `tools/mt5_inventory_tester.py`
- Output: `data/reports/mt5/inventory_tester/run_20260307_071613`
- Output: `data/reports/mt5/inventory_tester/run_20260307_071724`

**Step 1: Execute the 2024 validation run**

Run:

```bash
python3 tools/mt5_inventory_tester.py \
  --job-set all \
  --job historical-s-bred940-trend-core \
  --job legend-macd-above-zero-cross \
  --job legend-macd-signal-cross \
  --job legend-pullback-breakout \
  --from-date 2024.01.01 \
  --to-date 2024.12.31 \
  --timeout-sec 1800
```

**Step 2: Execute the 2025 validation run**

Run:

```bash
python3 tools/mt5_inventory_tester.py \
  --job-set all \
  --job historical-s-bred940-trend-core \
  --job legend-macd-above-zero-cross \
  --job legend-macd-signal-cross \
  --job legend-pullback-breakout \
  --from-date 2025.01.01 \
  --to-date 2025.12.31 \
  --timeout-sec 1800
```

### Task 2: Run fixed OOS half-year windows

**Files:**
- Output: `data/reports/mt5/inventory_tester/run_20260307_071842`
- Output: `data/reports/mt5/inventory_tester/run_20260307_071952`
- Output: `data/reports/mt5/inventory_tester/run_20260307_072101`
- Output: `data/reports/mt5/inventory_tester/run_20260307_072157`

**Step 1: Execute 2024H1 and 2024H2**

Run:

```bash
python3 tools/mt5_inventory_tester.py \
  --job-set all \
  --job historical-s-bred940-trend-core \
  --job legend-macd-above-zero-cross \
  --job legend-macd-signal-cross \
  --job legend-pullback-breakout \
  --from-date 2024.01.01 \
  --to-date 2024.06.30 \
  --timeout-sec 1800

python3 tools/mt5_inventory_tester.py \
  --job-set all \
  --job historical-s-bred940-trend-core \
  --job legend-macd-above-zero-cross \
  --job legend-macd-signal-cross \
  --job legend-pullback-breakout \
  --from-date 2024.07.01 \
  --to-date 2024.12.31 \
  --timeout-sec 1800
```

**Step 2: Execute 2025H1 and 2025H2**

Run:

```bash
python3 tools/mt5_inventory_tester.py \
  --job-set all \
  --job historical-s-bred940-trend-core \
  --job legend-macd-above-zero-cross \
  --job legend-macd-signal-cross \
  --job legend-pullback-breakout \
  --from-date 2025.01.01 \
  --to-date 2025.06.30 \
  --timeout-sec 1800

python3 tools/mt5_inventory_tester.py \
  --job-set all \
  --job historical-s-bred940-trend-core \
  --job legend-macd-above-zero-cross \
  --job legend-macd-signal-cross \
  --job legend-pullback-breakout \
  --from-date 2025.07.01 \
  --to-date 2025.12.31 \
  --timeout-sec 1800
```

### Task 3: Audit the two unstable Legend ports

**Files:**
- Read: `src/mt5/legend_batch3/Legend_LondonBreakoutV1.mq5`
- Read: `src/mt5/legend_batch3/Legend_RSIReversionV1.mq5`
- Read: `data/reports/mt5/inventory_tester/run_20260307_064816/legend-london-breakout-v1/run_20260307_064816_legend-london-breakout-v1.htm`
- Read: `data/reports/mt5/inventory_tester/run_20260307_064816/legend-rsi-reversion-v1/run_20260307_064816_legend-rsi-reversion-v1.htm`

**Step 1: Confirm the code contracts**

Check:
- London breakout only trades after the `08:00-09:00` range, once per day, and forces exit at `16:00`.
- RSI reversion uses `RSI(2)` on `M5` with symmetric `10/90` thresholds and fixed `0.10` price SL/TP.

**Step 2: Confirm tester behavior**

Verify:
- London report entries happen once per active day and mostly terminate via `sl`.
- RSI report entries are symmetric on long/short direction but extremely high-frequency across the full day.

### Task 4: Publish the audit note and verify

**Files:**
- Create: `doc/knowledge/legend_mt5_followup_validation_20260307.md`

**Step 1: Write the audit note**

Include:
- yearly results
- half-year OOS ranking
- parameter audit conclusions
- remaining risks

**Step 2: Run verification**

Run:

```bash
python3 -m unittest \
  tools.tests.test_mt5_inventory_tester \
  tools.tests.test_legend_mq5_docs \
  tools.tests.test_historical_s_mq5_docs \
  -v
```
