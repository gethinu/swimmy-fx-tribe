# Legend Gate Comparison Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a local, dependency-free comparator that runs Legend strategies with SL/TP + slippage and compares baseline vs model-gated results on USDJPY/EURUSD/GBPUSD.

**Architecture:** Single Python script (`tools/legend_gate_compare.py`) with importable functions for indicators, model gate, simulation, and metrics. A small unittest module verifies core computations and SL/TP handling without external dependencies.

**Tech Stack:** Python 3 standard library (csv, math, unittest).

### Task 1: Core Indicators + Gate Logic

**Files:**
- Create: `tools/legend_gate_compare.py`
- Test: `tools/tests/test_legend_gate_compare.py`

**Step 1: Write the failing test**

```python
# tools/tests/test_legend_gate_compare.py
import unittest
from tools.legend_gate_compare import sma_series, rsi_series, model_gate_predict

class TestIndicators(unittest.TestCase):
    def test_sma_basic(self):
        values = [1, 2, 3, 4, 5]
        sma = sma_series(values, 3)
        self.assertIsNone(sma[0])
        self.assertIsNone(sma[1])
        self.assertAlmostEqual(sma[2], 2.0)

    def test_rsi_basic(self):
        values = [1, 2, 3, 4, 5, 6, 7]
        rsi = rsi_series(values, 2)
        self.assertIsNotNone(rsi[-1])

    def test_model_gate_shape(self):
        closes = [1.0 + i * 0.01 for i in range(60)]
        preds = model_gate_predict(closes)
        self.assertEqual(len(preds), len(closes))

if __name__ == "__main__":
    unittest.main()
```

**Step 2: Run test to verify it fails**

Run: `python3 -m unittest tools/tests/test_legend_gate_compare.py -v`
Expected: FAIL with "ModuleNotFoundError" or "name not defined".

**Step 3: Write minimal implementation**

```python
# tools/legend_gate_compare.py (skeleton)

def sma_series(values, period):
    ...

def rsi_series(values, period):
    ...

def model_gate_predict(closes):
    ...
```

**Step 4: Run test to verify it passes**

Run: `python3 -m unittest tools/tests/test_legend_gate_compare.py -v`
Expected: PASS.

**Step 5: Commit**

```bash
git add tools/legend_gate_compare.py tools/tests/test_legend_gate_compare.py
git commit -m "feat: add legend gate indicator and gate helpers"
```

### Task 2: SL/TP Simulator + Metrics + CLI

**Files:**
- Modify: `tools/legend_gate_compare.py`
- Test: `tools/tests/test_legend_gate_compare.py`

**Step 1: Write the failing test**

```python
# Add to tools/tests/test_legend_gate_compare.py
from tools.legend_gate_compare import simulate_trades

class TestSimulator(unittest.TestCase):
    def test_sl_tp_priority(self):
        # One bar after entry hits both SL and TP; SL should win.
        opens = [100.0, 100.0]
        highs = [100.0, 101.0]
        lows = [100.0, 99.0]
        closes = [100.0, 100.5]
        trades = simulate_trades(
            opens, highs, lows, closes,
            entries=[0], exits=[],
            sl=0.5, tp=0.5,
            slippage=0.0,
        )
        self.assertEqual(len(trades), 1)
        self.assertLess(trades[0][1], 0.0)
```

**Step 2: Run test to verify it fails**

Run: `python3 -m unittest tools/tests/test_legend_gate_compare.py -v`
Expected: FAIL with "simulate_trades not defined".

**Step 3: Write minimal implementation**

```python
# tools/legend_gate_compare.py

def simulate_trades(...):
    ...

# Add CLI entrypoint that prints IS/OOS tables for the 6 cases
# using USDJPY/EURUSD/GBPUSD + legend strategy definitions.
```

**Step 4: Run test to verify it passes**

Run: `python3 -m unittest tools/tests/test_legend_gate_compare.py -v`
Expected: PASS.

**Step 5: Commit**

```bash
git add tools/legend_gate_compare.py tools/tests/test_legend_gate_compare.py
git commit -m "feat: add legend gate simulator and report CLI"
```

### Task 3: Run the Comparison and Capture Output

**Files:**
- None (runtime only)

**Step 1: Run the script**

Run: `python3 tools/legend_gate_compare.py --pairs USDJPY EURUSD GBPUSD`
Expected: Prints IS/OOS tables for baseline vs gated across both strategies.

**Step 2: Save summary in notes**

Capture the output in the response (no file changes needed).
