---
description: Run CI/CD test suite and benchmarks
---

# Run Tests and Benchmarks

This workflow runs the Graham benchmark suite to verify system quality.

## Quick Test
// turbo
1. Run the test script:
```bash
cd /home/swimmy/swimmy && chmod +x ci-test.sh && ./ci-test.sh
```

## Full Benchmark
2. Run all benchmarks in SBCL:
```lisp
(load "brain.lisp")
(load "src/lisp/benchmark.lisp")
(run-all-benchmarks)
```

## Sharpe Verification
3. Check 90-day Sharpe ratio:
```lisp
(run-sharpe-benchmark)
```

## Naval Proof Check
4. Check 3-month profitability proof:
```lisp
(naval-proof-report)
```

## Expected Results
- Sharpe > 1.0 over 90 days
- Max drawdown < 15%
- Naval proof in progress or passed
