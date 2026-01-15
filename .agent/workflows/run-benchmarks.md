---
description: Run CI/CD test suite and benchmarks
---

# Run Tests and Benchmarks

This workflow runs the Graham benchmark suite to verify system quality.

## Steps

1. **Quick Impact Test (CI)**
   Run the standard test suite to catch regressions.
   // turbo
   ```bash
   cd /home/swimmy/swimmy && chmod +x ci-test.sh && ./ci-test.sh
   ```

2. **Full Benchmark (SBCL)**
   Run performance benchmarks.
   ```bash
   sbcl --noinform --load run.sh --eval '(swimmy-benchmarks:run-all-benchmarks)' --quit
   ```

3. **Sharpe Verification**
   Verify the Sharpe Ratio calculation logic (fixes applied 2026-01-16).
   ```bash
   sbcl --noinform --load run.sh --eval '(swimmy.school:verify-sharpe-logic)' --quit
   ```

## Expected Results
- All unit tests PASS.
- Benchmark timings within acceptable limits (<100ms per tick).
