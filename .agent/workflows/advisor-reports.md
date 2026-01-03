---
description: Generate advisor reports (Taleb, Naval, Graham)
---

# Advisor Reports

Generate comprehensive reports from all advisors.

## All Advisors Report
1. Load the system:
```lisp
(load "brain.lisp")
```

## Taleb Report
2. Run Taleb's full analysis:
```lisp
(taleb-full-report)
```
This includes:
- Sharpe ratio (30/60/90 day)
- Antifragility metric
- Fat tail detection
- Barbell strategy compliance

## Naval Report
3. Run Naval's meta-learning report:
```lisp
(meta-learning-report)
(transfer-learning-report)
(naval-proof-report)
```

## Graham Report
4. Run Graham's benchmark suite:
```lisp
(load "src/lisp/benchmark.lisp")
(run-all-benchmarks)
```

## Combined Summary
5. Quick status check:
```lisp
(format t "~%═══ ADVISOR STATUS ═══~%")
(format t "Taleb: ~a~%" (if (> (calculate-rolling-sharpe 90) 1.0) "✅" "⚠️"))
(format t "Naval: ~a~%" (getf (get-proof-progress) :verdict))
(format t "Graham: CI ready~%")
```
