(in-package :swimmy.tests)

;;; ==========================================
;;; SCHOOL-MONTE-CARLO TESTS (Phase 11)
;;; ==========================================

(deftest test-bootstrap-sampling
  "Test internal bootstrap sampling properties"
  (let* ((pnl (vector 10.0 -5.0 2.0 8.0 -1.0))
         (sample (swimmy.school:internal-bootstrap-sample pnl 100)))
    (assert-equal 100 (length sample) "Sample size matches")
    ;; Check that values are from the original set
    (loop for x across sample do
      (assert-true (find x pnl) "Sample value must exist in population"))))

(deftest test-max-drawdown-calc
  "Test Max Drawdown Calculation Logic"
  ;; Equity: 100 -> 110 -> 100 (9% DD) -> 90 (18% DD) -> 120
  (let ((curve (vector 100.0 110.0 100.0 90.0 120.0)))
    ;; Peak 110. Low 90. DD = (110-90)/110 = 20/110 = 0.1818...
    (let ((dd (swimmy.school::calculate-max-drawdown curve)))
      (assert-true (> dd 0.18) "DD should be > 18%")
      (assert-true (< dd 0.19) "DD should be < 19%"))))

(deftest test-mc-integration
  "Integration Test: MC Simulation Flow"
  ;; Mock DB fetch? Or just pass list directly since run-mc-validation takes list.
  (let ((mock-pnls (loop repeat 50 collect (if (> (random 1.0) 0.4) 10.0 -5.0)))) ;; 60% win rate
    
    (let ((result (swimmy.school:run-mc-validation "TestStrategyMC" mock-pnls)))
      (assert-not-nil result "Should return MC result struct")
      (assert-equal "TestStrategyMC" (swimmy.school::mc-result-strategy-name result))
      (assert-true (numberp (swimmy.school::mc-result-median-dd result)) "Median DD is number")
      (format t "[TEST] MC Prob Ruin: ~a~%" (swimmy.school::mc-result-prob-ruin result)))))
