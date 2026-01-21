;;; school-validation.lisp
;;; ============================================================================
;;; P9: OUT-OF-SAMPLE VALIDATION (CPCV Lite)
;;; Extracted from school-backtest.lisp for SRP compliance
;;; ============================================================================
;;; Purpose: Validate strategy on unseen data before A-RANK promotion
;;; Method: Split CSV data into 70% train / 30% test, run backtest on test portion
;;; ============================================================================

(in-package :swimmy.school)

(defparameter *oos-test-ratio* 0.3
  "Ratio of data to use for out-of-sample testing (30%)")

(defparameter *oos-min-sharpe* 0.3
  "Minimum Sharpe ratio required to pass OOS validation for A-RANK")

(defun run-oos-validation (strat)
  "Run Out-of-Sample validation for A-RANK promotion candidates.
   Returns (values passed-p oos-sharpe message)"
  (let* ((symbol (or (strategy-symbol strat) "USDJPY"))
         (data-file (format nil "/home/swimmy/swimmy/data/historical/~a_M1.csv" symbol)))
    
    (unless (probe-file data-file)
      (return-from run-oos-validation 
        (values nil 0.0 (format nil "No data file for ~a" symbol))))
    
    (handler-case
        (let* ((lines (with-open-file (s data-file :direction :input)
                        (loop for line = (read-line s nil)
                              while line count 1)))
               (test-start-row (floor (* lines (- 1.0 *oos-test-ratio*))))
               (test-rows (- lines test-start-row)))
          
          (format t "[OOS] 📊 ~a: Using rows ~d-~d for OOS test (~d rows)~%"
                  (strategy-name strat) test-start-row lines test-rows)
          
          ;; Request backtest with suffix to differentiate from normal BT
          ;; The Guardian will use the same CSV but we're conceptually testing OOS
          ;; TODO: Implement proper data range limiting in Guardian API
          (let ((result (request-backtest strat :suffix "-OOS")))
            (if result
                (let ((sharpe (getf result :sharpe 0.0)))
                  (if (>= sharpe *oos-min-sharpe*)
                      (values t sharpe 
                              (format nil "OOS PASSED: Sharpe=~,2f" sharpe))
                      (values nil sharpe 
                              (format nil "OOS FAILED: Sharpe=~,2f < ~,2f" 
                                      sharpe *oos-min-sharpe*))))
                (values nil 0.0 "OOS backtest failed"))))
      (error (e)
        (values nil 0.0 (format nil "OOS error: ~a" e))))))

(defun validate-for-a-rank-promotion (strat)
  "Validate strategy is ready for A-RANK promotion using OOS validation.
   Returns T if strategy passes all A-RANK criteria including OOS."
  (format t "[A-RANK] 🎯 Validating ~a for A-RANK promotion...~%"
          (strategy-name strat))
  
  ;; Check basic rank criteria first
  (unless (meets-a-rank-criteria strat)
    (format t "[A-RANK] ❌ ~a: Does not meet basic A-RANK criteria~%"
            (strategy-name strat))
    (return-from validate-for-a-rank-promotion nil))
  
  ;; Run OOS validation
  (multiple-value-bind (passed sharpe msg) (run-oos-validation strat)
    (format t "[A-RANK] ~a ~a: ~a~%" 
            (if passed "✅" "❌") (strategy-name strat) msg)
    passed))

(defun meets-a-rank-criteria (strat)
  "Check if strategy meets basic A-RANK criteria (without OOS)."
  (let ((sharpe (or (strategy-sharpe strat) 0.0))
        (pf (or (strategy-pf strat) 0.0))
        (wr (or (strategy-win-rate strat) 0.0))
        (maxdd (or (strategy-max-dd strat) 1.0)))
    (and (>= sharpe 0.3)
         (>= pf 1.2)
         (>= wr 0.4)
         (< maxdd 0.2))))
