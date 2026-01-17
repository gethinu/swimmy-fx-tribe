(in-package :swimmy.school)

;;; ============================================================================
;;; SCHOOL-MONTE-CARLO (López de Prado)
;;; ============================================================================
;;; "The only thing we know about the future is that it will be different from the past."
;;; 
;;; Purpose:
;;; Provide probabilistic risk assessment using Combinatorial Purged Cross-Validation (CPCV)
;;; concepts and Bootstrap Aggregation (Bagging) to estimate the probability of
;;; ruin and extreme drawdowns.
;;;
;;; Key Metrics:
;;; - Prob(MaxDD > X%)
;;; - Prob(AnnualPnL < 0)
;;; - VaR (Value at Risk) @ 95%
;;;
;;; Reference:
;;; Marcos López de Prado, "Advances in Financial Machine Learning"

(defparameter *mc-iterations* 1000 "Number of Monte Carlo simulations to run")
(defparameter *mc-confidence-level* 0.05 "Confidence level for VaR (e.g., 0.05 for 95%)")

(defstruct mc-result
  (strategy-name "Unknown")
  (iterations 0)
  (median-dd 0.0)
  (p95-dd 0.0)       ; 95th percentile Max Drawdown (Worst case)
  (prob-ruin 0.0)    ; Probability of hitting User's Max DD limit
  (prob-loss 0.0)    ; Probability of negative total return
  (var-95 0.0)       ; Value at Risk (95%)
  (is-fragile nil))  ; Verdict

;;; Core Math ------------------------------------------------------------------

(defun internal-bootstrap-sample (pnl-vector &optional (size nil))
  "Generate a random sample with replacement from the PnL vector."
  (let* ((len (length pnl-vector))
         (n (or size len))
         (sample (make-array n :element-type 'float :initial-element 0.0)))
    (loop for i from 0 below n do
      (setf (aref sample i) (elt pnl-vector (random len))))
    sample))

(defun calculate-max-drawdown (equity-curve)
  "Calculate Maximum Drawdown from an equity curve vector."
  (let ((peak -1.0e10)
        (max-dd 0.0))
    (loop for value across equity-curve do
      (when (> value peak) (setf peak value))
      (let ((dd (if (> peak 0) 
                    (/ (- peak value) peak) 
                    0.0))) ; Handle zero/negative base reasonably, though equity usually starts at 1
        (when (> dd max-dd) (setf max-dd dd))))
    max-dd))

(defun generate-equity-curve (pnl-sample &optional (starting-equity 100000.0))
  "Simulate an equity curve from PnL changes."
  (let* ((len (length pnl-sample))
         (curve (make-array (1+ len) :element-type 'float :initial-element starting-equity)))
    (loop for i from 0 below len
          for cum = starting-equity then (+ cum (aref pnl-sample i))
          do (setf (aref curve (1+ i)) cum))
    curve))

;;; Simulation Logic -----------------------------------------------------------

(defun run-monte-carlo-simulation (trades &key (iterations *mc-iterations*) (starting-equity 100000.0))
  "Run Monte Carlo simulation on a list of trade PnLs."
  (let* ((pnl-vector (coerce trades 'vector))
         (drawdowns (make-array iterations :element-type 'float))
         (final-pnls (make-array iterations :element-type 'float)))
    
    (when (zerop (length pnl-vector))
      (format t "[MC] ⚠️ Warning: Empty trade history for MC.~%")
      (return-from run-monte-carlo-simulation nil))

    (format t "[MC] 🎲 Running ~d simulations on ~d trades...~%" iterations (length pnl-vector))
    
    (loop for i from 0 below iterations do
      (let* ((sample (internal-bootstrap-sample pnl-vector))
             (curve (generate-equity-curve sample starting-equity))
             (dd (calculate-max-drawdown curve))
             (final-equity (aref curve (1- (length curve))))
             (total-pnl (- final-equity starting-equity)))
        (setf (aref drawdowns i) dd)
        (setf (aref final-pnls i) total-pnl)))
    
    (values drawdowns final-pnls)))

(defun analyze-monte-carlo-results (strategy-name drawdowns final-pnls)
  "Analyze simulation results and generate report."
  (let* ((sorted-dd (sort (copy-seq drawdowns) #'<))
         (sorted-pnl (sort (copy-seq final-pnls) #'<))
         (n (length drawdowns))
         (idx-50 (floor (* n 0.50)))
         (idx-95 (floor (* n 0.95)))
         (idx-05 (floor (* n 0.05)))
         
         (median-dd (aref sorted-dd idx-50))
         (p95-dd (aref sorted-dd idx-95))
         
         ;; Prob of Ruin > 20% (Hardcoded threshold for now, or use Global)
         (ruin-count (count-if (lambda (x) (> x 0.20)) drawdowns)) 
         (prob-ruin (/ ruin-count (float n)))
         
         ;; Prob of Loss < 0
         (loss-count (count-if (lambda (x) (< x 0.0)) final-pnls))
         (prob-loss (/ loss-count (float n)))
         
         ;; VaR 95% (5th percentile of PnL distribution)
         (var-95 (aref sorted-pnl idx-05))
         
         ;; Fragility Criteria:
         ;; 1. 95% worst DD > 30%
         ;; 2. Prob Ruin > 5%
         (is-fragile (or (> p95-dd 0.30) (> prob-ruin 0.05))))
    
    (make-mc-result
     :strategy-name strategy-name
     :iterations n
     :median-dd median-dd
     :p95-dd p95-dd
     :prob-ruin prob-ruin
     :prob-loss prob-loss
     :var-95 var-95
     :is-fragile is-fragile)))

;;; Public API -----------------------------------------------------------------

(defun run-mc-validation (strategy-name trade-history-pnl)
  "Main entry point: Run MC validation for a strategy."
  ;; trade-history-pnl should be a list of float values
  (multiple-value-bind (dds pnls) 
      (run-monte-carlo-simulation trade-history-pnl)
    (when (and dds pnls)
      (let ((result (analyze-monte-carlo-results strategy-name dds pnls)))
        (format t "[MC] 📊 Report for ~a:~%" strategy-name)
        (format t "     Median DD: ~,1f% | 95% Worst DD: ~,1f%~%" (* 100 (mc-result-median-dd result)) (* 100 (mc-result-p95-dd result)))
        (format t "     Prob Ruin (>20%): ~,1f% | Prob Loss: ~,1f%~%" (* 100 (mc-result-prob-ruin result)) (* 100 (mc-result-prob-loss result)))
        (format t "     VaR (95%): $~,2f~%" (mc-result-var-95 result))
        (if (mc-result-is-fragile result)
            (format t "[MC] 🚩 RESULT: FRAGILE (High Risk Detected)~%")
            (format t "[MC] ✅ RESULT: ROBUST~%"))
        result))))
