;;; school-scoring.lisp - Strategy Scoring & Confidence Estimation
;;; Phase 5 Implementation (memo3.txt Section 5 & 6)

(in-package :swimmy.school)

;;; ============================================================================
;;; SCORING PARAMETERS
;;; ============================================================================

(defparameter *confidence-decay* 0.95 "Decay factor for confidence estimation")
(defparameter *min-confidence* 0.1 "Minimum confidence floor")

;;; ============================================================================
;;; CONFIDENCE ESTIMATOR (Edge Ratio v1)
;;; ============================================================================

(defun estimate-confidence (strategy)
  "Estimate strategy confidence using 'edge_ratio_v1' logic.
   Confidence = Sigmoid(Recent PnL Normalized) * Consistency Factor"
  (if (and strategy (strategy-pnl-history strategy))
      (let* ((history (strategy-pnl-history strategy))
             (recent (subseq history 0 (min 20 (length history))))
             (wins (count-if (lambda (x) (> x 0)) recent))
             (total (length recent))
             (win-rate (if (> total 0) (/ wins total) 0.5))
             (avg-pnl (if (> total 0) (/ (reduce #'+ recent) total) 0.0))
             ;; Consistency: penalize high variance
             (consistency (if (> win-rate 0.4) 1.0 0.5)))
        
        ;; Simple Edge Confidence: 0.0 to 1.0
        ;; If avg-pnl > 0 and WinRate > 40%, high confidence
        (float (max *min-confidence*
                    (min 1.0 
                         (* (+ 0.5 (* 0.5 (if (> avg-pnl 0) 1.0 -1.0))) ;; Base direction
                            win-rate 
                            consistency
                            (if (> (length history) 30) 1.2 0.8)))))) ;; Boost for longevity
      0.5)) ;; Default neutral confidence

;;; ============================================================================
;;; STATE VECTOR
;;; ============================================================================

(defun get-market-state-vector ()
  "Return current market state vector (Regime One-Hot for now)"
  (let ((regime (if (boundp '*current-regime*) *current-regime* :unknown)))
    ;; Return as an alist or hash-table equivalent
    ;; For MVP, we pass the label directly to adaptation map lookup
    regime))

;;; ============================================================================
;;; STRATEGY SCORING
;;; ============================================================================

(defun calculate-strategy-score (strategy context)
  "Calculate composite score: dot(AdaptVector, State) * Confidence
   Context is usually the current market regime."
  (when strategy
    (let* ((adapt-score (if (fboundp 'get-adaptation-multiplier)
                            (get-adaptation-multiplier strategy context)
                            1.0))
           (confidence (estimate-confidence strategy)))
      
      ;; Score = Adaptation Multiplier * Confidence
      ;; Example: 1.2 (Good Regime) * 0.8 (High Conf) = 0.96
      ;; Example: 0.8 (Bad Regime) * 0.8 (High Conf) = 0.64
      (* adapt-score confidence))))

;;; Helper to sort candidates
(defun rank-strategies-for-execution (strategies context)
  "Return list of (strategy . score) sorted by score descending"
  (let ((scored (mapcar (lambda (s) 
                          (cons s (calculate-strategy-score s context)))
                        strategies)))
    (sort scored #'> :key #'cdr)))
