(in-package :swimmy.school)

;;; ============================================================================
;;; INDICATORS LIBRARY (V49.5 - Advanced Strategy Diversity)
;;; ============================================================================
;;; Defines regime-specific indicator sets to prevent "Genetic Stagnation"
;;; and ensure strategies use tools appropriate for their market context.
;;; ============================================================================

(defun get-indicators-for-regime (regime)
  "Return a list of indicators recommended for a given regime."
  (cdr (assoc regime *indicator-catalog*)))

(defparameter *indicator-weights* (make-hash-table :test 'equal)
  "V49.6: Adaptive weights for indicator selection (Bandit logic).")

(defparameter *indicator-catalog*
  '((:trend      . ((ema 20) (ema 50) (ema 200) (sma 100) (adx 14) (ichimoku 9 26 52) (psar 0.02 0.2)))
    (:reversion  . ((rsi 14) (bollinger 20 2) (stochastic 14 3 3) (mfi 14) (cci 20) (wpr 14)))
    (:breakout   . ((atr 14) (keltner 20 2) (donchian 20) (volume-osc 5 10) (standard-deviation 20)))
    (:volatility . ((atr 14) (bollinger-width 20 2) (vix-fix 22 20 8) (choppiness 14))))
  "Mapping of market regimes to optimal indicator sets.")

(defun get-indicator-weight (indicator regime)
  "Get weight for a specific indicator in a regime."
  (gethash (list indicator regime) *indicator-weights* 1.0))

(defun update-indicator-weight (indicator regime score)
  "Update weight based on child success (Bandit Reinforcement)."
  (let* ((key (list indicator regime))
         (old (gethash key *indicator-weights* 1.0))
         (alpha 0.1)) ; Learning rate
    (setf (gethash key *indicator-weights*) (+ old (* alpha (- score old))))))

(defun get-random-indicator-for-regime (regime)
  "Pick a weighted random indicator from the regime's specialized catalog."
  (let* ((catalog (get-indicators-for-regime regime))
         (weights (mapcar (lambda (i) (get-indicator-weight i regime)) catalog))
         (total (reduce #'+ weights))
         (rnd (random total))
         (running-sum 0))
    (loop for i in catalog
          for w in weights
          do (incf running-sum w)
          if (<= rnd running-sum) return i
          finally (return (car (last catalog))))))

(defun get-all-indicators ()
  "Return every possible indicator in the catalog (for general mutation)."
  (reduce #'append (mapcar #'cdr *indicator-catalog*)))
