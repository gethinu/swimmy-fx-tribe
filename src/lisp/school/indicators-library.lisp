(in-package :swimmy.school)

;;; ============================================================================
;;; INDICATORS LIBRARY (V49.5 - Advanced Strategy Diversity)
;;; ============================================================================
;;; Defines regime-specific indicator sets to prevent "Genetic Stagnation"
;;; and ensure strategies use tools appropriate for their market context.
;;; ============================================================================

(defparameter *indicator-catalog*
  '((:trend      . ((ema 20) (ema 50) (ema 200) (sma 100) (adx 14) (ichimoku 9 26 52) (psar 0.02 0.2)))
    (:reversion  . ((rsi 14) (bollinger 20 2) (stochastic 14 3 3) (mfi 14) (cci 20) (wpr 14)))
    (:breakout   . ((atr 14) (keltner 20 2) (donchian 20) (volume-osc 5 10) (standard-deviation 20)))
    (:volatility . ((atr 14) (bollinger-width 20 2) (vix-fix 22 20 8) (choppiness 14))))
  "Mapping of market regimes to optimal indicator sets.")

(defun get-indicators-for-regime (regime)
  "Return a list of indicators recommended for a given regime."
  (cdr (assoc regime *indicator-catalog*)))

(defun get-random-indicator-for-regime (regime)
  "Pick a random indicator from the regime's specialized catalog."
  (let ((catalog (get-indicators-for-regime regime)))
    (if catalog
        (nth (random (length catalog)) catalog)
        '(rsi 14)))) ; Fallback

(defun get-all-indicators ()
  "Return every possible indicator in the catalog (for general mutation)."
  (reduce #'append (mapcar #'cdr *indicator-catalog*)))
