;;; school-alchemy.lisp - The Alchemy of FX Multibaggers (Phase 28)
;;;
;;; "The Philosophers' Stone for FX is Carry + Volatility."
;;; Based on Birmingham City University Paper (2025) & Expert Panel V50.7
;;;
;;; [Concept Translation]
;;; 1. Value (Earnings Yield) -> Carry Yield (Swap Points)
;;; 2. Growth (Asset Growth)  -> Volatility Expansion (Liquidity)
;;; 3. Timing (Momentum Rev)  -> Mean Reversion (Low Price Range)

(defpackage :swimmy.school.alchemy
  (:use :cl :swimmy.school)
  (:export #:calculate-carry-yield
           #:calculate-volatility-growth
           #:calculate-price-position-score
           #:is-alchemy-buy-signal))

(in-package :swimmy.school.alchemy)

;;; ============================================================================
;;; 1. VALUE FACTOR (Carry Yield)
;;; ============================================================================

(defun calculate-carry-yield (price swap-long)
  "Calculate annualized Carry Yield.
   Yield = (SwapLong * 365) / Price
   (Note: Swap is usually in points, so adjustment might be needed depending on point value.
    Here we assume standard MT5 point value relative to price).
   Target: > 0 (Positive Carry is non-negotiable for Value)"
  (if (or (zerop price) (null swap-long))
      0.0
      (let ((annual-swap (* swap-long 365.0)))
        ;; Normalize: If price is 150.00 (JPY), 1 point is 0.001.
        ;; This normalization depends on the symbol digits.
        ;; For now, we assume swap-long is already adjusted or we look at raw ratio.
        ;; Simple raw ratio for relative scoring.
        (/ annual-swap price))))

;;; ============================================================================
;;; 2. GROWTH FACTOR (Volatility Expansion)
;;; ============================================================================

(defun calculate-volatility-growth (short-vol long-vol)
  "Calculate 'Asset Growth' equivalent: Volatility Expansion.
   We want expanding volatility (Liquidity injection).
   Ratio > 1.0 means Volatility is growing."
  (if (or (zerop long-vol) (null short-vol))
      1.0
      (/ short-vol long-vol)))

;;; ============================================================================
;;; 3. TIMING FACTOR (Mean Reversion / Price Position)
;;; ============================================================================

(defun calculate-price-position-score (current-price high-1y low-1y)
  "Where is the price relative to 52-week High/Low?
   0.0 = At Low (Best for Reversion/Value)
   1.0 = At High (Worst for Reversion, Good for Momentum)
   
   Birmingham Paper Finding: '12-month High' stocks underperform (Reversion).
   Target: < 0.2 (Buy near lows)"
  (if (= high-1y low-1y)
      0.5
      (/ (- current-price low-1y) 
         (- high-1y low-1y))))

;;; ============================================================================
;;; ALCHEMY SIGNAL
;;; ============================================================================

(defun is-alchemy-buy-signal (price swap-long short-vol long-vol high-1y low-1y)
  "The Alchemist's Buy Signal.
   1. Positive Carry (Value)
   2. Expanding Volatility (Growth)
   3. Low Price Position (Reversion/Safety)"
  (let ((yield (calculate-carry-yield price swap-long))
        (vol-growth (calculate-volatility-growth short-vol long-vol))
        (position (calculate-price-position-score price high-1y low-1y)))
    
    (cond
      ((<= yield 0) nil)                 ; No Carry, No Deal.
      ((> position 0.3) nil)             ; Too expensive (Mean Reversion risk).
      ((< vol-growth 1.0) nil)           ; Volatility contracting (Dead market).
      (t t))))                           ; All Green.
