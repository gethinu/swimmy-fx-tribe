;;; school-founders-breakout.lisp - The Tactician (Momentum Breakout)
;;; Phase 30: Based on Koroush AK's "Momentum Breakout Strategy" (Staircase)

(in-package :swimmy.school)

;;; ============================================================
;;; HELPER FUNCTIONS (Pattern Recognition)
;;; ============================================================

(defun get-resistance-level (history period)
  "Get the highest high of the previous [period] candles (excluding current/recent breakout)."
  ;; We need a stable resistance level.
  ;; Strategy: Max High of candles [3 to period+3] (lagged to avoid self-reference during breakout)
  (if (> (length history) (+ period 3))
      (loop for i from 3 to (+ period 3)
            maximize (candle-high (nth i history)))
      999999.0)) ; Fallback

(defun is-staircase-pattern (history period)
  "Check for 'Staircase' pattern: Higher Lows and Low Volatility (Grinding).
   Condition 1: Linear Regression Slope of Lows is Positive.
   Condition 2: No single candle range > 3 * ATR (Avoid exhaustion spikes)."
  (if (> (length history) period)
      (let* ((lows (loop for i from 0 to (1- period) collect (candle-low (nth i history))))
             (ranges (loop for i from 0 to (1- period) 
                           collect (- (candle-high (nth i history)) (candle-low (nth i history)))))
             (avg-range (/ (reduce #'+ ranges) period))
             (spike-found (some (lambda (r) (> r (* 3.0 avg-range))) ranges))
             (slope (calculate-slope (reverse lows)))) ; Reverse because history is usually new->old
        
        (and (> slope 0)
             (not spike-found)))
      nil))

(defun calculate-slope (values)
  "Simple Linear Regression Slope."
  (let* ((n (length values))
         (x-mean (/ (1- n) 2.0))
         (y-mean (/ (reduce #'+ values) n))
         (numerator 0.0)
         (denominator 0.0))
    (dotimes (i n)
      (let ((x (float i))
            (y (elt values i)))
        (incf numerator (* (- x x-mean) (- y y-mean)))
        (incf denominator (expt (- x x-mean) 2))))
    (if (zerop denominator) 0.0 (/ numerator denominator))))

(defun is-volume-increasing (history period)
  "Check if volume is trending up."
  (if (> (length history) period)
      (let ((vols (loop for i from 0 to (1- period) collect (candle-volume (nth i history)))))
        (> (calculate-slope (reverse vols)) 0))
      nil))

(defun confirm-breakout (history period)
  "Check if last 2 candles closed above Resistance."
  ;; Resistance is calculated from [3..period+3]
  (let ((res (get-resistance-level history period))
        (c0 (candle-close (nth 0 history)))
        (c1 (candle-close (nth 1 history))))
    (and (> c1 res)
         (> c0 res))))

;;; ============================================================
;;; STRATEGY FACTORY
;;; ============================================================

(defun deploy-breakout-strategy (symbol)
  "Deploy a Breakout-Staircase strategy for the given symbol."
  (format t "[TACTICIAN] 🛡️ Deploying Breakout-Staircase-M1 for ~a~%" symbol)
  
  (let ((strat (make-strategy
                :name (format nil "Breakout-Staircase-~a" symbol)
                :symbol symbol
                :timeframe 1 ; M1 Scalping
                :category :breakout
                :rank :A ; Starts at A-Rank (Prototype)
                :direction :BOTH ; Can flip logic for Short
                :entry `((and 
                          ;; 1. Staircase Pattern (Grinding Up)
                          (is-staircase-pattern 20)
                          ;; 2. Volume Support
                          (is-volume-increasing 10)
                          ;; 3. SMMA Filter (Trend Check - Close above 30 SMMA)
                          (> close (ind-smma 30))
                          ;; 4. Breakout Confirmation (Trigger)
                          (confirm-breakout 50)))
                :exit `((or 
                         ;; Stop Loss: Close below 30 SMMA (Trend Broken)
                         (< close (ind-smma 30))
                         ;; Take Profit: Fixed 20 pips or dynamic? 
                         ;; Using standard TP for now, managed by Executor.
                         ))
                :sl 15.0 ; Pips (Wide enough for M1 variance)
                :tp 20.0 ; Pips (1:1.3 Risk/Reward)
                :indicators '((ind-smma 30)))))
    
    (add-to-kb strat "The Tactician")
    strat))

(format t "[SCHOOL] 🛡️ school-founders-breakout.lisp loaded - The Tactician Active~%")
