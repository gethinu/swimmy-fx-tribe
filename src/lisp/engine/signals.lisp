;;; ============================================================================
;;; engine/signals.lisp - Market Analysis & Signal Generation
;;; ============================================================================
;;; Pure technical analysis logic. 
;;; Input: Market Data (Candles) + Strategy Params
;;; Output: Trade Signals (:BUY, :SELL, :HOLD)
;;; Part of "The Efficient Gardener" refactoring (Phase 6)
;;; ============================================================================

(in-package :swimmy.engine)

;;; ==========================================
;;; TECHNICAL INDICATORS
;;; ==========================================

(defun calculate-sma (n history)
  "Calculate Simple Moving Average over n periods."
  (when (>= (length history) n)
    (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 n))) n)))

(defun calculate-adx (history &optional (n 14))
  "Calculate Average Directional Index."
  (when (>= (length history) (+ n 2))
    (let ((plus-dm 0) (minus-dm 0) (tr-sum 0))
      (dotimes (i n)
        (let* ((c (nth i history)) (p (nth (1+ i) history)))
          (incf plus-dm (max 0 (- (candle-high c) (candle-high p))))
          (incf minus-dm (max 0 (- (candle-low p) (candle-low c))))
          (incf tr-sum (- (candle-high c) (candle-low c)))))
      (/ (abs (- plus-dm minus-dm)) (max (+ plus-dm minus-dm) 0.001) 0.01))))

(defun calculate-volatility (history)
  "Calculate volatility from price history."
  (when (and history (> (length history) 1))
    (let* ((closes (mapcar #'candle-close history))
           (returns (loop for i from 1 below (length closes)
                          collect (/ (- (nth i closes) (nth (1- i) closes))
                                     (max 0.0001 (nth (1- i) closes)))))
           (mean (/ (reduce #'+ returns) (max 1 (length returns))))
           (sq-diffs (mapcar (lambda (r) (expt (- r mean) 2)) returns)))
      (sqrt (/ (reduce #'+ sq-diffs) (max 1 (length sq-diffs)))))))

(defun detect-regime ()
  "Detect current market regime based on ADX."
  (let ((adx (calculate-adx *candle-history*)))
    (if (and adx (> adx 25)) "TREND" "RANGE")))

;;; ==========================================
;;; SIGNAL GENERATION
;;; ==========================================

(defun check-neural-confirmation (signal-type)
  "Check if Neural Network confirms the signal."
  (let* ((warmup-p (< *total-trades* 50))
         (conf (if (and (boundp '*last-confidence*) (numberp *last-confidence*))
                   *last-confidence* 0.0))
         (pred (if (and (boundp '*last-prediction*) *last-prediction*)
                   *last-prediction* "HOLD")))
    (if warmup-p
        (values t 0.0) ; Always confirm during warmup
        (values (and (string= pred (symbol-name signal-type)) 
                     (> conf *nn-threshold*))
                conf))))

(defun generate-signal (params history)
  "Generate signal for a specific strategy arm.
   Params: (sma-s sma-l sl tp vol)
   Returns: (signal confidence sl tp volume)"
  (let* ((sma-s (nth 0 params))
         (sma-l (nth 1 params))
         (sl-p (nth 2 params))
         (tp-p (nth 3 params))
         (vol (nth 4 params))
         ;; Current and previous values
         (s-now (calculate-sma sma-s history))
         (l-now (calculate-sma sma-l history))
         (s-prev (calculate-sma sma-s (cdr history)))
         (l-prev (calculate-sma sma-l (cdr history))))
    
    (when (and s-now l-now s-prev l-prev)
      (cond
        ;; GOLDEN CROSS (Buy)
        ((and (< s-prev l-prev) (> s-now l-now))
         (multiple-value-bind (nn-ok conf) (check-neural-confirmation :BUY)
           (if nn-ok
               (list :BUY conf sl-p tp-p vol)
               (progn
                 (format t "[SIGNAL] Filtered BUY (NN: ~,0f%)~%" (* conf 100))
                 nil))))
        
        ;; DEAD CROSS (Sell)
        ((and (> s-prev l-prev) (< s-now l-now))
         (multiple-value-bind (nn-ok conf) (check-neural-confirmation :SELL)
           (if nn-ok
               (list :SELL conf sl-p tp-p vol)
               (progn
                 (format t "[SIGNAL] Filtered SELL (NN: ~,0f%)~%" (* conf 100))
                 nil))))
        
        (t nil)))))
        
(format t "[ENGINE] signals.lisp loaded~%")
