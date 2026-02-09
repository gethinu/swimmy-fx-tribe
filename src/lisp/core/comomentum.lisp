;;; comomentum.lisp - Market Crowding Indicator
;;; Extracted from mixseek.lisp (V48.0: Category Unification)
;;; 
;;; Measures how "crowded" a trade is by detecting correlation clustering
;;; High Comomentum = Many traders doing the same thing = Danger

(in-package :swimmy.school)

;;; ══════════════════════════════════════════════════════════════════
;;;  COMOMENTUM INDICATOR (混雑度)
;;; ══════════════════════════════════════════════════════════════════

(defparameter *comomentum-threshold-warning* 0.15
  "Comomentum level that triggers caution")

(defparameter *comomentum-threshold-danger* 0.20
  "Comomentum level that triggers exposure reduction")

(defun calculate-comomentum (history &key (lookback 20))
  "Calculate Comomentum (co-movement momentum) as proxy for market crowding.
   Returns value between 0 (uncrowded) and 1 (extremely crowded).
   Based on correlation of price changes across time windows."
  (when (>= (length history) (* lookback 2))
    (let* ((closes (mapcar #'candle-close (subseq history 0 (* lookback 2))))
           (returns (loop for i from 0 below (1- (length closes))
                          collect (/ (- (nth i closes) (nth (1+ i) closes))
                                    (max 0.0001 (nth (1+ i) closes)))))
           ;; Split into two windows
           (recent (subseq returns 0 lookback))
           (prior (subseq returns lookback))
           ;; Calculate correlation
           (mean-r (/ (reduce #'+ recent) lookback))
           (mean-p (/ (reduce #'+ prior) lookback))
           (cov 0.0) (var-r 0.0) (var-p 0.0))
      (loop for r in recent
            for p in prior
            do (incf cov (* (- r mean-r) (- p mean-p)))
               (incf var-r (expt (- r mean-r) 2))
               (incf var-p (expt (- p mean-p) 2)))
      ;; Normalize correlation to [0, 1]
      (let ((denom (sqrt (* var-r var-p))))
        (if (< denom 0.0001)
            0.0
            (abs (/ cov denom)))))))

(defun comomentum-risk-level (comomentum)
  "Categorize Comomentum into risk levels"
  (cond
    ((null comomentum) :unknown)
    ((< comomentum *comomentum-threshold-warning*) :safe)
    ((< comomentum *comomentum-threshold-danger*) :caution)
    (t :danger)))

(defun comomentum-exposure-multiplier (comomentum)
  "Return position size multiplier based on crowding.
   Reduces exposure when market is crowded."
  (cond
    ((null comomentum) 1.0)
    ((< comomentum *comomentum-threshold-warning*) 1.0)  ; Full size
    ((< comomentum *comomentum-threshold-danger*) 0.7)   ; 30% reduction
    (t 0.5)))  ; 50% reduction in danger zone

(format t "[COMOMENTUM] Market crowding indicator loaded~%")
