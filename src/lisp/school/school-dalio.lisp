;;; school-dalio.lisp - Uncorrelated Return Streams & Risk Parity
;;; Phase 27: The Holy Grail (Ray Dalio)
;;;
;;; "The Holy Grail of investing is having 15 or more good, uncorrelated return streams."
;;; - Ray Dalio
;;;
;;; This module complements `school-macro.lisp` and `school-kb.lisp`.
;;; While school-kb checks LOGIC similarity (Simons), this module checks OUTCOME correlation.
;;; It ensures that the portfolio is not exposed to a single risk factor.

(defpackage :swimmy.school.dalio
  (:use :cl :swimmy.school :swimmy.globals)
  (:export #:calculate-holy-grail-score
           #:is-uncorrelated-p
           #:get-correlation-matrix
           #:optimize-risk-parity))

(in-package :swimmy.school.dalio)

;;; ============================================================================
;;; STATE
;;; ============================================================================

(defparameter *correlation-threshold* 0.2 "Dalio's threshold for 'Uncorrelated'.")
(defparameter *correlation-cache* (make-hash-table :test 'equal))

;;; ============================================================================
;;; OUTCOME CORRELATION (PnL)
;;; ============================================================================

(defun get-returns-vector (strategy &optional (period 30))
  "Extract last N daily PnL values for the strategy.
   Returns a vector of floats."
  (declare (ignore strategy))
  ;; Placeholder: Requires trade history access from `school-analytics`
  ;; Aggregate trades by day... 
  ;; For now, returning a mock random vector for architectural verification
  (make-array period :initial-element (random 1.0)))

(defun pearson-correlation (v1 v2)
  "Calculate Pearson correlation coefficient between two vectors."
  (let* ((n (length v1))
         (mean1 (/ (reduce #'+ v1) n))
         (mean2 (/ (reduce #'+ v2) n))
         (num 0.0)
         (den1 0.0)
         (den2 0.0))
    (dotimes (i n)
      (let ((d1 (- (aref v1 i) mean1))
            (d2 (- (aref v2 i) mean2)))
        (incf num (* d1 d2))
        (incf den1 (* d1 d1))
        (incf den2 (* d2 d2))))
    (if (or (zerop den1) (zerop den2))
        0.0
        (/ num (sqrt (* den1 den2))))))

(defun calculate-holy-grail-score (portfolio)
  "Calculate the 'Holy Grail Score' of a portfolio.
   Score = Ratio of uncorrelated pairs (< 0.2) to total pairs.
   Target: > 0.8"
  (let ((strategies (if (listp portfolio) portfolio (list portfolio)))
        (count 0)
        (uncorrelated 0))
    (let ((n (length strategies)))
      (when (< n 2) (return-from calculate-holy-grail-score 1.0))
      
      (dotimes (i n)
        (dotimes (j i)
          (let* ((s1 (nth i strategies))
                 (s2 (nth j strategies))
                 (corr (calculate-strategy-correlation s1 s2)))
            (incf count)
            (when (< (abs corr) *correlation-threshold*)
              (incf uncorrelated))))))
    
    (if (zerop count) 1.0
        (/ (float uncorrelated) count))))

(defun calculate-strategy-correlation (s1 s2)
  "Calculate PnL correlation between two strategies."
  (let ((v1 (get-returns-vector s1))
        (v2 (get-returns-vector s2)))
    (pearson-correlation v1 v2)))

(defun is-uncorrelated-p (strategy portfolio &optional (threshold 0.2))
  "Check if a new strategy is uncorrelated with the existing portfolio."
  (dolist (s portfolio)
    (let ((corr (calculate-strategy-correlation strategy s)))
      (when (> (abs corr) threshold)
        ;; If highly correlated, check if it improves Sharpe (Highlander Rule applied to Risk)
        (return-from is-uncorrelated-p (values nil s corr)))))
  (values t nil 0.0))

(defun optimize-risk-parity (portfolio)
  "Adjust weights to equalize risk contribution (Dalio All Weather).
   Inverse volatility weighting."
  ;; To be implemented
  portfolio)

(format t "[DALIO] 🌐 school-dalio.lisp loaded - Seeking the Holy Grail (Uncorrelated Alpha)~%")
