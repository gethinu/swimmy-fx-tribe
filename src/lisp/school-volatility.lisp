;;; ============================================================================
;;; school-volatility.lisp - Volatility Detection System (Uncle Bob Split)

(in-package :swimmy.school)
;;; ============================================================================
;;; Extracted from school.lisp for Single Responsibility Principle
;;; Contains: ATR calculation, volatility shift detection, HDRL risk classification
;;; ============================================================================

;;; ==========================================
;;; VOLATILITY PARAMETERS
;;; ==========================================

(defparameter *volatility-history* nil      "Recent volatility readings")
(defparameter *volatility-history-size* 20  "Keep last 20 readings")
(defparameter *volatility-shift-threshold* 2.0 "2x change = shift")
(defparameter *current-volatility-state* :normal "Current state: :normal, :elevated, :extreme")
(defparameter *last-shift-time* 0)

(defstruct volatility-reading
  timestamp
  atr           ; Average True Range
  range-pct     ; High-Low as % of price
  direction-change-count ; How many direction changes in last N candles
  state)        ; :normal, :elevated, :extreme

;;; ==========================================
;;; ATR CALCULATION
;;; ==========================================

(defun calculate-atr (history &optional (n 14))
  "Calculate Average True Range over n periods"
  (when (> (length history) n)
    (let ((tr-sum 0.0))
      (dotimes (i n)
        (let* ((c (nth i history))
               (prev (nth (1+ i) history))
               (high (candle-high c))
               (low (candle-low c))
               (prev-close (and prev (candle-close prev)))
               (tr (if prev-close
                       (max (- high low)
                            (abs (- high prev-close))
                            (abs (- low prev-close)))
                       (- high low))))
          (incf tr-sum tr)))
      (/ tr-sum n))))

(defun count-direction-changes (history n)
  "Count how many times price direction changed in last n candles"
  (when (> (length history) n)
    (let ((changes 0)
          (prev-dir nil))
      (dotimes (i n)
        (let* ((c (nth i history))
               (dir (if (> (candle-close c) (candle-open c)) :up :down)))
          (when (and prev-dir (not (eq dir prev-dir)))
            (incf changes))
          (setf prev-dir dir)))
      changes)))

;;; ==========================================
;;; VOLATILITY SHIFT DETECTION
;;; ==========================================

(defun detect-volatility-shift ()
  "Check for significant volatility changes"
  (when (and *candle-history* (> (length *candle-history*) 25))
    (let* ((current-atr (calculate-atr *candle-history* 5))
           (baseline-atr (calculate-atr *candle-history* 20))
           (direction-changes (count-direction-changes *candle-history* 10))
           (close (candle-close (first *candle-history*)))
           (range-pct (when (and current-atr close (> close 0))
                        (* 100 (/ current-atr close))))
           (volatility-ratio (if (and current-atr baseline-atr (> baseline-atr 0))
                                 (/ current-atr baseline-atr)
                                 1.0))
           (new-state (cond
                        ((> volatility-ratio 3.0) :extreme)
                        ((> volatility-ratio *volatility-shift-threshold*) :elevated)
                        (t :normal)))
           (is-shift (not (eq new-state *current-volatility-state*))))
      
      ;; Record reading
      (push (make-volatility-reading
             :timestamp (get-universal-time)
             :atr current-atr
             :range-pct range-pct
             :direction-change-count direction-changes
             :state new-state)
            *volatility-history*)
      (when (> (length *volatility-history*) *volatility-history-size*)
        (setf *volatility-history* (subseq *volatility-history* 0 *volatility-history-size*)))
      
      ;; Alert on state change
      (when is-shift
        (setf *last-shift-time* (get-universal-time))
        (format t "~%[L] ⚡ VOLATILITY SHIFT DETECTED!~%")
        (format t "[L] ⚡ State: ~a → ~a (ratio: ~,1fx)~%" 
                *current-volatility-state* new-state volatility-ratio)
        (format t "[L] ⚡ Direction changes: ~d in last 10 candles~%~%" direction-changes)
        (setf *current-volatility-state* new-state))
      
      ;; Return current state
      (list :state new-state
            :ratio volatility-ratio
            :direction-changes direction-changes
            :is-shift is-shift))))

;;; ==========================================
;;; VOLATILITY TRADING RULES
;;; ==========================================

(defun volatility-allows-trading-p ()
  "Check if current volatility state allows normal trading"
  (case *current-volatility-state*
    (:normal t)
    (:elevated 
     (format t "[L] ⚠️ ELEVATED VOLATILITY: Reducing position size~%")
     t)
    (:extreme
     (format t "[L] 🛑 EXTREME VOLATILITY: Trading with minimal size~%")
     t)))

(defun get-volatility-lot-multiplier ()
  "Get lot size multiplier based on volatility"
  (case *current-volatility-state*
    (:normal 1.0)
    (:elevated 0.5)
    (:extreme 0.0)))

;;; ==========================================
;;; HDRL CURRENCY RISK CLASSIFICATION
;;; ==========================================
;;; Research Paper #34: SAMP-HDRL - Hierarchical Portfolio Management

(defparameter *currency-risk-cache* (make-hash-table :test 'equal))
(defparameter *currency-risk-cache-time* (make-hash-table :test 'equal))

(defun classify-currency-risk (symbol)
  "SAMP-HDRL: Classify currency pair by risk level based on volatility"
  (let* ((history (gethash symbol *candle-histories*))
         (cache-time (gethash symbol *currency-risk-cache-time*))
         (current-time (get-universal-time)))
    (if (and cache-time (< (- current-time cache-time) 300))
        (gethash symbol *currency-risk-cache*)
        (when (and history (> (length history) 20))
          (let* ((atr (calculate-atr history 14))
                 (price (candle-close (first history)))
                 (atr-percent (if (and atr price (> price 0)) 
                                  (* 100 (/ atr price)) 
                                  0.0))
                 (risk-class (cond
                               ((> atr-percent 0.15) :high-risk)
                               ((< atr-percent 0.08) :low-risk)
                               (t :medium-risk))))
            (setf (gethash symbol *currency-risk-cache*) risk-class)
            (setf (gethash symbol *currency-risk-cache-time*) current-time)
            risk-class)))))

(defun hdrl-adjusted-lot (symbol base-lot)
  "SAMP-HDRL: Adjust lot size based on currency risk classification"
  (let ((risk-class (classify-currency-risk symbol)))
    (case risk-class
      (:high-risk (* base-lot 0.5))
      (:low-risk (* base-lot 1.5))
      (t base-lot))))

(format t "[SCHOOL] school-volatility.lisp loaded - Volatility Detection System~%")
