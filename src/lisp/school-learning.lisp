;;; ============================================================================
;;; school-learning.lisp - Failure Learning System (Uncle Bob Split)

(in-package :swimmy.school)
;;; ============================================================================
;;; Extracted from school.lisp for Single Responsibility Principle
;;; Contains: Trade recording, failure;;; Extracted from school.lisp (V6.14) to reduce complexity
;;; ============================================================================

;;; ==========================================
;;; LEARNING PARAMETERS
;;; ==========================================

(defparameter *failure-log* nil           "List of failure records")
(defparameter *success-log* nil           "List of success records for comparison")
(defparameter *max-log-size* 500          "Keep more history")
(defparameter *decay-half-life* 3600      "1 hour in seconds")
(defparameter *min-samples-for-block* 5   "Need 5+ samples before blocking")

;;; ==========================================
;;; TRADE RECORD STRUCTURE
;;; ==========================================

(defstruct trade-record
  timestamp
  symbol
  direction           ; :buy or :sell
  category            ; :trend, :reversion, :breakout, :scalp
  strategy-name
  ;; Market Context (15+ variables)
  regime              ; :trending, :ranging
  volatility          ; :high, :normal, :low
  sma-position        ; :above, :below, :crossing
  rsi-value           ; actual RSI value 0-100
  rsi-zone            ; :overbought, :neutral, :oversold
  momentum            ; :accelerating, :decelerating, :flat
  spread-condition    ; :tight, :normal, :wide
  session             ; :tokyo, :london, :newyork, :overlap, :off
  day-of-week         ; 0-6
  hour-of-day         ; 0-23
  price-vs-high       ; distance from recent high (%)
  price-vs-low        ; distance from recent low (%)
  consecutive-candles ; number of same-direction candles
  volume-condition    ; :high, :normal, :low
  atr-percentile      ; current ATR vs historical
  ;; Outcome
  pnl
  hold-time           ; how long position was held (seconds)
  max-drawdown        ; worst point during trade
  hit-sl-or-tp)       ; :sl, :tp, :manual, :signal

;;; ==========================================
;;; SESSION & MOMENTUM DETECTION
;;; ==========================================

(defun get-current-session ()
  "Determine current trading session based on UTC time"
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (declare (ignore sec min))
    (cond
      ((and (>= hour 0) (< hour 7)) :tokyo)
      ((and (>= hour 7) (< hour 8)) :overlap)
      ((and (>= hour 8) (< hour 12)) :london)
      ((and (>= hour 12) (< hour 14)) :overlap)
      ((and (>= hour 14) (< hour 21)) :newyork)
      (t :off))))

(defun calculate-momentum (history n)
  "Calculate price momentum over n candles"
  (when (> (length history) (+ n 1))
    (let* ((recent (mapcar #'candle-close (subseq history 0 n)))
           (older (mapcar #'candle-close (subseq history n (* 2 n))))
           (recent-change (- (first recent) (car (last recent))))
           (older-change (- (first older) (car (last older)))))
      (cond
        ((> recent-change (* older-change 1.5)) :accelerating)
        ((< recent-change (* older-change 0.5)) :decelerating)
        (t :flat)))))

(defun count-consecutive-candles (history direction)
  "Count consecutive candles in same direction"
  (let ((count 0))
    (dolist (c history)
      (let ((bullish (> (candle-close c) (candle-open c))))
        (if (eq (if bullish :up :down) direction)
            (incf count)
            (return count))))
    count))

;;; ==========================================
;;; MARKET CONTEXT CAPTURE
;;; ==========================================

(defun get-rich-market-context (symbol)
  "Capture comprehensive market context (15+ variables)"
  (let* ((history (or (gethash symbol *candle-histories*) *candle-history*))
         (close (and history (candle-close (first history))))
         (open (and history (candle-open (first history))))
         (sma20 (and history (> (length history) 20)
                     (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 20))) 20)))
         (rsi (and history (> (length history) 14) 
                   (when (fboundp 'ind-rsi) (ind-rsi 14 history))))
         (atr (and history (> (length history) 14) 
                   (when (fboundp 'ind-atr) (ind-atr 14 history))))
         (recent-high (and history (> (length history) 20)
                           (reduce #'max (mapcar #'candle-high (subseq history 0 20)))))
         (recent-low (and history (> (length history) 20)
                          (reduce #'min (mapcar #'candle-low (subseq history 0 20)))))
         (now (multiple-value-list (get-decoded-time))))
    (list
     :regime (if (boundp '*current-regime*) *current-regime* :unknown)
     :volatility (if (boundp '*volatility-regime*) *volatility-regime* :normal)
     :sma-position (cond
                     ((null sma20) :unknown)
                     ((and close (> (abs (- close sma20)) (* close 0.0001)))
                      (if (> close sma20) :above :below))
                     (t :crossing))
     :rsi-value (or rsi 50)
     :rsi-zone (cond ((null rsi) :neutral)
                     ((> rsi 70) :overbought)
                     ((< rsi 30) :oversold)
                     (t :neutral))
     :momentum (calculate-momentum history 5)
     :spread-condition :normal
     :session (get-current-session)
     :day-of-week (nth 6 now)
     :hour-of-day (nth 2 now)
     :price-vs-high (if (and close recent-high (> recent-high 0))
                        (* 100 (/ (- recent-high close) recent-high)) 0)
     :price-vs-low (if (and close recent-low (> recent-low 0))
                       (* 100 (/ (- close recent-low) recent-low)) 0)
     :consecutive-candles (if (and history close open)
                              (count-consecutive-candles history (if (> close open) :up :down))
                              0)
     :atr-percentile (if atr (min 100 (* atr 1000)) 50))))

;;; ==========================================
;;; PATTERN SIMILARITY & DECAY
;;; ==========================================

(defun time-decay-weight (timestamp)
  "Calculate exponential decay weight based on age"
  (let* ((age (- (get-universal-time) timestamp))
         (half-lives (/ age *decay-half-life*)))
    (expt 0.5 half-lives)))

(defun pattern-similarity (ctx1 ctx2)
  "Calculate similarity between two contexts (0.0 to 1.0)"
  (let ((score 0) (total 0))
    ;; Exact matches for categorical variables (weight: 2)
    (dolist (key '(:regime :volatility :sma-position :rsi-zone :momentum :session))
      (incf total 2)
      (when (eq (getf ctx1 key) (getf ctx2 key))
        (incf score 2)))
    ;; Fuzzy matches for numerical variables (weight: 1)
    (let ((rsi1 (getf ctx1 :rsi-value))
          (rsi2 (getf ctx2 :rsi-value)))
      (incf total 1)
      (when (and rsi1 rsi2 (< (abs (- rsi1 rsi2)) 15))
        (incf score 1)))
    (let ((hour1 (getf ctx1 :hour-of-day))
          (hour2 (getf ctx2 :hour-of-day)))
      (incf total 1)
      (when (and hour1 hour2 (< (abs (- hour1 hour2)) 3))
        (incf score 1)))
    ;; Day of week match
    (incf total 1)
    (when (eql (getf ctx1 :day-of-week) (getf ctx2 :day-of-week))
      (incf score 1))
    (/ score (max 1 total))))

;;; ==========================================
;;; FAILURE CONFIDENCE CALCULATION
;;; ==========================================

(defun calculate-failure-confidence (symbol direction category)
  "Calculate confidence that this trade will fail (0.0 to 1.0)"
  (let* ((current-ctx (get-rich-market-context symbol))
         (weighted-failures 0.0)
         (weighted-successes 0.0)
         (similarity-sum 0.0))
    ;; Analyze failure log with time decay and similarity
    (dolist (record *failure-log*)
      (when (and (eq (trade-record-direction record) direction)
                 (eq (trade-record-category record) category))
        (let* ((record-ctx (list :regime (trade-record-regime record)
                                 :volatility (trade-record-volatility record)
                                 :sma-position (trade-record-sma-position record)
                                 :rsi-zone (trade-record-rsi-zone record)
                                 :rsi-value (trade-record-rsi-value record)
                                 :momentum (trade-record-momentum record)
                                 :session (trade-record-session record)
                                 :day-of-week (trade-record-day-of-week record)
                                 :hour-of-day (trade-record-hour-of-day record)))
               (similarity (pattern-similarity current-ctx record-ctx))
               (decay (time-decay-weight (trade-record-timestamp record)))
               (weight (* similarity decay)))
          (when (> similarity 0.5)
            (incf weighted-failures weight)
            (incf similarity-sum weight)))))
    ;; Analyze success log similarly
    (dolist (record *success-log*)
      (when (and (eq (trade-record-direction record) direction)
                 (eq (trade-record-category record) category))
        (let* ((record-ctx (list :regime (trade-record-regime record)
                                 :volatility (trade-record-volatility record)
                                 :sma-position (trade-record-sma-position record)
                                 :rsi-zone (trade-record-rsi-zone record)
                                 :rsi-value (trade-record-rsi-value record)
                                 :momentum (trade-record-momentum record)
                                 :session (trade-record-session record)
                                 :day-of-week (trade-record-day-of-week record)
                                 :hour-of-day (trade-record-hour-of-day record)))
               (similarity (pattern-similarity current-ctx record-ctx))
               (decay (time-decay-weight (trade-record-timestamp record)))
               (weight (* similarity decay)))
          (when (> similarity 0.5)
            (incf weighted-successes weight)
            (incf similarity-sum weight)))))
    ;; Calculate failure probability
    (if (< similarity-sum 0.5)
        0.0  ; Not enough similar data
        (/ weighted-failures (+ weighted-failures weighted-successes 0.001)))))

(defun get-failure-penalty (symbol direction category)
  "Get lot size multiplier based on failure probability (0.3 to 1.0)"
  (let ((failure-prob (calculate-failure-confidence symbol direction category)))
    (cond
      ((> failure-prob 0.8) 
       (format t "[L] 🛑 HIGH FAILURE RISK (~,1f%) - blocking~%" (* failure-prob 100))
       0.0)
      ((> failure-prob 0.6)
       (format t "[L] ⚠️ ELEVATED RISK (~,1f%) - reducing to 50%~%" (* failure-prob 100))
       0.5)
      ((> failure-prob 0.4)
       (format t "[L] 📊 MODERATE RISK (~,1f%) - reducing to 70%~%" (* failure-prob 100))
       0.7)
      (t 1.0))))

(defun should-block-trade-p (symbol direction category)
  "Check if this trade should be blocked based on failure analysis"
  (let ((penalty (get-failure-penalty symbol direction category)))
    (zerop penalty)))

;;; ==========================================
;;; TRADE OUTCOME RECORDING
;;; ==========================================

(defun record-trade-outcome (symbol direction category strategy-name pnl &key (hit :unknown) (hold-time 0))
  "Record trade outcome for learning (both wins and losses)"
  (let* ((ctx (get-rich-market-context symbol))
         (record (make-trade-record
                  :timestamp (get-universal-time)
                  :symbol symbol
                  :direction direction
                  :category category
                  :strategy-name strategy-name
                  :regime (getf ctx :regime)
                  :volatility (getf ctx :volatility)
                  :sma-position (getf ctx :sma-position)
                  :rsi-value (getf ctx :rsi-value)
                  :rsi-zone (getf ctx :rsi-zone)
                  :momentum (getf ctx :momentum)
                  :spread-condition (getf ctx :spread-condition)
                  :session (getf ctx :session)
                  :day-of-week (getf ctx :day-of-week)
                  :hour-of-day (getf ctx :hour-of-day)
                  :price-vs-high (getf ctx :price-vs-high)
                  :price-vs-low (getf ctx :price-vs-low)
                  :consecutive-candles (getf ctx :consecutive-candles)
                  :atr-percentile (getf ctx :atr-percentile)
                  :pnl pnl
                  :hold-time hold-time
                  :hit-sl-or-tp hit))
         (regime (getf ctx :regime))
         (won-p (> pnl 0)))
    (if (< pnl 0)
        (progn
          (push record *failure-log*)
          (when (> (length *failure-log*) *max-log-size*)
            (setf *failure-log* (subseq *failure-log* 0 *max-log-size*)))
          (format t "[L] 📉 FAILURE: ~a | ~a ~a | RSI:~,0f | Session:~a~%"
                  strategy-name direction (getf ctx :regime) 
                  (getf ctx :rsi-value) (getf ctx :session)))
        (progn
          (push record *success-log*)
          (when (> (length *success-log*) *max-log-size*)
            (setf *success-log* (subseq *success-log* 0 *max-log-size*)))
          (format t "[L] ✅ SUCCESS: ~a | ~a ~a | RSI:~,0f | Session:~a~%"
                  strategy-name direction (getf ctx :regime)
                  (getf ctx :rsi-value) (getf ctx :session))))
    
    ;; Homework integration hooks
    (handler-case
        (when (fboundp 'on-trade-close-meta)
          (on-trade-close-meta regime strategy-name won-p pnl))
      (error () nil))
    (handler-case
        (when (fboundp 'record-proof-trade)
          (record-proof-trade pnl))
      (error () nil))
    (handler-case
        (when (and (fboundp 'record-template-result) category)
          (record-template-result category won-p 0.0))
      (error () nil))))

;; Legacy compatibility
(defun record-failure (symbol direction category strategy-name pnl)
  (record-trade-outcome symbol direction category strategy-name pnl))

;;; ==========================================
;;; FAILURE ANALYSIS
;;; ==========================================

(defun analyze-failure-patterns ()
  "Statistical analysis of failure patterns"
  (let ((pattern-stats (make-hash-table :test 'equal)))
    (dolist (record *failure-log*)
      (let ((key (format nil "~a|~a|~a" 
                         (trade-record-direction record)
                         (trade-record-regime record)
                         (trade-record-session record))))
        (incf (gethash key pattern-stats 0))))
    (let ((sorted nil))
      (maphash (lambda (k v) (push (cons k v) sorted)) pattern-stats)
      (sort sorted #'> :key #'cdr))))

(defun get-failure-summary ()
  "Get detailed failure summary"
  (let* ((patterns (analyze-failure-patterns))
         (total-failures (length *failure-log*))
         (total-successes (length *success-log*))
         (overall-rate (if (> (+ total-failures total-successes) 0)
                           (* 100 (/ total-successes (+ total-failures total-successes)))
                           50)))
    (format nil "Overall win rate: ~,1f% (~d wins, ~d losses)"
            overall-rate total-successes total-failures)))

(format t "[SCHOOL] school-learning.lisp loaded - Failure Learning System~%")
