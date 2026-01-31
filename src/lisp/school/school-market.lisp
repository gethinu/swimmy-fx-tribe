;;; school-market.lisp - Market Regime & Volatility Analysis
;;; Part of the Swimmy School System
;;; Extracted from school.lisp to comply with SRP (Expert Panel 2026-01-13)

(in-package :swimmy.school)

;;; ==========================================
;;; MARKET STATE DEFINITIONS (V5.0 Granular)
;;; ==========================================
;;; ==========================================
;;; MARKET STATE DEFINITIONS (V5.0 Granular)
;;; ==========================================
(defparameter *market-regimes* 
  '(:trend-early :trend-mature :trend-exhausted 
    :range-expansion :range-compression 
    :volatile-spike :illiquid :unknown))

(defparameter *current-regime* :unknown)
(defparameter *volatility-regime* :normal)  ; :low, :normal, :high

;; V50.2 Global Macro Matrix definitions moved to school-state.lisp (Phase 23)

(defparameter *regime-history* nil)          ; Rich history with features
(defparameter *max-regime-history* 120)      ; Keep 2 hours of data
(defparameter *predicted-regime* nil)
(defparameter *predicted-volatility* nil)
(defparameter *regime-confidence* 0.5)       ; Confidence in prediction

(defstruct regime-snapshot
  timestamp
  regime                ; Granular regime
  volatility            ; :high, :normal, :low
  trend-strength        ; 0.0 to 1.0 (ADX-like)
  volatility-value      ; actual number
  sma-spread            ; SMA20 - SMA50 as % of price
  momentum              ; price change momentum
  hour-of-day)

;;; ==========================================
;;; VOLATILITY & REGIME DETECTION
;;; ==========================================

(defun calculate-price-volatility (closes n)
  "Calculate simple volatility (standard deviation of returns)"
  (when (> (length closes) n)
    (let* ((returns (loop for i from 0 below (1- n)
                          collect (/ (- (nth i closes) (nth (1+ i) closes)) 
                                    (nth (1+ i) closes))))
           (mean (/ (reduce #'+ returns) (length returns)))
           (sq-diffs (mapcar (lambda (r) (expt (- r mean) 2)) returns)))
      (sqrt (/ (reduce #'+ sq-diffs) (length sq-diffs))))))

(defun calculate-slope (data n)
  "Calculate linear regression slope of last n points"
  (when (>= (length data) n)
    (let* ((y (subseq data 0 n))
           (n-val (float n)))
      ;; Simplified slope: (first - last) / n
      (/ (- (first y) (car (last y))) n-val))))

;;; ==========================================
;;; SOFTMAX REGIME DETECTION (Phase 12)
;;; ==========================================

(defparameter *regime-scores* (make-hash-table :test 'eq) "Raw probability scores for each regime")

(defun softmax-normalize (scores)
  "Normalize a list of (key . score) using softmax"
  (let* ((values (mapcar #'cdr scores))
         (max-val (apply #'max values))
         (exp-values (mapcar (lambda (x) (exp (- x max-val))) values)) ; Stabilized softmax
         (sum-exp (reduce #'+ exp-values)))
    (loop for (key . val) in scores
          for exp-val in exp-values
          collect (cons key (/ exp-val sum-exp)))))

(defun calculate-regime-scores (spread-abs-pct vol vol-change momentum-10)
  "Calculate raw scores for each regime based on features"
  (let ((scores nil))
    
    ;; 1. Trend Early Score (Spread expanding, Momentum aligning)
    (push (cons :trend-early 
                (+ (* (if (and (> spread-abs-pct 0.0005) (< spread-abs-pct 0.002)) 1.0 0.0) 2.0)
                   (* (min 1.0 (abs (* momentum-10 1000))) 1.0)))
          scores)

    ;; 2. Trend Mature Score (Spread strong and stable)
    (push (cons :trend-mature 
                (+ (* (if (> spread-abs-pct 0.002) 1.0 0.0) 2.0)
                   (* (if (> vol 0.002) 1.0 0.0) 0.5)))
          scores)

    ;; 3. Trend Exhausted Score (Spread huge but Divergence or Climax Vol)
    (push (cons :trend-exhausted 
                (+ (* (if (> spread-abs-pct 0.004) 1.0 0.0) 1.5)
                   (* (if (> vol 0.008) 1.0 0.0) 2.0)))
          scores)

    ;; 4. Range Expansion Score (Vol Up, No Spread)
    (push (cons :range-expansion 
                (+ (* (if (< spread-abs-pct 0.001) 1.0 0.0) 1.0)
                   (* (min 2.0 (* vol-change 5.0)) 1.0))) ; Vol increasing
          scores)

    ;; 5. Range Compression Score (Vol Dead, No Spread)
    (push (cons :range-compression 
                (+ (* (if (< spread-abs-pct 0.0005) 1.0 0.0) 1.5)
                   (* (if (< vol 0.001) 1.0 0.0) 1.5)))
          scores)

    ;; 6. Volatile Spike Score (Extreme Vol)
    (push (cons :volatile-spike 
                (* (max 0.0 (- (* vol 1000) 5.0)) 1.0)) ; Vol > 0.5% starts scoring
          scores)

    ;; 7. Illiquid Score (Tiny Vol)
    (push (cons :illiquid 
                (if (< vol 0.0005) 5.0 0.0))
          scores)

    scores))

(defun detect-market-regime ()
  (unless (and *candle-history* (> (length *candle-history*) 60))
    (format t "[L] ⚠️ Not enough history for regime detection. Forcing :ILLIQUID.~%")
    (setf *current-regime* :illiquid)
    (return-from detect-market-regime :illiquid))

  (when (and *candle-history* (> (length *candle-history*) 60))
    (let* ((closes (mapcar #'candle-close (subseq *candle-history* 0 60)))
           (close (first closes))
           (sma20 (/ (reduce #'+ (subseq closes 0 20)) 20))
           (sma50 (/ (reduce #'+ (subseq closes 0 50)) 50))
           
           ;; Features
           (spread-pct (/ (- sma20 sma50) close))
           (spread-abs-pct (abs spread-pct))
           (vol (or (calculate-price-volatility closes 20) 0.001))
           (vol-prev (or (calculate-price-volatility (subseq closes 10 30) 20) 0.001))
           (vol-change (if (> vol-prev 1.0e-6) 
                           (/ (- vol vol-prev) vol-prev)
                           0.0))
           (momentum-10 (/ (- close (nth 10 closes)) (nth 10 closes))))

      ;; 1. Determine Volatility Regime
      (setf *volatility-regime*
            (cond
              ((> vol 0.005) :high)
              ((< vol 0.001) :low)
              (t :normal)))
      
      ;; 2. Calculate Softmax Regime Probability
      (let* ((raw-scores (calculate-regime-scores spread-abs-pct vol vol-change momentum-10))
             (probs (softmax-normalize raw-scores)))
        
        ;; Update Global Score Map
        (clrhash *regime-scores*)
        (loop for (key . val) in probs do
          (setf (gethash key *regime-scores*) val))
        
        ;; Set Current Regime to Max Probability (Backwards Compatibility)
        (setf *current-regime* (car (first (sort (copy-list probs) #'> :key #'cdr))))
        
        ;; Log Top 3
        (let ((sorted (sort (copy-list probs) #'> :key #'cdr)))
          (format t "[L] 📊 Market: ~a (Conf: ~,2f) | 2nd: ~a (~,2f)~%" 
                  (car (nth 0 sorted)) (cdr (nth 0 sorted))
                  (car (nth 1 sorted)) (cdr (nth 1 sorted)))))
      
      ;; 3. MACRO x MICRO MATRIX (Phase 33: 2-Axis Detection)
      ;; "The Market can remain irrational longer than you can remain solvent." - Keynes
      ;; "But Price is Truth." - Simons
      
      (let ((arrow-status (detect-broken-arrow))
            (vix (get-macro-latest "VIX"))
            (micro-trend-strength spread-abs-pct)) ;; 0.002 = 0.2% = Strong Trend
        
        ;; VIX Check
        (when (> vix 30.0)
          (format t "[L] ⚠️ MACRO ALERT: VIX at ~a (Risk-OFF). Forcing :VOLATILE-SPIKE~%" vix)
          (setf *current-regime* :volatile-spike))
          
        ;; Broken Arrow Check (US10Y/USDJPY Decoupling)
        (when (eq arrow-status :decoupled)
          (cond
            ;; Case A: Broken Arrow + STRONG MICRO TREND (> 0.2%)
            ;; The market is ignoring the macro disconnect. FOLLOW THE PRICE.
            ((> micro-trend-strength 0.002)
             (format t "[L] ⚔️ REGIME CONFLICT: Broken Arrow (Macro) vs Strong Trend (Micro).~%")
             (format t "[L] 🛡️ DECISION: Price is Truth. IGNORING Macro override. Regime stays: ~a~%" *current-regime*)
             ;; Ensure we don't accidentally default to a weak regime if Micro score was split
             (unless (or (eq *current-regime* :trend-mature) (eq *current-regime* :volatile-spike))
               (format t "[L] 🔧 UPGRADING regime to :TREND-MATURE based on price action.~%")
               (setf *current-regime* :trend-mature)))

            ;; Case B: Broken Arrow + WEAK/RANGING MICRO
            ;; Standard "Trend Exhaustion" logic applies.
            (t
             (format t "[L] 🏹 MACRO ALERT: Broken Arrow + Weak Price. Forcing :TREND-EXHAUSTED~%")
             (setf *current-regime* :trend-exhausted)))))

      *current-regime*)))

;;; ==========================================
;;; REGIME FORECASTING v2.0
;;; ==========================================

(defun capture-regime-snapshot ()
  "Capture comprehensive regime snapshot with features"
  (when (and *candle-history* (> (length *candle-history*) 50))
    (let* ((closes (mapcar #'candle-close (subseq *candle-history* 0 50)))
           (close (first closes))
           (sma20 (/ (reduce #'+ (subseq closes 0 20)) 20))
           (sma50 (/ (reduce #'+ closes) 50))
           (spread (if (> close 0) (/ (- sma20 sma50) close) 0))
           (vol (or (calculate-price-volatility closes 20) 0.001))
           (momentum (if (> (length closes) 10)
                         (- (first closes) (nth 9 closes))
                         0))
           (now (multiple-value-list (get-decoded-time))))
      (make-regime-snapshot
       :timestamp (get-universal-time)
       :regime *current-regime*
       :volatility *volatility-regime*
       :trend-strength (min 1.0 (abs (* spread 100)))
       :volatility-value vol
       :sma-spread spread
       :momentum momentum
       :hour-of-day (nth 2 now)))))

(defun record-regime ()
  "Record comprehensive regime snapshot"
  (let ((snapshot (capture-regime-snapshot)))
    (when snapshot
      (push snapshot *regime-history*)
      (when (> (length *regime-history*) *max-regime-history*)
        (setf *regime-history* (subseq *regime-history* 0 *max-regime-history*))))))

(defun calculate-transition-matrix ()
  "Build regime transition probability matrix"
  (let ((matrix (make-hash-table :test 'equal)))
    (loop for i from 0 below (1- (length *regime-history*))
          for curr = (regime-snapshot-regime (nth i *regime-history*))
          for prev = (regime-snapshot-regime (nth (1+ i) *regime-history*))
          do (incf (gethash (cons prev curr) matrix 0)))
    matrix))

(defun analyze-trend-momentum ()
  "Analyze trend momentum to predict regime changes"
  (when (> (length *regime-history*) 5)
    (let* ((recent (subseq *regime-history* 0 5))
           (strengths (mapcar #'regime-snapshot-trend-strength recent))
           (avg-strength (/ (reduce #'+ strengths) (length strengths)))
           (increasing (> (first strengths) (car (last strengths)))))
      (list :strength avg-strength
            :direction (if increasing :strengthening :weakening)))))

(defun analyze-volatility-trend ()
  "Analyze how volatility is evolving"
  (when (> (length *regime-history*) 5)
    (let* ((recent (subseq *regime-history* 0 5))
           (vols (mapcar #'regime-snapshot-volatility-value recent))
           (current (first vols))
           (older (car (last vols))))
      (cond
        ((> current (* older 1.3)) :increasing)
        ((< current (* older 0.7)) :decreasing)
        (t :stable)))))

(defun get-hour-patterns ()
  "Analyze regime patterns by hour of day"
  (let ((hour-stats (make-hash-table)))
    (dolist (snapshot *regime-history*)
      (let* ((hour (regime-snapshot-hour-of-day snapshot))
             (regime (regime-snapshot-regime snapshot))
             (key (cons hour regime))
             (stats (gethash key hour-stats (cons 0 0))))
        (incf (car stats))
        (setf (gethash key hour-stats) stats)))
    hour-stats))

(defun predict-next-regime ()
  "Advanced regime prediction with confidence scoring"
  (when (> (length *regime-history*) 10)
    (let* ((transition-matrix (calculate-transition-matrix))
           (current *current-regime*)
           (trend-analysis (analyze-trend-momentum))
           (vol-trend (analyze-volatility-trend))
           ;; Transition probabilities
           (trending-count (gethash (cons current :trending) transition-matrix 0))
           (ranging-count (gethash (cons current :ranging) transition-matrix 0))
           (total-trans (+ trending-count ranging-count 0.001))
           (trending-prob (/ trending-count total-trans))
           (ranging-prob (/ ranging-count total-trans))
           ;; Momentum adjustment
           (strength (getf trend-analysis :strength))
           (strengthening (eq (getf trend-analysis :direction) :strengthening)))
      
      ;; Calculate prediction with confidence
      (multiple-value-bind (predicted confidence)
          (cond
            ;; Strong trend getting stronger -> stay trending
            ((and (eq current :trending) strengthening (> strength 0.5))
             (values :trending 0.8))
            ;; Strong trend weakening -> might range
            ((and (eq current :trending) (not strengthening) (< strength 0.3))
             (values :ranging 0.6))
            ;; Ranging but trend building -> might trend
            ((and (eq current :ranging) strengthening (> strength 0.4))
             (values :trending 0.65))
            ;; Use transition probabilities
            ((> trending-prob 0.65)
             (values :trending (* 0.5 (+ 0.5 trending-prob))))
            ((> ranging-prob 0.65)
             (values :ranging (* 0.5 (+ 0.5 ranging-prob))))
            ;; Stay with current if uncertain
            (t (values current 0.4)))
        
        (setf *predicted-regime* predicted)
        (setf *regime-confidence* confidence)
        
        ;; Volatility prediction with trend
        (setf *predicted-volatility*
              (case vol-trend
                (:increasing (case *volatility-regime*
                               (:low :normal)
                               (:normal :high)
                               (:high :high)))
                (:decreasing (case *volatility-regime*
                               (:high :normal)
                               (:normal :low)
                               (:low :low)))
                (otherwise *volatility-regime*)))
        
        ;; Log prediction with confidence
        (format t "[L] 🔮 FORECAST: ~a → ~a (~,0f% confidence) | Vol: ~a → ~a (~a)~%" 
                *current-regime* *predicted-regime* (* confidence 100)
                *volatility-regime* *predicted-volatility* vol-trend)
        
        ;; Return prediction info
        (list :regime predicted
              :confidence confidence
              :volatility *predicted-volatility*
              :trend-strength strength
              :vol-trend vol-trend)))))
