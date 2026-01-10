;; school.lisp - Swimmy School: Team-based Portfolio Management
;; V6.14: Shared state moved to school-state.lisp (loaded before this file)

(in-package :swimmy.school)

;;; ==========================================
;;; PORTFOLIO CORRELATION RISK MANAGEMENT
;;; ==========================================
;; Note: *pair-correlations*, *symbol-exposure*, *max-symbol-exposure*, 
;;       *max-total-exposure* defined in school-state.lisp

;;; ==========================================
;;; DANGER AVOIDANCE SYSTEM (天敵回避)
;;; ==========================================
;;; V41.3: Moved to school-danger.lisp (loaded before this file)
;;; Functions: record-trade-result, get-current-price, execute-tactical-retreat,
;;;            activate-danger-cooldown, danger-cooldown-active-p,
;;;            get-cooldown-remaining, reset-danger-state

;;; ==========================================
;;; RESIGNATION JUDGMENT (投了判断)
;;; ==========================================
;;; V41.3: Moved to school-resignation.lisp (loaded before this file)
;;; Functions: check-resignation, has-resigned-p, is-safe-to-trade-p

(defun get-correlation (sym1 sym2)
  "Get correlation between two symbols"
  (let ((pairs (cdr (assoc sym1 *pair-correlations* :test #'string=))))
    (if pairs
        (or (cdr (assoc sym2 pairs :test #'string=)) 0.0)
        0.0)))

(defun calculate-correlated-exposure (symbol)
  "Calculate total exposure considering correlations"
  (let ((total 0.0))
    (maphash (lambda (sym exp)
               (let ((corr (abs (get-correlation symbol sym))))
                 (incf total (* exp corr))))
             *symbol-exposure*)
    total))

(defun correlation-adjusted-lot (symbol base-lot)
  "Reduce lot size if highly correlated positions exist"
  (let* ((corr-exposure (calculate-correlated-exposure symbol))
         (symbol-exp (gethash symbol *symbol-exposure* 0.0))
         (headroom (- *max-symbol-exposure* symbol-exp))
         (corr-factor (max 0.3 (- 1.0 (* corr-exposure 0.5)))))  ; Min 30% of base lot
    (min headroom (* base-lot corr-factor))))

(defun update-symbol-exposure (symbol lot action)
  "Update exposure tracking on trade open/close"
  (if (eq action :open)
      (incf (gethash symbol *symbol-exposure* 0.0) lot)
      (decf (gethash symbol *symbol-exposure* 0.0) lot)))

(defun total-exposure-allowed-p ()
  "Check if total exposure is within limits"
  (let ((total 0.0))
    (maphash (lambda (sym exp) (declare (ignore sym)) (incf total exp)) *symbol-exposure*)
    (< total *max-total-exposure*)))

;;; ==========================================
;;; EXTRACTED TO SEPARATE FILES (Uncle Bob Split)
;;; ==========================================
;;; The following modules have been extracted:
;;; - school-learning.lisp: Failure Learning System (lines 60-766)
;;; - school-volatility.lisp: Volatility Detection (ATR, HDRL)
;;; - school-research.lisp: Prediction, Explanation, Risk Parity
;;; ==========================================

;;; ══════════════════════════════════════════════════════════════════
;;;  SWIMMY CIVILIZATION: MODULES EXTRACTED
;;; ══════════════════════════════════════════════════════════════════
;;; The following logic has been moved to separate files (Phase 5 Refactor):
;;; - Clans & Strategy Ranks -> school-strategy.lisp
;;; - Swarm & High Council   -> school-voting.lisp
;;; - Learning & Failure     -> school-learning.lisp
;;; ==================================================================

;;; ==========================================
;;; Memory and Ecosystem Systems moved to school-learning.lisp
(defparameter *current-regime* :unknown)
(defparameter *volatility-regime* :normal)  ; :low, :normal, :high

(defun calculate-price-volatility (closes n)
  "Calculate simple volatility (standard deviation of returns)"
  (when (> (length closes) n)
    (let* ((returns (loop for i from 0 below (1- n)
                          collect (/ (- (nth i closes) (nth (1+ i) closes)) 
                                    (nth (1+ i) closes))))
           (mean (/ (reduce #'+ returns) (length returns)))
           (sq-diffs (mapcar (lambda (r) (expt (- r mean) 2)) returns)))
      (sqrt (/ (reduce #'+ sq-diffs) (length sq-diffs))))))

(defun detect-market-regime ()
  (when (and *candle-history* (> (length *candle-history*) 50))
    (let* ((closes (mapcar #'candle-close (subseq *candle-history* 0 50)))
           (sma20 (/ (reduce #'+ (subseq closes 0 20)) 20))
           (sma50 (/ (reduce #'+ closes) 50))
           (close (first closes))
           (vol (or (calculate-price-volatility closes 20) 0.001)))
      ;; Trend detection
      (setf *current-regime*
            (cond
              ((> (abs (- sma20 sma50)) (* close 0.002)) :trending)
              (t :ranging)))
      ;; Volatility detection
      (setf *volatility-regime*
            (cond
              ((> vol 0.005) :high)    ; > 0.5% per bar = high vol
              ((< vol 0.001) :low)     ; < 0.1% per bar = low vol
              (t :normal)))
      ;; V3.0: Enhanced volatility shift detection (previously unused!)
      (handler-case (detect-volatility-shift)
        (error (e) (format t "[L] Vol shift error: ~a~%" e)))
      ;; V3.0: Predict next regime (previously unused!)
      (handler-case
          (let ((next-regime (predict-next-regime)))
            (when next-regime
              (format t "[L] 🔮 Next regime prediction: ~a~%" next-regime)))
        (error (e) (format t "[L] Regime prediction error: ~a~%" e)))
      (format t "[L] 📊 Regime: ~a | Volatility: ~a (~,3f%)~%" 
              *current-regime* *volatility-regime* (* vol 100))
      *current-regime*)))

;;; ==========================================
;;; REGIME FORECASTING v2.0 (最高品質先読み)
;;; ==========================================
;;; Features:
;;; - Multi-feature regime analysis
;;; - Confidence scoring (0-100%)
;;; - Multiple time horizon forecasts
;;; - Trend momentum tracking
;;; - Time-based pattern recognition

(defparameter *regime-history* nil)          ; Rich history with features
(defparameter *max-regime-history* 120)      ; Keep 2 hours of data
(defparameter *predicted-regime* nil)
(defparameter *predicted-volatility* nil)
(defparameter *regime-confidence* 0.5)       ; Confidence in prediction

(defstruct regime-snapshot
  timestamp
  regime                ; :trending, :ranging
  volatility            ; :high, :normal, :low
  trend-strength        ; 0.0 to 1.0
  volatility-value      ; actual number
  sma-spread            ; SMA20 - SMA50 as % of price
  momentum              ; price change momentum
  hour-of-day)

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
           (hour (nth 2 (multiple-value-list (get-decoded-time))))
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


(defun get-regime-weights ()
  ;; Use predicted regime if available for proactive positioning
  (let* ((effective-regime (or *predicted-regime* *current-regime*))
         (effective-volatility (or *predicted-volatility* *volatility-regime*))
         (base-weights 
           (case effective-regime
             (:trending   '((:trend . 0.50) (:reversion . 0.20) (:breakout . 0.20) (:scalp . 0.10)))
             (:ranging    '((:trend . 0.20) (:reversion . 0.50) (:breakout . 0.10) (:scalp . 0.20)))
             (otherwise   *category-allocation*))))
    ;; Adjust for volatility
    (case effective-volatility
      (:high  ; High volatility: reduce all, favor scalping
       (mapcar (lambda (cw) (cons (car cw) (* (cdr cw) 0.5))) base-weights))
      (:low   ; Low volatility: increase positions
       (mapcar (lambda (cw) (cons (car cw) (* (cdr cw) 1.2))) base-weights))
      (otherwise base-weights))))

(defun select-best-from-pool (category n)
  (let* ((pool (gethash category *category-pools*))
         (sorted (sort (copy-list pool) #'> 
                       :key (lambda (s) (or (strategy-sharpe s) -999)))))
    (subseq sorted 0 (min n (length sorted)))))

(defun recruit-from-evolution ()
  "Promote evolved strategies from *evolved-strategies* to master knowledge base"
  (when (and (boundp '*evolved-strategies*) *evolved-strategies*)
    (let ((count 0))
      (dolist (strat *evolved-strategies*)
        ;; Avoid duplicates in knowledge base
        (unless (find (strategy-name strat) *strategy-knowledge-base* :key #'strategy-name :test #'string=)
          (push strat *strategy-knowledge-base*)
          ;; Add to category pool for selection
          (let ((cat (infer-strategy-category strat)))
            (when (boundp '*category-pools*)
               (push strat (gethash cat *category-pools*))))
          (incf count)
          (format t "[RECRUIT] 🛡️ Inducted: ~a (Category: ~a)~%" (strategy-name strat) (infer-strategy-category strat))))
      
      (when (> count 0)
        (format t "[RECRUIT] 🔥 ~d strategies promoted from evolution!~%" count)
        (notify-discord-recruit (format nil "🔥 Recruited ~d new strategies!" count) :color 3066993)
        ;; Clear the waiting list so we don't re-add
        (setf *evolved-strategies* nil)))))

(defun assemble-team ()
  (recruit-from-evolution) ; Check for new recruits first
  (detect-market-regime)
  (record-regime)          ; Track for pattern analysis
  (predict-next-regime)    ; Forecast next regime
  (let ((weights (get-regime-weights)))
    (clrhash *active-team*)
    (dolist (cat-weight weights)
      (let* ((cat (car cat-weight))
             (slots (cdr (assoc cat *slots-per-category*)))
             (best (select-best-from-pool cat slots)))
        (setf (gethash cat *active-team*) best)))))

;;; ══════════════════════════════════════════════════════════════════
;;;  61-STRATEGY SIGNAL SYSTEM (全戦略シグナルシステム)
;;; ══════════════════════════════════════════════════════════════════

;; Category inference from strategy name
(defun infer-strategy-category (strat)
  "Infer clan category from strategy name/indicators AND TP/SL values"
  (let ((name (string-downcase (strategy-name strat)))
        (tp (strategy-tp strat))
        (sl (strategy-sl strat)))
    (cond
      ;; Breakout strategies
      ((or (search "breakout" name) (search "squeeze" name) (search "low-vol" name)) :breakout)
      ;; Reversion strategies
      ((or (search "oversold" name) (search "overbought" name) (search "reversal" name) 
           (search "bounce" name) (search "reversion" name)) :reversion)
      ;; Scalp: tight TP (under 0.30 = 30 pips) and specific keywords
      ((and (<= tp 0.30) (or (search "scalp" name) (search "pop" name) (search "1m" name))) :scalp)
      ;; Everything else is trend
      (t :trend))))

;; Get actual indicator values for narrative
(defun get-indicator-values (strat history)
  "Calculate current indicator values for display"
  (let ((values nil))
    (dolist (ind (strategy-indicators strat))
      (let ((type (car ind)) (p (cdr ind)))
        (handler-case
            (case type
              (sma (push (list (format nil "SMA-~d" (car p)) (float (ind-sma (car p) history))) values))
              (ema (push (list (format nil "EMA-~d" (car p)) (float (ind-ema (car p) history))) values))
              (rsi (push (list (format nil "RSI-~d" (car p)) (float (ind-rsi (car p) history))) values))
              (macd (multiple-value-bind (m s) (ind-macd (first p) (second p) (third p) history)
                      (push (list "MACD" (float m)) values)))
              (bb (multiple-value-bind (m u l) (ind-bb (first p) (second p) history)
                    (push (list "BB-Mid" (float m)) values))))
          (error (e) nil))))
    (nreverse values)))

;; Duplicate trade prevention
(defparameter *last-clan-trade-time* (make-hash-table :test 'eq))
(defparameter *min-trade-interval* 300)  ; 5 min cooldown to reduce Discord spam

(defun can-clan-trade-p (category)
  (let ((last-time (gethash category *last-clan-trade-time* 0)))
    (> (- (get-universal-time) last-time) *min-trade-interval*)))

(defun record-clan-trade-time (category)
  (setf (gethash category *last-clan-trade-time*) (get-universal-time)))

;; Collect signals from all 61 strategies
(defun collect-strategy-signals (symbol history)
  "Evaluate ALL strategies and return triggered signals"
  (let ((signals nil))
    (dolist (strat *strategy-knowledge-base*)
      (handler-case
          (let* ((name (strategy-name strat))
                 ;; V5.1: Skip benched strategies
                 (benched (and (fboundp 'strategy-benched-p) (strategy-benched-p name))))
            (unless benched
              (let ((sig (evaluate-strategy-signal strat history)))
                (when (member sig '(:buy :sell))
                  ;; V4.0: Record for correlation analysis
                  (record-strategy-signal name sig (get-universal-time))
                  (push (list :strategy-name name
                              :category (infer-strategy-category strat)
                              :direction sig
                              :sl (strategy-sl strat)
                              :tp (strategy-tp strat)
                              :indicator-values (get-indicator-values strat history))
                        signals)))))
        (error (e) nil)))
    signals))

;; Generate dynamic narrative with actual values
(defun generate-dynamic-narrative (strat-signal symbol price)
  "Generate natural language explanation"
  (let* ((name (getf strat-signal :strategy-name))
         (direction (getf strat-signal :direction))
         (category (getf strat-signal :category))
         (ind-vals (getf strat-signal :indicator-values))
         ;; V5.1: Default SL/TP when strategy has nil
         (sl (or (getf strat-signal :sl) 0.15))  ; Default 15 pips
         (tp (or (getf strat-signal :tp) 0.40))  ; Default 40 pips
         (clan (get-clan category)))
    (format nil "
═══════════════════════════════
~a 【~a】が戦場に立つ！
═══════════════════════════════

📊 発動戦略: ~a

~{~a~^~%~}

📍 ~a @ ~,3f
~a

🎯 利確: +~d pips | 🛡️ 損切: -~d pips

💪 この条件で行く。
═══════════════════════════════"
            (if clan (clan-emoji clan) "🏛️") 
            (if clan (clan-name clan) "Unknown")
            name
            (mapcar (lambda (iv) (format nil "• ~a = ~,2f" (first iv) (second iv))) ind-vals)
            symbol price
            (if (eq direction :buy) "🟢 BUY - 上昇を狙う" "🔴 SELL - 下落を狙う")
            (round (* 100 tp)) (round (* 100 sl)))))

(defparameter *category-positions* (make-hash-table :test 'eq))
(defparameter *total-capital* 0.10)
(defparameter *lstm-threshold* 0.60)

(defun get-category-lot (category)
  (let ((alloc (cdr (assoc category (get-regime-weights)))))
    (if alloc (max 0.01 (* *total-capital* alloc)) 0.01)))

(defun is-safe-trading-time-p (strategy-name)
  "Check if current time is safe for trading (JST)"
  (multiple-value-bind (s m h d mo y dow) (decode-universal-time (get-universal-time))
    (declare (ignore s d mo y))
    ;; Exempt specific time-based strategies
    (when (search "Gotobi" strategy-name)
      (return-from is-safe-trading-time-p t))
      
    (cond
      ;; 1. ROLLOVER (Spread widen): 6:55 - 7:05
      ((= h 6) nil) 
      ((and (= h 7) (< m 5)) nil)
      
      ;; 2. ASIAN LUNCH (Low Volatility): 11:30 - 14:00 (approx)
      ;;    Often choppy and creates false signals for trend strategies
      ((or (= h 12) (= h 13)) nil)
      
      ;; 3. PRE-LONDON CHOP (Fakeouts): 15:00 - 15:30
      ;;    Often creates false breakouts before real volume comes in
      ((and (= h 15) (< m 30)) nil)
      
      ;; 4. FRIDAY CLOSE (Weekend Risk): After 23:00 on Friday
      ((and (= dow 5) (>= h 23)) nil)
      
      (t t))))

