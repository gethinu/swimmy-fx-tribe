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
    (let ((count 0)
          (new-names nil))
      (dolist (strat *evolved-strategies*)
        ;; Avoid duplicates in knowledge base
        (unless (find (strategy-name strat) *strategy-knowledge-base* :key #'strategy-name :test #'string=)
          (push strat *strategy-knowledge-base*)
          ;; Add to category pool for selection
          (let ((cat (categorize-strategy strat))) ; V8.7: Use correct categorization
            (when (boundp '*category-pools*)
               (push strat (gethash cat *category-pools*))))
          (incf count)
          (push (strategy-name strat) new-names)
          (format t "[RECRUIT] 🛡️ Inducted: ~a (Category: ~a)~%" (strategy-name strat) (categorize-strategy strat))))
      
      (when (> count 0)
        (format t "[RECRUIT] 🔥 ~d strategies promoted from evolution!~%" count)
        (notify-discord-recruit 
         (format nil "🔥 Recruited ~d new strategies!~%~{~a~%~}" count (reverse new-names)) 
         :color 3066993)
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

📍 ~a @ ~,3f (🕐 ~a)
~a

🎯 利確: +~d pips | 🛡️ 損切: -~d pips

💪 この条件で行く。
═══════════════════════════════"
            (if clan (clan-emoji clan) "🏛️") 
            (if clan (clan-name clan) "Unknown")
            name
            (mapcar (lambda (iv) (format nil "• ~a = ~,2f" (first iv) (second iv))) ind-vals)
            symbol price 
            (swimmy.core:get-jst-timestamp)
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

    ;; [V9.3] WEEKEND PROTECTION (Absolute Priority - Blocks Gotobi too)
    (cond
      ((= dow 6) (return-from is-safe-trading-time-p nil))                 ; Sunday = CLOSED
      ((and (= dow 5) (>= h 7)) (return-from is-safe-trading-time-p nil))  ; Saturday after 7:00 = CLOSED
      ((and (= dow 0) (< h 5)) (return-from is-safe-trading-time-p nil)))   ; Monday before 5:00 = CLOSED

    ;; Exempt specific time-based strategies (Must be AFTER weekend check)
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

(defun transform-cross-calls-helper (expr pkg)
  (flet ((add-prev-suffix (sym)
           (if (symbolp sym)
               (intern (format nil "~a-PREV" (symbol-name sym)) pkg)
               sym)))
    (cond
      ((atom expr) expr)
      ((and (listp expr) 
            (member (car expr) '(cross-above cross-below))
            (= (length expr) 3))
       (let ((fn (first expr))
             (a (second expr))
             (b (third expr)))
         (list fn 
               (if (symbolp a) a (eval a)) 
               (if (symbolp b) b (eval b)) 
               (if (symbolp a) (add-prev-suffix a) (eval a)) 
               (if (symbolp b) (add-prev-suffix b) (eval b)))))
      (t (mapcar (lambda (e) (transform-cross-calls-helper e pkg)) expr)))))

(defun evaluate-strategy-signal (strat history)
  (unless (is-safe-trading-time-p (strategy-name strat))
    (return-from evaluate-strategy-signal :hold))
  (when (and history (> (length history) 100))
    (let* ((indicators (strategy-indicators strat))
           (entry-logic (strategy-entry strat))
           (rest-hist (rest history))
           (bindings 
            (loop for ind in indicators
                  append (let ((type (car ind)) (p (cdr ind))
                                (pkg (find-package :swimmy.school)))
                            (case type
                              (sma `((,(intern (format nil "SMA-~d" (car p)) pkg) ,(ind-sma (car p) history))
                                     (,(intern (format nil "SMA-~d-PREV" (car p)) pkg) ,(ind-sma (car p) rest-hist))))
                              (ema `((,(intern (format nil "EMA-~d" (car p)) pkg) ,(ind-ema (car p) history))
                                     (,(intern (format nil "EMA-~d-PREV" (car p)) pkg) ,(ind-ema (car p) rest-hist))))
                              (rsi `((,(intern (format nil "RSI-~d" (car p)) pkg) ,(ind-rsi (car p) history))
                                     (,(intern (format nil "RSI-~d-PREV" (car p)) pkg) ,(ind-rsi (car p) rest-hist))))
                              (cci `((,(intern (format nil "CCI-~d" (car p)) pkg) ,(ind-cci (car p) history))
                                     (,(intern (format nil "CCI-~d-PREV" (car p)) pkg) ,(ind-cci (car p) rest-hist))))
                              (atr `((,(intern (format nil "ATR-~d" (car p)) pkg) ,(ind-atr (car p) history))))
                              (macd (multiple-value-bind (m s) (ind-macd (first p) (second p) (third p) history)
                                      (multiple-value-bind (pm ps) (ind-macd (first p) (second p) (third p) rest-hist)
                                        `((,(intern "MACD-LINE" pkg) ,m) (,(intern "SIGNAL-LINE" pkg) ,s) 
                                          (,(intern "MACD-LINE-PREV" pkg) ,pm) (,(intern "SIGNAL-LINE-PREV" pkg) ,ps)))))
                              (bb (multiple-value-bind (m u l) (ind-bb (first p) (second p) history)
                                    (multiple-value-bind (pm pu pl) (ind-bb (first p) (second p) rest-hist)
                                      (let ((dev (second p)))
                                        `((,(intern (format nil "BB-MIDDLE-~d" dev) pkg) ,m)
                                          (,(intern (format nil "BB-UPPER-~d" dev) pkg) ,u)
                                          (,(intern (format nil "BB-LOWER-~d" dev) pkg) ,l)
                                          (,(intern (format nil "BB-MIDDLE-~d-PREV" dev) pkg) ,pm)
                                          (,(intern (format nil "BB-UPPER-~d-PREV" dev) pkg) ,pu)
                                          (,(intern (format nil "BB-LOWER-~d-PREV" dev) pkg) ,pl)
                                          (,(intern "BB-MIDDLE" pkg) ,m) (,(intern "BB-UPPER" pkg) ,u) (,(intern "BB-LOWER" pkg) ,l)
                                          (,(intern "BB-MIDDLE-PREV" pkg) ,pm) (,(intern "BB-UPPER-PREV" pkg) ,pu) (,(intern "BB-LOWER-PREV" pkg) ,pl))))))
                              (stoch (let ((k (ind-stoch (first p) (second p) history))
                                           (pk (ind-stoch (first p) (second p) rest-hist)))
                                      `((,(intern "STOCH-K" pkg) ,k) (,(intern "STOCH-K-PREV" pkg) ,pk) 
                                        (,(intern "STOCH-D" pkg) 50) (,(intern "STOCH-D-PREV" pkg) 50))))
                              (session-high `((,(intern (format nil "SESSION-HIGH-~d-~d" (first p) (second p)) pkg) 
                                               ,(ind-session-high (first p) (second p) history))))
                              (session-low `((,(intern (format nil "SESSION-LOW-~d-~d" (first p) (second p)) pkg) 
                                              ,(ind-session-low (first p) (second p) history))))
                              (t 
                               (format t "[L] ⚠️ Unknown indicator type in ~a: ~a~%" (strategy-name strat) type)
                               nil))))))
      (let ((pkg (find-package :swimmy.school)))
        (push `(,(intern "CLOSE" pkg) ,(candle-close (first history))) bindings)
        (push `(,(intern "CLOSE-PREV" pkg) ,(candle-close (second history))) bindings)
        (push `(,(intern "HIGH" pkg) ,(candle-high (first history))) bindings)
        (push `(,(intern "HIGH-PREV" pkg) ,(candle-high (second history))) bindings)
        (push `(,(intern "LOW" pkg) ,(candle-low (first history))) bindings)
        (push `(,(intern "LOW-PREV" pkg) ,(candle-low (second history))) bindings)
        
        (multiple-value-bind (sec min hour day month year dow) (decode-universal-time (get-universal-time))
          (declare (ignore sec day month year dow))
          (push `(,(intern "HOUR" pkg) ,hour) bindings)
          (push `(,(intern "MINUTE" pkg) ,min) bindings)
          (let ((is-gotobi (if (fboundp (intern "GOTOBI-DAY-P" pkg))
                               (if (funcall (intern "GOTOBI-DAY-P" pkg)) t nil)
                               nil)))
            (push `(,(intern "GOTOBI-P" pkg) ,is-gotobi) bindings))))
      
      (setf bindings (remove-duplicates bindings :key #'car :from-end t))
      
      (let* ((pkg (find-package :swimmy.school))
             (transformed-logic (transform-cross-calls-helper entry-logic pkg)))
        (handler-case
            (locally (declare (sb-ext:muffle-conditions style-warning))
              (let ((entry-result (eval `(let ,bindings ,transformed-logic))))
                (cond
                  (entry-result :buy)
                  ((and (strategy-exit strat)
                        (eval `(let ,bindings ,(transform-cross-calls-helper (strategy-exit strat) pkg)))) :sell)
                  (t :hold))))
          (error (e) 
            (format t "[L] Eval error ~a: ~a~%" (strategy-name strat) e)
            :hold))))))
;; V5.2: Warrior ID System - 16 Global Slots (4 clans x 4 warriors)
;; V6.2 (Graham): Gotobi/Kelly/WhyLog moved to school-fortress.lisp
;; ============================================================

(defparameter *warrior-allocation* (make-hash-table :test 'equal))

(defun get-clan-id (category)
  "Get numeric ID for clan (used in Magic Number calculation)"
  (case category
    (:hunters 10) (:shamans 20) (:breakers 30) (:raiders 40) (t 90)))

(defun get-warrior-magic (category index)
  "Generate unique Magic Number for warrior: BASE + CLAN*10 + INDEX"
  (+ 123456 (* (get-clan-id category) 10) index))

(defun find-free-warrior-slot (category)
  "Find first available warrior slot (0-3) for the clan, returns nil if full"
  (loop for i from 0 to 3
        for key = (format nil "~a-~d" category i)
        when (null (gethash key *warrior-allocation*))
        return i))

(defun debug-warrior-status ()
  "Debug: Show all active warriors"
  (format t "=== WARRIOR STATUS ===~%")
  (if (= (hash-table-count *warrior-allocation*) 0)
      (format t "No active warriors.~%")
      (maphash (lambda (k v)
                 (format t "Slot ~a: ~a (~a) Magic:~d~%" 
                         k (getf v :symbol) (getf v :category) (getf v :magic)))
               *warrior-allocation*))
  (format t "Total: ~d/16~%" (hash-table-count *warrior-allocation*)))

(defun debug-reset-warriors ()
  "EMERGENCY: Clear all warrior allocations"
  (clrhash *warrior-allocation*)
  (format t "[DEBUG] 🧹 CLEARED *warrior-allocation* hash table. System reset.~%"))


(defun close-opposing-clan-positions (category new-direction symbol price reason)
  "Close positions in the opposite direction for Doten (Stop and Reverse) logic"
  (let ((opposing-direction (if (eq new-direction :buy) :short :long))
        (closed-count 0))
    (maphash 
     (lambda (key warrior)
       (when (and warrior 
                  (eq (getf warrior :category) category)
                  (eq (getf warrior :direction) opposing-direction)
                  (equal (getf warrior :symbol) symbol))
         ;; Close it
         (let* ((magic (getf warrior :magic))
                (entry (getf warrior :entry))
                (lot (or (getf warrior :lot) 0.01))
                (pnl (if (eq opposing-direction :long)
                         (- price entry)
                         (- entry price))))
           ;; Send CLOSE command
           (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" symbol) ("magic" magic))))
           ;; Free slot
           (remhash key *warrior-allocation*)
           (update-symbol-exposure symbol lot :close)
           ;; Logging & Recording (copied from close-category-positions)
           (format t "[L] 🔄 DOTEN: Closing ~a ~a for ~a signal (PnL: ~5f)~%" category opposing-direction new-direction pnl)
           (incf *daily-pnl* (round (* pnl 1000 100)))
           (record-trade-result (if (> pnl 0) :win :loss))
           (record-trade-outcome symbol (if (eq opposing-direction :long) :buy :sell) category "Doten" pnl)
           ;; Record logic for strategy stats
           (let ((lead-strat (first (gethash category *active-team*))))
               (when lead-strat
                 (record-strategy-trade (strategy-name lead-strat) (if (> pnl 0) :win :loss) pnl)))
           ;; Discord Notification
           (notify-discord-symbol symbol (format nil "🔄 **DOTEN** ~a ~a closed ~,2f" (if (> pnl 0) "✅" "❌") category pnl) 
                                  :color (if (> pnl 0) 3066993 15158332))
           (incf closed-count))))
     *warrior-allocation*)
    closed-count))

(defun execute-category-trade (category direction symbol bid ask)

  (format t "[TRACE] execute-category-trade ~a ~a symbol=~a bid=~a ask=~a~%" category direction symbol bid ask)
  (format t "[TRACE] Conditions: numberp-bid=~a numberp-ask=~a exposure-ok=~a~%" (numberp bid) (numberp ask) (total-exposure-allowed-p))
  (handler-case
    (when (and (numberp bid) (numberp ask) (total-exposure-allowed-p))  ; Safety + exposure check
    (let* ((strategies (gethash category *active-team*))
           (lead-strat (first strategies))
           (lead-name (when lead-strat (strategy-name lead-strat)))
           (rank-data (when lead-name (get-strategy-rank lead-name)))
           (rank (if rank-data (strategy-rank-rank rank-data) :scout))
           (rank-mult (calculate-rank-multiplier rank))  ; V2.0: Rank-based lot multiplier
           (base-lot (get-category-lot category))
           (history (gethash symbol *candle-histories*))
           ;; V2.0 Research Paper #18: Volatility-scaled lot size
           (vol-scaled-lot (if (and (fboundp 'volatility-scaled-lot) history)
                               (volatility-scaled-lot base-lot history)
                               base-lot))
           ;; V3.0: Apply volatility multiplier (previously unused!)
           (vol-mult (handler-case (get-volatility-lot-multiplier)
                       (error () 1.0)))
           ;; V3.0: Consider risk-parity lot (previously unused!)
           (rp-lot (handler-case (get-risk-parity-lot category)
                     (error () base-lot)))
           ;; V5.2 Research Paper #34: HDRL portfolio risk adjustment
           (hdrl-lot (handler-case (hdrl-adjusted-lot symbol base-lot)
                       (error () base-lot)))
           ;; V5.7 (Thorp): Kelly Criterion adjustment
           (kelly-adj (if lead-name (get-strategy-kelly-lot lead-name base-lot) base-lot))
           ;; Final lot: min of all adjustments, then apply rank multiplier
           (lot (max 0.01 (* rank-mult vol-mult 
                             (min (correlation-adjusted-lot symbol vol-scaled-lot) rp-lot hdrl-lot kelly-adj))))
           ;; V3.0: Track positions by strategy (not by category) for multi-position support
           (conf (or (and (boundp '*last-confidence*) *last-confidence*) 0.0))
           (pred (or (and (boundp '*last-prediction*) *last-prediction*) "HOLD"))
           (swarm-consensus (or (and (boundp '*last-swarm-consensus*) *last-swarm-consensus*) 0))
           (sl-pips 0.15) (tp-pips 0.40)
           (warmup-p (< *category-trades* 50))  ; First 50 trades = warmup
           ;; V2.1: Conditions for High Council convening
           (large-lot-p (> lot 0.05))      ; Large position needs council approval
           (high-rank-p (member rank '(:veteran :legend)))  ; High rank strategy
           (danger-p (and (boundp '*danger-level*) (> *danger-level* 2))))  ; High danger
      ;; Ensure conf is a number
      (setf conf (if (numberp conf) conf 0.0))
      
      ;; ════════════════════════════════════════════════════════════════════
      ;; P0 STARTUP SAFEGUARDS (Expert Panel 2026-01-07)
      ;; ════════════════════════════════════════════════════════════════════
      
      ;; P0-1: WARMUP GUARD - Block ALL trades during warmup period
      (let ((now (get-universal-time)))
        ;; Transition from warmup to trading when time expires
        (when (and (eq *system-state* :warmup) 
                   (> now *warmup-end-time*))
          (setf *system-state* :trading)
          (format t "[L] ✅ P0 WARMUP COMPLETE: Trading ENABLED~%"))
        
        ;; Block trades during warmup
        (when (eq *system-state* :warmup)
          (format t "[L] ⏳ P0 WARMUP: Trade blocked (~d sec remaining)~%" 
                  (- *warmup-end-time* now))
          (return-from execute-category-trade nil))
        
        ;; P0-2: ENTRY RATE LIMIT - Max 1 entry per second
        (when (< (- now *last-entry-time*) *min-entry-interval-seconds*)
          (format t "[L] ⚡ P0 RATE LIMIT: Too fast, skipping (last entry ~a sec ago)~%"
                  (- now *last-entry-time*))
          (return-from execute-category-trade nil))
        
        ;; P0-3: STARTUP POSITION LIMIT - Max 1 position during first 60 sec after warmup
        (let ((startup-period-end (+ *warmup-end-time* 60)))
          (when (and (< now startup-period-end)
                     (> (hash-table-count *warrior-allocation*) 0))
            (format t "[L] 🛡️ P0 STARTUP LIMIT: Max 1 position during startup~%")
            (return-from execute-category-trade nil)))
            
        ;; P0-4: MARKET HOURS CHECK - Block weekend trades to prevent ghost entries
        (unless (market-open-p)
          (format t "[L] 💤 MARKET CLOSED: Weekend/Holiday hours. Trade skipped.~%")
          (return-from execute-category-trade nil))

            
        ;; ════════════════════════════════════════════════════════════════════
        ;; P1 FAILURE SAFETY (Dynamic Circuit Breaker)
        ;; ════════════════════════════════════════════════════════════════════
        (when *circuit-breaker-active*
          (if (> now *breaker-cooldown-end*)
              (progn
                (setf *circuit-breaker-active* nil)
                (setf *recent-losses* nil)
                (format t "[L] ✅ CIRCUIT BREAKER RESET: Cooldown expired. Resuming trading.~%"))
              (progn
                (format t "[L] ⚡ CIRCUIT BREAKER ACTIVE: Trading HALTED for ~ds~%"
                        (- *breaker-cooldown-end* now))
                (return-from execute-category-trade nil)))))
      
      ;; V2.1: Convene High Council for important decisions
      (when (and (not warmup-p)
                 (or large-lot-p high-rank-p danger-p)
                 (fboundp 'convene-high-council))
        (let* ((proposal (format nil "~a ~a ~,2f lot (~a戦略: ~a)"
                                 symbol direction lot rank lead-name))
               (urgency (cond (danger-p :critical)
                              (large-lot-p :high)
                              (t :normal)))
               (council-result (convene-high-council proposal category :urgency urgency)))
          ;; If council rejects, skip the trade
          (when (eq council-result :rejected)
            (format t "[L] 🏛️ HIGH COUNCIL REJECTED: ~a~%" proposal)
            (return-from execute-category-trade nil))))
      
      ;; Check for failure pattern BLOCK
      (unless (should-block-trade-p symbol direction category)
        ;; P0 HARDENED: ALL trades MUST pass prediction filter (no warmup bypass)
        (let* ((prediction (handler-case (predict-trade-outcome symbol direction)
                             (error (e) (progn (format t "[L] Prediction error: ~a~%" e) nil))))
               ;; P0: NO BYPASS - prediction filter is MANDATORY
               (should-trade (if prediction 
                                 (should-take-trade-p prediction)
                                 nil)))  ;; No prediction = no trade
          ;; Explain decision
          (when prediction
            (handler-case
                (let ((factors (trade-prediction-factors prediction))
                      (action (if should-trade :execute :skip)))
                  (explain-trade-decision symbol direction action factors))
              (error (e) (format t "[L] Explain error: ~a~%" e))))
          
          ;; V5.5 (Soros): Global Panic Protocol
          (when (global-panic-active-p)
            (format t "[L] 💉 SOROS: Elevated volatility - trading with reduced size~%"))
          
          ;; V5.5 (Darwin): Unlearning Check
          (when (should-unlearn-p symbol)
            (setf should-trade nil)
            (format t "[L] 🧬 DARWIN: Recent performance toxic. Unlearning active.~%"))

          ;; V5.6 (Paper #36): Parallel Verification Loops
          (when should-trade
            (unless (verify-parallel-scenarios symbol direction category)
              (setf should-trade nil)
              (format t "[L] 🧬 PARALLEL VERIFICATION: FAILED (Trade rejected)~%")))
              
            (when should-trade
              ;; V5.5 (Sun Tzu): Operation Mist (Entry Jitter & Feint)
              (sleep (/ (random 2000) 1000.0)) ; 0-2s delay
              (when (< (random 100) 5)     ; 5% Feint chance
                 (format t "[L] ⚔️ SUN TZU: Operation Mist - Feint executed (Trade skipped)~%")
                 (return-from execute-category-trade nil))
              
              ;; V9.1 DOTEN: Check for and close opposing positions ("Stop and Reverse")
              (close-opposing-clan-positions category direction symbol (if (eq direction :buy) bid ask) "Doten")

              ;; V5.2: Warrior Allocation - Find free slot (0-3) for this clan
              (let ((slot-index (find-free-warrior-slot category)))

                (if slot-index
                     ;; Execute trade with unique Magic Number
                    (let* ((magic (get-warrior-magic category slot-index))
                           (key (format nil "~a-~d" category slot-index))
                           ;; V8.5 (Panel Decision): Tiered lot sizing based on Sharpe
                           ;; Sharpe 0.0 ~ 0.3 = cap at 0.01 (proving ground)
                           (strat (or (find lead-name *evolved-strategies* :key #'strategy-name :test #'string=)
                                      (find lead-name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
                           (sharpe (if strat (or (strategy-sharpe strat) 0.0) 0.0))
                           (lot (if (and (>= sharpe 0.0) (< sharpe 0.3))
                                    (progn
                                      (format t "[L] 📊 TIERED LOT: ~a (Sharpe ~,2f) capped at 0.01~%" lead-name sharpe)
                                      0.01)
                                    lot)))
                      (format t "[TRACE] Slot found: ~d Magic: ~d Direction: ~a (eq :buy? ~a)~%" slot-index magic direction (eq direction :buy))
                      (cond
                        ((eq direction :buy)
                         (let ((sl (- bid sl-pips)) (tp (+ bid tp-pips)))
                           ;; V5.7 (Feynman): Why Log
                           (log-why-trade symbol :buy category 
                                         :strategy lead-name 
                                         :tribe-cons (if (boundp '*tribe-consensus*) *tribe-consensus* 0)
                                         :swarm-cons swarm-consensus
                                         :parallel-score 2
                                         :elder-ok (not (should-block-trade-p symbol :buy category)))
                           ;; USE SAFE-ORDER for centralized risk check
                           (when (safe-order "BUY" symbol lot sl tp magic)
                             (setf (gethash key *warrior-allocation*) 
                                   (list :symbol symbol :category category :direction :long :entry bid :magic magic :lot lot :start-time (get-universal-time)))
                             (update-symbol-exposure symbol lot :open)
                             (incf *category-trades*)
                             ;; P0: Update last entry time for rate limiting
                             (setf *last-entry-time* (get-universal-time))
                             (format t "[L] ⚔️ WARRIOR #~d DEPLOYED: ~a -> ~a BUY (Magic ~d)~%" (1+ slot-index) category symbol magic)
                             ;; V8.1: Discord Notification
                             (swimmy.shell:notify-discord-symbol symbol 
                               (format nil "⚔️ **WARRIOR DEPLOYED** (🕐 ~a)~%Strategy: ~a~%Action: BUY ~a~%Lot: ~,2f~%Magic: ~d" 
                                       (swimmy.core:get-jst-timestamp)
                                       lead-name symbol lot magic)
                               :color 3066993))))
                        ((eq direction :sell)
                         (let ((sl (+ ask sl-pips)) (tp (- ask tp-pips)))
                           ;; USE SAFE-ORDER for centralized risk check
                           (when (safe-order "SELL" symbol lot sl tp magic)
                             (setf (gethash key *warrior-allocation*) 
                                   (list :symbol symbol :category category :direction :short :entry ask :magic magic :lot lot :start-time (get-universal-time)))
                             (update-symbol-exposure symbol lot :open)
                             (incf *category-trades*)
                             ;; P0: Update last entry time for rate limiting
                             (setf *last-entry-time* (get-universal-time))
                             (format t "[L] ⚔️ WARRIOR #~d DEPLOYED: ~a -> ~a SELL (Magic ~d)~%" (1+ slot-index) category symbol magic)
                             ;; V8.1: Discord Notification
                             (swimmy.shell:notify-discord-symbol symbol 
                               (format nil "⚔️ **WARRIOR DEPLOYED** (🕐 ~a)~%Strategy: ~a~%Action: SELL ~a~%Lot: ~,2f~%Magic: ~d" 
                                       (swimmy.core:get-jst-timestamp)
                                       lead-name symbol lot magic)
                               :color 15158332))))))
                    ;; Clan is full (4/4 warriors deployed)
                    (format t "[L] ⚠️ Clan ~a is fully deployed (4/4 warriors)!~%" category))))))))
    (error (e) (format t "[TRACE] 🚨 ERROR in execute-category-trade: ~a~%" e))))

;; Track entry prices for each category
(defparameter *category-entries* (make-hash-table :test 'eq))

(defun close-category-positions (symbol bid ask)
  "V5.2: Close warrior positions at SL/TP using warrior-allocation"
  (maphash 
   (lambda (key warrior)
     (when (and warrior (equal (getf warrior :symbol) symbol))
       (let* ((category (getf warrior :category))
              (pos (getf warrior :direction))
              (entry (getf warrior :entry))
               (magic (getf warrior :magic))
               (lot (or (getf warrior :lot) 0.01)) ; Use stored lot or default
               (sl-pips 0.15) (tp-pips 0.40)
               (pnl 0) (closed nil))
         (when (and entry (numberp bid) (numberp ask))
           (cond
             ((eq pos :long)
              (let ((sl (- entry sl-pips)) (tp (+ entry tp-pips)))
                (when (or (<= bid sl) (>= bid tp))
                  (setf pnl (- bid entry) closed t))))
             ((eq pos :short)
              (let ((sl (+ entry sl-pips)) (tp (- entry tp-pips)))
                (when (or (>= ask sl) (<= ask tp))
                  (setf pnl (- entry ask) closed t)))))
           (when closed
             ;; Send CLOSE with Magic Number
             (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" symbol) ("magic" magic))))
             ;; Free the slot
             (remhash key *warrior-allocation*)
             (update-symbol-exposure symbol lot :close)
             (format t "[L] ⚔️ WARRIOR #~d RETURNS: ~a (~a) PnL: ~5f~%" (1+ (parse-integer (subseq (string key) (1+ (position #\- (string key)))))) category (if (> pnl 0) "WIN" "LOSS") pnl)
             (incf *daily-pnl* (round (* pnl 1000 100)))
             (record-trade-result (if (> pnl 0) :win :loss))
             (record-trade-outcome symbol (if (eq pos :long) :buy :sell) category "Warriors" pnl)
             (let ((lead-strat (first (gethash category *active-team*))))
               (when lead-strat
                 (record-strategy-trade (strategy-name lead-strat) (if (> pnl 0) :win :loss) pnl)))
             (format t "[L] 🏁 WARRIOR RETURNED (~a): ~a pips~%" category (round (* pnl 100)))
             (contribute-to-treasury category pnl :trade (format nil "Trade ~a" (if (> pnl 0) "win" "loss")))
             (notify-discord-symbol symbol (format nil "~a ~a closed ~,2f" (if (> pnl 0) "✅" "❌") category pnl) 
                            :color (if (> pnl 0) 3066993 15158332)))))))
   *warrior-allocation*))


;; trading-allowed-p removed - now in risk-manager.lispenabled*

;; Global consensus tracker
(defvar *last-swarm-consensus* 0)

(defun process-category-trades (symbol bid ask)
  "Process trades using Swarm Intelligence, Memory System, Danger Avoidance, AND Research Insights"
  (when (and (trading-allowed-p) *candle-history* (> (length *candle-history*) 100))
    ;; First check if any positions need to be closed
    (close-category-positions symbol bid ask)
    
    ;; ===== DANGER AVOIDANCE: Check if safe to trade =====
    (unless (is-safe-to-trade-p)
      (return-from process-category-trades nil))
    
    ;; ===== VOLATILITY CHECK: Extreme volatility blocks trading =====
    (unless (volatility-allows-trading-p)
      (return-from process-category-trades nil))
    
    ;; ===== RESEARCH ENHANCED ANALYSIS (Auto-integrated Paper Insights) =====
    (let ((research-analysis nil))
      (when (>= (length *candle-history*) 50)
        (setf research-analysis (research-enhanced-analysis *candle-history*))
        ;; Apply volatility-based model selection
        (select-optimal-model *candle-history*)
        ;; Detect HMM regime
        (detect-regime-hmm *candle-history*))
    
    ;; ===== SWARM INTELLIGENCE: Collect votes from all strategies =====
    (let* ((swarm-decision (swarm-trade-decision symbol *candle-history*))
           (consensus (swarm-decision-consensus-strength swarm-decision))
           (swarm-direction (swarm-decision-direction swarm-decision))
           ;; ===== MEMORY SYSTEM: Check past experience =====
           (memory-suggestion (memory-suggests-direction symbol))
           (memory-confidence (if memory-suggestion 
                                  (get-memory-confidence symbol memory-suggestion)
                                  0.5))
           ;; ===== RESEARCH: Dual trend agreement check =====
           (dual-trend (when research-analysis (getf research-analysis :dual-trend)))
           (trend-agrees (or (null dual-trend)
                            (not (listp dual-trend))
                            (eq (getf dual-trend :agreement) :aligned)
                            (eq (getf dual-trend :direction) 
                                (case swarm-direction (:BUY :UP) (:SELL :DOWN) (t :FLAT))))))
      
      ;; Log collective decision with research enhancement
      (format t "[L] 🐟🐟🐟 SWARM: ~a (~,0f% consensus)~%" swarm-direction (* 100 consensus))
      (when (and dual-trend (listp dual-trend))
        (format t "[L] 📊 RESEARCH: ~a trend ~a~%" 
                (getf dual-trend :direction)
                (if trend-agrees "✓ agrees" "⚠ diverges")))
      
      ;; Store consensus for trade execution (NEW)
      (setf *last-swarm-consensus* consensus)
      
      (when memory-suggestion
        (format t "[L] 🧠 MEMORY suggests: ~a (~,0f% confidence)~%" 
                memory-suggestion (* 100 memory-confidence)))
      
      ;; ===== LEADER FISH: Check leader's opinion =====
      (elect-leader)  ; Update leader based on current performance
      (let ((boosted-decision (get-leader-boosted-decision swarm-decision)))
        (setf swarm-decision boosted-decision))
      
      ;; ===== UNIFIED DECISION MAKING (with Research Enhancement) =====
      ;; V3.0: 61 strategies are the ONLY entry source (removed 4-clan hardcoded signals)
      (handler-case
        (let* ((min-consensus-to-trade 0.25)
               (any-strong-signal nil))
        ;; V3.0: Always use 61-STRATEGY SIGNALS (no more tribe-signals check)
        (when (or t  ; Always proceed - strategies have their own conditions
                 (> consensus min-consensus-to-trade))
            ;; V3.0: Use 61-STRATEGY SIGNALS
            (progn
              (format t "[L] 🎯 61-STRATEGY SIGNAL SCAN~%")
              (let ((strat-signals (collect-strategy-signals symbol *candle-history*)))
                (setf any-strong-signal (and strat-signals t))
                (when strat-signals
                  (format t "[L] 📊 ~d strategies triggered signals~%" (length strat-signals))
                  ;; Group by category and pick best for each clan
                  (let ((by-category (make-hash-table :test 'eq)))
                    ;; Group signals by category
                    (dolist (sig strat-signals)
                      (let ((cat (getf sig :category)))
                        (push sig (gethash cat by-category))))
                    ;; Trade TOP 3 strategies per clan (not just 1)
                    (dolist (category '(:trend :reversion :breakout :scalp))
                      (let ((cat-sigs (gethash category by-category)))
                        (when cat-sigs
                          ;; Take up to 3 strategies per clan
                          (let ((top-sigs (subseq cat-sigs 0 (min 4 (length cat-sigs)))))
                            (dolist (sig top-sigs)
                              (let* ((strat-name (getf sig :strategy-name))
                                     (direction (getf sig :direction))
                                     ;; Per-strategy cooldown
                                     (strat-key (intern (format nil "~a-~a" category strat-name) :keyword)))
                                ;; Check per-strategy cooldown (not per-clan)
                                (when (can-clan-trade-p strat-key)
                                  (let ((narrative (generate-dynamic-narrative sig symbol bid)))
                                    ;; Log the dynamic narrative
                                    (format t "~a~%" narrative)
                                    ;; Send to Discord
                                    (handler-case
                                        (swimmy.shell:notify-discord-symbol symbol narrative :color (if (eq direction :buy) 3066993 15158332))
                                      (error (e) (format t "[L] Discord error: ~a~%" e)))
                                    ;; Record trade time for this strategy
                                    (record-clan-trade-time strat-key)
                                    ;; Execute the trade
                                    (format t "[TRACE] Calling execute-category-trade for ~a~%" category)
                                    (execute-category-trade category direction symbol bid ask)
                                    ;; Record for rank promotion
                                    (record-strategy-trade strat-name :trade 0)))))))))))))
            
            ;; No trade - explain why
            (cond
              ((not any-strong-signal)
               (format t "[L] ⏸️ HOLD: No strong signals~%"))
              ((< consensus min-consensus-to-trade)
               (format t "[L] ⏸️ HOLD: Weak consensus (~,0f%)~%" (* 100 consensus)))
              ((not trend-agrees)
               (format t "[L] ⏸️ HOLD: Research trend divergence~%")))))
        (error (e) nil))))))  ;; Suppress tribe processing errors

(defun force-recruit-strategy (name)
  "Forcefully recruit a strategy from knowledge base into active service (Special Forces)"
  (let ((strat (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
    (if strat
        (progn
          ;; Add to evolved strategies if not present
          (pushnew strat *evolved-strategies* :test #'string= :key #'strategy-name)
          ;; Add to category pools
          (let ((cat (categorize-strategy strat)))
             ;; Ensure the category list exists or update it
            (setf (gethash cat *category-pools*) 
                  (cons strat (remove (strategy-name strat) (gethash cat *category-pools*) 
                                    :key #'strategy-name :test #'string=))))
          (format t "[L] 🎖️ Special Force Recruited: ~a~%" name)
          t)
        (format t "[L] ⚠️ Special Force NOT FOUND: ~a~%" name))))

(defun recruit-special-forces ()
  (force-recruit-strategy "T-Nakane-Gotobi")
  ;; V8.9: Recruit External Founders (Diversity Injection)
  (recruit-founder :volvo)
  (recruit-founder :london)
  ;; V9.0: Auto-Immigration (Andrew Ng)
  (immigration-census))

(defun init-school ()
  ;; V8.7: Reclassify ALL strategies (KB + Evolved) to fix category bugs
  (build-category-pools) ; Clears pools and adds *strategy-knowledge-base*
  
  ;; Add Evolved/Recruited strategies to pools with NEW categorization logic
  (when (boundp '*evolved-strategies*)
    (dolist (strat *evolved-strategies*)
      (let ((cat (categorize-strategy strat)))
        (setf (strategy-category strat) cat) ; Update the slot permanently
        (push strat (gethash cat *category-pools* nil)))))
        
  (recruit-special-forces) ; V7.0: Inject special forces
  (clrhash *category-positions*)
  (format t "[SCHOOL] Swimmy School ready (Strategies Reclassified & Pools Built)~%"))

;;; ══════════════════════════════════════════════════════════════════
;;;  V3.0: 4氏族シグナル関数を削除
;;;  61戦略が唯一のエントリーロジック源 (collect-strategy-signals)
;;;  各戦略はinfer-strategy-categoryで氏族(:trend,:reversion,:breakout,:scalp)に配属
;;;  Kalman/HMM等の研究論文実装は品質フィルターとして使用
;;; ══════════════════════════════════════════════════════════════════

;; V6.3 (Graham): TRIBES stubs removed - collect-all-tribe-signals, aggregate-tribe-signals
;; were dead code always returning nil/0%. Trade decisions now use SWARM only.

;;; ══════════════════════════════════════════════════════════════════
;;;  V4.0: STRATEGY CORRELATION ANALYSIS (教授指摘)
;;;  目的: 冗長な戦略を特定して除去
;;; ══════════════════════════════════════════════════════════════════

(defparameter *strategy-signal-history* (make-hash-table :test 'equal))
(defparameter *strategy-correlation-cache* nil)

(defun record-strategy-signal (strategy-name direction timestamp)
  "Record strategy signal for correlation analysis"
  (let* ((key strategy-name)
         (history (gethash key *strategy-signal-history* nil))
         (signal (list timestamp direction)))
    (push signal history)
    ;; Keep only last 100 signals
    (when (> (length history) 100)
      (setf history (subseq history 0 100)))
    (setf (gethash key *strategy-signal-history*) history)))

(defun calculate-signal-correlation (name1 name2)
  "Calculate correlation between two strategies' signals"
  (let ((h1 (gethash name1 *strategy-signal-history*))
        (h2 (gethash name2 *strategy-signal-history*)))
    (if (and h1 h2 (> (length h1) 10) (> (length h2) 10))
        (let ((matches 0)
              (comparisons 0))
          ;; Compare signals within same time windows
          (dolist (s1 h1)
            (let ((t1 (first s1))
                  (d1 (second s1)))
              (dolist (s2 h2)
                (let ((t2 (first s2))
                      (d2 (second s2)))
                  ;; Same minute window
                  (when (< (abs (- t1 t2)) 60)
                    (incf comparisons)
                    (when (eq d1 d2)
                      (incf matches)))))))
          (if (> comparisons 0)
              (float (/ matches comparisons))
              0.0))
        0.0)))

(defun analyze-strategy-correlation ()
  "Analyze all strategies for correlation and identify redundant ones"
  (let ((strategies (mapcar (lambda (s) (strategy-name s)) *strategy-knowledge-base*))
        (high-corr nil))
    (format t "~%[L] 🔬 V4.0: STRATEGY CORRELATION ANALYSIS~%")
    (format t "[L] Analyzing ~d strategies...~%" (length strategies))
    
    ;; Compare all pairs
    (loop for i from 0 below (length strategies)
          for s1 = (nth i strategies) do
          (loop for j from (1+ i) below (length strategies)
                for s2 = (nth j strategies) do
                (let ((corr (calculate-signal-correlation s1 s2)))
                  (when (> corr 0.85)
                    (push (list s1 s2 corr) high-corr)
                    (format t "[L] ⚠️ High correlation (~,0f%): ~a ↔ ~a~%"
                            (* 100 corr) s1 s2)))))
    
    (setf *strategy-correlation-cache* high-corr)
    
    (if high-corr
        (format t "[L] 📊 Found ~d highly correlated pairs~%" (length high-corr))
        (format t "[L] ✅ No highly correlated strategies found~%"))
    
    high-corr))

(defun get-redundant-strategies ()
  "Get list of strategies that might be redundant"
  (unless *strategy-correlation-cache*
    (analyze-strategy-correlation))
  (let ((redundant nil))
    (dolist (pair *strategy-correlation-cache*)
      (let ((s1 (first pair))
            (s2 (second pair)))
        ;; Keep the one with better name (shorter, more descriptive)
        (if (< (length s1) (length s2))
            (pushnew s2 redundant :test 'equal)
            (pushnew s1 redundant :test 'equal))))
    redundant))

;;; ══════════════════════════════════════════════════════════════════
;;;  V5.8 (Graham): STRATEGY PRUNING - Remove redundant strategies
;;; ══════════════════════════════════════════════════════════════════

(defun prune-redundant-strategies ()
  "Remove strategies with >85% correlation (Graham's simplicity)"
  (let ((redundant (get-redundant-strategies))
        (removed 0))
    (when redundant
      (format t "[L] 🧹 GRAHAM: Pruning ~d redundant strategies...~%" (length redundant))
      (dolist (name redundant)
        (let ((strat (find name *strategy-knowledge-base* 
                          :key #'strategy-name :test #'string=)))
          (when strat
            (setf *strategy-knowledge-base* 
                  (remove strat *strategy-knowledge-base*))
            (incf removed)
            (format t "[L]   ✂️ Removed: ~a~%" name))))
      (format t "[L] 🧹 GRAHAM: Removed ~d strategies. Remaining: ~d~%" 
              removed (length *strategy-knowledge-base*)))
    removed))

(defun count-strategies-by-type ()
  "Count strategies by indicator type for analysis"
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (strat *strategy-knowledge-base*)
      (let* ((indicators (strategy-indicators strat))
             (types (mapcar (lambda (ind) 
                             (if (listp ind) (car ind) ind)) 
                           indicators)))
        (dolist (type types)
          (incf (gethash (symbol-name type) counts 0)))))
    (format t "[L] 📊 Strategy Type Distribution:~%")
    (maphash (lambda (k v) (format t "[L]   ~a: ~d~%" k v)) counts)
    counts))

(init-school)
;;; ══════════════════════════════════════════════════════════════════
;;;  V6.0: Load Fortress Module (Graham's Simplicity)
;;;  Features: V5.5 (Global Panic, Unlearning), V5.6 (Parallel Verification)
;;;            V5.7 (Kelly, Why Log), V5.8 (Gotobi, Pruning), High Council
;;; ══════════════════════════════════════════════════════════════════
;; (load (merge-pathnames "school-fortress.lisp" (directory-namestring *load-truename*)))

