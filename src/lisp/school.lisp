;; school.lisp - Swimmy School: Team-based Portfolio Management

;;; ==========================================
;;; FORWARD DECLARATIONS (eliminate warnings)
;;; ==========================================
(defvar *has-resigned-today* nil)
(defvar *current-leader* nil)
(defvar *trade-history* (make-hash-table :test 'eq))
(defvar *category-entries* (make-hash-table :test 'eq))
(defvar *last-swarm-consensus* 0)
(defvar *category-positions* nil)  ; Forward declaration for apply-hedge-logic
(defvar *daily-pnl* 0)
(defvar *accumulated-pnl* 0)
(defvar *monthly-goal* 100000)

;; V2.0: Tribe signal integration
(defvar *tribe-direction* :hold "Current tribe consensus direction")
(defvar *tribe-consensus* 0.0 "Current tribe consensus strength")

;;; ==========================================
;;; PORTFOLIO CORRELATION RISK MANAGEMENT
;;; ==========================================

;; Known correlations between pairs (approximate)
(defparameter *pair-correlations*
  '(("USDJPY" . (("EURJPY" . 0.85) ("GBPJPY" . 0.80) ("EURUSD" . -0.60) ("GBPUSD" . -0.50)))
    ("EURUSD" . (("GBPUSD" . 0.90) ("USDJPY" . -0.60) ("EURJPY" . 0.30)))
    ("GBPUSD" . (("EURUSD" . 0.90) ("USDJPY" . -0.50) ("GBPJPY" . 0.40)))))

;; Track total exposure per symbol
(defparameter *symbol-exposure* (make-hash-table :test 'equal))
(defparameter *max-symbol-exposure* 0.15)  ; Max 15% of capital per symbol
(defparameter *max-total-exposure* 0.30)   ; Max 30% total exposure

;;; ==========================================
;;; DANGER AVOIDANCE SYSTEM (天敵回避)
;;; ==========================================
;;; Inspired by: AlphaGo's resignation logic + RLHF safety
;;; Purpose: Protect the school from predators (consecutive losses)

;; Consecutive loss tracking
(defparameter *consecutive-losses* 0)
(defparameter *consecutive-wins* 0)
(defparameter *last-trade-result* nil)  ; :win, :loss, or nil

;; Cooldown state
(defparameter *danger-cooldown-until* 0)  ; Unix time when cooldown ends
(defparameter *danger-level* 0)           ; 0=safe, 1=caution, 2=danger, 3=flee

;; Cooldown durations (seconds)
(defparameter *cooldown-durations*
  '((1 . 0)      ; Level 0: No cooldown
    (2 . 120)    ; Level 1: 2 minutes (2 consecutive losses)
    (3 . 300)    ; Level 2: 5 minutes (3 consecutive losses)  
    (4 . 600)    ; Level 3: 10 minutes (4 consecutive losses)
    (5 . 1800))) ; Level 4+: 30 minutes (5+ consecutive losses)

(defun record-trade-result (result)
  "Record trade result and update danger level"
  (setf *last-trade-result* result)
  (if (eq result :loss)
      (progn
        (incf *consecutive-losses*)
        (setf *consecutive-wins* 0)
        ;; Trigger cooldown on consecutive losses
        (when (>= *consecutive-losses* 2)
          (activate-danger-cooldown)))
      (progn
        (incf *consecutive-wins*)
        (setf *consecutive-losses* 0)
        ;; Recovery from danger
        (when (> *danger-level* 0)
          (decf *danger-level*)
          (format t "[L] 🩹 RECOVERY: Danger level decreased to ~d~%" *danger-level*)))))

(defun activate-danger-cooldown ()
  "Activate cooldown based on consecutive losses"
  (let* ((losses *consecutive-losses*)
         (cooldown-entry (or (assoc losses *cooldown-durations* :test #'<=)
                             (cons 5 1800)))
         (duration (cdr cooldown-entry)))
    (setf *danger-level* (min 3 (- losses 1)))
    (setf *danger-cooldown-until* (+ (get-universal-time) duration))
    (format t "~%[L] 🦈🦈🦈 DANGER DETECTED! ~d consecutive losses~%" losses)
    (format t "[L] 🏃 FLEE MODE: Trading suspended for ~d seconds~%" duration)
    (format t "[L] 🐟 School retreating to safety...~%~%")))

(defun danger-cooldown-active-p ()
  "Check if we're in danger cooldown mode"
  (> *danger-cooldown-until* (get-universal-time)))

(defun get-cooldown-remaining ()
  "Get remaining cooldown time in seconds"
  (max 0 (- *danger-cooldown-until* (get-universal-time))))

;; is-safe-to-trade-p is defined after resignation judgment section

(defun reset-danger-state ()
  "Reset danger state (e.g., at start of new day)"
  (setf *consecutive-losses* 0)
  (setf *consecutive-wins* 0)
  (setf *danger-level* 0)
  (setf *danger-cooldown-until* 0)
  (setf *has-resigned-today* nil)  ; Reset resignation flag
  (format t "[L] 🌅 Danger state reset - new day, fresh start~%"))

;;; ==========================================
;;; RESIGNATION JUDGMENT (投了判断)
;;; ==========================================
;;; Inspired by: Shogi/Chess AI resignation logic
;;; "Knowing when to stop is as important as knowing when to trade"

(defparameter *has-resigned-today* nil)      ; Already resigned today?
(defparameter *resignation-threshold* -5000) ; Daily loss to trigger resign
(defparameter *resignation-loss-count* 7)    ; Or this many consecutive losses

(defun check-resignation ()
  "Check if today's trading should be abandoned"
  (when *has-resigned-today*
    (return-from check-resignation t))
  
  (let ((should-resign nil)
        (reason nil))
    
    ;; Condition 1: Daily loss exceeds threshold
    (when (< *daily-pnl* *resignation-threshold*)
      (setf should-resign t)
      (setf reason (format nil "Daily loss ¥~:d exceeds limit ¥~:d" 
                           (round *daily-pnl*) *resignation-threshold*)))
    
    ;; Condition 2: Too many consecutive losses
    (when (>= *consecutive-losses* *resignation-loss-count*)
      (setf should-resign t)
      (setf reason (format nil "~d consecutive losses - strategy mismatch" 
                           *consecutive-losses*)))
    
    ;; Condition 3: Daily goal already met (positive resignation)
    (when (and (> *daily-pnl* 0) 
               (> *daily-pnl* (* 1.5 (get-daily-target))))
      (setf should-resign t)
      (setf reason (format nil "Daily goal 150%% achieved: ¥~:d" (round *daily-pnl*))))
    
    (when should-resign
      (setf *has-resigned-today* t)
      (format t "~%[L] 🏳️ ═══════════════════════════════════════~%")
      (format t "[L] 🏳️  RESIGNATION: Today's trading ends~%")
      (format t "[L] 🏳️  Reason: ~a~%" reason)
      (format t "[L] 🏳️  Tomorrow is another day.~%")
      (format t "[L] 🏳️ ═══════════════════════════════════════~%~%"))
    
    should-resign))

(defun has-resigned-p ()
  "Check if we've resigned today"
  *has-resigned-today*)

(defun is-safe-to-trade-p ()
  "Master safety check - combines all safety conditions including resignation"
  (cond
    ;; Already resigned - no more trading today
    ((has-resigned-p)
     (format t "[L] 🏳️ RESIGNED: No trading until tomorrow~%")
     nil)
    ;; Check if we should resign
    ((check-resignation)
     nil)
    ;; In cooldown - definitely not safe
    ((danger-cooldown-active-p)
     (format t "[L] ⏸️ COOLDOWN: ~d seconds remaining~%" (get-cooldown-remaining))
     nil)
    ;; High danger level - extra caution
    ((>= *danger-level* 2)
     (format t "[L] ⚠️ HIGH DANGER: Level ~d - trading cautiously~%" *danger-level*)
     t)
    ;; Safe to trade
    (t t)))

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
;;; FAILURE LEARNING SYSTEM v2.0 (最高品質)
;;; ==========================================
;;; Features:
;;; - 15+ context variables for rich pattern matching
;;; - Time decay (exponential) - recent failures matter more
;;; - Confidence scoring instead of binary block
;;; - Fuzzy pattern matching (similar contexts contribute)
;;; - Statistical analysis with significance testing

(defparameter *failure-log* nil)           ; List of failure records
(defparameter *success-log* nil)           ; List of success records for comparison
(defparameter *max-log-size* 500)          ; Keep more history
(defparameter *decay-half-life* 3600)      ; 1 hour in seconds
(defparameter *min-samples-for-block* 5)   ; Need 5+ samples before blocking

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

(defun get-current-session ()
  "Determine current trading session based on UTC time"
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (declare (ignore sec min))
    (cond
      ((and (>= hour 0) (< hour 7)) :tokyo)      ; 00:00-07:00 UTC
      ((and (>= hour 7) (< hour 8)) :overlap)    ; Tokyo-London overlap
      ((and (>= hour 8) (< hour 12)) :london)    ; 08:00-12:00 UTC
      ((and (>= hour 12) (< hour 14)) :overlap)  ; London-NY overlap
      ((and (>= hour 14) (< hour 21)) :newyork)  ; 14:00-21:00 UTC
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
;;; VOLATILITY SHIFT DETECTION (評価値揺らぎ監視)
;;; ==========================================
;;; Inspired by: Shogi AI evaluation fluctuation monitoring
;;; Detects sudden market regime changes that require immediate response

(defparameter *volatility-history* nil)      ; Recent volatility readings
(defparameter *volatility-history-size* 20)  ; Keep last 20 readings
(defparameter *volatility-shift-threshold* 2.0) ; 2x change = shift
(defparameter *current-volatility-state* :normal) ; :normal, :elevated, :extreme
(defparameter *last-shift-time* 0)

(defstruct volatility-reading
  timestamp
  atr           ; Average True Range
  range-pct     ; High-Low as % of price
  direction-change-count ; How many direction changes in last N candles
  state)        ; :normal, :elevated, :extreme

(defun calculate-atr (history n)
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
      
      ;; Return current state and whether shift occurred
      (list :state new-state
            :ratio volatility-ratio
            :direction-changes direction-changes
            :is-shift is-shift))))

(defun volatility-allows-trading-p ()
  "Check if current volatility state allows normal trading"
  (case *current-volatility-state*
    (:normal t)
    (:elevated 
     (format t "[L] ⚠️ ELEVATED VOLATILITY: Reducing position size~%")
     t)  ; Allow but with caution
    (:extreme
     ;; V5.0: Allow trading during extreme volatility but with tiny size
     ;; Previously this blocked all trading, causing no entries for extended periods
     (format t "[L] 🛑 EXTREME VOLATILITY: Trading with minimal size~%")
     t)))

(defun get-volatility-lot-multiplier ()
  "Get lot size multiplier based on volatility"
  (case *current-volatility-state*
    (:normal 1.0)
    (:elevated 0.5)   ; Half size in elevated volatility
    (:extreme 0.0)))

;;; ==========================================
;;; TRADE OUTCOME PREDICTION (終局予測)
;;; ==========================================
;;; Inspired by: Go AI endgame prediction (KataGo score estimation)
;;; Predicts likely trade outcome before entering

(defparameter *prediction-history* nil)     ; Track predictions vs actuals
(defparameter *prediction-accuracy* 0.0)    ; Running accuracy score

(defstruct trade-prediction
  timestamp
  symbol
  direction          ; :buy or :sell
  predicted-outcome  ; :win or :loss
  confidence         ; 0.0-1.0
  factors            ; Factors that influenced prediction
  actual-outcome)    ; Filled in after trade closes

(defun predict-trade-outcome (symbol direction)
  "Predict whether a trade will be profitable"
  (let* ((history (or (gethash symbol *candle-histories*) *candle-history*))
         (factors nil)
         (win-score 0.0)
         (total-weight 0.0))
    
    (when (and history (> (length history) 50))
      ;; Factor 1: Trend alignment (weight: 3)
      (let* ((sma20 (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 20))) 20))
             (sma50 (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 50))) 50))
             (price (candle-close (first history)))
             (trend-up (and (> price sma20) (> sma20 sma50)))
             (trend-down (and (< price sma20) (< sma20 sma50)))
             (aligned (or (and trend-up (eq direction :buy))
                          (and trend-down (eq direction :sell)))))
        (incf total-weight 3.0)
        (when aligned (incf win-score 3.0))
        (push (list :trend-aligned aligned) factors))
      
      ;; Factor 2: Volatility state (weight: 2)
      (let ((vol-ok (not (eq *current-volatility-state* :extreme))))
        (incf total-weight 2.0)
        (when vol-ok (incf win-score 2.0))
        (push (list :volatility-ok vol-ok) factors))
      
      ;; Factor 3: Leader agreement (weight: 2)
      (when *current-leader*
        (let* ((leader-dir (get-leader-direction history))
               (agrees (eq leader-dir direction)))
          (incf total-weight 2.0)
          (when agrees (incf win-score 2.0))
          (push (list :leader-agrees agrees) factors)))
      
      ;; Factor 4: Consecutive candles (weight: 1.5)
      (let* ((up-count (count-consecutive-candles history :up))
             (down-count (count-consecutive-candles history :down))
             (momentum-aligned (or (and (> up-count 2) (eq direction :buy))
                                   (and (> down-count 2) (eq direction :sell)))))
        (incf total-weight 1.5)
        (when momentum-aligned (incf win-score 1.5))
        (push (list :momentum-aligned momentum-aligned) factors))
      
      ;; Factor 5: Session favorability (weight: 1)
      (let* ((session (get-current-session))
             (active-session (member session '(:tokyo :london :newyork :overlap))))
        (incf total-weight 1.0)
        (when active-session (incf win-score 1.0))
        (push (list :active-session (not (null active-session))) factors)))
    
    ;; Calculate prediction
    (let* ((confidence (if (> total-weight 0) (/ win-score total-weight) 0.5))
           (predicted-outcome (if (> confidence 0.5) :win :loss))
           (prediction (make-trade-prediction
                        :timestamp (get-universal-time)
                        :symbol symbol
                        :direction direction
                        :predicted-outcome predicted-outcome
                        :confidence confidence
                        :factors factors
                        :actual-outcome nil)))
      
      ;; Log prediction
      (format t "[L] 🔮 PREDICTION: ~a ~a → ~a (~,0f% confidence)~%"
              direction symbol predicted-outcome (* 100 confidence))
      
      ;; Store for later validation
      (push prediction *prediction-history*)
      (when (> (length *prediction-history*) 100)
        (setf *prediction-history* (subseq *prediction-history* 0 100)))
      
      prediction)))

(defun get-predicted-win-rate ()
  "Get recent prediction win rate"
  (let ((validated (remove-if-not #'trade-prediction-actual-outcome *prediction-history*))
        (correct 0))
    (dolist (p validated)
      (when (eq (trade-prediction-predicted-outcome p)
                (trade-prediction-actual-outcome p))
        (incf correct)))
    (if (> (length validated) 0)
        (/ correct (length validated))
        0.5)))

(defun should-take-trade-p (prediction)
  "Decide whether to take trade based on prediction"
  (let ((conf (trade-prediction-confidence prediction)))
    (cond
      ((< conf 0.35)
       (format t "[L] ❌ PREDICTION TOO LOW: Skipping trade~%")
       nil)
      ((< conf 0.5)
       (format t "[L] ⚠️ WEAK PREDICTION: Reducing size~%")
       t)
      (t t))))

;;; ==========================================
;;; RISK PARITY (リスクパリティ)
;;; ==========================================
;;; Inspired by: Modern portfolio theory + All Weather Fund
;;; Equal risk contribution from each category

(defparameter *category-volatilities* (make-hash-table :test 'equal))
(defparameter *target-total-risk* 0.02)  ; 2% of portfolio per trade round

(defun calculate-category-volatility (category)
  "Calculate historical volatility for a category's trades"
  (let ((trades (gethash category *trade-history*)))
    (if (and trades (> (length trades) 5))
        (let* ((pnls (mapcar #'trade-record-pnl trades))
               (mean (/ (reduce #'+ pnls) (length pnls)))
               (sq-diffs (mapcar (lambda (p) (expt (- p mean) 2)) pnls)))
          (sqrt (/ (reduce #'+ sq-diffs) (length sq-diffs))))
        0.01)))  ; Default volatility

(defun calculate-risk-parity-lots ()
  "Calculate lot sizes for equal risk contribution"
  (let ((volatilities nil)
        (total-inv-vol 0.0)
        (lots (make-hash-table :test 'equal)))
    
    ;; Calculate each category's volatility
    (dolist (cat '(:trend :reversion :breakout :scalp))
      (let ((vol (max 0.001 (calculate-category-volatility cat))))
        (setf (gethash cat *category-volatilities*) vol)
        (push (cons cat vol) volatilities)
        (incf total-inv-vol (/ 1.0 vol))))
    
    ;; Allocate lots inversely proportional to volatility
    (dolist (cat-vol volatilities)
      (let* ((cat (car cat-vol))
             (vol (cdr cat-vol))
             (weight (/ (/ 1.0 vol) total-inv-vol))
             (lot (* *target-total-risk* weight 10)))  ; Scale to lot size
        (setf (gethash cat lots) (max 0.01 (min 0.1 lot)))))
    
    lots))

(defun get-risk-parity-lot (category)
  "Get risk-parity adjusted lot for a category"
  (let ((lots (calculate-risk-parity-lots)))
    (or (gethash category lots) 0.01)))

;;; ==========================================
;;; SELF-EXPLANATION (自己説明)
;;; ==========================================
;;; Inspired by: LLM Chain-of-Thought, XAI (Explainable AI)
;;; Generate human-readable reasoning for every trade

(defparameter *trade-explanations* nil)
(defparameter *max-explanations* 50)

(defstruct trade-explanation
  timestamp
  symbol
  direction
  action          ; :execute, :skip, :reduce
  reasoning       ; List of reasons
  summary         ; One-line summary
  confidence)

(defun explain-trade-decision (symbol direction action factors)
  "Generate human-readable explanation for trade decision"
  (let* ((reasons nil)
         (summary ""))
    
    ;; Build reasoning from factors
    (dolist (f factors)
      (let ((name (first f))
            (value (second f)))
        (push (case name
                (:trend-aligned 
                 (if value "✓ Trend aligned with direction" "✗ Against trend"))
                (:volatility-ok
                 (if value "✓ Volatility acceptable" "✗ High volatility risk"))
                (:leader-agrees
                 (if value "✓ Leader confirms direction" "✗ Leader disagrees"))
                (:momentum-aligned
                 (if value "✓ Momentum supports entry" "✗ Weak momentum"))
                (:active-session
                 (if value "✓ Active trading session" "✗ Off-hours"))
                (otherwise (format nil "~a: ~a" name value)))
              reasons)))
    
    ;; Generate summary
    (setf summary
          (case action
            (:execute (format nil "~a ~a: Confidence high, executing" direction symbol))
            (:skip (format nil "SKIP ~a ~a: Conditions unfavorable" direction symbol))
            (:reduce (format nil "~a ~a: Reduced size due to mixed signals" direction symbol))))
    
    ;; Create explanation
    (let ((explanation (make-trade-explanation
                        :timestamp (get-universal-time)
                        :symbol symbol
                        :direction direction
                        :action action
                        :reasoning (nreverse reasons)
                        :summary summary
                        :confidence (or (getf factors :confidence) 0.5))))
      
      ;; Store and log
      (push explanation *trade-explanations*)
      (when (> (length *trade-explanations*) *max-explanations*)
        (setf *trade-explanations* (subseq *trade-explanations* 0 *max-explanations*)))
      
      ;; Output explanation
      (format t "[L] 📝 ~a~%" summary)
      (dolist (r reasons)
        (format t "[L]    ~a~%" r))
      
      explanation)))

(defun get-recent-trade-summary ()
  "Get summary of recent trades with explanations"
  (let ((recent (subseq *trade-explanations* 0 (min 5 (length *trade-explanations*)))))
    (format nil "Recent decisions:~%~{  - ~a~%~}"
            (mapcar #'trade-explanation-summary recent))))


(defun get-rich-market-context (symbol)
  "Capture comprehensive market context (15+ variables)"
  (let* ((history (or (gethash symbol *candle-histories*) *candle-history*))
         (close (and history (candle-close (first history))))
         (open (and history (candle-open (first history))))
         (sma20 (and history (> (length history) 20)
                     (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 20))) 20)))
         (rsi (and history (> (length history) 14) (ind-rsi 14 history)))
         (atr (and history (> (length history) 14) (ind-atr 14 history)))
         (recent-high (and history (> (length history) 20)
                           (reduce #'max (mapcar #'candle-high (subseq history 0 20)))))
         (recent-low (and history (> (length history) 20)
                          (reduce #'min (mapcar #'candle-low (subseq history 0 20)))))
         (now (multiple-value-list (get-decoded-time))))
    (list
     :regime *current-regime*
     :volatility *volatility-regime*
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
     :spread-condition :normal  ; Would need real spread data
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
          (when (> similarity 0.5)  ; Only count if reasonably similar
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
       (format t "[L] � HIGH FAILURE RISK (~,1f%) - blocking~%" (* failure-prob 100))
       0.0)  ; Block trade
      ((> failure-prob 0.6)
       (format t "[L] ⚠️ ELEVATED RISK (~,1f%) - reducing to 50%~%" (* failure-prob 100))
       0.5)
      ((> failure-prob 0.4)
       (format t "[L] 📊 MODERATE RISK (~,1f%) - reducing to 70%~%" (* failure-prob 100))
       0.7)
      (t 1.0))))  ; Full size

(defun should-block-trade-p (symbol direction category)
  "Check if this trade should be blocked based on failure analysis"
  (let ((penalty (get-failure-penalty symbol direction category)))
    (zerop penalty)))

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
                  :hit-sl-or-tp hit)))
    (if (< pnl 0)
        (progn
          (push record *failure-log*)
          (when (> (length *failure-log*) *max-log-size*)
            (setf *failure-log* (subseq *failure-log* 0 *max-log-size*)))
          (format t "[L] � FAILURE: ~a | ~a ~a | RSI:~,0f | Session:~a | Prob:~,1f%~%"
                  strategy-name direction (getf ctx :regime) 
                  (getf ctx :rsi-value) (getf ctx :session)
                  (* 100 (calculate-failure-confidence symbol direction category))))
        (progn
          (push record *success-log*)
          (when (> (length *success-log*) *max-log-size*)
            (setf *success-log* (subseq *success-log* 0 *max-log-size*)))
          (format t "[L] ✅ SUCCESS: ~a | ~a ~a | RSI:~,0f | Session:~a~%"
                  strategy-name direction (getf ctx :regime)
                  (getf ctx :rsi-value) (getf ctx :session))))))

;; Legacy compatibility wrapper
(defun record-failure (symbol direction category strategy-name pnl)
  (record-trade-outcome symbol direction category strategy-name pnl))

(defun analyze-failure-patterns ()
  "Statistical analysis of failure patterns"
  (let ((pattern-stats (make-hash-table :test 'equal)))
    ;; Aggregate by key patterns
    (dolist (record *failure-log*)
      (let ((key (format nil "~a|~a|~a" 
                         (trade-record-direction record)
                         (trade-record-regime record)
                         (trade-record-session record))))
        (incf (gethash key pattern-stats 0))))
    ;; Sort by frequency
    (let ((sorted nil))
      (maphash (lambda (k v) (push (cons k v) sorted)) pattern-stats)
      (sort sorted #'> :key #'cdr))))

(defun get-failure-summary ()
  "Get detailed failure summary for Gemini feedback"
  (let* ((patterns (analyze-failure-patterns))
         (total-failures (length *failure-log*))
         (total-successes (length *success-log*))
         (overall-rate (if (> (+ total-failures total-successes) 0)
                           (* 100 (/ total-successes (+ total-failures total-successes)))
                           50)))
    (format nil "Overall win rate: ~,1f% (~d wins, ~d losses). Top failure patterns: ~{~a (~d)~^, ~}"
            overall-rate total-successes total-failures
            (loop for (k . v) in (subseq patterns 0 (min 3 (length patterns)))
                  collect k collect v))))

;;; ══════════════════════════════════════════════════════════════════
;;;  SWIMMY CIVILIZATION: THE FOUR GREAT CLANS (4大氏族)
;;; ══════════════════════════════════════════════════════════════════
;;; "勝とうとするな。ただ、生き残れ。そうすれば、最後に立っているのは我々だ。"
;;; — Swimmy Constitution, Article 0

;; Clan definitions with personas and battle cries
(defstruct clan
  id              ; :trend, :reversion, :breakout, :scalp (original)
  name            ; Tribal name
  title           ; Japanese title
  emoji           ; Icon
  persona         ; Character description
  battle-cry      ; What they say when trading
  philosophy)     ; Their core belief

(defparameter *clans*
  (list
   (make-clan :id :trend
              :name "Hunters"
              :title "追跡者"
              :emoji "🏹"
              :persona "忍耐・豪快"
              :battle-cry "北への風が強まっています。掟の範囲内で最大まで張ります。"
              :philosophy "風が吹いている")
   
   (make-clan :id :reversion
              :name "Shamans"
              :title "呪術師"
              :emoji "🔮"
              :persona "論理・冷静"
              :battle-cry "グラフは歪んでいます。反動に備えます。"
              :philosophy "高すぎるものは落ちる")
   
   (make-clan :id :breakout
              :name "Breakers"
              :title "破壊者"
              :emoji "⚔️"
              :persona "好戦的・爆発力"
              :battle-cry "城壁崩壊！チャンスは一瞬！Rustの許可範囲で突撃します。"
              :philosophy "壁は壊された")
   
   (make-clan :id :scalp
              :name "Raiders"
              :title "盗賊"
              :emoji "🗡️"
              :persona "敏捷・狡猾"
              :battle-cry "市場は荒れていますが、隙間で稼ぎました。今日の食い扶持です。"
              :philosophy "隙あり")))

(defun get-clan (category-id)
  "Get clan by category ID"
  (find category-id *clans* :key #'clan-id))

(defun get-clan-display (category-id)
  "Get display string for category: emoji Name (ORIGINAL)"
  (let ((clan (get-clan category-id)))
    (if clan
        (format nil "~a ~a (~a)" 
                (clan-emoji clan) (clan-name clan) (string-upcase (symbol-name category-id)))
        (string-upcase (symbol-name category-id)))))

(defun get-clan-battle-cry (category-id)
  "Get the battle cry for a clan"
  (let ((clan (get-clan category-id)))
    (if clan (clan-battle-cry clan) "")))

(defun generate-clan-narrative (category-id direction confidence symbol price)
  "Generate natural language narrative for clan trade entry"
  (let* ((clan (get-clan category-id))
         (strategy-desc (case category-id
                          (:trend "🎯 MACD + ADX momentum with Kalman trend filter. We ride the wave until exhaustion.")
                          (:reversion "🔮 RSI oversold/overbought + Bollinger deviation. The price must return to equilibrium.")
                          (:breakout "💥 Bollinger band breakout confirmed by volume. Once the walls fall, we charge.")
                          (:scalp "⚡ EMA velocity + micro-swing detection. Quick profits in the chaos.")
                          (t "Unknown strategy")))
         (exit-plan (case direction
                      (:buy "📈 Exit: Take profit at +1.0%, Stop loss at -0.3%")
                      (:sell "📉 Exit: Take profit at +1.0%, Stop loss at -0.3%")
                      (t "Unknown")))
         (dir-emoji (if (eq direction :buy) "🟢 BUY" "🔴 SELL"))
         (dir-str (string-upcase (symbol-name direction))))
    (format nil "
═══════════════════════════════
~a 【~a】 ENTERS THE BATTLEFIELD!
═══════════════════════════════
「~a」

📍 Symbol: ~a @ ~,3f
~a

~a
~a

💪 Confidence: ~,0f%
═══════════════════════════════"
            (clan-emoji clan) (clan-name clan)
            (clan-philosophy clan)
            symbol price
            dir-emoji
            strategy-desc
            exit-plan
            (* 100 confidence))))

;; announce-clan-trade removed in V3.0 - duplicate notification (narrative already sent)


;;; ══════════════════════════════════════════════════════════════════
;;;  HIERARCHY SYSTEM (階級制度)
;;; ══════════════════════════════════════════════════════════════════
;;; Warriors (戦士) vs Scouts (斥候) - 実弾 vs テスト

(defparameter *strategy-ranks* (make-hash-table :test 'equal))

(defstruct strategy-rank
  name              ; Strategy name
  rank              ; :scout, :warrior, :veteran, :legend
  trades            ; Total trades executed
  wins              ; Winning trades
  total-pnl         ; Cumulative PnL
  promotion-date    ; When promoted
  last-trade)       ; Last trade timestamp

(defun get-strategy-rank (name)
  "Get or create rank for strategy"
  (or (gethash name *strategy-ranks*)
      (setf (gethash name *strategy-ranks*)
            (make-strategy-rank
             :name name
             :rank :scout  ; All start as Scouts
             :trades 0 :wins 0 :total-pnl 0
             :promotion-date nil :last-trade nil))))

(defun calculate-rank-multiplier (rank)
  "Get lot multiplier based on rank"
  (case rank
    (:scout    0.25)   ; 25% - Learning
    (:warrior  1.00)   ; 100% - Full combat
    (:veteran  1.25)   ; 125% - Proven
    (:legend   1.50)   ; 150% - Hall of Fame material
    (otherwise 0.50)))

(defun check-promotion (strategy-name)
  "Check if strategy deserves promotion"
  (let ((rank-data (get-strategy-rank strategy-name)))
    (let* ((trades (strategy-rank-trades rank-data))
           (wins (strategy-rank-wins rank-data))
           (pnl (strategy-rank-total-pnl rank-data))
           (win-rate (if (> trades 0) (/ wins trades) 0))
           (current-rank (strategy-rank-rank rank-data)))
      
      ;; Promotion criteria
      (cond
        ;; Scout → Warrior: 10+ trades, 40%+ win rate, positive PnL
        ((and (eq current-rank :scout)
              (>= trades 10)
              (>= win-rate 0.40)
              (> pnl 0))
         (setf (strategy-rank-rank rank-data) :warrior)
         (setf (strategy-rank-promotion-date rank-data) (get-universal-time))
         ;; Coming of Age ceremony
         (coming-of-age strategy-name "Scout" "Warrior")
         :warrior)
        
        ;; Warrior → Veteran: 50+ trades, 50%+ win rate, 500+ PnL
        ((and (eq current-rank :warrior)
              (>= trades 50)
              (>= win-rate 0.50)
              (> pnl 500))
         (setf (strategy-rank-rank rank-data) :veteran)
         (setf (strategy-rank-promotion-date rank-data) (get-universal-time))
         (coming-of-age strategy-name "Warrior" "Veteran")
         :veteran)
        
        ;; Veteran → Legend: 100+ trades, 55%+ win rate, 2000+ PnL
        ((and (eq current-rank :veteran)
              (>= trades 100)
              (>= win-rate 0.55)
              (> pnl 2000))
         (setf (strategy-rank-rank rank-data) :legend)
         (setf (strategy-rank-promotion-date rank-data) (get-universal-time))
         (coming-of-age strategy-name "Veteran" "Legend")
         ;; Induct to Hall of Fame
         (induct-to-hall-of-fame strategy-name pnl current-rank 
                                  (format nil "Win rate ~,0f%%" (* 100 win-rate)))
         :legend)
        
        ;; Demotion: 5+ consecutive losses at Warrior level
        ((and (member current-rank '(:warrior :veteran))
              (< pnl -300))  ; Significant loss
         (setf (strategy-rank-rank rank-data) :scout)
         (format t "[L] ⚠️ ~a demoted to Scout due to poor performance~%" strategy-name)
         :scout)
        
        (t current-rank)))))

(defun record-strategy-trade (strategy-name outcome pnl)
  "Record trade and update rank"
  (let ((rank-data (get-strategy-rank strategy-name)))
    (incf (strategy-rank-trades rank-data))
    (when (eq outcome :win) (incf (strategy-rank-wins rank-data)))
    (incf (strategy-rank-total-pnl rank-data) pnl)
    (setf (strategy-rank-last-trade rank-data) (get-universal-time))
    ;; Check for promotion
    (check-promotion strategy-name)))


;;; ══════════════════════════════════════════════════════════════════
;;;  INTER-TRIBAL ECONOMICS (氏族間経済)
;;; ══════════════════════════════════════════════════════════════════
;;; Mutual Aid: Raiders feed Hunters during waiting periods

(defparameter *clan-treasury* (make-hash-table :test 'eq))
(defparameter *mutual-aid-history* nil)

(defun initialize-clan-treasury ()
  "Initialize treasury for each clan"
  (dolist (cat '(:trend :reversion :breakout :scalp))
    (setf (gethash cat *clan-treasury*) 0)))

(defstruct treasury-entry
  clan
  amount
  source         ; :trade, :mutual-aid, :tribute
  timestamp
  description)

(defun contribute-to-treasury (clan amount source desc)
  "Add to clan treasury"
  (incf (gethash clan *clan-treasury* 0) amount)
  (push (make-treasury-entry
         :clan clan :amount amount :source source
         :timestamp (get-universal-time) :description desc)
        *mutual-aid-history*))

(defun calculate-mutual-aid ()
  "Calculate mutual aid between clans
   Raiders (scalp) share 10% of profits with waiting Hunters (trend)"
  (let ((raider-treasury (gethash :scalp *clan-treasury* 0))
        (hunter-treasury (gethash :trend *clan-treasury* 0)))
    
    ;; If Raiders profitable and Hunters struggling
    (when (and (> raider-treasury 100)
               (< hunter-treasury 0))
      (let ((aid-amount (* 0.10 raider-treasury)))
        ;; Transfer
        (decf (gethash :scalp *clan-treasury*) aid-amount)
        (incf (gethash :trend *clan-treasury*) aid-amount)
        
        (format t "[L] 🤝 MUTUAL AID: Raiders share ¥~,0f with Hunters~%" aid-amount)
        (format t "[L]    「市場は荒れていますが、我々の日銭が仲間を支えます」~%")
        
        (push (list :from :scalp :to :trend :amount aid-amount 
                    :time (get-universal-time))
              *mutual-aid-history*)
        
        aid-amount))))

(defun apply-hedge-logic (main-clan direction symbol bid ask)
  "Shamans provide hedge for Breakers' aggressive trades.
   When Breakers go aggressive, Shamans take a small counter-position.
   This is the 'inter-tribal cooperation' aspect of the civilization."
  (when (and (eq main-clan :breakout)
             (member direction '(:buy :sell)))
    ;; Shamans take 30% size counter-position
    (let* ((counter-direction (if (eq direction :buy) :sell :buy))
           (hedge-lot 0.01)  ; Small hedge position
           (hedge-sl 0.10)
           (hedge-tp 0.20))
      (format t "[L] 🔮 Shamans: 「Breakersの~a突撃に備え、~a反動用意」~%" 
              direction counter-direction)
      ;; Only execute hedge if we don't already have a Shaman position
      (unless (gethash :reversion *category-positions*)
        ;; Execute small counter-trade
        (cond
          ((eq counter-direction :buy)
           (let ((sl (- bid hedge-sl)) (tp (+ bid hedge-tp)))
             (pzmq:send *cmd-publisher* 
                        (jsown:to-json 
                         (jsown:new-js ("action" "BUY") ("symbol" symbol) 
                                       ("volume" hedge-lot) ("sl" sl) ("tp" tp))))
             (setf (gethash :reversion *category-positions*) :long)
             (format t "[L] 🔮 Shamans HEDGE BUY ~,2f lot~%" hedge-lot)))
          ((eq counter-direction :sell)
           (let ((sl (+ ask hedge-sl)) (tp (- ask hedge-tp)))
             (pzmq:send *cmd-publisher*
                        (jsown:to-json 
                         (jsown:new-js ("action" "SELL") ("symbol" symbol)
                                       ("volume" hedge-lot) ("sl" sl) ("tp" tp))))
             (setf (gethash :reversion *category-positions*) :short)
             (format t "[L] 🔮 Shamans HEDGE SELL ~,2f lot~%" hedge-lot))))
        ;; Log the inter-tribal cooperation
        (format t "[L] 🤝 氏族間協力: Breakers攻撃 ⇔ Shamansヘッジ~%")
        t))))

(defun get-clan-treasury-summary ()
  "Get summary of all clan treasuries"
  (format t "~%[L] ═══════════════════════════════════════~%")
  (format t "[L] 💰 CLAN TREASURIES~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (maphash (lambda (clan amount)
             (let ((clan-obj (get-clan clan)))
               (when clan-obj
                 (format t "[L] ~a ~a: ¥~:d~%" 
                         (clan-emoji clan-obj) (clan-name clan-obj) (round amount)))))
           *clan-treasury*)
  (format t "[L] ═══════════════════════════════════════~%~%"))


;;; ==========================================
;;; CATEGORY ALLOCATION (with Clan mapping)
;;; ===========================================

(defparameter *category-allocation*
  '((:trend . 0.40) (:reversion . 0.30) (:breakout . 0.20) (:scalp . 0.10)))

(defparameter *slots-per-category*
  '((:trend . 2) (:reversion . 2) (:breakout . 1) (:scalp . 1)))

(defparameter *category-pools* (make-hash-table :test 'eq))
(defparameter *active-team* (make-hash-table :test 'eq))

(defun categorize-strategy (strat)
  (let ((name (string-downcase (strategy-name strat))))
    (cond
      ((or (search "cross" name) (search "ema" name) (search "sma" name) (search "macd" name)) :trend)
      ((or (search "reversion" name) (search "bounce" name) (search "stoch" name)) :reversion)
      ((or (search "break" name) (search "atr" name)) :breakout)
      ((or (search "scalp" name) (search "fast" name)) :scalp)
      (t :trend))))

(defun build-category-pools ()
  (clrhash *category-pools*)
  (dolist (strat *strategy-knowledge-base*)
    (let ((cat (categorize-strategy strat)))
      (push strat (gethash cat *category-pools* nil)))))

;;; ==========================================
;;; SWARM INTELLIGENCE (群れの知恵)
;;; ==========================================
;;; Features:
;;; - Strategy voting with weighted votes
;;; - Consensus threshold for trade execution
;;; - Minority report tracking
;;; - Confidence aggregation

(defparameter *swarm-consensus-threshold* 0.6)  ; 60% agreement needed
(defparameter *swarm-vote-log* nil)             ; Track voting history
(defparameter *max-vote-log* 100)

(defstruct strategy-vote
  strategy-name
  direction       ; :buy :sell :hold
  confidence      ; 0.0-1.0
  weight          ; based on sharpe/win-rate
  timestamp
  category)

(defstruct swarm-decision
  timestamp
  direction          ; :buy :sell :hold
  consensus-strength ; 0.0-1.0
  votes-for
  votes-against
  votes-hold
  minority-report    ; dissenting opinions
  confidence)

(defun calculate-strategy-weight (strat)
  "Calculate voting weight based on strategy performance"
  (let* ((sharpe (or (strategy-sharpe strat) 0))
         (base-weight 1.0))
    ;; Weight based on Sharpe ratio
    (cond
      ((> sharpe 1.0) (* base-weight 2.0))
      ((> sharpe 0.5) (* base-weight 1.5))
      ((> sharpe 0) (* base-weight 1.2))
      ((< sharpe -0.5) (* base-weight 0.5))
      (t base-weight))))

(defun collect-strategy-votes (symbol history)
  "Collect votes from all active strategies"
  (let ((votes nil))
    (maphash 
     (lambda (category strategies)
       (dolist (strat strategies)
         (let* ((signal (evaluate-strategy-signal strat history))
                (weight (calculate-strategy-weight strat))
                (vote (make-strategy-vote
                       :strategy-name (strategy-name strat)
                       :direction signal
                       :confidence (if (eq signal :hold) 0.3 0.7)
                       :weight weight
                       :timestamp (get-universal-time)
                       :category category)))
           (push vote votes))))
     *active-team*)
    votes))

(defun aggregate-swarm-votes (votes)
  "Aggregate votes into a collective decision"
  (let ((buy-weight 0.0)
        (sell-weight 0.0)
        (hold-weight 0.0)
        (total-weight 0.0)
        (buy-votes 0)
        (sell-votes 0)
        (hold-votes 0)
        (minority nil))
    
    ;; Sum weighted votes
    (dolist (vote votes)
      (let ((w (strategy-vote-weight vote))
            (dir (strategy-vote-direction vote)))
        (incf total-weight w)
        (case dir
          (:buy (incf buy-weight w) (incf buy-votes))
          (:sell (incf sell-weight w) (incf sell-votes))
          (otherwise (incf hold-weight w) (incf hold-votes)))))
    
    ;; Determine winning direction
    (let* ((max-weight (max buy-weight sell-weight hold-weight))
           (winner (cond
                     ((= max-weight buy-weight) :buy)
                     ((= max-weight sell-weight) :sell)
                     (t :hold)))
           (consensus (if (> total-weight 0) (/ max-weight total-weight) 0)))
      
      ;; Identify minority reports (strong dissenting votes)
      (dolist (vote votes)
        (when (and (not (eq (strategy-vote-direction vote) winner))
                   (> (strategy-vote-weight vote) 1.2))
          (push (format nil "~a votes ~a" 
                        (strategy-vote-strategy-name vote)
                        (strategy-vote-direction vote))
                minority)))
      
      (make-swarm-decision
       :timestamp (get-universal-time)
       :direction winner
       :consensus-strength consensus
       :votes-for (case winner (:buy buy-votes) (:sell sell-votes) (t hold-votes))
       :votes-against (case winner (:buy (+ sell-votes hold-votes)) 
                                   (:sell (+ buy-votes hold-votes))
                                   (t (+ buy-votes sell-votes)))
       :votes-hold hold-votes
       :minority-report minority
       :confidence (* consensus 0.8)))))  ; Scale confidence by consensus

(defun swarm-trade-decision (symbol history)
  "Get swarm's collective trading decision"
  (let* ((votes (collect-strategy-votes symbol history))
         (decision (aggregate-swarm-votes votes)))
    
    ;; Log significant decisions
    (when (> (swarm-decision-consensus-strength decision) 0.5)
      (format t "[L] 🐟 SWARM: ~a (~,0f% consensus, ~d for/~d against)~%"
              (swarm-decision-direction decision)
              (* 100 (swarm-decision-consensus-strength decision))
              (swarm-decision-votes-for decision)
              (swarm-decision-votes-against decision)))
    
    ;; Log minority reports
    (when (swarm-decision-minority-report decision)
      (format t "[L] 📢 Minority: ~{~a~^, ~}~%" 
              (subseq (swarm-decision-minority-report decision) 
                      0 (min 3 (length (swarm-decision-minority-report decision))))))
    
    ;; Record for analysis
    (push decision *swarm-vote-log*)
    (when (> (length *swarm-vote-log*) *max-vote-log*)
      (setf *swarm-vote-log* (subseq *swarm-vote-log* 0 *max-vote-log*)))
    
    decision))

(defun swarm-should-trade-p (decision)
  "Check if swarm consensus is strong enough to trade"
  (and (not (eq (swarm-decision-direction decision) :hold))
       (>= (swarm-decision-consensus-strength decision) *swarm-consensus-threshold*)))

(defun get-swarm-confidence (decision)
  "Get confidence level from swarm decision"
  (swarm-decision-confidence decision))

;;; ==========================================
;;; LEADER FISH SYSTEM (リーダーフィッシュ)
;;; ==========================================
;;; Inspired by: Ensemble Meta-Learning + Natural flocking behavior
;;; The best performing strategy leads the school

(defparameter *current-leader* nil)           ; Current leader strategy
(defparameter *leader-tenure* 0)              ; How long current leader has led
(defparameter *min-leader-tenure* 10)         ; Minimum tenure before leader change
(defparameter *leader-bonus-weight* 2.0)      ; Extra voting weight for leader
(defparameter *leader-history* nil)           ; Track leader performance

(defstruct leader-info
  strategy-name
  sharpe
  win-rate
  tenure-start
  trades-as-leader
  pnl-as-leader)

(defun elect-leader ()
  "Elect the best performing strategy as leader"
  (let* ((all-strategies (append *strategy-knowledge-base* *evolved-strategies*))
         (candidates (remove-if-not (lambda (s) 
                                      (and (strategy-sharpe s)
                                           (> (strategy-sharpe s) 0)))
                                    all-strategies))
         (sorted (sort (copy-list candidates) #'> :key #'strategy-sharpe))
         (best (first sorted)))
    (when best
      (let ((new-leader-name (strategy-name best)))
        ;; Only change leader if tenure exceeded or no current leader
        (when (or (null *current-leader*)
                  (> *leader-tenure* *min-leader-tenure*))
          (unless (and *current-leader* 
                       (string= new-leader-name 
                                (leader-info-strategy-name *current-leader*)))
            ;; New leader elected!
            (when *current-leader*
              (push *current-leader* *leader-history*))
            (setf *current-leader*
                  (make-leader-info
                   :strategy-name new-leader-name
                   :sharpe (strategy-sharpe best)
                   :win-rate 0.0
                   :tenure-start (get-universal-time)
                   :trades-as-leader 0
                   :pnl-as-leader 0.0))
            (setf *leader-tenure* 0)
            (format t "[L] 👑 NEW LEADER: ~a (Sharpe: ~,2f)~%" 
                    new-leader-name (strategy-sharpe best)))))))
  *current-leader*)

(defun get-leader-direction (history)
  "Get the leader's trading signal"
  (when *current-leader*
    (let* ((leader-name (leader-info-strategy-name *current-leader*))
           (leader-strat (or (find leader-name *strategy-knowledge-base* 
                                   :key #'strategy-name :test #'string=)
                             (find leader-name *evolved-strategies*
                                   :key #'strategy-name :test #'string=))))
      (when leader-strat
        (evaluate-strategy-signal leader-strat history)))))

(defun leader-agrees-p (decision)
  "Check if leader agrees with swarm decision"
  (when (and *current-leader* *candle-history*)
    (let ((leader-signal (get-leader-direction *candle-history*)))
      (eq leader-signal (swarm-decision-direction decision)))))

(defun get-leader-boosted-decision (decision)
  "Boost swarm decision if leader agrees, or flag caution if not"
  (if (leader-agrees-p decision)
      (progn
        (format t "[L] 👑 LEADER CONFIRMS: ~a~%" (swarm-decision-direction decision))
        ;; Boost confidence when leader agrees
        (setf (swarm-decision-confidence decision)
              (min 1.0 (* (swarm-decision-confidence decision) 1.3)))
        decision)
      (progn
        (when *current-leader*
          (format t "[L] ⚠️ LEADER DISAGREES: ~a says ~a~%" 
                  (leader-info-strategy-name *current-leader*)
                  (get-leader-direction *candle-history*)))
        ;; Reduce confidence when leader disagrees
        (setf (swarm-decision-confidence decision)
              (* (swarm-decision-confidence decision) 0.7))
        decision)))

(defun update-leader-stats (pnl)
  "Update leader's performance statistics"
  (when *current-leader*
    (incf (leader-info-trades-as-leader *current-leader*))
    (incf (leader-info-pnl-as-leader *current-leader*) pnl)
    (incf *leader-tenure*)))

(defun analyze-swarm-accuracy ()
  "Analyze historical accuracy of swarm decisions"
  (let ((correct 0) (total 0))
    ;; Compare swarm decisions with actual outcomes
    ;; This would need to be correlated with trade results
    (dolist (decision *swarm-vote-log*)
      (when (> (swarm-decision-consensus-strength decision) 0.6)
        (incf total)))
    (if (> total 0)
        (/ correct total)
        0.5)))

;;; ==========================================
;;; MEMORY SYSTEM (記憶と想起)
;;; ==========================================
;;; Features:
;;; - Store market patterns with outcomes
;;; - Similarity-based pattern retrieval
;;; - Experience-based decision making
;;; - Episodic and semantic memory

(defparameter *episodic-memory* nil)          ; Specific trade memories
(defparameter *semantic-memory* nil)          ; Generalized patterns
(defparameter *max-episodic-memory* 1000)
(defparameter *max-semantic-memory* 100)
(defparameter *memory-similarity-threshold* 0.7)

(defstruct memory-episode
  timestamp
  symbol
  ;; Pattern features
  pattern-hash          ; Quick lookup key
  regime
  volatility
  rsi-value
  momentum-direction
  sma-position
  hour-of-day
  day-of-week
  consecutive-candles
  price-range           ; High-low range as %
  ;; What happened
  trade-direction
  outcome               ; :win :loss :breakeven
  pnl
  hold-time)

(defstruct semantic-pattern
  key                   ; Pattern identifier
  occurrences           ; How many times seen
  win-count
  loss-count
  total-pnl
  avg-hold-time
  best-direction        ; Most successful direction
  last-seen)

(defun create-pattern-hash (regime volatility rsi-zone momentum sma-pos hour)
  "Create a hash key for pattern lookup"
  (format nil "~a|~a|~a|~a|~a|~a" 
          regime volatility rsi-zone momentum sma-pos 
          (cond ((< hour 8) :asia)
                ((< hour 16) :europe)
                (t :america))))

(defun capture-current-pattern (symbol)
  "Capture current market pattern for memory"
  (let* ((ctx (get-rich-market-context symbol))
         (history (or (gethash symbol *candle-histories*) *candle-history*))
         (range (if (and history (> (length history) 10))
                    (let* ((highs (mapcar #'candle-high (subseq history 0 10)))
                           (lows (mapcar #'candle-low (subseq history 0 10)))
                           (max-h (reduce #'max highs))
                           (min-l (reduce #'min lows)))
                      (if (> min-l 0) (* 100 (/ (- max-h min-l) min-l)) 0))
                    0))
         (rsi-zone (getf ctx :rsi-zone))
         (hour (getf ctx :hour-of-day)))
    (list
     :hash (create-pattern-hash (getf ctx :regime)
                                (getf ctx :volatility)
                                rsi-zone
                                (getf ctx :momentum)
                                (getf ctx :sma-position)
                                hour)
     :regime (getf ctx :regime)
     :volatility (getf ctx :volatility)
     :rsi-value (getf ctx :rsi-value)
     :momentum (getf ctx :momentum)
     :sma-position (getf ctx :sma-position)
     :hour hour
     :day (getf ctx :day-of-week)
     :consecutive (getf ctx :consecutive-candles)
     :range range)))

(defun store-memory (symbol direction outcome pnl hold-time)
  "Store a trade experience in episodic memory"
  (let* ((pattern (capture-current-pattern symbol))
         (episode (make-memory-episode
                   :timestamp (get-universal-time)
                   :symbol symbol
                   :pattern-hash (getf pattern :hash)
                   :regime (getf pattern :regime)
                   :volatility (getf pattern :volatility)
                   :rsi-value (getf pattern :rsi-value)
                   :momentum-direction (getf pattern :momentum)
                   :sma-position (getf pattern :sma-position)
                   :hour-of-day (getf pattern :hour)
                   :day-of-week (getf pattern :day)
                   :consecutive-candles (getf pattern :consecutive)
                   :price-range (getf pattern :range)
                   :trade-direction direction
                   :outcome outcome
                   :pnl pnl
                   :hold-time hold-time)))
    ;; Add to episodic memory
    (push episode *episodic-memory*)
    (when (> (length *episodic-memory*) *max-episodic-memory*)
      (setf *episodic-memory* (subseq *episodic-memory* 0 *max-episodic-memory*)))
    
    ;; Update semantic memory (generalized patterns)
    (update-semantic-memory episode)))

(defun update-semantic-memory (episode)
  "Update generalized pattern knowledge from episode"
  (let* ((key (memory-episode-pattern-hash episode))
         (existing (find key *semantic-memory* :key #'semantic-pattern-key :test #'string=)))
    (if existing
        (progn
          (incf (semantic-pattern-occurrences existing))
          (if (eq (memory-episode-outcome episode) :win)
              (incf (semantic-pattern-win-count existing))
              (incf (semantic-pattern-loss-count existing)))
          (incf (semantic-pattern-total-pnl existing) (memory-episode-pnl episode))
          (setf (semantic-pattern-last-seen existing) (get-universal-time)))
        ;; Create new semantic pattern
        (let ((new-pattern (make-semantic-pattern
                            :key key
                            :occurrences 1
                            :win-count (if (eq (memory-episode-outcome episode) :win) 1 0)
                            :loss-count (if (eq (memory-episode-outcome episode) :loss) 1 0)
                            :total-pnl (memory-episode-pnl episode)
                            :avg-hold-time (memory-episode-hold-time episode)
                            :best-direction (memory-episode-trade-direction episode)
                            :last-seen (get-universal-time))))
          (push new-pattern *semantic-memory*)
          (when (> (length *semantic-memory*) *max-semantic-memory*)
            (setf *semantic-memory* 
                  (subseq (sort *semantic-memory* #'> :key #'semantic-pattern-occurrences)
                          0 *max-semantic-memory*)))))))

(defun calculate-pattern-similarity (pattern1 pattern2)
  "Calculate similarity between two patterns (0.0 to 1.0)"
  (let ((score 0) (total 8))
    (when (eq (getf pattern1 :regime) (memory-episode-regime pattern2))
      (incf score 2))
    (when (eq (getf pattern1 :volatility) (memory-episode-volatility pattern2))
      (incf score 2))
    (when (eq (getf pattern1 :sma-position) (memory-episode-sma-position pattern2))
      (incf score 1))
    (when (eq (getf pattern1 :momentum) (memory-episode-momentum-direction pattern2))
      (incf score 1))
    (when (< (abs (- (or (getf pattern1 :rsi-value) 50) 
                     (or (memory-episode-rsi-value pattern2) 50))) 15)
      (incf score 1))
    (when (< (abs (- (or (getf pattern1 :hour) 12) 
                     (or (memory-episode-hour-of-day pattern2) 12))) 4)
      (incf score 1))
    (/ score total)))

(defun recall-similar-experiences (symbol &optional (limit 10))
  "Retrieve similar past experiences from memory"
  (let* ((current (capture-current-pattern symbol))
         (matches nil))
    ;; Search episodic memory for similar patterns
    (dolist (episode *episodic-memory*)
      (let ((sim (calculate-pattern-similarity current episode)))
        (when (> sim *memory-similarity-threshold*)
          (push (cons sim episode) matches))))
    ;; Sort by similarity and return top matches
    (mapcar #'cdr 
            (subseq (sort matches #'> :key #'car)
                    0 (min limit (length matches))))))

(defun memory-suggests-direction (symbol)
  "Use memory to suggest trading direction"
  (let* ((similar (recall-similar-experiences symbol 20))
         (buy-wins 0) (buy-losses 0)
         (sell-wins 0) (sell-losses 0))
    ;; Analyze outcomes of similar situations
    (dolist (episode similar)
      (case (memory-episode-trade-direction episode)
        (:buy (if (eq (memory-episode-outcome episode) :win)
                  (incf buy-wins)
                  (incf buy-losses)))
        (:sell (if (eq (memory-episode-outcome episode) :win)
                   (incf sell-wins)
                   (incf sell-losses)))))
    
    (let* ((buy-total (+ buy-wins buy-losses))
           (sell-total (+ sell-wins sell-losses))
           (buy-rate (if (> buy-total 0) (/ buy-wins buy-total) 0.5))
           (sell-rate (if (> sell-total 0) (/ sell-wins sell-total) 0.5)))
      
      (when (> (+ buy-total sell-total) 5)
        (format t "[L] 🧠 MEMORY: ~d similar patterns | BUY:~,0f% SELL:~,0f%~%"
                (length similar) (* 100 buy-rate) (* 100 sell-rate)))
      
      (cond
        ((and (> buy-rate 0.6) (> buy-total 3)) :buy)
        ((and (> sell-rate 0.6) (> sell-total 3)) :sell)
        (t nil)))))

(defun get-memory-confidence (symbol direction)
  "Get confidence level based on memory of similar situations"
  (let* ((similar (recall-similar-experiences symbol 20))
         (matching 0)
         (winning 0))
    (dolist (episode similar)
      (when (eq (memory-episode-trade-direction episode) direction)
        (incf matching)
        (when (eq (memory-episode-outcome episode) :win)
          (incf winning))))
    (if (> matching 0)
        (/ winning matching)
        0.5)))

(defun semantic-pattern-win-rate (key)
  "Get win rate for a semantic pattern"
  (let ((pattern (find key *semantic-memory* :key #'semantic-pattern-key :test #'string=)))
    (if (and pattern (> (semantic-pattern-occurrences pattern) 0))
        (/ (semantic-pattern-win-count pattern) (semantic-pattern-occurrences pattern))
        0.5)))

;;; ==========================================
;;; ECOSYSTEM DYNAMICS (生態系ダイナミクス)
;;; ==========================================
;;; Features:
;;; - Strategy diversity maintenance
;;; - Niche management (category balance)
;;; - Natural selection pressure
;;; - Population health monitoring
;;; - Symbiotic relationships

(defparameter *min-diversity-score* 0.3)    ; Minimum acceptable diversity
(defparameter *max-species-per-niche* 10)   ; Max strategies per category
(defparameter *extinction-threshold* -0.5)  ; Sharpe below this = extinction risk
(defparameter *reproduction-threshold* 0.5) ; Sharpe above this = reproduction chance

(defstruct ecosystem-state
  total-population
  diversity-score
  niche-balance
  health-score
  dominant-species
  endangered-species
  timestamp)

(defun calculate-diversity-score ()
  "Calculate Shannon diversity index of strategy population"
  (let* ((categories '(:trend :reversion :breakout :scalp))
         (counts (mapcar (lambda (cat) 
                          (length (gethash cat *category-pools*)))
                        categories))
         (total (reduce #'+ counts)))
    (if (> total 0)
        (let ((proportions (mapcar (lambda (c) (if (> c 0) (/ c total) 0)) counts)))
          ;; Shannon diversity: -sum(p * ln(p))
          (- (reduce #'+ (mapcar (lambda (p) 
                                  (if (> p 0) (* p (log p)) 0))
                                proportions))))
        0)))

(defun calculate-niche-balance ()
  "Calculate how balanced the niches (categories) are"
  (let* ((categories '(:trend :reversion :breakout :scalp))
         (counts (mapcar (lambda (cat) 
                          (length (gethash cat *category-pools*)))
                        categories))
         (total (max 1 (reduce #'+ counts)))
         (ideal (/ total 4.0))
         (deviations (mapcar (lambda (c) (abs (- c ideal))) counts))
         (avg-deviation (/ (reduce #'+ deviations) 4)))
    ;; Return 0-1 where 1 is perfect balance
    (max 0 (- 1 (/ avg-deviation ideal)))))

(defun identify-endangered-species ()
  "Find strategies at risk of extinction"
  (let ((endangered nil))
    (dolist (strat *evolved-strategies*)
      (when (and (strategy-sharpe strat)
                 (< (strategy-sharpe strat) *extinction-threshold*))
        (push (strategy-name strat) endangered)))
    endangered))

(defun identify-dominant-species ()
  "Find most successful strategies"
  (let ((dominant nil))
    (dolist (strat *evolved-strategies*)
      (when (and (strategy-sharpe strat)
                 (> (strategy-sharpe strat) *reproduction-threshold*))
        (push (cons (strategy-name strat) (strategy-sharpe strat)) dominant)))
    (sort dominant #'> :key #'cdr)))

(defun calculate-ecosystem-health ()
  "Calculate overall ecosystem health score"
  (let* ((diversity (calculate-diversity-score))
         (balance (calculate-niche-balance))
         (endangered (length (identify-endangered-species)))
         (dominant (length (identify-dominant-species)))
         (total-pop (length *evolved-strategies*)))
    ;; Health = diversity + balance - endangered ratio + dominant bonus
    (let ((health (+ (* diversity 0.3)
                     (* balance 0.3)
                     (if (> total-pop 0) (* 0.2 (- 1 (/ endangered total-pop))) 0.2)
                     (if (> total-pop 0) (* 0.2 (min 1 (/ dominant total-pop 0.5))) 0))))
      (max 0 (min 1 health)))))

;; Alias for brain.lisp compatibility
(defun get-population-health ()
  "Get overall ecosystem health score (alias)"
  (calculate-ecosystem-health))

(defun get-ecosystem-state ()
  "Capture current ecosystem state"
  (make-ecosystem-state
   :total-population (length *evolved-strategies*)
   :diversity-score (calculate-diversity-score)
   :niche-balance (calculate-niche-balance)
   :health-score (calculate-ecosystem-health)
   :dominant-species (mapcar #'car (identify-dominant-species))
   :endangered-species (identify-endangered-species)
   :timestamp (get-universal-time)))

(defun ecosystem-needs-diversity-p ()
  "Check if ecosystem needs more diversity"
  (< (calculate-diversity-score) *min-diversity-score*))

(defun get-underpopulated-niche ()
  "Find the category with fewest strategies"
  (let* ((categories '(:trend :reversion :breakout :scalp))
         (counts (mapcar (lambda (cat) 
                          (cons cat (length (gethash cat *category-pools*))))
                        categories))
         (sorted (sort counts #'< :key #'cdr)))
    (car (first sorted))))

(defun apply-natural-selection ()
  "Remove poor performers and allow good ones to reproduce"
  (let ((endangered (identify-endangered-species))
        (removed 0)
        (reproduced 0))
    
    ;; Remove endangered species (poor performers) WITH FUNERAL RITES
    (when endangered
      (dolist (name endangered)
        (let ((strat (find name *evolved-strategies* :key #'strategy-name :test #'string=)))
          (when (and strat (> (random 1.0) 0.3))  ; 70% chance of extinction
            (let ((final-pnl (or (strategy-sharpe strat) 0))
                  (lessons (format nil "Strategy ~a failed in ~a regime" 
                                   name (symbol-name (or *current-regime* :unknown)))))
              ;; Hold funeral ceremony
              (hold-funeral name final-pnl lessons)))))
      (setf *evolved-strategies* 
            (remove-if (lambda (s) 
                        (and (member (strategy-name s) endangered :test #'string=)
                             (> (random 1.0) 0.3)))
                      *evolved-strategies*))
      (setf removed (length endangered)))
    
    ;; Allow dominant species to reproduce (mutate)
    (let ((dominant (identify-dominant-species)))
      (when (and dominant (< (length *evolved-strategies*) 50))
        (dolist (d (subseq dominant 0 (min 2 (length dominant))))
          (let ((parent (find (car d) *evolved-strategies* 
                             :key #'strategy-name :test #'string=)))
            (when parent
              ;; Mutation with small changes
              (let ((child (mutate-strategy parent 0.2)))
                (push child *evolved-strategies*)
                (incf reproduced)))))))
    
    ;; Log ecosystem changes
    (when (or (> removed 0) (> reproduced 0))
      (format t "[L] 🌿 ECOSYSTEM: ~d extinct, ~d born | Health: ~,0f%~%"
              removed reproduced (* 100 (calculate-ecosystem-health))))))

(defun maintain-ecosystem-balance ()
  "Periodic ecosystem maintenance"
  ;; Apply natural selection pressure
  (apply-natural-selection)
  
  ;; Check diversity and suggest focus for new strategies
  (when (ecosystem-needs-diversity-p)
    (let ((weak-niche (get-underpopulated-niche)))
      (format t "[L] 🌱 Ecosystem needs ~a strategies for diversity~%" weak-niche)))
  
  ;; Report ecosystem state every N calls
  (let ((state (get-ecosystem-state)))
    (format t "[L] 🏞️ Population: ~d | Diversity: ~,2f | Balance: ~,0f% | Health: ~,0f%~%"
            (ecosystem-state-total-population state)
            (ecosystem-state-diversity-score state)
            (* 100 (ecosystem-state-niche-balance state))
            (* 100 (ecosystem-state-health-score state)))))

(defun get-ecosystem-recommendation ()
  "Get recommendation for new strategy generation"
  (let ((weak-niche (get-underpopulated-niche))
        (health (calculate-ecosystem-health)))
    (cond
      ((< health 0.3)
       (list :action :diversify
             :focus weak-niche
             :message "Ecosystem unhealthy - need diverse strategies"))
      ((ecosystem-needs-diversity-p)
       (list :action :specialize
             :focus weak-niche
             :message (format nil "Focus on ~a category" weak-niche)))
      (t
       (list :action :evolve
             :focus nil
             :message "Ecosystem healthy - continue evolution")))))

(defparameter *current-regime* :unknown)
(defparameter *volatility-regime* :normal)  ; :low, :normal, :high

(defun calculate-volatility (closes n)
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
           (vol (or (calculate-volatility closes 20) 0.001)))
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
           (vol (or (calculate-volatility closes 20) 0.001))
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

(defun assemble-team ()
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

(defun evaluate-strategy-signal (strat history)
  (when (and history (> (length history) 100))
    (let* ((indicators (strategy-indicators strat))
           (entry-logic (strategy-entry strat))
           (rest-hist (rest history))
           (bindings 
            (loop for ind in indicators
                  append (let ((type (car ind)) (p (cdr ind)))
                           (case type
                             (sma `((,(intern (format nil "SMA-~d" (car p))) ,(ind-sma (car p) history))
                                    (,(intern (format nil "SMA-~d-PREV" (car p))) ,(ind-sma (car p) rest-hist))))
                             (ema `((,(intern (format nil "EMA-~d" (car p))) ,(ind-ema (car p) history))
                                    (,(intern (format nil "EMA-~d-PREV" (car p))) ,(ind-ema (car p) rest-hist))))
                             (rsi `((,(intern (format nil "RSI-~d" (car p))) ,(ind-rsi (car p) history))
                                    (,(intern (format nil "RSI-~d-PREV" (car p))) ,(ind-rsi (car p) rest-hist))))
                             (cci `((,(intern (format nil "CCI-~d" (car p))) ,(ind-cci (car p) history))
                                    (,(intern (format nil "CCI-~d-PREV" (car p))) ,(ind-cci (car p) rest-hist))))
                             (atr `((,(intern (format nil "ATR-~d" (car p))) ,(ind-atr (car p) history))))
                             (macd (multiple-value-bind (m s) (ind-macd (first p) (second p) (third p) history)
                                     (multiple-value-bind (pm ps) (ind-macd (first p) (second p) (third p) rest-hist)
                                       `((macd-line ,m) (signal-line ,s) (macd-line-prev ,pm) (signal-line-prev ,ps)))))
                             (bb (multiple-value-bind (m u l) (ind-bb (first p) (second p) history)
                                   (multiple-value-bind (pm pu pl) (ind-bb (first p) (second p) rest-hist)
                                     (let ((dev (second p)))
                                       ;; Both unique and generic names - generic will use last BB's values
                                       `((,(intern (format nil "BB-MIDDLE-~d" dev)) ,m)
                                         (,(intern (format nil "BB-UPPER-~d" dev)) ,u)
                                         (,(intern (format nil "BB-LOWER-~d" dev)) ,l)
                                         ;; V5.1: Add PREV values for cross detection
                                         (,(intern (format nil "BB-MIDDLE-~d-PREV" dev)) ,pm)
                                         (,(intern (format nil "BB-UPPER-~d-PREV" dev)) ,pu)
                                         (,(intern (format nil "BB-LOWER-~d-PREV" dev)) ,pl)
                                         ;; Generic aliases for strategies using simple bb-upper, etc.
                                         (bb-middle ,m) (bb-upper ,u) (bb-lower ,l)
                                         (bb-middle-prev ,pm) (bb-upper-prev ,pu) (bb-lower-prev ,pl))))))
                             (stoch (let ((k (ind-stoch (first p) (second p) history))
                                          (pk (ind-stoch (first p) (second p) rest-hist)))
                                      `((stoch-k ,k) (stoch-k-prev ,pk) (stoch-d 50) (stoch-d-prev 50)))))))))
      (push `(close ,(candle-close (first history))) bindings)
      (push `(close-prev ,(candle-close (second history))) bindings)
      (push `(high ,(candle-high (first history))) bindings)
      (push `(high-prev ,(candle-high (second history))) bindings)  ; V5.0
      (push `(low ,(candle-low (first history))) bindings)
      (push `(low-prev ,(candle-low (second history))) bindings)  ; V5.0
      
      ;; Remove duplicate bindings (keep first occurrence) for multi-BB strategies
      (setf bindings (remove-duplicates bindings :key #'car :from-end t))
      
      ;; V4.0 FIX: Transform cross-above/cross-below to include PREV arguments
      ;; (cross-above sma-50 sma-200) → (cross-above sma-50 sma-200 sma-50-prev sma-200-prev)
      (labels ((add-prev-suffix (sym)
                 (if (symbolp sym)
                     (intern (format nil "~a-PREV" (symbol-name sym)))
                     sym))  ; V5.0: Return non-symbols unchanged
               (transform-cross-calls (expr)
                 (cond
                   ((atom expr) expr)
                   ((and (listp expr) 
                         (member (car expr) '(cross-above cross-below))
                         (= (length expr) 3))  ; Only 2-arg calls
                    (let ((fn (first expr))
                          (a (second expr))
                          (b (third expr)))
                      ;; V5.0: Only transform if both are symbols
                      (if (and (symbolp a) (symbolp b))
                          (list fn a b (add-prev-suffix a) (add-prev-suffix b))
                          expr)))  ; Return unchanged if numbers involved
                   (t (mapcar #'transform-cross-calls expr)))))
        (let ((transformed-logic (transform-cross-calls entry-logic)))
          (handler-case
              (let ((entry-result (eval `(let ,bindings ,transformed-logic))))
                (cond
                  (entry-result :buy)
                  ;; 逆張り戦略の場合、exit条件をSELLシグナルとして使用
                  ((and (strategy-exit strat)
                        (eval `(let ,bindings ,(transform-cross-calls (strategy-exit strat))))) :sell)
                  (t :hold)))
            (error (e) (format t "[L] Eval error ~a: ~a~%" (strategy-name strat) e) :hold)))))))
(defparameter *category-trades* 0)  ; Track category trade count for warmup

(defun execute-category-trade (category direction symbol bid ask)
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
           ;; Final lot: min of all adjustments, then apply rank multiplier
           (lot (max 0.01 (* rank-mult vol-mult 
                             (min (correlation-adjusted-lot symbol vol-scaled-lot) rp-lot))))
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
        ;; Trade conditions (FIXED - more permissive after warmup):
        ;; 1. Warmup period: trade freely to gather data
        ;; 2. High NN confidence (>35%) AND NN agrees 
        ;; 3. Strong swarm consensus (>70%)
        ;; 4. V2.0: Strong tribe consensus (>60%) AND tribe agrees with direction
        (let ((tribe-agrees (or warmup-p  ; Always true in warmup
                                (and (numberp *tribe-consensus*)
                                     (> (float *tribe-consensus*) 0.6)
                                     (or (and (eq direction :buy) (eq *tribe-direction* :buy))
                                         (and (eq direction :sell) (eq *tribe-direction* :sell)))))))
        (when (or warmup-p
                  (and (> conf 0.35)
                       (or (and (eq direction :buy) (string= pred "BUY"))
                           (and (eq direction :sell) (string= pred "SELL"))))
                  (> swarm-consensus 0.70)
                  tribe-agrees)  ; V2.0: Tribe consensus override
          ;; V2.0: Apply hedge logic for Breakers (aggressive trades get counter-hedge)
          (when (eq category :breakout)
            (apply-hedge-logic category direction symbol bid ask))
          ;; V3.0: PREDICTION CHECK - use previously unused function
          (let* ((prediction (handler-case (predict-trade-outcome symbol direction)
                               (error (e) (progn (format t "[L] Prediction error: ~a~%" e) nil))))
                 (should-trade (or warmup-p 
                                   (null prediction)
                                   (should-take-trade-p prediction))))
            ;; V3.0: Explain trade decision (previously unused!)
            (when prediction
              (handler-case
                  (let ((factors (trade-prediction-factors prediction))
                        (action (if should-trade :execute :skip)))
                    (explain-trade-decision symbol direction action factors))
                (error (e) (format t "[L] Explain error: ~a~%" e))))
            (when should-trade
              ;; V3.0: Multiple warriors per clan (no pos check - allow up to 4 per clan)
              (cond
            ((eq direction :buy)
             (let ((sl (- bid sl-pips)) (tp (+ bid tp-pips)))
               (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "BUY") ("symbol" symbol) ("volume" lot) ("sl" sl) ("tp" tp))))
               ;; V5.0: Record position for exit tracking
               (setf (gethash category *category-positions*) :long)
               (setf (gethash category *category-entries*) bid)
               (update-symbol-exposure symbol lot :open)
               (incf *category-trades*)
               (format t "[L] ~a ~a BUY ~,2f lot (~a)~a~%" 
                       (get-clan-display category) (clan-emoji (get-clan category)) lot symbol 
                       (if warmup-p " [WARMUP]" ""))))
               ;; NOTE: Discord narrative already sent above - no duplicate notify here
            ((eq direction :sell)
             (let ((sl (+ ask sl-pips)) (tp (- ask tp-pips)))
               (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "SELL") ("symbol" symbol) ("volume" lot) ("sl" sl) ("tp" tp))))
               ;; V5.0: Record position for exit tracking
               (setf (gethash category *category-positions*) :short)
               (setf (gethash category *category-entries*) ask)
               (update-symbol-exposure symbol lot :open)
               (incf *category-trades*)
               (format t "[L] ~a ~a SELL ~,2f lot (~a)~a~%" 
                       (get-clan-display category) (clan-emoji (get-clan category)) lot symbol 
                       (if warmup-p " [WARMUP]" "")))))))))))))

;; Track entry prices for each category
(defparameter *category-entries* (make-hash-table :test 'eq))

(defun close-category-positions (symbol bid ask)
  "Close category positions at SL/TP or on reverse signal"
  (maphash 
   (lambda (category pos)
     (when pos
       (let* ((entry (gethash category *category-entries*))
              (sl-pips 0.15) (tp-pips 0.40)
              (pnl 0) (closed nil))
         (when (and entry (numberp bid) (numberp ask))
           (cond
             ;; Long position: close on SL/TP or SELL signal
             ((eq pos :long)
              (let ((sl (- entry sl-pips)) (tp (+ entry tp-pips)))
                (when (or (<= bid sl) (>= bid tp))
                  (setf pnl (- bid entry) closed t))))
             ;; Short position: close on SL/TP or BUY signal  
             ((eq pos :short)
              (let ((sl (+ entry sl-pips)) (tp (- entry tp-pips)))
                (when (or (>= ask sl) (<= ask tp))
                  (setf pnl (- entry ask) closed t)))))
           (when closed
             (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" symbol))))
             (setf (gethash category *category-positions*) nil)
             (setf (gethash category *category-entries*) nil)
             (update-symbol-exposure symbol 0.01 :close)  ; Release exposure
             ;; Update daily PnL and train NN
             (incf *daily-pnl* pnl)
             ;; ===== DANGER AVOIDANCE: Record result for cooldown tracking =====
             (record-trade-result (if (> pnl 0) :win :loss))
              (train-neural (if (> pnl 0) (if (eq pos :long) 0 1) (if (eq pos :long) 1 0)))
              ;; Record outcome for learning (both wins and losses)
              (record-trade-outcome symbol (if (eq pos :long) :buy :sell) category "active" pnl)
              ;; V2.0: Record for hierarchy (promotions, ceremonies)
              (let ((lead-strat (first (gethash category *active-team*))))
                (when lead-strat
                  (record-strategy-trade (strategy-name lead-strat) 
                                         (if (> pnl 0) :win :loss) pnl)
                  ;; V2.1: Record failures for Failure Learning v2.0
                  (when (and (< pnl 0) (fboundp 'record-failure))
                    (record-failure symbol 
                                    (if (eq pos :long) :buy :sell) 
                                    category 
                                    (strategy-name lead-strat)
                                    pnl))))
             (format t "[L] 🐟 ~a CLOSED ~a pnl=~,2f~%" category (if (> pnl 0) "✅" "❌") pnl)
             ;; V2.0: Update clan treasury with trade profits/losses
             (contribute-to-treasury category pnl :trade (format nil "Trade ~a" (if (> pnl 0) "profit" "loss")))
             (notify-discord (format nil "~a ~a closed ~,2f" (if (> pnl 0) "✅" "❌") category pnl) 
                            :color (if (> pnl 0) 3066993 15158332)))))))
   *category-positions*))

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
      (defvar *last-swarm-consensus* 0)
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
        (let* ((min-consensus-to-trade 0.25))
        ;; V3.0: Always use 61-STRATEGY SIGNALS (no more tribe-signals check)
        (when (or t  ; Always proceed - strategies have their own conditions
                 (> consensus min-consensus-to-trade))
            ;; V3.0: Use 61-STRATEGY SIGNALS
            (progn
              (format t "[L] 🎯 61-STRATEGY SIGNAL SCAN~%")
              (let ((strat-signals (collect-strategy-signals symbol *candle-history*)))
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
                                        (notify-discord narrative :color (if (eq direction :buy) 3066993 15158332))
                                      (error (e) (format t "[L] Discord error: ~a~%" e)))
                                    ;; Record trade time for this strategy
                                    (record-clan-trade-time strat-key)
                                    ;; Execute the trade
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

(defun init-school ()
  (build-category-pools)
  (clrhash *category-positions*)
  (format t "[SCHOOL] Swimmy School ready~%"))

;;; ══════════════════════════════════════════════════════════════════
;;;  V3.0: 4氏族シグナル関数を削除
;;;  61戦略が唯一のエントリーロジック源 (collect-strategy-signals)
;;;  各戦略はinfer-strategy-categoryで氏族(:trend,:reversion,:breakout,:scalp)に配属
;;;  Kalman/HMM等の研究論文実装は品質フィルターとして使用
;;; ══════════════════════════════════════════════════════════════════

;; Stub functions for backward compatibility (actual logic uses 61 strategies now)
(defun collect-all-tribe-signals (symbol history)
  "V3.0: Stub - returns nil, 61-strategy signals are used instead"
  (declare (ignore symbol history))
  nil)

(defun aggregate-tribe-signals (signals)
  "V3.0: Stub - returns hold, 61-strategy signals are used instead"
  (declare (ignore signals))
  (list :direction :hold :consensus 0.0 :signals nil))

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

(init-school)
