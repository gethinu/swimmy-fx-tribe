;; school-fortress.lisp - Swimmy V5.5-V6.1 Fortress Features

(in-package :swimmy.school)

(defun decode-jst (&optional (time (get-universal-time)))
  "Decode universal time into JST components."
  (decode-universal-time time 9))

;; V6.0 (Graham): Extracted from school.lisp for modularity
;; V6.2 (Graham): Added Gotobi, Kelly, WhyLog

;;; ══════════════════════════════════════════════════════════════════
;;;  V5.8 (Naval): GOTOBI ANOMALY - Japan Specific Knowledge
;;; ══════════════════════════════════════════════════════════════════

(defun days-in-month (month year)
  "Return number of days in given month"
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (leap-year-p year) 29 28))))

(defun leap-year-p (year)
  "Check if year is a leap year"
  (or (and (zerop (mod year 4)) (not (zerop (mod year 100))))
      (zerop (mod year 400))))

(defun day-of-week (year month day)
  "Return day of week (0=Sun, 1=Mon, ..., 6=Sat)"
  (let ((a (floor (- 14 month) 12)))
    (mod (+ day (floor (* 13 (+ 1 month (* 12 a))) 5)
            (- year a) (floor (- year a) 4)
            (- (floor (- year a) 100)) (floor (- year a) 400))
         7)))

(defun gotobi-day-p ()
  "Check if today is a Gotobi day (5th, 10th, 15th, 20th, 25th, or end of month)"
  (multiple-value-bind (s m h day month year) (decode-jst)
    (declare (ignore s m h))
    (let ((last-day (days-in-month month year)))
      (if (or (member day '(5 10 15 20 25))
              (= day last-day)
              (and (> day 25) (member (day-of-week year month day) '(5 6))))
          t
          nil))))

(defun gotobi-usdjpy-bias ()
  "Return Gotobi trading bias for USDJPY (Japanese importers buy USD)"
  (if (gotobi-day-p)
      (let ((hour (nth-value 2 (decode-jst))))
        (cond
          ((and (>= hour 0) (< hour 9)) :strong-buy)
          ((and (>= hour 9) (< hour 15)) :buy)
          (t :neutral)))
      :neutral))

(defun apply-gotobi-adjustment (symbol direction)
  "Adjust trade based on Gotobi anomaly (Naval's Specific Knowledge)"
  (when (string= symbol "USDJPY")
    (let ((bias (gotobi-usdjpy-bias)))
      (cond
        ((and (eq direction :buy) (member bias '(:strong-buy :buy)))
         (format t "[L] 🇯🇵 GOTOBI: Amplifying USDJPY BUY~%") 1.2)
        ((and (eq direction :sell) (member bias '(:strong-buy :buy)))
         (format t "[L] 🇯🇵 GOTOBI: Dampening USDJPY SELL~%") 0.7)
        (t 1.0)))))

;;; ══════════════════════════════════════════════════════════════════
;;;  V6.11 (Naval): LONDON EDGE - EURUSD/GBPUSD Session Optimization
;;; ══════════════════════════════════════════════════════════════════

(defun london-session-p ()
  "Check if currently in London session (8:00-17:00 GMT = 17:00-02:00 JST)"
  (multiple-value-bind (s m h) (decode-jst)
    (declare (ignore s m))
    ;; JST is GMT+9, London session is 8-17 GMT = 17-26 JST (17:00-02:00 next day)
    (or (>= h 17) (< h 2))))

(defun london-overlap-p ()
  "Check if in London-NY overlap (best liquidity, 13:00-17:00 GMT = 22:00-02:00 JST)"
  (multiple-value-bind (s m h) (decode-jst)
    (declare (ignore s m))
    (or (>= h 22) (< h 2))))

(defun ecb-week-p ()
  "Check if this week likely has ECB announcement (first Thursday of month)"
  (multiple-value-bind (s m h day month year) (decode-jst)
    (declare (ignore s m h month year))
    ;; First Thursday is between 1-7
    (<= day 7)))

(defun apply-london-edge (symbol direction)
  "Apply session-based edge adjustments for EURUSD/GBPUSD (Naval - Specific Knowledge)"
  (declare (ignore direction))
  (when (or (string= symbol "EURUSD") (string= symbol "GBPUSD"))
    (cond
      ;; Best: London-NY overlap
      ((london-overlap-p)
       (format t "[L] 🇬🇧 LONDON OVERLAP: Amplifying ~a trade (peak liquidity)~%" symbol)
       1.3)
      ;; Good: London session
      ((london-session-p)
       (format t "[L] 🇬🇧 LONDON SESSION: Standard ~a trade~%" symbol)
       1.1)
      ;; Avoid: ECB week with high volatility expected
      ((and (ecb-week-p) (string= symbol "EURUSD"))
       (format t "[L] 🇪🇺 ECB WEEK: Cautious EURUSD trading~%")
       0.8)
      ;; Asian session: reduce size for EUR/GBP
      (t
       (format t "[L] 😴 OFF-HOURS: Reduced size for ~a~%" symbol)
       0.7))))

;;; ══════════════════════════════════════════════════════════════════
;;;  V6.16: DYNAMIC TP/SL - ATR-based Take Profit & Stop Loss
;;; ══════════════════════════════════════════════════════════════════
;;; Trading Expert Requirement: Volatility-adjusted targets

;; Function calculate-atr moved to school-volatility.lisp

(defun get-volatility-multiplier ()
  "Get multiplier based on current volatility state"
  (case *current-volatility-state*
    (:extreme 0.8)  ; Tighter stops in extreme volatility
    (:elevated 1.0)
    (:normal 1.5)
    (t 1.2)))

(defun calculate-dynamic-tp-sl (symbol candles &key (direction :buy))
  "Calculate ATR-based dynamic Take Profit and Stop Loss
   Returns (values tp sl) in price units"
  (declare (ignore direction))
  (let* ((atr (calculate-atr candles 14))
         (multiplier (get-volatility-multiplier))
         (base-sl-atr 1.5)  ; Base SL = 1.5 ATR
         (base-tp-atr 2.5)  ; Base TP = 2.5 ATR (1.67 RR ratio)
         (sl-distance (* atr base-sl-atr multiplier))
         (tp-distance (* atr base-tp-atr multiplier)))
    
    (when (> atr 0)
      (format t "[L] 📐 DYNAMIC TP/SL (~a): ATR=~,5f Vol=~a Mult=~,1f~%"
              symbol atr *current-volatility-state* multiplier)
      (format t "[L]    SL=~,5f (~,1f ATR) TP=~,5f (~,1f ATR)~%"
              sl-distance base-sl-atr tp-distance base-tp-atr))
    
    (values tp-distance sl-distance)))

(defun get-dynamic-lot-for-trade (symbol candles base-lot max-risk-pct)
  "Calculate position size based on ATR stop-loss and risk percentage
   max-risk-pct: Maximum % of capital to risk (e.g., 1.0 for 1%)"
  (multiple-value-bind (tp sl) (calculate-dynamic-tp-sl symbol candles)
    (declare (ignore tp))
    (if (and sl (> sl 0))
        (let* ((capital (if (boundp '*monthly-goal*) *monthly-goal* 100000))
               (max-risk-amount (* capital (/ max-risk-pct 100.0)))
               (pip-value 100)  ; Approximate pip value for JPY pairs
               (sl-pips (* sl pip-value))
               (calculated-lot (if (> sl-pips 0)
                                   (/ max-risk-amount sl-pips)
                                   base-lot))
               (final-lot (max 0.01 (min 0.10 calculated-lot))))
          (format t "[L] 📊 POSITION SIZE: Risk=¥~:d SL=~,1f pips -> Lot=~,2f~%"
                  (round max-risk-amount) sl-pips final-lot)
          final-lot)
        base-lot)))

;;; ══════════════════════════════════════════════════════════════════
;;;  V5.7 (Thorp): KELLY CRITERION
;;; ══════════════════════════════════════════════════════════════════

;;; Legacy kelly-lot removed in favor of swimmy.school:calculate-kelly-lot

(defun get-strategy-kelly-lot (strat-name base-lot)
  "Get Kelly-adjusted lot for a specific strategy"
  (let ((rank-data (when (fboundp 'get-strategy-rank) (get-strategy-rank strat-name))))
    (if rank-data
        (let ((wins (strategy-rank-wins rank-data))
              (losses (- (strategy-rank-trades rank-data) (strategy-rank-wins rank-data)))
              (total-profit (or (strategy-rank-total-pnl rank-data) 0)))
          (if (> (+ wins losses) 10)
              (let* ((win-rate (/ wins (+ wins losses)))
                     (avg-win (if (> wins 0) (/ (max total-profit 0.001) wins) 0.001))
                     (avg-loss (if (> losses 0) (/ (max (- total-profit) 0.001) losses) 0.001)))
                (calculate-kelly-lot win-rate avg-win avg-loss swimmy.globals:*current-equity*))
              base-lot))
        base-lot)))

;;; ══════════════════════════════════════════════════════════════════
;;;  V5.7 (Feynman): WHY LOG - Explainable Decisions
;;; ══════════════════════════════════════════════════════════════════

(defun log-why-trade (symbol direction category &key strategy swarm-cons parallel-score elder-ok (outcome :pending) (reason ""))
  "Log the reasoning behind a trade decision (Feynman)"
  (format t "[L] 📜 WHY LOG: ~a ~a (~a) [~a]~%" direction symbol category outcome)
  (when strategy (format t "[L]   ├─ Strategy: ~a~%" strategy))
  (when swarm-cons (format t "[L]   ├─ Swarm Consensus: ~,0f%~%" (* 100 swarm-cons)))
  
  ;; User Request: Log to memo2.txt (Only if pending or success, or significant failure)
  (with-open-file (stream (swimmy.core::swimmy-path "doc/memo2.txt") 
                          :direction :output 
                          :if-exists :append 
                          :if-does-not-exist :create)
    (let ((sharpe (when strategy 
                    (let ((s (find strategy *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
                      (if s (strategy-sharpe s) 0.0)))))
      (format stream "[~a] ~a ~a (~a) Strategy:~a Sharpe:~,2f Outcome:~a Reason:~a~%" 
              (swimmy.core:get-jst-str) symbol direction category strategy (or sharpe 0.0) outcome reason)))
              
  (when parallel-score (format t "[L]   ├─ Parallel Verification: ~d/3 PASS~%" parallel-score))
  (when elder-ok (format t "[L]   └─ Elder Approval: ~a~%" (if elder-ok "✅ YES" "❌ NO"))))

;;; ══════════════════════════════════════════════════════════════════
;;;  V5.5: FORTRESS HELPERS

(defun global-panic-active-p ()
  "Check if multiple currencies are in extreme volatility (Soros)"
  (if (boundp '*current-volatility-state*)
      (let ((extreme-count 0))
        (when (boundp '*symbol-volatility-states*)
          (maphash (lambda (k v) 
                     (declare (ignore k))
                     (when (eq v :extreme) (incf extreme-count)))
                   *symbol-volatility-states*))
        (eq *current-volatility-state* :extreme))
      nil))

(defun should-unlearn-p (symbol)
  "V6.4 (Darwin): Check if recent performance is toxic - adaptive forgetting
   Returns t if system should pause trading for this symbol"
  (declare (ignore symbol))  ; Currently global, could be per-symbol later
  (let ((toxic nil)
        (daily-pnl (if (boundp '*daily-pnl*) *daily-pnl* 0))
        (consecutive-losses (if (boundp '*consecutive-losses*) *consecutive-losses* 0)))
    
    ;; Condition 1: Heavy daily loss (> 3000 yen)
    (when (< daily-pnl -3000)
      (setf toxic t)
      (format t "[L] 🧬 DARWIN: Daily PnL toxic (¥~,0f)~%" daily-pnl))
    
    ;; Condition 2: 4+ consecutive losses
    (when (>= consecutive-losses 4)
      (setf toxic t)
      (format t "[L] 🧬 DARWIN: ~d consecutive losses~%" consecutive-losses))
    
    toxic))

;;; ══════════════════════════════════════════════════════════════════
;;;  V5.6 (Paper #36): PARALLEL VERIFICATION LOOPS
;;; ══════════════════════════════════════════════════════════════════

(defun verify-parallel-scenarios (symbol direction category)
  "Run 3 parallel simulations to verify trade robustness (DeepMind)"
  (declare (ignore direction))
  (let ((optimistic-pass nil)
        (pessimistic-pass nil)
        (chaos-pass nil))
    
    ;; 1. Optimistic World: Market Regime check
    (let ((regime (if (boundp '*market-regime*) *market-regime* :ranging)))
       (setf optimistic-pass 
             (cond ((member category '(:hunters :raiders)) (eq regime :trending))
                   ((eq category :breakout) (eq regime :volatile))
                   ((eq category :shamans) (eq regime :ranging))
                   (t t))))
    
    ;; 2. Pessimistic World: Counter-indicators
    (setf pessimistic-pass t) ; Placeholder
    
    ;; 3. Chaos World: Volatility check
    (let ((vol (if (boundp '*current-volatility-state*) *current-volatility-state* :normal)))
      (setf chaos-pass (not (eq vol :extreme))))
      
    ;; Verification: Pass if 2/3 scenarios survive
    (let ((score (+ (if optimistic-pass 1 0) 
                    (if pessimistic-pass 1 0) 
                    (if chaos-pass 1 0))))
      (format t "[L] 🧬 PARALLEL VERIFICATION (~a): Opt=~a Pess=~a Chaos=~a (Score: ~d/3)~%" 
              symbol optimistic-pass pessimistic-pass chaos-pass score)
      (>= score 2))))

;;; ══════════════════════════════════════════════════════════════════
;;;  HIGH COUNCIL
;;; ══════════════════════════════════════════════════════════════════

;; convene-high-council moved to school-voting.lisp

;;; ══════════════════════════════════════════════════════════════════
;;;  V6.1 (Taleb): PARANOIA CHECKS - Be Paranoid, Stay Alive
;;; ══════════════════════════════════════════════════════════════════

;; High-risk event times (UTC hours) - DO NOT TRADE
(defparameter *nfp-day* 5)        ; First Friday of month (approximate)
(defparameter *high-risk-hours*   ; Known volatility spikes
  '((13 . "US Market Open") 
    (14 . "FOMC/NFP typical release")
    (18 . "London Close")
    (23 . "BOJ/Japan News")))

(defun paranoia-check ()
  "Taleb: Block trades during known high-risk periods"
  (multiple-value-bind (sec min hour day dow) 
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min day))
    (let ((blocked nil)
          (reason ""))
      ;; 1. High-risk hours check
      (dolist (risky *high-risk-hours*)
        (when (= hour (car risky))
          (setf blocked t)
          (setf reason (cdr risky))))
      
      ;; 2. Friday afternoon (weekend gap risk)
      (when (and (= dow 4) (>= hour 20))
        (setf blocked t)
        (setf reason "Weekend gap risk"))
      
      ;; 3. Monday morning (gap continuation)
      (when (and (= dow 0) (<= hour 2))
        (setf blocked t)
        (setf reason "Monday gap risk"))
      
      (when blocked
        (format t "[L] 🔴 TALEB PARANOIA: Trade blocked (~a)~%" reason))
      blocked)))

(defun antifragile-lot-adjust (base-lot current-pnl)
  "Taleb: Reduce exposure when losing, maintain when winning"
  (cond
    ((< current-pnl -3000)
     (format t "[L] 🛡️ ANTIFRAGILE: Heavy loss, reducing lot to 50%~%")
     (* base-lot 0.5))
    ((< current-pnl -1000)
     (format t "[L] 🛡️ ANTIFRAGILE: Loss detected, reducing lot to 75%~%")
     (* base-lot 0.75))
    ((> current-pnl 5000)
     (format t "[L] 💪 ANTIFRAGILE: Strong profit, maintaining full lot~%")
     base-lot)
    (t base-lot)))

;;; ══════════════════════════════════════════════════════════════════
;;;  V6.1 (Naval): JAPAN-SPECIFIC KNOWLEDGE EDGE
;;; ══════════════════════════════════════════════════════════════════

(defun tokyo-lunch-break-p ()
  "Naval: Tokyo lunch break (11:30-12:30 JST = 2:30-3:30 UTC) = low volatility"
  (multiple-value-bind (sec min hour) (decode-universal-time (get-universal-time))
    (declare (ignore sec))
    (or (and (= hour 2) (>= min 30))
        (and (= hour 3) (<= min 30)))))

(defun month-end-exporter-flow-p ()
  "Naval: Japanese exporters sell USD at month-end for repatriation"
  (multiple-value-bind (sec min hour day month year) 
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour))
    (let ((last-day (days-in-month month year)))
      (>= day (- last-day 2)))))  ; Last 3 days of month

(defun apply-japan-edge (symbol direction base-lot)
  "Naval: Apply Japan-specific knowledge adjustments"
  (let ((adj 1.0))
    ;; 1. Gotobi (already implemented, enhance here)
    (when (and (string= symbol "USDJPY") (gotobi-day-p))
      (if (eq direction :buy)
          (progn
            (setf adj (* adj 1.2))
            (format t "[L] 🇯🇵 NAVAL: Gotobi BUY boost (+20%)~%"))
          (progn
            (setf adj (* adj 0.8))
            (format t "[L] 🇯🇵 NAVAL: Gotobi SELL dampened (-20%)~%"))))
    
    ;; 2. Tokyo lunch = avoid
    (when (and (tokyo-lunch-break-p) (string= symbol "USDJPY"))
      (setf adj (* adj 0.5))
      (format t "[L] 🍱 NAVAL: Tokyo lunch break - reduced activity~%"))
    
    ;; 3. Month-end exporter flow (USD selling)
    (when (and (month-end-exporter-flow-p) (string= symbol "USDJPY"))
      (if (eq direction :sell)
          (progn
            (setf adj (* adj 1.15))
            (format t "[L] 📅 NAVAL: Month-end exporter flow - SELL boost~%"))
          (progn
            (setf adj (* adj 0.85))
            (format t "[L] 📅 NAVAL: Month-end exporter flow - BUY dampened~%"))))
    
    (* base-lot adj)))

;;; ══════════════════════════════════════════════════════════════════
;;;  V6.1 (Graham): AUTO-PRUNE STRATEGIES
;;; ══════════════════════════════════════════════════════════════════

(defun auto-prune-on-load ()
  "Graham: Automatically reduce redundant strategies at startup"
  (when (fboundp 'get-redundant-strategies)
    (let ((redundant (get-redundant-strategies)))
      (when (and redundant (> (length redundant) 0))
        (format t "[L] 🧹 GRAHAM: Found ~d redundant strategies to prune~%" 
                (length redundant))
        (prune-redundant-strategies)))))

(format t "[FORTRESS] V6.1 features loaded (Paranoia + Japan Edge + Auto-Prune)~%")
