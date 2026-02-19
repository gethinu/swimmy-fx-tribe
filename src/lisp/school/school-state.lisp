;; school-state.lisp - Shared State for Swimmy Trading System
;; V6.13: Extracted from school.lisp for modular architecture
;; All shared state variables are defined here and exported

(in-package :swimmy.school)
 
;;; ==========================================
;;; TRADE RECORD STRUCTURE
;;; ==========================================

(defstruct trade-record
  timestamp
  symbol
  direction           ; :buy or :sell
  category            ; :trend, :reversion, :breakout, :scalp
  strategy-name
  pair-id
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

(defstruct strategy-rank
  name
  rank
  trades
  wins
  total-pnl
  promotion-date
  last-trade)

(defvar *predicted-regime* nil)
(defvar *predicted-volatility* nil)
(defvar *hall-of-fame* nil)
(defvar *startup-mode* t)

;;; = : = = = = = = = = = = 
;;; FORWARD DECLARATIONS (Local to School or for early referencing)
;;; = = = = = = = = = = = = = 
(defvar *trade-history* (make-hash-table :test 'eq))
(defvar *category-entries* (make-hash-table :test 'eq))
(defvar *category-positions* nil)
(defvar *yesterday-pnl* 0 "PnL from the previous trading day (for reporting)")
(defvar *category-trades* 0)

;; P8: Rich Hickey - Atomic KB Operations (V48.2)
;; Moved to school-state.lisp for early initialization (ASDF loading order fix)
(defvar *kb-lock* (bt:make-lock "KB-LOCK")
  "Lock for modifying *strategy-knowledge-base* and *category-pools*.")

(defparameter *category-pools* (make-hash-table :test 'equal)
  "Strategies grouped by category (TF x Direction x Symbol).")

(defparameter *regime-pools* (make-hash-table :test 'equal)
  "Strategies grouped by inferred semantic regime class (:trend/:reversion/:breakout/:scalp).")

;; Global Analytics State (Legacy or and specifics)
(defvar *total-wins* 0 "Total winning trades count")
(defvar *total-trades-count* 0 "Total trades count")

;; Phase 6c: Graveyard (Unified Persistence)
(defparameter *graveyard* nil "List of failed strategy parameters (Code-as-Data)")

;; V44.4: Strategy Cooldown State (P12.5 - Prevent Rapid Entries)
(defvar *strategy-cooldowns* (make-hash-table :test 'equal) 
  "Hash table mapping 'strategy-symbol' to last-trade universal-time")
(defparameter *strategy-symbol-cooldown-seconds* 300 
  "Minimum seconds between trades for same strategy on same symbol (5 min)")

;;; ==========================================
;;; P0 STARTUP SAFEGUARDS (Expert Panel 2026-01-07)
;;; ==========================================
;;; Prevent mass entries on history load
(defparameter *system-state* :initializing 
  "System state: :initializing, :warmup, :trading, :halted")
(defparameter *warmup-end-time* 0 
  "Universal time when warmup period ends (60s after history load)")
(defparameter *last-entry-time* 0 
  "Universal time of last entry (for rate limiting)")
(defparameter *warmup-duration-seconds* 60 
  "How long to wait after history load before trading")
(defparameter *min-entry-interval-seconds* 1.0 
  "Minimum seconds between entries (rate limit)")

;;; ==========================================
;;; P1 FAILURE SAFETY (Dynamic Circuit Breaker)
;;; ==========================================
(defparameter *circuit-breaker-active* nil "Is circuit breaker currently tripped?")
(defparameter *breaker-cooldown-end* 0 "Time when breaker resets")
(defparameter *recent-losses* nil 
  "List of timestamps of recent losses. Used to detect failure cascades.")
(defparameter *max-loss-window-seconds* 300 "Window to check for consecutive losses (5m)")
(defparameter *consecutive-loss-threshold* 3 "Number of losses in window to trip breaker")
(defparameter *breaker-cooldown-seconds* 900 "Duration to Halt trading (15m)")

;;; ==========================================
;;; P31: MECU (Maximum Exposure Control Unit)
;;; ==========================================
(defparameter *max-gross-exposure-pct* 0.30 "Max Σ|Nominal| / Equity (30%)")
(defparameter *max-net-exposure-pct* 0.15 "Max Σ(Nominal) / Equity (15%)")
(defparameter *max-currency-exposure-pct* 0.10 "Max Single Currency Exposure / Equity (10%)")

;; Warrior System (school-danger.lisp)
;; *warrior-allocation* moved to globals.lisp

;; Ritual History (rituals.lisp)
(defparameter *win-rate-history* nil "Historical win rates")
(defparameter *max-win-rate-history* 100 "Max size of win rate history")

;; Signal integration (moved to globals.lisp)

;;; ==========================================
;;; GLOBAL MACRO STATE (Phase 19/23 - Simons)
;;; ==========================================
(defparameter *macro-drivers* 
  '("DXY" "US10Y" "JP10Y" "DE10Y" "UK10Y" 
    "VIX" "WTI" "XAU" "SPX" "NI225" "DAX" "FTSE"
    "JXY" "EXY" "GXY")
  "List of global macro drivers to track.")

(defparameter *macro-state* (make-hash-table :test 'equal)
  "Hash: Driver -> List of (Date . Close) history.")

(defparameter *last-macro-load-time* 0)

(defstruct macro-snapshot
  timestamp
  drivers  ; Hash of driver -> value
  regime   ; Determined regime
  alerts)  ; List of active alerts (e.g. Broken Arrow)

;;; ==========================================
;;; CORRELATION & EXPOSURE MANAGEMENT
;;; ==========================================
(defparameter *pair-correlations*
  '(("USDJPY" . (("EURJPY" . 0.85) ("GBPJPY" . 0.80) ("EURUSD" . -0.60) ("GBPUSD" . -0.50)))
    ("EURUSD" . (("GBPUSD" . 0.90) ("USDJPY" . -0.60) ("EURJPY" . 0.30)))
    ("GBPUSD" . (("EURUSD" . 0.90) ("USDJPY" . -0.50) ("GBPJPY" . 0.40)))))

(defun resolve-swimmy-home ()
  "Resolve Swimmy project root (env override supported)."
  (let ((env (uiop:getenv "SWIMMY_HOME")))
    (uiop:ensure-directory-pathname
     (if (and env (> (length env) 0))
         env
         (uiop:getcwd)))))

(defparameter cycle-completed nil "Legacy flag used by maintenance logs")
(defparameter *symbol-exposure* (make-hash-table :test 'equal))
(defparameter *max-symbol-exposure* 0.30)
(defparameter *max-total-exposure* 0.60)

;;; ==========================================
;;; DYNAMIC CORRELATION (Taleb Homework #3)
;;; ==========================================
;;; Calculate rolling correlations from live price data
;;; instead of relying on static historical averages

(defparameter *dynamic-correlations* (make-hash-table :test 'equal)
  "Hash: (sym1 . sym2) -> correlation value from rolling calculation")
(defparameter *correlation-window* 100
  "Number of candles for correlation calculation")
(defparameter *correlation-alert-threshold* 0.8
  "Alert when correlation exceeds this (correlation breakdown risk)")
(defparameter *last-correlation-update* 0)
(defparameter *correlation-update-interval* 300)  ;; Update every 5 minutes

(defun get-price-returns (history n)
  "Calculate price returns from candle history"
  (when (and history (> (length history) n))
    (loop for i from 0 below n
          for c1 = (nth i history)
          for c2 = (nth (1+ i) history)
          when (and c1 c2 (candle-close c1) (candle-close c2) (> (candle-close c2) 0))
          collect (/ (- (candle-close c1) (candle-close c2)) (candle-close c2)))))

(defun calculate-dynamic-correlation (sym1 sym2)
  "Calculate rolling correlation between two symbols using price history"
  (let ((history1 (gethash sym1 *candle-histories*))
        (history2 (gethash sym2 *candle-histories*)))
    (when (and history1 history2
               (> (length history1) *correlation-window*)
               (> (length history2) *correlation-window*))
      (let* ((returns1 (get-price-returns history1 *correlation-window*))
             (returns2 (get-price-returns history2 *correlation-window*))
             (n (min (length returns1) (length returns2))))
        (when (> n 10)
          (let* ((r1 (subseq returns1 0 n))
                 (r2 (subseq returns2 0 n))
                 (mean1 (/ (reduce #'+ r1) n))
                 (mean2 (/ (reduce #'+ r2) n))
                 (cov (/ (reduce #'+ (mapcar (lambda (a b) (* (- a mean1) (- b mean2))) r1 r2)) n))
                 (var1 (/ (reduce #'+ (mapcar (lambda (a) (expt (- a mean1) 2)) r1)) n))
                 (var2 (/ (reduce #'+ (mapcar (lambda (a) (expt (- a mean2) 2)) r2)) n))
                 (std1 (sqrt var1))
                 (std2 (sqrt var2)))
            (if (and (> std1 0) (> std2 0))
                (/ cov (* std1 std2))
                0.0)))))))

(defun update-all-correlations ()
  "Update all dynamic correlations between supported symbols"
  (let ((symbols (if (boundp '*supported-symbols*) *supported-symbols* '("USDJPY" "EURUSD" "GBPUSD")))
        (updated 0))
    (dolist (sym1 symbols)
      (dolist (sym2 symbols)
        (unless (string= sym1 sym2)
          (let ((corr (calculate-dynamic-correlation sym1 sym2)))
            (when corr
              (setf (gethash (cons sym1 sym2) *dynamic-correlations*) corr)
              (incf updated))))))
    (setf *last-correlation-update* (get-universal-time))
    (format t "[C] 📊 Updated ~d dynamic correlations~%" updated)))

(defun maybe-update-correlations ()
  "Update correlations if interval has passed"
  (when (> (- (get-universal-time) *last-correlation-update*) *correlation-update-interval*)
    (update-all-correlations)))

(defun get-dynamic-correlation (sym1 sym2)
  "Get dynamic correlation, falling back to static if not available"
  (or (gethash (cons sym1 sym2) *dynamic-correlations*)
      (gethash (cons sym2 sym1) *dynamic-correlations*)
      ;; Fall back to static
      (let ((pairs (cdr (assoc sym1 *pair-correlations* :test #'string=))))
        (if pairs (or (cdr (assoc sym2 pairs :test #'string=)) 0.0) 0.0))))

(defun check-correlation-breakdown ()
  "Detect correlation regime change (correlations moving toward 1.0)"
  (let ((alerts nil))
    (maphash
     (lambda (pair corr)
       (let* ((sym1 (car pair))
              (sym2 (cdr pair))
              (static (let ((pairs (cdr (assoc sym1 *pair-correlations* :test #'string=))))
                        (if pairs (or (cdr (assoc sym2 pairs :test #'string=)) 0.0) 0.0)))
              (abs-corr (abs corr))
              (abs-static (abs static))
              (drift (- abs-corr abs-static)))
         ;; Alert if correlation significantly increased
         (when (and (> abs-corr *correlation-alert-threshold*)
                    (> drift 0.2))
           (push (list :pair pair :dynamic corr :static static :drift drift) alerts))))
     *dynamic-correlations*)
    (when alerts
      (format t "[C] ⚠️ CORRELATION BREAKDOWN DETECTED!~%")
      (dolist (a alerts)
        (format t "[C]    ~a-~a: ~,2f (was ~,2f, drift +~,2f)~%"
                (car (getf a :pair)) (cdr (getf a :pair))
                (getf a :dynamic) (getf a :static) (getf a :drift)))
      (when (fboundp 'notify-discord-alert)
        (notify-discord-alert
         (format nil "⚠️ CORRELATION BREAKDOWN~%~{~a~%~}"
                 (mapcar (lambda (a)
                           (format nil "~a-~a: ~,2f → ~,2f"
                                   (car (getf a :pair)) (cdr (getf a :pair))
                                   (getf a :static) (getf a :dynamic)))
                         alerts)))))
    alerts))

(defun get-portfolio-correlation-risk ()
  "Calculate overall portfolio correlation risk score (0-1)"
  (maybe-update-correlations)
  (let ((total-risk 0.0)
        (count 0))
    (maphash
     (lambda (pair corr)
       (declare (ignore pair))
       (incf total-risk (abs corr))
       (incf count))
     *dynamic-correlations*)
    (if (> count 0) (/ total-risk count) 0.0)))

(defun check-correlation-risk (symbol direction)
  "Check correlation risk for a new trade (used by risk-manager.lisp)"
  (declare (ignore direction))
  (maybe-update-correlations)
  (let ((max-corr 0.0))
    (maphash
     (lambda (sym exp)
       (when (and (> exp 0) (not (string= sym symbol)))
         (let ((corr (abs (get-dynamic-correlation symbol sym))))
           (when (> corr max-corr)
             (setf max-corr corr)))))
     *symbol-exposure*)
    max-corr))


;;; ==========================================
;;; DANGER AVOIDANCE SYSTEM
;;; ==========================================
;; Danger Cooldowns (moved to globals.lisp)
(defparameter *last-trade-result* nil)

;; V44.0: Unified 11-Tier Cooldown System (Expert Panel Approved)
;; Tiers: 3min → 5min → 10min → 15min → 30min → 45min → 1h → 2h → 3h → 4h → EOD
(defvar *cooldown-tier* 0 "Current cooldown tier (0-10). Escalates on each loss.")
(defparameter *cooldown-durations*
  '(180 300 600 900 1800 2700 3600 7200 10800 14400 :eod)
  "Cooldown durations in seconds. :eod = end of day (no more trading)")

;;; ==========================================
;;; RESIGNATION JUDGMENT
;;; ==========================================
(defparameter *resignation-threshold* -5000)
(defparameter *resignation-loss-count* 7)

;;; ==========================================
;;; FAILURE ANALYSIS
;;; ==========================================
(defparameter *failure-log* nil)
(defparameter *success-log* nil)
(defparameter *max-log-size* 500)
(defparameter *decay-half-life* 3600)
(defparameter *min-samples-for-block* 5)

;;; ==========================================
;;; VOLATILITY TRACKING
;;; ==========================================
(defparameter *volatility-history* nil)
(defparameter *volatility-history-size* 20)
(defparameter *volatility-shift-threshold* 2.0)
(defparameter *current-volatility-state* :normal)
(defparameter *last-shift-time* 0)

;;; ==========================================
;;; CURRENCY RISK
;;; ==========================================
(defparameter *currency-risk-cache* (make-hash-table :test 'equal))
(defparameter *currency-risk-cache-time* (make-hash-table :test 'equal))

;;; ==========================================
;;; PREDICTION TRACKING
;;; ==========================================
(defparameter *prediction-history* nil)
(defparameter *prediction-accuracy* 0.0)

;;; ==========================================
;;; CATEGORY & RISK MANAGEMENT
;;; ==========================================
(defparameter *category-volatilities* (make-hash-table :test 'equal))
(defparameter *target-total-risk* 0.02)
(defparameter *trade-explanations* nil)
(defparameter *max-explanations* 50)

;;; ==========================================
;;; STRATEGY RANKS
;;; ==========================================
(defparameter *strategy-ranks* (make-hash-table :test 'equal))
(defparameter *rank-db-path* (merge-pathnames ".swimmy/strategy_ranks.lisp" (resolve-swimmy-home)))

(defun save-strategy-ranks ()
  "Save strategy ranks to file"
  (ensure-directories-exist *rank-db-path*)
  (let ((data nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push (list :name (strategy-rank-name v)
                           :rank (strategy-rank-rank v)
                           :trades (strategy-rank-trades v)
                           :wins (strategy-rank-wins v)
                           :total-pnl (strategy-rank-total-pnl v)
                           :promotion-date (strategy-rank-promotion-date v)
                           :last-trade (strategy-rank-last-trade v))
                     data))
             *strategy-ranks*)
    (with-open-file (out *rank-db-path* :direction :output :if-exists :supersede)
      (write data :stream out :pretty t))
    (format t "[S] 💾 Saved ~d strategy ranks to ~a~%" (length data) *rank-db-path*)))

(defun load-strategy-ranks ()
  "Load strategy ranks from file"
  (with-open-file (in *rank-db-path* :direction :input :if-does-not-exist nil)
    (when in
      (let ((data (read in nil nil))
            (count 0))
        (dolist (entry data)
          (let ((name (getf entry :name)))
            (setf (gethash name *strategy-ranks*)
                  (make-strategy-rank
                   :name name
                   :rank (getf entry :rank)
                   :trades (getf entry :trades)
                   :wins (getf entry :wins)
                   :total-pnl (getf entry :total-pnl)
                   :promotion-date (getf entry :promotion-date)
                   :last-trade (getf entry :last-trade)))
            (incf count)))
        (format t "[S] 📂 Loaded ~d strategy ranks from ~a~%" count *rank-db-path*)))))

;;; ==========================================
;;; LEADER SYSTEM
;;; ==========================================
(defparameter *leader-tenure* 0)
(defparameter *min-leader-tenure* 10)
(defparameter *leader-bonus-weight* 2.0)
(defparameter *leader-history* nil)

;;; ==========================================
;;; MEMORY SYSTEM
;;; ==========================================
(defparameter *episodic-memory* nil)
(defparameter *semantic-memory* nil)
(defparameter *max-episodic-memory* 1000)

;;; ==========================================
;;; V7.8+++: STRATEGY TTL & AUTO-PRUNING (Naval Critique #4)
;;; Strategies have a lifespan and get pruned if underperforming
;;; ==========================================
(defparameter *strategy-registry* (make-hash-table :test 'equal)
  "Hash table: strategy-id -> (birth-time last-used-time trades wins losses win-rate)")
(defparameter *strategy-max-ttl* 604800   ;; 7 days in seconds
  "Maximum time a strategy can live without being used")
(defparameter *strategy-min-win-rate* 0.35
  "Minimum win rate to avoid auto-pruning")
(defparameter *strategy-min-trades* 10
  "Minimum trades before win-rate is enforced")

(defun register-strategy (strategy-id)
  "Register a new strategy with TTL tracking"
  (let ((now (get-universal-time)))
    (setf (gethash strategy-id *strategy-registry*)
          (list now now 0 0 0 0.5))  ;; (birth last-used trades wins losses win-rate)
    (format t "[S] 🆕 Strategy registered: ~a (TTL: ~a days)~%"
            strategy-id (/ *strategy-max-ttl* 86400))))

(defun update-strategy-result (strategy-id won-p)
  "Update strategy win/loss tracking after a trade"
  (let ((entry (gethash strategy-id *strategy-registry*)))
    (if entry
        (let* ((birth (first entry))
               (now (get-universal-time))
               (trades (+ 1 (third entry)))
               (wins (+ (if won-p 1 0) (fourth entry)))
               (losses (+ (if won-p 0 1) (fifth entry)))
               (win-rate (if (> trades 0) (/ wins trades) 0.5)))
          (setf (gethash strategy-id *strategy-registry*)
                (list birth now trades wins losses win-rate))
          (format t "[S] 📊 Strategy ~a: ~a/~a trades, ~,1f% WR~%"
                  strategy-id wins trades (* 100 win-rate)))
        (register-strategy strategy-id))))

(defun prune-underperforming-strategies ()
  "Auto-prune strategies that are expired or underperforming. Returns count pruned."
  (let ((now (get-universal-time))
        (pruned 0))
    (maphash
     (lambda (id entry)
       (let ((birth (first entry))
             (last-used (second entry))
             (trades (third entry))
             (win-rate (sixth entry)))
         ;; Prune if: TTL expired OR (enough trades AND low win-rate)
         (when (or (> (- now last-used) *strategy-max-ttl*)
                   (and (>= trades *strategy-min-trades*)
                        (< win-rate *strategy-min-win-rate*)))
           (remhash id *strategy-registry*)
           (incf pruned)
           (format t "[S] 🗑️ Pruned strategy ~a: WR=~,1f% trades=~a age=~a days~%"
                   id (* 100 win-rate) trades 
                   (floor (- now birth) 86400)))))
     *strategy-registry*)
    (when (> pruned 0)
      (format t "[S] 🧹 Naval Critique #4: Auto-pruned ~a underperforming strategies~%" pruned))
    pruned))


(format t "[L] 📦 school-state.lisp loaded - Shared state initialized~%")
(format t "[L] 🕐 Strategy TTL system enabled (max TTL: ~a days, min WR: ~a%)~%" 
        (floor *strategy-max-ttl* 86400) (* 100 *strategy-min-win-rate*))
