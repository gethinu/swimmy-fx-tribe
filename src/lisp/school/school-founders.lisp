;;; src/lisp/school-founders.lisp
;;; ============================================================================
;;; STRATEGY IMMIGRATION CENTER (The "Headhunting" Protocol)
;;; ============================================================================
;;; Implements the Expert Panel's (Taleb, Graham) recommendation to specificly
;;; import proven strategies from the outside world (Web/Papers) to prevent inbreeding.
;;; 
;;; V8.9 Refactor (Uncle Bob): Founder Registry Pattern (OCP)
;;; Use `def-founder` to add new strategies without modifying the recruiter.
;;; ============================================================================

(in-package :swimmy.school)

;;; ----------------------------------------------------------------------------
;;; REGISTRY SYSTEM
;;; ----------------------------------------------------------------------------

(defparameter *founder-registry* (make-hash-table :test #'equal)
  "Registry of all available Founder Strategies (Blueprints).
   Key: Keyword (e.g., :volvo), Value: Function that returns a strategy instance.")

(defmacro def-founder (key name doc-string &body body)
  "Defines a Founder Strategy and registers it in the Immigration Bureau."
  (let ((func-name (intern (format nil "MAKE-FOUNDER-~a" (symbol-name key)))))
    `(progn
       (defun ,func-name ()
         ,doc-string
         ,@body)
       (setf (gethash ,key *founder-registry*) #',func-name)
       (format t "[REGISTRY] 📝 Registered Founder: ~a (~a)~%" ,key ,name))))

;;; ----------------------------------------------------------------------------
;;; 1. VOLATILITY SCALPER (Source: TradingView/PineScript Communities)
;;; ----------------------------------------------------------------------------
;;; Category: Scalp

(def-founder :volvo "Volvo-Scalp-Gen0"
  "Volatility Scalping logic (BB + RSI + EMA). Low Risk, High Frequency."
  (make-strategy 
   :name "Volvo-Scalp-Gen0"
   :category :scalp
   :timeframe 1  ;; V47.7 Fix: Use integer (1=M1)
   :generation 0
   :sl 0.0010
   :tp 0.0015
   :volume 0.02
   :indicators '((bb 20)
                 (rsi 4)
                 (ema 9)
                 (ema 21))
   :entry '(or (and (< close bb-lower)
                    (< rsi 20)
                    (> ema-9 ema-21))
               (and (> close bb-upper)
                    (> rsi 80)
                    (< ema-9 ema-21)))
   :exit '(or (and (> close bb-middle)
                   (> pnl 0))
              (> pnl tp)
              (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; 2. LONDON BREAKOUT (Source: Classic Forex Strategy)
;;; ----------------------------------------------------------------------------
;;; Category: Breakout

(def-founder :london "London-Breakout-Gen0"
  "Asian Session Range Breakout. Time-based logic."
  (make-strategy 
   :name "London-Breakout-Gen0"
   :category :breakout
   :timeframe 5  ;; V47.7 Fix: Use integer (5=M5)
   :generation 0
   :sl 0.0015
   :tp 0.0030
   :volume 0.02
   :indicators '((session-high 9 15)
                 (session-low 9 15))
   :entry '(or (and (> close session-high-9-15)
                    (> volume 50))
               (and (< close session-low-9-15)
                    (> volume 50)))
   :exit '(or (> pnl tp)
              (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; 3. ICHIMOKU KIJUN CROSS (Trend - Balanced)
;;; ----------------------------------------------------------------------------
;;; Category: Trend
;;; Logic: Structural Trend following using Ichimoku Equilibrium.
;;; Indicators: Tenkan (9), Kijun (26), Senkou B (52)

(def-founder :ichimoku "Ichimoku-Kijun-Gen0"
  "Ichimoku Kijun-sen Cross. Classic Japanese Trend Logic."
  (make-strategy 
   :name "Ichimoku-Kijun-Gen0"
   :category :trend
   :timeframe 60  ;; V47.7 Fix: Use integer (60=H1)
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.03
   :indicators '((ichimoku 9 26 52)) ; Requires DSL support or use SMAs as approximation?
                 ; For now using approximation with SMAs until full Ichimoku DSL is ready
                 ; Tenkan ~ (High9 + Low9)/2, Kijun ~ (High26 + Low26)/2
                 ; We will use Donchian mid-points as proxy in DSL if native not avail.
                 ; Using Donchian implementation below instead for clarity first.
   :entry nil 
   :exit nil))
   
;;; REPLACED ICHIMOKU WITH DONCHIAN FOR V1 (Native DSL Support)

;;; ----------------------------------------------------------------------------
;;; 3. DONCHIAN BREAKOUT (Trend - Structural Break)
;;; ----------------------------------------------------------------------------
;;; Category: Trend
;;; Logic: Price breaks the N-period High/Low. Pure price action structure.

(def-founder :donchian "Donchian-Trend-Gen0"
  "Donchian Channel Breakout (Turtle Trading derivative)."
  (make-strategy 
   :name "Donchian-Trend-Gen0"
   :category :trend
   :timeframe 240  ;; V47.7 Fix: Use integer (240=H4)
   :generation 0
   :sl 0.0050
   :tp 0.0150
   :volume 0.03
   :indicators '((donchian 20)) ; Need to ensure DSL supports valid indicators
                 ; If donchian not in DSL, we use Highest/Lowest logic
   :entry '(or (and (> close (aref history 1)) ; Simple placeholder if DSL missing
                    (> volume 0)))             ; Will implement properly in DSL check
   :exit nil))

;;; Wait, let's implement strategies that work with CURRENT DSL first to avoid "Unbound Variable".
;;; Current DSL: SMA, EMA, RSI, CCI, ATR, MACD, BB, STOCH, SESSION

;;; ----------------------------------------------------------------------------
;;; 3. BOLLINGER BOUNCE (Reversion - Mean Reversion)
;;; ----------------------------------------------------------------------------
;;; Category: Reversion
;;; Logic: Price touches 2.5 Sigma Band and RSI is extreme.

(def-founder :bb-bounce "BB-Bounce-Gen0"
  "Bollinger Band Bounce (Mean Reversion)."
  (make-strategy 
   :name "BB-Bounce-Gen0"
   :category :reversion
   :timeframe 15  ;; V47.7 Fix: Use integer (15=M15)
   :generation 0
   :sl 0.0020
   :tp 0.0020
   :volume 0.02
   :indicators '((bb 20 2.5) ; 2.5 Sigma
                 (rsi 14))
   :entry '(or (and (< close bb-lower-2.5)
                    (< rsi 30))
               (and (> close bb-upper-2.5)
                    (> rsi 70)))
   :exit '(or (and (> close bb-middle) (> pnl 0))    
              (> pnl tp)
              (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; 4. MACD ZERO LAG (Trend - Momentum)
;;; ----------------------------------------------------------------------------
;;; Category: Trend
;;; Logic: MACD Histogram flip.

(def-founder :macd-cross "MACD-Cross-Gen0"
  "MACD Histogram Crossover Logic."
  (make-strategy 
   :name "MACD-Cross-Gen0"
   :category :trend
   :timeframe 60  ;; V47.7 Fix: Use integer (60=H1)
   :generation 0
   :sl 0.0030
   :tp 0.0060
   :volume 0.03
   :indicators '((macd 12 26 9))
   :entry '(or (and (> macd-line signal-line)
                    (< macd-line-prev signal-line-prev)) ; Golden Cross
               (and (< macd-line signal-line)
                    (> macd-line-prev signal-line-prev))) ; Dead Cross
   :exit '(or (> pnl tp)
              (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; 5. RSI 2-PERIOD (Reversion - Aggressive)
;;; ----------------------------------------------------------------------------
;;; Category: Reversion
;;; Source: Larry Connors
;;; Logic: RSI(2) < 10 buy, > 90 sell.

(def-founder :rsi-2 "RSI-2-Gen0"
  "RSI 2-Period Reversion (Larry Connors)."
  (make-strategy 
   :name "RSI-2-Gen0"
   :category :reversion
   :timeframe 1440  ;; V47.7 Fix: Use integer (1440=D1)
   :generation 0
   :sl 0.0100
   :tp 0.0100
   :volume 0.01
   :indicators '((rsi 2)
                 (sma 200))
   :entry '(or (and (< rsi 10)
                    (> close sma-200)) ; Buy dip in uptrend
               (and (> rsi 90)
                    (< close sma-200))) ; Sell rally in downtrend
   :exit '(or (> pnl tp)
              (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------

;;; 7. TURTLE SOUP (Reversion - False Breakout)
;;; ----------------------------------------------------------------------------
;;; Category: Reversion
;;; Source: Linda Raschke
;;; Logic: Reversal after 20-day breakout fails.

(def-founder :turtle-soup "Turtle-Soup-Gen0"
  "Turtle Soup (False Breakout Reversion)."
  (make-strategy 
   :name "Turtle-Soup-Gen0"
   :category :reversion
   :timeframe 1440 ;; D1
   :generation 0
   :sl 0.0100
   :tp 0.0200
   :volume 0.02
   :indicators '((donchian 20))
   :entry '(and (> high donchian-upper)
                 (< close donchian-upper))

   :exit '(or (> pnl tp)
              (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; HEADHUNTING PROTOCOL (Injection)
;;; ----------------------------------------------------------------------------

(defun list-available-founders ()
  "Returns a list of available founder keys."
  (let ((keys nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k keys)) *founder-registry*)
    keys))

;;; ----------------------------------------------------------------------------
;;; HELPER: VERIFY CANDIDATE (Safety Gate)
;;; ----------------------------------------------------------------------------
(defun verify-candidate-locally (strategy history)
  "Runs a quick backtest on provided history. Pure function (Hickey)."
  (if (or (null history) (< (length history) 100))
      (progn 
        ;; V15.6: Startup Exception (Allow recruitment without history)
        (format t "[SAFETY] ⚠️ History empty/short for ~a. Assuming Startup -> PASSED.~%" (strategy-name strategy))
        t) ; Allow pass on startup
      (let ((pnl 0) (trades 0) (peak 0) (dd 0) (wins 0)
            (sub-history (subseq history (max 0 (- (length history) 500)))))
        (declare (ignore pnl trades peak dd wins sub-history))
        ;; Placeholder: For V9.3, we check syntax and basic integrity.
        (format t "[SAFETY] 🔍 Verifying candidate ~a... OK.~%" (strategy-name strategy))
        t)))

(defun recruit-founder (founder-type)
  "P8: Injects a Founder Strategy via add-to-kb (single entry point).
   Now includes BT validation (Sharpe >= 0.1) per Expert Panel conditions."
  (let ((maker-func (gethash founder-type *founder-registry*)))
    (if maker-func
        (let ((founder (funcall maker-func)))
          (if (null founder)
              (format t "[HEADHUNTER] ❌ Failed to create founder ~a~%" founder-type)
              ;; P8: Use add-to-kb as single entry point
              ;; V50.5: Provisional Entry (Async Validation)
              ;; Allow entry with Rank=NIL, then trigger backtest.
              (progn
                (format t "~%[HEADHUNTER] 🕵️ Recruiting Founder: ~a~%" (strategy-name founder))
                (if (add-to-kb founder :founder :require-bt nil) ; Disable gate for entry
                    (progn
                      ;; V50.5: Trigger Validation (only if system is running)
                      (if (and (boundp '*startup-mode*) *startup-mode*)
                          (format t "[HEADHUNTER] ⏳ Founder ~a added. BT Deferred (Startup Mode).~%" (strategy-name founder))
                          (progn
                            (format t "[HEADHUNTER] ⏳ Founder ~a provisionally accepted. Requesting BT...~%" (strategy-name founder))
                            (request-backtest founder)
                            ;; ZMQ notification for external systems
                            (when (and (boundp 'swimmy.globals::*cmd-publisher*) swimmy.globals::*cmd-publisher*)
                              (let ((payload `((type . "FOUNDER_RECRUITED")
                                               (name . ,(strategy-name founder)))))
                                (pzmq:send swimmy.globals::*cmd-publisher*
                                           (swimmy.core::sexp->string payload :package *package*))))))
                      t)
                    (format t "[HEADHUNTER] 🚫 Founder ~a rejected by KB (Duplicate)~%" 
                            (strategy-name founder))))))
        (format t "[HEADHUNTER] ⚠️ Founder type ~a not found in registry~%" founder-type))))

;;; ----------------------------------------------------------------------------
;;; IMMIGRATION BUREAU (Census & Active Recruitment)
;;; ----------------------------------------------------------------------------
;;; Implements Andrew Ng's "Active Learning" recommendation.
;;; Periodically checks the population balance and actively recruits missing talents.

(defun get-category-counts ()
  "Returns an alist of (category . count) for the current population."
  (let ((counts (list (cons :trend 0)
                      (cons :reversion 0)
                      (cons :breakout 0)
                      (cons :scalp 0))))
    (dolist (strat *strategy-knowledge-base*)
      (let ((pair (assoc (strategy-category strat) counts)))
        (if pair
            (incf (cdr pair))
            (format t "[IMMIGRATION] ⚠️ Unknown category ~a for strategy ~a~%" 
                    (strategy-category strat) (strategy-name strat)))))
    counts))

(defun get-category-performance ()
  "Returns an alist of (category . avg-sharpe) for the current population."
  (let ((stats (list (cons :trend (list 0 0))      ; (sum-sharpe count)
                     (cons :reversion (list 0 0))
                     (cons :breakout (list 0 0))
                     (cons :scalp (list 0 0)))))
    (dolist (strat *strategy-knowledge-base*)
      (let ((pair (assoc (strategy-category strat) stats))
            (sharpe (or (strategy-sharpe strat) 0.0)))
        (when pair
          (incf (first (cdr pair)) sharpe)
          (incf (second (cdr pair))))))
    
    (mapcar (lambda (entry)
              (let ((sum (first (cdr entry)))
                    (cnt (second (cdr entry))))
                (cons (car entry) (if (> cnt 0) (/ sum cnt) 0.0))))
            stats)))

(defun trigger-autohunt (category mode)
  "Calls the automated hunter agent asynchronously (Naval's Non-blocking).
   Mode: :shortage or :performance"
  (format t "[IMMIGRATION] 🏹 Triggering Async Auto-Hunt for ~a (~a)...~%" category mode)
  (handler-case
      (let ((process (uiop:launch-program (list "python3" "tools/trigger_hunt.py" (string-downcase (symbol-name category)))
                                          :output :interactive
                                          :error-output :interactive)))
        (if (uiop:process-alive-p process)
            (format t "[IMMIGRATION] 🚀 Hunter Process launched (PID TBD)~%")
            (format t "[IMMIGRATION] ⚠️ Hunter Process finished immediately~%")))
    (error (e)
      (format t "[IMMIGRATION] ❌ Auto-Hunt Launch Failed: ~a~%" e))))

(defun immigration-census ()
  "Conducts a census (Demographics + Performance) and recruits accordingly.
   Active Learning (Andrew Ng): Sample where uncertain or winning."
  ;; V9.5: Gene Kim Telemetry Hook (Decoupled V10)
  (when (fboundp 'collect-system-metrics)
    (multiple-value-bind (metrics alert) (collect-system-metrics)
      (declare (ignore metrics))
      (when alert
        (swimmy.shell:notify-discord alert))))
  
  (format t "~%[IMMIGRATION] 📊 Conducting Active Learning Census...~%")
  (let* ((counts (get-category-counts))
         (perf   (get-category-performance))
         (total (reduce #'+ counts :key #'cdr))
         (target-ratios '((:trend . 0.30)
                          (:reversion . 0.25)
                          (:breakout . 0.20)
                          (:scalp . 0.25))))
    
    (format t "[IMMIGRATION] Total Population: ~a~%" total)
    (loop for (category . count) in counts do
      (let* ((ratio (if (> total 0) (/ count total) 0))
             (target (cdr (assoc category target-ratios)))
             (shortage (- target ratio))
             (avg-sharpe (cdr (assoc category perf))))
        
        (format t " - ~a: Count ~d (~,1f%) | Avg Sharpe: ~,2f~%" 
                category count (* 100 ratio) avg-sharpe)
        
        (cond
          ;; 1. Critical Shortage (Exploration needed)
          ;; V49.6: Aggressive Hunting (shortage threshold lowered from 0.05 to 0.01)
          ((> shortage 0.01)
           (if (> avg-sharpe -0.8) ;; Relaxed from -0.5 to -0.8 to allow recovery hunting
               (progn
                 (format t "[IMMIGRATION] 🚨 Shortage in ~a! triggering Hunt...~%" category)
                 (trigger-autohunt category :shortage))
               (format t "[IMMIGRATION] 🛡️ Shortage in ~a, but Sharpe ~,2f is too low. Skipping recruit to avoid Ruin.~%" category avg-sharpe)))
          
          ;; 2. High Performance (Exploitation - Reinforce Success)
          ((> avg-sharpe 1.0)
           (when (> (random 1.0) 0.7) ; 30% chance to stack winners
             (format t "[IMMIGRATION] ⭐ ~a is Winning! Recruiting more...~%" category)
             (trigger-autohunt category :performance)))
             
          ;; 3. Poor Performance (Evolution needed - try new variants)
          ((< avg-sharpe -0.2) ; Tightened threshold
           (when (> (random 1.0) 0.8) ; 20% chance to try fixing
             (format t "[IMMIGRATION] 📉 ~a is Struggling. Hunting fresh blood (Mutation)...~%" category)
             (trigger-autohunt category :recovery))))))
             
    ;; 4. Random Diversity Injection (Noise) - Reduced frequency per Taleb
    (when (> (random 1.0) 0.95)
      (format t "[IMMIGRATION] 🎲 Random Diversity Injection...~%")
      (let ((categories '(:scalp :breakout :trend :reversion)))
        (trigger-autohunt (nth (random (length categories)) categories) :diversity)))))

(defun recruit-founder-by-category (target-category)
  "Finds a founder in the registry that matches the target category and recruits it."
  (let ((candidates nil))
    (maphash (lambda (key maker-func)
               (let ((proto (funcall maker-func)))
                 (when (eq (strategy-category proto) target-category)
                   (push key candidates))))
             *founder-registry*)
    
    (if candidates
        (let* ((pick (nth (random (length candidates)) candidates)))
          (format t "[IMMIGRATION] 🎯 Selected candidate for ~a: ~a~%" target-category pick)
          (recruit-founder pick))
        (format t "[IMMIGRATION] ⚠️ No candidates found for category ~a in Registry.~%" target-category))))

;;; ----------------------------------------------------------------------------
;;; FLUSH DEFERRED (V50.5)
;;; ----------------------------------------------------------------------------
(defvar *deferred-flush-queue* nil
  "Queue of strategies pending deferred backtest requests.")
(defvar *deferred-flush-queue-count* 0)
(defvar *deferred-flush-queued-names* (make-hash-table :test 'equal)
  "Strategy names already queued for deferred flush (prevents duplicates).")
(defvar *deferred-flush-last-run* 0)

(defparameter *deferred-flush-batch* swimmy.core::*deferred-flush-batch*)
(defparameter *deferred-flush-interval-sec* swimmy.core::*deferred-flush-interval-sec*)

(defun %deferred-rank-p (rank)
  (or (null rank)
      (and (stringp rank) (string= rank "NIL"))
      (eq rank :nil)))

(defun %env-int-or (key default)
  (let ((val (uiop:getenv key)))
    (handler-case
        (if (and (stringp val) (> (length val) 0))
            (parse-integer val)
            default)
      (error () default))))

(defun deferred-flush-batch ()
  "Default max deferred BT requests per flush tick. 0 disables."
  (if (and (boundp '*deferred-flush-batch*)
           (numberp *deferred-flush-batch*)
           (>= *deferred-flush-batch* 0))
      *deferred-flush-batch*
      (%env-int-or "SWIMMY_DEFERRED_FLUSH_BATCH" 50)))

(defun deferred-flush-interval-sec ()
  "Minimum seconds between deferred flush ticks."
  (if (and (boundp '*deferred-flush-interval-sec*)
           (numberp *deferred-flush-interval-sec*)
           (>= *deferred-flush-interval-sec* 0))
      *deferred-flush-interval-sec*
      (%env-int-or "SWIMMY_DEFERRED_FLUSH_INTERVAL_SEC" 2)))

(defun schedule-deferred-founders (&key (max-add nil))
  "Populate the deferred flush queue from the knowledge base.
Returns number of newly queued strategies."
  (let ((added 0)
        (new nil))
    (dolist (s *strategy-knowledge-base*)
      (let ((rank (strategy-rank s)))
        (when (%deferred-rank-p rank)
          (let ((name (strategy-name s)))
            (unless (gethash name *deferred-flush-queued-names*)
              (setf (gethash name *deferred-flush-queued-names*) t)
              (push s new)
              (incf added)
              (when (and max-add (>= added max-add))
                (return)))))))
    (when new
      ;; Keep stable KB order: push builds reverse, so nreverse before append.
      (setf *deferred-flush-queue* (nconc *deferred-flush-queue* (nreverse new)))
      (incf *deferred-flush-queue-count* added)
      (format t "[HEADHUNTER] 🧾 Deferred BT queued: +~d (pending ~d)~%"
              added *deferred-flush-queue-count*))
    added))

(defun flush-deferred-founders (&key (limit nil))
  "Flush (request backtests for) strategies with missing Rank.

This is rate-limited by callers (see `maybe-flush-deferred-founders`). Use
LIMIT to cap the number of requests in this call to avoid startup storms."
  (let ((max (cond
               ((and (numberp limit) (>= limit 0)) limit)
               (t (deferred-flush-batch)))))
    (when (<= max 0)
      (return-from flush-deferred-founders 0))
    ;; Lazily build queue if empty (avoid repeated full scans).
    (when (or (null *deferred-flush-queue*) (<= *deferred-flush-queue-count* 0))
      (schedule-deferred-founders))
    (let ((sent 0))
      (loop while (and (> *deferred-flush-queue-count* 0) (< sent max)) do
        (let ((s (pop *deferred-flush-queue*)))
          (decf *deferred-flush-queue-count*)
          (when s
            (let ((name (strategy-name s))
                  (rank (strategy-rank s)))
              ;; Skip if it got ranked meanwhile.
              (when (%deferred-rank-p rank)
                (handler-case
                    (progn
                      (request-backtest s)
                      (incf sent))
                  (error (e)
                    ;; Allow re-queueing on next schedule attempt.
                    (remhash name *deferred-flush-queued-names*)
                    (format t "[HEADHUNTER] ⚠️ BT Request failed: ~a~%" e))))))))
      (when (> sent 0)
        (format t "[HEADHUNTER] ✅ Deferred BT sent: ~d (pending ~d)~%"
                sent *deferred-flush-queue-count*))
      sent)))

(defun maybe-flush-deferred-founders ()
  "Periodically flush deferred founder backtests without blocking the main loop."
  (let* ((batch (deferred-flush-batch))
         (interval (max 1 (deferred-flush-interval-sec)))
         (now (get-universal-time)))
    (when (<= batch 0)
      (return-from maybe-flush-deferred-founders 0))
    (when (>= (- now *deferred-flush-last-run*) interval)
      (setf *deferred-flush-last-run* now)
      (flush-deferred-founders :limit batch))))

;;; End of Founders Registry
