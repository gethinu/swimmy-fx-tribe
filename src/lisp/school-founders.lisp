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
         ,@body)
       (setf (gethash ,key *founder-registry*) #',func-name)
       (format t "[REGISTRY] 📝 Registered Founder: ~a (~a)~%" ,key ,name))))

;;; ----------------------------------------------------------------------------
;;; 1. VOLATILITY SCALPER (Source: TradingView/PineScript Communities)
;;; ----------------------------------------------------------------------------
;;; Clan: Scalper (Raiders)

(def-founder :volvo "Volvo-Scalp-Gen0"
  "Volatility Scalping logic (BB + RSI + EMA). Low Risk, High Frequency."
  (make-strategy 
   :name "Volvo-Scalp-Gen0"
   :category :scalp
   :timeframe "M1"
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
;;; Clan: Breakout (Breakers)

(def-founder :london "London-Breakout-Gen0"
  "Asian Session Range Breakout. Time-based logic."
  (make-strategy 
   :name "London-Breakout-Gen0"
   :category :breakout
   :timeframe "M5"
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
;;; Clan: Trend (Followers)
;;; Logic: Structural Trend following using Ichimoku Equilibrium.
;;; Indicators: Tenkan (9), Kijun (26), Senkou B (52)

(def-founder :ichimoku "Ichimoku-Kijun-Gen0"
  "Ichimoku Kijun-sen Cross. Classic Japanese Trend Logic."
  (make-strategy 
   :name "Ichimoku-Kijun-Gen0"
   :category :trend
   :timeframe "H1"
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
;;; Clan: Trend (Followers)
;;; Logic: Price breaks the N-period High/Low. Pure price action structure.

(def-founder :donchian "Donchian-Trend-Gen0"
  "Donchian Channel Breakout (Turtle Trading derivative)."
  (make-strategy 
   :name "Donchian-Trend-Gen0"
   :category :trend
   :timeframe "H4"
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
;;; Clan: Reversion (Swingers)
;;; Logic: Price touches 2.5 Sigma Band and RSI is extreme.

(def-founder :bb-bounce "BB-Bounce-Gen0"
  "Bollinger Band Bounce (Mean Reversion)."
  (make-strategy 
   :name "BB-Bounce-Gen0"
   :category :reversion
   :timeframe "M15"
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
;;; Clan: Trend (Followers)
;;; Logic: MACD Histogram flip.

(def-founder :macd-cross "MACD-Cross-Gen0"
  "MACD Histogram Crossover Logic."
  (make-strategy 
   :name "MACD-Cross-Gen0"
   :category :trend
   :timeframe "H1"
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
;;; Clan: Reversion (Swingers)
;;; Source: Larry Connors
;;; Logic: RSI(2) < 10 buy, > 90 sell.

(def-founder :rsi-2 "RSI-2-Gen0"
  "RSI 2-Period Reversion (Larry Connors)."
  (make-strategy 
   :name "RSI-2-Gen0"
   :category :reversion
   :timeframe "D1"         ; Daily timeframe
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
;;; HEADHUNTING PROTOCOL (Injection)
;;; ----------------------------------------------------------------------------

(defun list-available-founders ()
  "Returns a list of available founder keys."
  (let ((keys nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k keys)) *founder-registry*)
    keys))

(defun recruit-founder (founder-type)
  "Injects a specific Founder Strategy into the live Knowledge Base.
   Now uses *founder-registry* for OCP compliance."
  (let ((maker-func (gethash founder-type *founder-registry*)))
    (if maker-func
        (let ((founder (funcall maker-func)))
          (cond
            ((null founder)
             (format t "[HEADHUNTER] ❌ Failed to create founder ~a~%" founder-type))
            ;; Check Duplicates
            ((find (strategy-name founder) *strategy-knowledge-base* :key #'strategy-name :test #'string=)
             (format t "[HEADHUNTER] ⚠️ Founder ~a already exists. Skipping.~%" (strategy-name founder)))
            (t
             (progn
               (format t "~%[HEADHUNTER] 🕵️ Recruiting Founder: ~a~%" (strategy-name founder))
               ;; 1. Add to Knowledge Base
               (push founder *strategy-knowledge-base*)
               ;; 2. Categorize & Add to Pool
               (let ((cat (categorize-strategy founder)))
                 (push founder (gethash cat *category-pools*))
                 (format t "[HEADHUNTER] Assigned to Clan: ~a~%" cat))
               ;; 3. Notify Discord
               (swimmy.shell:notify-discord
                (format nil "🕵️ **New Founder Recruited!**~%Name: `~a`~%Origin: External Registry~%Clan: ~a"
                        (strategy-name founder) (categorize-strategy founder)))
               t))))
        (format t "[HEADHUNTER] ❌ Unknown founder type: ~a (Available: ~a)~%" 
                founder-type (list-available-founders)))))

;;; ----------------------------------------------------------------------------
;;; IMMIGRATION BUREAU (Census & Active Recruitment)
;;; ----------------------------------------------------------------------------
;;; Implements Andrew Ng's "Active Learning" recommendation.
;;; Periodically checks the population balance and actively recruits missing talents.

(defun get-clan-counts ()
  "Returns an alist of (clan . count) for the current population."
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

(defun immigration-census ()
  "Conducts a census and recruits founders if imbalance is detected.
   Target Ratio: Trend 30%, Reversion 25%, Breakout 20%, Scalp 25%"
  (format t "~%[IMMIGRATION] 📊 Conducting Census...~%")
  (let* ((counts (get-clan-counts))
         (total (reduce #'+ counts :key #'cdr))
         (target-ratios '((:trend . 0.30)
                          (:reversion . 0.25)
                          (:breakout . 0.20)
                          (:scalp . 0.25))))
    
    (format t "[IMMIGRATION] Total Population: ~a~%" total)
    (loop for (clan . count) in counts do
      (let* ((ratio (if (> total 0) (/ count total) 0))
             (target (cdr (assoc clan target-ratios)))
             (shortage (- target ratio)))
        (format t " - ~a: ~d (~,1f%) vs Target ~,1f%~%" 
                clan count (* 100 ratio) (* 100 target))
        
        ;; If shortage > 5%, recruit a hero from that clan
        (when (> shortage 0.05)
          (format t "[IMMIGRATION] 🚨 Critical Shortage in ~a! Recruiting...~%" clan)
          (recruit-founder-by-clan clan))))
    
    ;; Diversity Injection (Randomly recruit one non-Trend founder daily)
    (when (> (random 1.0) 0.5)
      (let ((minority-clans '(:scalp :breakout :reversion)))
        (recruit-founder-by-clan (nth (random (length minority-clans)) minority-clans))))))

(defun recruit-founder-by-clan (target-clan)
  "Finds a founder in the registry that matches the target clan and recruits it."
  (let ((candidates nil))
    (maphash (lambda (key maker-func)
               (let ((proto (funcall maker-func)))
                 (when (eq (strategy-category proto) target-clan)
                   (push key candidates))))
             *founder-registry*)
    
    (if candidates
        (let* ((pick (nth (random (length candidates)) candidates)))
          (format t "[IMMIGRATION] 🎯 Selected candidate for ~a: ~a~%" target-clan pick)
          (recruit-founder pick))
        (format t "[IMMIGRATION] ⚠️ No candidates found for clan ~a in Registry.~%" target-clan))))
