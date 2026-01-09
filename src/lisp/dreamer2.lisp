;; dreamer2.lisp - Code Generator (Genesis v2) + Backtester Integration

(in-package :swimmy.school)

;; 変数を関数の前に定義して警告を防ぐ
(defparameter *evolved-strategies* nil)

;; Forward declarations for variables defined in school.lisp
(defvar *failure-log* nil)
(defvar *success-log* nil)
(defvar *regime-performance* nil)
(defvar *current-regime* :unknown)
(defvar *volatility-regime* :normal)
(defvar *candle-histories* nil)
(defvar *category-pools* nil)  ; Used by seed-evolution-from-knowledge-base

;; Forward declarations for struct accessors
(declaim (ftype (function (t) t) trade-record-category))
(declaim (ftype (function (t) t) trade-record-pnl))
(declaim (ftype (function (t) t) trade-record-session))
(declaim (ftype (function (t) t) trade-record-regime))

;;; ==========================================
;;; NAVAL HOMEWORK: STRATEGY TEMPLATES (AI Generation)
;;; ==========================================
;;; Structured templates for AI-based strategy generation
;;; Instead of LLM generating from scratch, it fills templates

(defparameter *strategy-templates*
  '((:trend-follow
     :description "Follow established trends using moving average crossovers"
     :indicators ((sma :param1) (sma :param2))
     :entry (cross-above sma-param1 sma-param2)
     :exit (cross-below sma-param1 sma-param2)
     :optimal-regime :trending
     :param-ranges ((:param1 3 15) (:param2 20 100)))
    
    (:mean-reversion
     :description "Trade bounces from Bollinger Bands extremes"
     :indicators ((bb 20 2))
     :entry (< price bb-lower)
     :exit (> price bb-middle)
     :optimal-regime :ranging
     :param-ranges ((:period 10 30) (:std 1.5 3.0)))
    
    (:breakout
     :description "Trade breakouts from consolidation ranges"
     :indicators ((atr :param1))
     :entry (> price (+ high-period (* multiplier atr)))
     :exit (< price (- entry (* 0.5 atr)))
     :optimal-regime :volatile
     :param-ranges ((:param1 10 20) (:period 20 50) (:multiplier 1.5 3.0)))
    
    (:rsi-reversal
     :description "Trade RSI overbought/oversold reversals"
     :indicators ((rsi :param1))
     :entry (< rsi 30)
     :exit (> rsi 70)
     :optimal-regime :ranging
     :param-ranges ((:param1 7 21)))
    
    (:macd-crossover
     :description "Trade MACD signal crossovers"
     :indicators ((macd :fast :slow :signal))
     :entry (cross-above macd-line signal-line)
     :exit (cross-below macd-line signal-line)
     :optimal-regime :trending
     :param-ranges ((:fast 8 16) (:slow 21 34) (:signal 7 12))))
  "Strategy templates for AI-based generation")

(defun get-template-for-regime (regime)
  "Get optimal strategy template for current market regime"
  (let ((matching (remove-if-not 
                   (lambda (tmpl) (eq (getf (cdr tmpl) :optimal-regime) regime))
                   *strategy-templates*)))
    (if matching
        (nth (random (length matching)) matching)
        (nth (random (length *strategy-templates*)) *strategy-templates*))))

(defun fill-template-params (template params)
  "Fill parameter placeholders in template with actual values"
  (if (atom template)
      (let ((param-entry (assoc template params)))
        (if param-entry
            (floor (second param-entry))
            template))
      (mapcar (lambda (x) (fill-template-params x params)) template)))

(defun generate-strategy-from-template (template-name &optional custom-params)
  "Generate a concrete strategy from a template with randomized or custom parameters"
  (let ((template (assoc template-name *strategy-templates*)))
    (when template
      (let* ((desc (cdr template))
             (param-ranges (getf desc :param-ranges))
             (params (or custom-params
                         (mapcar (lambda (range)
                                   (let ((name (first range))
                                         (min-v (second range))
                                         (max-v (third range)))
                                     (list name (+ min-v (random (float (- max-v min-v)))))))
                                 param-ranges)))
             (name (format nil "~a-~a" template-name (random 1000))))
        
        ;; Create strategy with filled-in parameters
        (make-strategy
         :name name
         :indicators (fill-template-params (getf desc :indicators) params)
         :entry (getf desc :entry)
         :exit (getf desc :exit)
         :sl (case template-name
               (:trend-follow 0.3)
               (:mean-reversion 0.2)
               (:breakout 0.4)
               (:rsi-reversal 0.25)
               (:macd-crossover 0.3)
               (t 0.25))
         :tp (case template-name
               (:trend-follow 0.6)
               (:mean-reversion 0.3)
               (:breakout 0.8)
               (:rsi-reversal 0.4)
               (:macd-crossover 0.5)
               (t 0.4))
         :volume 0.01)))))



(defun auto-generate-strategy-for-regime ()
  "Automatically generate a strategy optimized for current regime"
  (let* ((regime (if (boundp '*current-regime*) *current-regime* :trending))
         (template (get-template-for-regime regime)))
    (when template
      (let ((strategy (generate-strategy-from-template (car template))))
        (format t "[G] 🎯 Auto-generated ~a strategy for ~a regime~%"
                (car template) regime)
        strategy))))

;;; ==========================================
;;; NAVAL IMPROVEMENT: SELF-GENERATING TEMPLATES
;;; ==========================================
;;; LLM proposes new template structures, not just parameters

(defparameter *template-performance* (make-hash-table :test 'eq)
  "Track performance of each template type: template -> (wins losses avg-sharpe)")

(defun record-template-result (template-name won-p sharpe)
  "Record template performance for evolution"
  (let ((stats (gethash template-name *template-performance* (list 0 0 0.0 0))))
    (if won-p (incf (first stats)) (incf (second stats)))
    ;; Running average of sharpe
    (let ((n (+ (first stats) (second stats))))
      (setf (fourth stats) (/ (+ (* (fourth stats) (1- n)) sharpe) n)))
    (setf (gethash template-name *template-performance*) stats)))

(defun get-best-template ()
  "Get best performing template based on win rate and sharpe"
  (let ((best nil) (best-score -999))
    (maphash
     (lambda (name stats)
       (let* ((wins (first stats))
              (losses (second stats))
              (total (+ wins losses))
              (avg-sharpe (fourth stats))
              (win-rate (if (> total 0) (/ wins (float total)) 0.5))
              (score (+ (* win-rate 0.5) (* avg-sharpe 0.5))))
         (when (and (> total 5) (> score best-score))
           (setf best-score score)
           (setf best name))))
     *template-performance*)
    best))

(defun generate-llm-template-proposal ()
  "Naval improvement: Ask LLM to propose a new template structure"
  (let* ((existing-templates (mapcar #'car *strategy-templates*))
         (performance-summary (template-performance-summary))
         (prompt (format nil "You are a trading strategy architect.

CURRENT TEMPLATES: ~{~a~^, ~}
PERFORMANCE: ~a

TASK: Propose ONE new template structure that is DIFFERENT from existing ones.

OUTPUT FORMAT (Lisp s-expression):
(:NEW-TEMPLATE-NAME
  :description \"What this strategy does\"
  :indicators ((indicator-name param1 param2))
  :entry (condition)
  :exit (condition)
  :optimal-regime :trending/:ranging/:volatile
  :param-ranges ((:param1 min max)))

Focus on strategies NOT covered by existing templates.
Output ONLY the s-expression."
                         existing-templates performance-summary)))
    ;; Return prompt for LLM call (actual call handled externally)
    prompt))

(defun template-performance-summary ()
  "Generate summary of template performance"
  (let ((result nil))
    (maphash
     (lambda (name stats)
       (push (format nil "~a: ~d/~d wins (Sharpe ~,2f)"
                     name (first stats) (+ (first stats) (second stats))
                     (fourth stats))
             result))
     *template-performance*)
    (format nil "~{~a~^; ~}" result)))

(defun evolve-template (template-name)
  "Evolve a template by mutating its param-ranges"
  (let ((template (assoc template-name *strategy-templates*)))
    (when template
      (let* ((desc (cdr template))
             (param-ranges (getf desc :param-ranges))
             ;; Mutate ranges slightly
             (new-ranges 
              (mapcar (lambda (range)
                        (let* ((name (first range))
                               (min-v (second range))
                               (max-v (third range))
                               (delta (* (- max-v min-v) 0.1)))
                          (list name
                                (+ min-v (* (- (random 2.0) 1.0) delta))
                                (+ max-v (* (- (random 2.0) 1.0) delta)))))
                      param-ranges)))
        (format t "[T] 🧬 Evolved ~a ranges: ~a~%" template-name new-ranges)
        new-ranges))))


;;; ==========================================
;;; SELF-ANALYSIS v2.0 (構造化自己分析)
;;; ==========================================
;;; Features:
;;; - Structured performance metrics by category/session/regime
;;; - Strategy performance ranking
;;; - Actionable insights generation
;;; - JSON-formatted feedback for Gemini


(defun analyze-by-category ()
  "Analyze performance by trading category"
  (let ((stats (make-hash-table :test 'eq)))
    ;; Initialize categories
    (dolist (cat '(:trend :reversion :breakout :scalp))
      (setf (gethash cat stats) (list 0 0 0.0)))  ; wins losses pnl
    ;; Aggregate from logs
    (when (boundp '*success-log*)
      (dolist (record *success-log*)
        (let* ((cat (trade-record-category record))
               (s (gethash cat stats (list 0 0 0.0))))
          (incf (first s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash cat stats) s))))
    (when (boundp '*failure-log*)
      (dolist (record *failure-log*)
        (let* ((cat (trade-record-category record))
               (s (gethash cat stats (list 0 0 0.0))))
          (incf (second s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash cat stats) s))))
    stats))

(defun analyze-by-session ()
  "Analyze performance by trading session"
  (let ((stats (make-hash-table :test 'eq)))
    (dolist (session '(:tokyo :london :newyork :overlap :off))
      (setf (gethash session stats) (list 0 0 0.0)))
    (when (boundp '*success-log*)
      (dolist (record *success-log*)
        (let* ((sess (trade-record-session record))
               (s (gethash sess stats (list 0 0 0.0))))
          (when s
            (incf (first s))
            (incf (third s) (or (trade-record-pnl record) 0))
            (setf (gethash sess stats) s)))))
    (when (boundp '*failure-log*)
      (dolist (record *failure-log*)
        (let* ((sess (trade-record-session record))
               (s (gethash sess stats (list 0 0 0.0))))
          (when s
            (incf (second s))
            (incf (third s) (or (trade-record-pnl record) 0))
            (setf (gethash sess stats) s)))))
    stats))

(defun analyze-by-regime ()
  "Analyze performance by market regime"
  (let ((stats (make-hash-table :test 'eq)))
    (dolist (regime '(:trending :ranging :unknown))
      (setf (gethash regime stats) (list 0 0 0.0)))
    (when (boundp '*success-log*)
      (dolist (record *success-log*)
        (let* ((reg (or (trade-record-regime record) :unknown))
               (s (gethash reg stats (list 0 0 0.0))))
          (incf (first s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash reg stats) s))))
    (when (boundp '*failure-log*)
      (dolist (record *failure-log*)
        (let* ((reg (or (trade-record-regime record) :unknown))
               (s (gethash reg stats (list 0 0 0.0))))
          (incf (second s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash reg stats) s))))
    stats))

(defun format-stats-summary (stats-hash)
  "Format hash table stats into readable string"
  (let ((result nil))
    (maphash (lambda (key val)
               (let* ((wins (first val))
                      (losses (second val))
                      (pnl (third val))
                      (total (+ wins losses))
                      (win-rate (if (> total 0) (* 100 (/ wins total)) 0)))
                 (when (> total 0)
                   (push (format nil "~a: ~,0f% win (~d trades, ~,2f PnL)" 
                                 key win-rate total pnl) result))))
             stats-hash)
    (format nil "~{~a~^; ~}" (reverse result))))

(defun get-best-and-worst-conditions ()
  "Identify best and worst trading conditions"
  (let ((conditions nil))
    ;; Analyze by session
    (let ((session-stats (analyze-by-session)))
      (maphash (lambda (sess val)
                 (let* ((wins (first val)) (losses (second val))
                        (total (+ wins losses)))
                   (when (> total 3)
                     (push (cons sess (/ wins (max 1 total))) conditions))))
               session-stats))
    ;; Sort by win rate
    (let ((sorted (sort conditions #'> :key #'cdr)))
      (list :best (car (first sorted))
            :best-rate (cdr (first sorted))
            :worst (car (car (last sorted)))
            :worst-rate (cdr (car (last sorted)))))))

(defun generate-actionable-insights ()
  "Generate specific actionable insights from analysis"
  (let ((insights nil)
        (conditions (get-best-and-worst-conditions)))
    ;; Session insights
    (when (and (getf conditions :best) (> (getf conditions :best-rate) 0.6))
      (push (format nil "FOCUS on ~a session (~,0f% win rate)" 
                    (getf conditions :best) (* 100 (getf conditions :best-rate))) insights))
    (when (and (getf conditions :worst) (< (getf conditions :worst-rate) 0.4))
      (push (format nil "AVOID ~a session (~,0f% win rate)" 
                    (getf conditions :worst) (* 100 (getf conditions :worst-rate))) insights))
    ;; Regime insights from regime-performance if available
    (when (boundp '*regime-performance*)
      (maphash (lambda (key val)
                 (let* ((wins (first val)) (losses (second val))
                        (total (+ wins losses)))
                   (when (and (> total 5) (< (/ wins (max 1 total)) 0.35))
                     (push (format nil "REDUCE trading in ~a conditions" key) insights))))
               *regime-performance*))
    ;; Return as formatted string
    (if insights
        (format nil "~{- ~a~^~%~}" insights)
        "No strong patterns detected yet. Continue gathering data.")))

(defun get-top-strategies (n)
  "Get top N performing strategies by Sharpe ratio"
  (when *evolved-strategies*
    (let ((sorted (sort (copy-list *evolved-strategies*) #'> 
                        :key (lambda (s) (or (strategy-sharpe s) -999)))))
      (subseq sorted 0 (min n (length sorted))))))

(defun get-structured-self-analysis ()
  "Generate comprehensive structured analysis for Gemini"
  (let* ((category-stats (analyze-by-category))
         (session-stats (analyze-by-session))
         (regime-stats (analyze-by-regime))
         (top-strats (get-top-strategies 3))
         (insights (generate-actionable-insights)))
    (format nil "
=== SELF-ANALYSIS REPORT ===

PERFORMANCE BY CATEGORY:
~a

PERFORMANCE BY SESSION:
~a

PERFORMANCE BY REGIME:
~a

TOP STRATEGIES:
~{~a (Sharpe: ~,2f)~^, ~}

ACTIONABLE INSIGHTS:
~a

CURRENT MARKET: Regime=~a, Volatility=~a
"
            (format-stats-summary category-stats)
            (format-stats-summary session-stats)
            (format-stats-summary regime-stats)
            (if top-strats
                (loop for s in top-strats 
                      collect (strategy-name s)
                      collect (or (strategy-sharpe s) 0))
                (list "None" 0))
            insights
            *current-regime*
            *volatility-regime*)))

(defun find-balanced-end (str start)
  "Find the end of a balanced s-expression starting at position start"
  (let ((depth 0) (in-string nil))
    (loop for i from start below (length str)
          for c = (char str i)
          do (cond
               ((and (char= c #\\) (not in-string) (< (1+ i) (length str)))
                (incf i))  ; skip escaped char
               ((char= c #\") (setf in-string (not in-string)))
               ((and (not in-string) (char= c #\()) (incf depth))
               ((and (not in-string) (char= c #\)))
                (decf depth)
                (when (zerop depth) (return (1+ i)))))
          finally (return nil))))

;; Extract SMA parameters from indicator list
;; V6.10: Fixed to handle RSI, BB, etc. (Taleb requirement - was causing Sharpe -3.75)
(defun extract-sma-params (indicators)
  "Extract short and long parameters from any indicator list for backtesting.
   Returns first two numeric params as (short, long) ordered ascending."
  (let ((all-params nil))
    ;; Collect all numeric parameters from all indicators
    (dolist (ind indicators)
      (when (listp ind)
        (let ((nums (remove-if-not #'numberp (cdr ind))))
          (dolist (n nums)
            (push n all-params)))))
    ;; If we have at least 2 params, use them as short/long
    (cond
      ((>= (length all-params) 2)
       (let ((sorted (sort (remove-duplicates all-params) #'<)))
         (values (first sorted) (second sorted))))
      ((= (length all-params) 1)
       ;; Single param (e.g., RSI 14) - use it and a derived value
       (let ((p (first all-params)))
         (values (max 3 (floor p 2)) p)))
      (t
       ;; No params found - use defaults based on indicator type
       (let ((first-ind (car indicators)))
         (cond
           ((and (listp first-ind) (member (car first-ind) '(rsi RSI)))
            (values 7 14))
           ((and (listp first-ind) (member (car first-ind) '(bb BB)))
            (values 10 20))
           ((and (listp first-ind) (member (car first-ind) '(stoch STOCH)))
            (values 7 14))
           ((and (listp first-ind) (member (car first-ind) '(macd MACD)))
            (values 12 26))
           (t (values 5 20))))))))  ; Ultimate fallback

;; V6.11: Detect indicator type from strategy indicators
(defun detect-indicator-type (indicators)
  "Detect primary indicator type from indicators list for Rust backtester"
  (when (and indicators (listp indicators))
    (let ((first-ind (if (listp (car indicators)) (caar indicators) (car indicators))))
      (cond
        ((member first-ind '(rsi RSI :rsi)) "rsi")
        ((member first-ind '(bb BB :bb bollinger)) "bb")
        ((member first-ind '(macd MACD :macd)) "macd")
        ((member first-ind '(stoch STOCH :stoch stochastic)) "stoch")
        (t "sma")))))  ; Default to SMA

;; Convert strategy to JSON for Rust backtester
;; V6.11: Now includes indicator_type
;; V7.12: Updated to support name-suffix for WFV
(defun strategy-to-json (strat &key (name-suffix ""))
  "Convert strategy struct to JSON for backtest request"
  (multiple-value-bind (sma-short sma-long) 
      (extract-sma-params (strategy-indicators strat))
    (jsown:new-js
      ("name" (format nil "~a~a" (strategy-name strat) name-suffix))
      ("sma_short" (or sma-short 5))
      ("sma_long" (or sma-long 20))
      ("sl" (strategy-sl strat))
      ("tp" (strategy-tp strat))
      ("volume" (strategy-volume strat))
      ("indicator_type" (detect-indicator-type (strategy-indicators strat))))))




;; Helper to resample candles (M1 -> M5, etc)
;; WARN: CRITICAL DATA ORDER ASSUMPTION (Recurrence Prevention)
;; Input 'candles' MUST be Newest-First (Time: T_n, T_n-1, ... T_1).
;; Since 'push' reverses the order, the 'chunk' variable becomes (Oldest ... Newest).
;; - first chunk = Oldest Candle (Start of Period) -> Use for OPEN
;; - last chunk  = Newest Candle (End of Period)   -> Use for CLOSE/TIMESTAMP
;;
;; [V8.2] Expert Panel Audit (Andrew Ng):
;;   LOOK-AHEAD BIAS CHECK: PASSED
;;   - Open price: Uses start-candle (oldest in chunk) → Correct
;;   - Close price: Uses end-candle (newest in chunk) → Correct
;;   - Timestamp: Uses end-candle timestamp → Correct (represents bar close time)
;;   ⚠️ CAVEAT: Caller must ensure the LATEST candle is COMPLETE before calling.
;;              If called mid-bar, the newest candle contains partial data.
(defun resample-candles (candles factor)
  (let ((result nil)
        (chunk nil)
        (count 0))
    (dolist (c candles)
       (push c chunk)
       (incf count)
       (when (= count factor)
         ;; Input candles are Newest-First (M5 M4 ...)
         ;; Chunk build:
         ;; 1. Push M5 -> (M5)
         ;; 2. Push M4 -> (M4 M5)
         ;; ...
         ;; 5. Push M1 -> (M1 M2 M3 M4 M5)
         ;; So 'chunk' is (Start/Oldest ... End/Newest)
         
         (let* ((start-candle (first chunk))      ; Oldest time (Open)
                (end-candle (car (last chunk)))   ; Newest time (Close/Timestamp)
                (high (loop for x in chunk maximize (candle-high x)))
                (low (loop for x in chunk minimize (candle-low x)))
                (vol (loop for x in chunk sum (candle-volume x))))
            (push (make-candle :timestamp (candle-timestamp end-candle)
                               :open (candle-open start-candle)
                               :close (candle-close end-candle)
                               :high high :low low :volume vol)
                  result))
         (setf chunk nil)
         (setf count 0)))
    ;; Result was pushed as (Agg1 Agg2 ...). 
    ;; Agg1 came from M5..M1 (Newest). So Agg1 is Newest.
    ;; Result is (Newest ... Oldest).
    ;; No need to reverse result if we want Newest-First output.
    ;; Wait, original code did (nreverse result).
    ;; Let's check:
    ;; Input (M10...M1).
    ;; Loop 1: M10..M6. Chunk -> Agg1 (Time M10). Push Agg1 -> (Agg1).
    ;; Loop 2: M5..M1. Chunk -> Agg2 (Time M5). Push Agg2 -> (Agg2 Agg1).
    ;; Result (Agg2 Agg1). Agg2 is M5 (Old), Agg1 is M10 (New).
    ;; So result is (Old New).
    ;; We want (New Old) -> (Agg1 Agg2).
    ;; So yes, we need nreverse.
    (nreverse result)))

;; Request backtest from Rust
;; Helper to convert numeric timeframe to string suffix
(defun get-tf-string (tf)
  (cond ((= tf 1) "M1")
        ((= tf 5) "M5")
        ((= tf 15) "M15")
        ((= tf 30) "M30")
        ((= tf 60) "H1")
        ((= tf 240) "H4")
        ((= tf 1440) "D1")
        ((= tf 10080) "W1")
        ((= tf 43200) "MN")
        (t "M1")))

;; Request backtest from Rust
(defun request-backtest (strat &key (candles *candle-history*) (suffix ""))
  "Send strategy to Rust for high-speed backtesting. Resamples based on strategy's timeframe."
  
  ;; V8.0: Multi-Timeframe Logic
  ;; Strategy decides its own destiny (timeframe)
  (let* ((tf-slot (if (slot-exists-p strat 'timeframe) (strategy-timeframe strat) 1))
         (timeframe (if (numberp tf-slot) tf-slot 1))
         (tf-str (get-tf-string timeframe))
         
         ;; Data Selection Logic (V41.6):
         ;; 1. Try specific timeframe data for USDJPY (default assumption for generic backtest)
         ;;    FIXME: *candle-history* implies USDJPY. If we want other symbols, we need symbol arg.
         ;;    For now, assume if candles==*candle-history*, we check USDJPY tf data.
         (tf-candles 
           (if (and (> timeframe 1) 
                    (eq candles *candle-history*) 
                    (gethash "USDJPY" *candle-histories-tf*)
                    (gethash tf-str (gethash "USDJPY" *candle-histories-tf*)))
               (progn
                 (format t "[L] 🎯 Using pre-loaded ~a data for backtest (Speed++)~%" tf-str)
                 (gethash tf-str (gethash "USDJPY" *candle-histories-tf*)))
               nil))
         
         ;; Resample if no specific data found
         (target-candles (or tf-candles
                             (if (> timeframe 1) 
                                 (resample-candles candles timeframe) 
                                 candles)))
         (len (length target-candles))
         (msg nil)) ;; Initialize msg
    
    (format t "[L] 📊 Requesting backtest for ~a~a (Candles: ~d / TF: M~d)...~%" 
            (strategy-name strat) suffix len timeframe)
    
    ;; Construct JSON payload
    (setf msg (jsown:to-json 
                (jsown:new-js 
                  ("action" "BACKTEST")
                  ("strategy" (strategy-to-json strat :name-suffix suffix))
                  ("candles" (swimmy.main:candles-to-json target-candles)))))

    ;; Send to appropriate service
    (if (and (boundp '*backtest-requester*) *backtest-requester*)
        (progn
          (pzmq:send *backtest-requester* msg)
          (format t "[L] 📤 Sent ~d bytes to Backtest Service (TF: M~d)~%" (length msg) timeframe))
        (progn
          (format t "[L] ⚠️ Backtest Service unavailable, using legacy channel~%")
          (pzmq:send *cmd-publisher* msg)))))
;;; ══════════════════════════════════════════════════════════════════
;;;  WALK-FORWARD VALIDATION (López de Prado)
;;; ══════════════════════════════════════════════════════════════════

(defparameter *wfv-pending-strategies* (make-hash-table :test 'equal)
  "Stores strategies currently undergoing WFV. Key: strategy-name")

(defun start-walk-forward-validation (strat)
  "Initiate OOS validation for a strategy by splitting data."
  (unless (and *candle-history* (> (length *candle-history*) 100))
    (format t "[L] ⚠️ Not enough data for WFV~%")
    (return-from start-walk-forward-validation))

  (let* ((len (length *candle-history*))
         (split-idx (floor (* len 0.2))) ; Top 20% is OOS (Newest)
         ;; *candle-history* is Newest-First.
         ;; Index 0 to split-idx-1: Recent Data (OOS)
         ;; Index split-idx to End: Past Data (IS)
         (oos-candles (subseq *candle-history* 0 split-idx))
         (is-candles (subseq *candle-history* split-idx)))
    
    (format t "[L] 🚦 Starting WFV for ~a. IS: ~d bars, OOS: ~d bars~%" 
            (strategy-name strat) (length is-candles) (length oos-candles))
            
    ;; Register in pending table
    (setf (gethash (strategy-name strat) *wfv-pending-strategies*) 
          (list :is-result nil :oos-result nil :strategy strat))
    
    ;; Request IS Backtest
    (request-backtest strat :candles is-candles :suffix "_IS")
    ;; Request OOS Backtest
    (request-backtest strat :candles oos-candles :suffix "_OOS")))

(defun process-wfv-result (name-with-suffix result-map)
  "Process returning backtest results for WFV"
  (let* ((suffix-start (or (search "_IS" name-with-suffix :from-end t) 
                           (search "_OOS" name-with-suffix :from-end t)))
         (base-name (if suffix-start (subseq name-with-suffix 0 suffix-start) name-with-suffix))
         (type (if (search "_IS" name-with-suffix) :is-result :oos-result))
         (entry (gethash base-name *wfv-pending-strategies*)))
    
    (when entry
      (setf (getf entry type) result-map)
      (format t "[L] 📥 WFV Part Rcvd: ~a (~a) Sharpe: ~,2f~%" base-name type (getf result-map :sharpe))
      
      ;; Check if both parts are ready
      (when (and (getf entry :is-result) (getf entry :oos-result))
        (complete-wfv base-name entry)))))

(defun complete-wfv (base-name entry)
  "Compare IS and OOS performance and decide fate"
  (let* ((is-res (getf entry :is-result))
         (oos-res (getf entry :oos-result))
         (is-sharpe (getf is-res :sharpe))
         (oos-sharpe (getf oos-res :sharpe))
         (strat (getf entry :strategy))
         (degradation (if (> is-sharpe 0.1) 
                          (/ (- is-sharpe oos-sharpe) is-sharpe)
                          0.0))) ; Avoid div by zero
    
    (format t "[L] ⚖️ WFV VERDICT for ~a: IS=~,2f OOS=~,2f (Degradation: ~,1f%)~%"
            base-name is-sharpe oos-sharpe (* degradation 100))
            
    (cond
      ;; Scenario 1: Robust Strategy (OOS is good or at least positive)
      ((and (> oos-sharpe 0.5) (< degradation 0.5))
       (format t "[L] ✅ VALIDATED: ~a is robust! Promoted.~%" base-name)
       (notify-discord-alert (format nil "Valid Strategy: ~a (OOS Sharpe: ~,2f)" base-name oos-sharpe) :color 3066993)
       ;; Mark as validated (could set a flag on strategy struct)
       )
      
      ;; Scenario 2: Overfit (Good IS, Bad OOS)
      ((> degradation 0.7)
       (format t "[L] 🚮 OVERFIT: ~a discarded. (Good past, bad future)~%" base-name)
       ;; Remove from evolved strategies if present
       (setf *evolved-strategies* 
             (remove-if (lambda (s) (string= (strategy-name s) base-name)) *evolved-strategies*)))
       
      ;; Scenario 3: Garbage (Both bad)
      (t
       (format t "[L] 🗑️ WEAK: ~a discarded.~%" base-name)
       (setf *evolved-strategies* 
             (remove-if (lambda (s) (string= (strategy-name s) base-name)) *evolved-strategies*))))
    
    ;; Cleanup pending
    (remhash base-name *wfv-pending-strategies*)))

;; Clone check threshold (0.85 = 85% similar = reject, more diversity)
(defparameter *clone-threshold* 0.85)

;; Convert existing evolved strategies to JSON for clone check
(defun evolved-strategies-to-json ()
  "Convert evolved strategies list to JSON for clone check"
  (mapcar #'strategy-to-json *evolved-strategies*))

;; Request clone check from Rust
(defun request-clone-check (new-strat callback)
  "Check if new strategy is a clone of existing ones"
  (when (and *evolved-strategies* (> (length *evolved-strategies*) 0))
    (format t "[L] 🧬 Checking for clones...~%")
    (let ((msg (jsown:to-json 
                 (jsown:new-js 
                   ("action" "CHECK_CLONE")
                   ("new_strategy" (strategy-to-json new-strat))
                   ("existing_strategies" (evolved-strategies-to-json))
                   ("threshold" *clone-threshold*)))))
      (pzmq:send *cmd-publisher* msg))))

(defun dream-code ()
  "Generate strategy CODE with comprehensive self-analysis feedback"
  (format t "~%[L] ☁️ Dreaming CODE with Self-Analysis v2.0... ")
  (let* ((random-seed (random 1000))
         (self-analysis (get-structured-self-analysis))
         (failure-summary (get-failure-summary))
         (prompt (format nil "You are an AI trading strategy designer. Generate a NEW strategy based on this performance analysis.

~a

FAILURE SUMMARY: ~a

=== STRATEGY GENERATION RULES ===
1. AVOID patterns that have been failing
2. FOCUS on conditions that work well (high win rate)
3. Use creative parameter combinations NOT in top strategies
4. Adapt to current market conditions

Generate a strategy with:
- Unique creative name (3-8 letters)
- SMA short: 3-15 (pick based on volatility)
- SMA long: 20-100 (must be > short)
- SL: 0.05-0.5 (tighter in ranging, wider in trending)
- TP: 0.1-1.0 (adjust to regime)

Random seed for variety: ~d

Format EXACTLY:
(defstrategy \"NAME\" :indicators ((sma SHORT) (sma LONG)) :entry (cross-above sma-SHORT sma-LONG) :exit (cross-below sma-SHORT sma-LONG) :sl SL :tp TP :volume 0.01)

Output ONLY the s-expression. No markdown or explanation."
                         self-analysis failure-summary random-seed))
         (resp (swimmy.main:call-gemini prompt))
         (start (when resp (search "(defstrategy" resp)))
         (end (when start (find-balanced-end resp start))))
    ;; 括弧バランスチェック
    (when (and start end)
      (let* ((code (subseq resp start end))
             (opens (count #\( code))
             (closes (count #\) code)))
        (if (/= opens closes)
            (format t "~%[L] ⚠️ Unbalanced parens: ~d/~d~%" opens closes)
            (progn
              (format t "~%[L] 📝 Generated: ~a~%" (subseq code 0 (min 80 (length code))))
              (multiple-value-bind (result valid) (safe-eval-strategy code)
                (if valid
                    (progn
                      (format t "[L] ✅ Valid strategy!~%")
                      ;; V6.13: Always notify ALL symbol channels
                      ;; V6.14: Centralized Genome Notification (Alerts Channel)
                      (notify-discord-alert (format nil "🧬 CODE: ~a" (subseq code 0 (min 100 (length code)))) :color 65535)
                      ;; Check for clones if we have existing strategies
                      (if (and *evolved-strategies* (> (length *evolved-strategies*) 0))
                          (progn
                            (setf *pending-strategy* result)  ; Save for clone check result
                            (request-clone-check result nil))
                          ;; No existing strategies, just add it
                          (progn
                            (push result *evolved-strategies*)
                            (when (and *candle-history* (> (length *candle-history*) 50))
                              (request-backtest result)))))
                    (format t "[L] ❌ Invalid code~%")))))))))


;; Store pending strategy for clone check result
(defparameter *pending-strategy* nil)

;; ===== GENETIC PROGRAMMING =====

;; Mutation: randomly adjust parameters
(defun mutate-value (val min-v max-v mutation-rate)
  "Mutate a numeric value with given probability"
  (if (< (random 1.0) mutation-rate)
      (let ((delta (* (- (random 2.0) 1.0) (/ (- max-v min-v) 4))))
        (max min-v (min max-v (+ val delta))))
      val))

(defun mutate-strategy (strat &optional (rate 0.3))
  "Mutate strategy parameters"
  (multiple-value-bind (sma-s sma-l) (extract-sma-params (strategy-indicators strat))
    (let* ((new-sma-s (floor (mutate-value (or sma-s 5) 3 15 rate)))
           (new-sma-l (floor (mutate-value (or sma-l 20) 20 100 rate)))
           (new-sl (mutate-value (strategy-sl strat) 0.05 0.5 rate))
           (new-tp (mutate-value (strategy-tp strat) 0.1 1.0 rate))
           (new-name (format nil "~a-M~d" (strategy-name strat) (random 100))))
      ;; Ensure sma-l > sma-s
      (when (<= new-sma-l new-sma-s) (setf new-sma-l (+ new-sma-s 10)))
      (make-strategy :name new-name
                     :indicators (list (list 'sma new-sma-s) (list 'sma new-sma-l))
                     :entry (strategy-entry strat)
                     :exit (strategy-exit strat)
                     :sl new-sl :tp new-tp :volume (strategy-volume strat)))))

;; Crossover: combine two strategies (IMPROVED - preserves strategy diversity)
(defun crossover-strategies (parent1 parent2)
  "Create child strategy by combining two parents - preserves strategy structure"
  ;; Randomly pick which parent's indicators/entry/exit to use (preserves strategy type)
  (let* ((indicator-parent (if (zerop (random 2)) parent1 parent2))
         (logic-parent (if (zerop (random 2)) parent1 parent2))
         ;; Blend SL/TP from both parents
         (child-sl (/ (+ (strategy-sl parent1) (strategy-sl parent2)) 2))
         (child-tp (/ (+ (strategy-tp parent1) (strategy-tp parent2)) 2))
         (child-name (format nil "~a×~a" 
                             (subseq (strategy-name parent1) 0 (min 4 (length (strategy-name parent1))))
                             (subseq (strategy-name parent2) 0 (min 4 (length (strategy-name parent2)))))))
    ;; Add slight mutation to SL/TP (10% variance)
    (when (< (random 1.0) 0.3)
      (setf child-sl (* child-sl (+ 0.9 (random 0.2)))))
    (when (< (random 1.0) 0.3)
      (setf child-tp (* child-tp (+ 0.9 (random 0.2)))))
    (make-strategy :name child-name
                   :indicators (strategy-indicators indicator-parent)  ; Preserve parent's indicators
                   :entry (strategy-entry logic-parent)
                   :exit (strategy-exit logic-parent)
                   :sl (max 0.05 (min 0.5 child-sl))  ; Clamp to valid range
                   :tp (max 0.1 (min 1.0 child-tp))
                   :volume 0.01)))

;; Seed evolution pool from knowledge base (ALL 61 strategies!)
(defun seed-evolution-from-knowledge-base ()
  "Seed *evolved-strategies* with top strategies from each category"
  (when (boundp '*category-pools*)
    (let ((categories '(:trend :reversion :breakout :scalp))
          (count 0))
      (setf *evolved-strategies* nil)
      ;; Add top 3 from each category (12 total)
      (dolist (cat categories)
        (let ((pool (gethash cat *category-pools*)))
          (when pool
            (dolist (strat (subseq pool 0 (min 3 (length pool))))
              (push strat *evolved-strategies*)
              (incf count)))))
      (when (> count 0)
        (format t "[L] 🧬 EVOLUTION SEEDED with ~d strategies:~%" count)
        (dolist (s (subseq *evolved-strategies* 0 (min 5 (length *evolved-strategies*))))
          (format t "[L]    • ~a~%" (strategy-name s)))))))

;; Evolve population using genetic operations (with auto-seeding)
(defun evolve-population ()
  "Apply genetic operations - auto-seeds from 61 strategies if pool is empty"
  ;; Seed from knowledge base if pool is empty
  (when (< (length *evolved-strategies*) 2)
    (seed-evolution-from-knowledge-base))
  
  ;; V3.0: Use ecosystem recommendation (previously unused function!)
  (handler-case
      (let ((recommendation (get-ecosystem-recommendation)))
        (when recommendation
          (format t "[L] 🌿 Ecosystem: ~a (focus: ~a)~%" 
                  (getf recommendation :message) (getf recommendation :focus))))
    (error (e) (format t "[L] Ecosystem check error: ~a~%" e)))
  
  (when (>= (length *evolved-strategies*) 2)
    (format t "[L] 🧬 Evolving (~d strategies in pool)~%" (length *evolved-strategies*))
    (let* ((sorted (sort (copy-list *evolved-strategies*) #'> :key #'strategy-sharpe))
           (parent1 (first sorted))
           (parent2 (second sorted)))
      ;; Log which strategies are being crossed (VERBOSE)
      (format t "[L] 🧬 Parents: ~a × ~a~%" (strategy-name parent1) (strategy-name parent2))
      (format t "[L]    P1 indicators: ~a~%" (strategy-indicators parent1))
      (format t "[L]    P2 indicators: ~a~%" (strategy-indicators parent2))
      ;; Crossover
      (let ((child (crossover-strategies parent1 parent2)))
        (format t "[L] 🧬 Child: ~a (inherits: ~a)~%" 
                (strategy-name child) (strategy-indicators child))
        ;; Mutate the child
        (let ((mutant (mutate-strategy child 0.3)))
          (format t "[L] 🧬 Mutant: ~a~%" (strategy-name mutant))
          (setf *pending-strategy* mutant)
          (request-clone-check mutant nil)
          ;; V6.9: Persist evolution to disk
          (when (fboundp 'save-genome)
            (save-genome)
            (format t "[L] 💾 Genome saved to disk~%")))))))

(format t "[DREAMER2] Loaded (with Backtester + Clone Detection + Genetic Programming)~%")

;;; ══════════════════════════════════════════════════════════════════
;;;  VERBALIZED SAMPLING (ArXiv:2510.01171)
;;; ══════════════════════════════════════════════════════════════════
;;; Mitigate mode collapse and unlock diversity in strategy generation.
;;; Instead of generating one strategy, generate multiple with probabilities.
;;;
;;; Key insight: Post-training alignment causes "typicality bias" where 
;;; models favor familiar outputs. VS prompts model to verbalize probability
;;; distribution over responses, increasing diversity 1.6-2.1x.

(defparameter *vs-candidate-count* 5
  "Number of strategy candidates to generate with Verbalized Sampling")

(defparameter *vs-temperature-boost* 0.3
  "Temperature boost for diversity when using VS")

(defun generate-vs-prompt (strategy-type context)
  "Generate a Verbalized Sampling prompt for diverse strategy generation.
   Based on ArXiv:2510.01171 methodology."
  (format nil "You are an expert algorithmic trading strategist.

TASK: Generate ~d distinct trading strategies of type '~a' with their confidence probabilities.

CONTEXT:
- Current regime: ~a
- Recent performance: ~a
- Market volatility: ~a

OUTPUT FORMAT (JSON):
{
  \"strategies\": [
    {
      \"name\": \"Strategy-Name-1\",
      \"probability\": 0.35,
      \"indicators\": [[\"sma\", 5], [\"sma\", 20]],
      \"entry\": \"cross-above sma-5 sma-20\",
      \"exit\": \"cross-below sma-5 sma-20\",
      \"sl\": 0.50,
      \"tp\": 1.00,
      \"reasoning\": \"Why this strategy fits the context\"
    },
    ...
  ]
}

RULES:
1. Each strategy MUST be meaningfully different (not just parameter variations)
2. Probabilities MUST sum to 1.0
3. Higher probability = more confident this fits current conditions
4. Include at least one contrarian/unconventional approach (low probability but valuable diversity)
5. DO NOT repeat the same structure with minor changes

Generate ~d strategies now:"
          *vs-candidate-count*
          strategy-type
          (or (getf context :regime) "unknown")
          (or (getf context :performance) "neutral")
          (or (getf context :volatility) "normal")
          *vs-candidate-count*))

(defun parse-vs-response (response-json)
  "Parse Verbalized Sampling response into list of (strategy . probability) pairs"
  (handler-case
      (let* ((parsed (jsown:parse response-json))
             (strategies (jsown:val parsed "strategies")))
        (mapcar (lambda (s)
                  (cons (make-strategy 
                         :name (jsown:val s "name")
                         :indicators (mapcar (lambda (ind) 
                                              (list (intern (string-upcase (first ind)) :keyword)
                                                    (second ind)))
                                            (jsown:val s "indicators"))
                         :entry (jsown:val s "entry")
                         :exit (jsown:val s "exit")
                         :sl (jsown:val s "sl")
                         :tp (jsown:val s "tp")
                         :volume 0.01)
                        (jsown:val s "probability")))
                strategies))
    (error (e)
      (format t "[VS] Parse error: ~a~%" e)
      nil)))

(defun select-from-vs-distribution (candidates)
  "Select a strategy from VS candidates using weighted random sampling.
   This maintains diversity while respecting confidence scores."
  (when candidates
    (let* ((total-prob (reduce #'+ candidates :key #'cdr))
           (roll (random total-prob))
           (cumulative 0.0))
      (dolist (pair candidates)
        (incf cumulative (cdr pair))
        (when (>= cumulative roll)
          (return-from select-from-vs-distribution (car pair))))
      ;; Fallback to highest probability
      (car (first (sort (copy-list candidates) #'> :key #'cdr))))))

(defun vs-explore-diverse (candidates)
  "Deliberately select a LOW probability candidate for exploration.
   This combats mode collapse by occasionally trying unconventional strategies."
  (when (and candidates (> (length candidates) 2))
    (let ((sorted (sort (copy-list candidates) #'< :key #'cdr)))
      ;; Return bottom 20% candidate
      (car (nth (min 1 (1- (length sorted))) sorted)))))

(defun dream-with-vs (strategy-type &key (explore-p nil))
  "Generate strategy using Verbalized Sampling for diversity.
   If explore-p is T, deliberately choose unconventional strategy."
  (format t "[VS] 🎲 Verbalized Sampling for ~a (explore=~a)~%" strategy-type explore-p)
  (let* ((context (list :regime *current-regime*
                        :volatility *volatility-regime*
                        :performance (if *evolved-strategies*
                                        (format nil "top sharpe: ~,2f" 
                                                (strategy-sharpe (first *evolved-strategies*)))
                                        "no data")))
         (prompt (generate-vs-prompt strategy-type context)))
    ;; Note: Actual API call would go here. For now, using genetic fallback.
    (format t "[VS] 📝 Prompt generated (~d chars)~%" (length prompt))
    ;; Return prompt for external use or fallback to genetic
    prompt))

(defun vs-diversity-score (strategies)
  "Calculate diversity score for a set of strategies.
   Higher = more diverse (better for avoiding mode collapse)."
  (if (< (length strategies) 2)
      0.0
      (let* ((n (length strategies))
             (pairs (/ (* n (1- n)) 2))
             (total-diff 0.0))
        (loop for i from 0 below (1- n)
              do (loop for j from (1+ i) below n
                       do (let* ((s1 (nth i strategies))
                                 (s2 (nth j strategies))
                                 (sl-diff (abs (- (strategy-sl s1) (strategy-sl s2))))
                                 (tp-diff (abs (- (strategy-tp s1) (strategy-tp s2)))))
                            (incf total-diff (+ sl-diff tp-diff)))))
        (/ total-diff pairs))))

(format t "[VS] Verbalized Sampling loaded (ArXiv:2510.01171)~%")







