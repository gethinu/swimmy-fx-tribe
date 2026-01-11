;;; school-backtest.lisp - Backtesting and Validation Logic
;;; Extracted from dreamer2.lisp

(in-package :swimmy.school)

;;; ==========================================
;;; UTILS
;;; ==========================================

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

(defun calculate-required-oos-sharpe (gen)
  "Calculate required OOS Sharpe Ratio based on generation number.
   López de Prado Rule: Higher generation = Higher burden of proof for overfitting."
  (let ((g (or gen 0))
        (base 0.1)
        (step 0.05))
    ;; Gen0: 0.1
    ;; Gen10: 0.6
    ;; Gen20: 1.1
    (min 2.0 (+ base (* g step)))))

(defun complete-wfv (base-name entry)
  "Compare IS and OOS performance and decide fate"
  (let* ((is-res (getf entry :is-result))
         (oos-res (getf entry :oos-result))
         (is-sharpe (getf is-res :sharpe))
         (oos-sharpe (getf oos-res :sharpe))
         (strat (getf entry :strategy))
         (gen (if (slot-exists-p strat 'generation) (strategy-generation strat) 0))
         (required-oos (calculate-required-oos-sharpe gen))
         (degradation (if (> is-sharpe 0.1) 
                          (/ (- is-sharpe oos-sharpe) is-sharpe)
                          0.0))) ; Avoid div by zero
    
    (format t "[L] ⚖️ WFV VERDICT for ~a (Gen ~d): IS=~,2f OOS=~,2f (Req: ~,2f) Degradation: ~,1f%~%"
            base-name gen is-sharpe oos-sharpe required-oos (* degradation 100))
            
    (cond
      ;; Scenario 1: Robust Strategy (OOS > Threshold AND Degradation < 50%)
      ((and (> oos-sharpe required-oos) (< degradation 0.5))
       (format t "[L] ✅ VALIDATED: ~a is robust! Promoted.~%" base-name)
       (notify-discord-alert (format nil "Valid Strategy: ~a (Gen ~d | OOS: ~,2f)" base-name gen oos-sharpe) :color 3066993))
      
      ;; Scenario 2: Overfit (Good IS, Bad OOS)
      ((> degradation 0.7)
       (format t "[L] 🚮 OVERFIT: ~a discarded. (Good past, bad future)~%" base-name)
       (setf *evolved-strategies* 
             (remove-if (lambda (s) (string= (strategy-name s) base-name)) *evolved-strategies*)))
       
      ;; Scenario 3: Weak (OOS < Required)
      (t
       (format t "[L] 🗑️ WEAK: ~a (OOS ~,2f < ~,2f) discarded.~%" base-name oos-sharpe required-oos)
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
