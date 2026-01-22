;;; school-backtest.lisp - Backtesting and Validation Logic
;;; Extracted from dreamer2.lisp

(in-package :swimmy.school)

;;; ==========================================
;;; UTILS
;;; ==========================================

(defvar *backtest-cache* (make-hash-table :test 'equal)
  "In-memory cache of backtest results. Key: STRATEGY-NAME, Value: RESULT-PLIST")

(defvar *backtest-cache-file* "data/backtest_cache.json"
  "Path to the persistent backtest cache file.")

(defvar *backtest-cache-validity* (* 24 3600)
  "Validity period for backtest results (24 hours).")

;;; ----------------------------------------------------------------------------
;;; Data Caching (Expert Panel 2026-01-16)
;;; ----------------------------------------------------------------------------
(defvar *sent-data-ids* (make-hash-table :test 'equal)
  "Tracks sent data IDs to avoid redundant transfer.")

(defun generate-data-id (candles &optional (suffix ""))
  "Generate ID for candle dataset."
  (if (and candles (listp candles))
      (let ((len (length candles))
            (first (car (last candles)))  ; Oldest (Newest is car)
            (last (first candles)))       ; Newest
        (format nil "DATA-~A-~A-~A~A" len 
                (if first (swimmy.engine:candle-timestamp first) 0)
                (if last (swimmy.engine:candle-timestamp last) 0)
                suffix))
      (format nil "EMPTY-~A" suffix)))

(defun send-zmq-msg (msg)
  "Helper to send ZMQ message with Throttling (Speed Demon Fix)"
  ;; V27: Throttle to prevent Guardian EOF (Rust Buffer Overflow)
  (sleep 0.005) 
  (if (and (boundp '*backtest-requester*) *backtest-requester*)
      (pzmq:send *backtest-requester* msg)
      (pzmq:send *cmd-publisher* msg)))

;;; ----------------------------------------------------------------------------
;;; Persistence (Expert Panel 2026-01-14)
;;; ----------------------------------------------------------------------------

(defun load-backtest-cache ()
  "Load backtest results from disk."
  (handler-case
      (when (probe-file *backtest-cache-file*)
        (let ((content (uiop:read-file-string *backtest-cache-file*)))
          (when (> (length content) 0)
            (let ((json (jsown:parse content)))
              (clrhash *backtest-cache*)
              (dolist (obj json)
                (let ((name (jsown:val obj "name"))
                      (timestamp (jsown:val obj "timestamp"))
                      (result (jsown:val obj "result")))
                  (setf (gethash name *backtest-cache*)
                        (list :timestamp timestamp :result result))))
              (format t "[BACKTEST] 📂 Loaded ~d cached results.~%" (hash-table-count *backtest-cache*))))))
    (error (e)
      (format t "[BACKTEST] ⚠️ Failed to load cache: ~a~%" e))))

(defun save-backtest-cache ()
  "Save backtest results to disk."
  (handler-case
      (let ((list nil))
        (maphash (lambda (k v)
                   (push (jsown:new-js
                           ("name" k)
                           ("timestamp" (getf v :timestamp))
                           ("result" (getf v :result)))
                         list))
                 *backtest-cache*)
        (ensure-directories-exist *backtest-cache-file*)
        (with-open-file (stream *backtest-cache-file*
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-string (jsown:to-json list) stream))
        (format t "[BACKTEST] 💾 Saved ~d results to cache.~%" (length list)))
    (error (e)
      (format t "[BACKTEST] ❌ Failed to save cache: ~a~%" e))))

(defun cache-backtest-result (strategy-name result)
  "Cache a backtest result."
  (setf (gethash strategy-name *backtest-cache*)
        (list :timestamp (get-universal-time)
              :result result))
  ;; Save on every update (low volume)
  (save-backtest-cache))

(defun get-cached-backtest (strategy-name)
  "Get a valid cached backtest result. Returns NIL if expired or missing."
  (let ((cached (gethash strategy-name *backtest-cache*)))
    (when cached
      (let ((timestamp (getf cached :timestamp))
            (now (get-universal-time)))
        (if (< (- now timestamp) *backtest-cache-validity*)
            (getf cached :result)
            (progn
              ;; remhash? No, keep logic simple.
              nil))))))


(defun initialize-backtest-system ()
  "Initialize the backtest system."
  (load-backtest-cache))

;; V9.1: Fix Undefined Function (Expert Panel 2026-01-17)
(defun candles-to-json (candles)
  "Convert list of candle structs to JSON-friendly list of plists."
  (mapcar (lambda (c)
            `((time . ,(candle-timestamp c))
              (open . ,(candle-open c))
              (high . ,(candle-high c))
              (low . ,(candle-low c))
              (close . ,(candle-close c))
              (volume . ,(candle-volume c))))
          candles))

(defun candles-to-alist (candles)
  "Convert list of candle structs to alist for S-Expression protocol."
  (candles-to-json candles)) ; Same structure for alists



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
        ;; Only collect INTEGER parameters (filter out floats like BB deviation 2.0)
        (let ((nums (remove-if-not (lambda (n) (and (numberp n) (integerp n))) (cdr ind))))
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

;; V7.13: Detect indicator type from strategy indicators (PROPERLY)
(defun detect-indicator-type (indicators)
  "Detect primary indicator type from indicators list for Rust backtester.
   Returns a string or nil."
  (when (and indicators (listp indicators))
    (let ((first-ind (if (listp (car indicators)) (caar indicators) (car indicators))))
      (cond
        ((member first-ind '(rsi RSI :rsi)) "rsi")
        ((member first-ind '(bb BB :bb bollinger)) "bb")
        ((member first-ind '(macd MACD :macd)) "macd")
        ((member first-ind '(stoch STOCH :stoch stochastic)) "stoch")
        (t "sma")))))

;; V7.13: Convert strategy to Alist for S-Expression Protocol
(defun strategy-to-alist (strat &key (name-suffix ""))
  "Convert strategy struct to an alist for S-Expression communication with Guardian."
  (multiple-value-bind (sma-short sma-long) 
      (extract-sma-params (strategy-indicators strat))
    `((name . ,(format nil "~a~a" (strategy-name strat) name-suffix))
      (sma_short . ,(or sma-short 5))
      (sma_long . ,(or sma-long 20))
      (sl . ,(or (strategy-sl strat) 0.0))
      (tp . ,(or (strategy-tp strat) 0.0))
      (volume . ,(or (strategy-volume strat) 0.01))
      ,@(let ((type (detect-indicator-type (strategy-indicators strat))))
          (if type `((indicator_type . ,type)) nil))
      (timeframe . ,(or (strategy-timeframe strat) 1))
      (filter_enabled . ,(if (and (slot-exists-p strat 'filter-enabled) (strategy-filter-enabled strat)) t nil))
      (filter_tf . ,(if (slot-exists-p strat 'filter-tf) (or (strategy-filter-tf strat) "") ""))
      (filter_period . ,(if (slot-exists-p strat 'filter-period) (or (strategy-filter-period strat) 0) 0))
      (filter_logic . ,(if (slot-exists-p strat 'filter-logic) (format nil "~a" (strategy-filter-logic strat)) "")))))


;; Helper to resample candles (M1 -> M5, etc)
;; WARN: CRITICAL DATA ORDER ASSUMPTION (Recurrence Prevention)
;; Input 'candles' MUST be Newest-First (Time: T_n, T_n-1, ... T_1).
(defun resample-candles (candles factor)
  (let ((result nil)
        (chunk nil)
        (count 0))
    (dolist (c candles)
       (push c chunk)
       (incf count)
       (when (= count factor)
         ;; Input candles are Newest-First (M5 M4 ...)
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
    (nreverse result)))

;;; ==========================================
;;; TICK REPLAY (Phase 12)
;;; ==========================================

(defstruct tick-lite timestamp bid ask)

(defun interpolate-ticks (candle)
  "Generate 4 synthetic ticks from a single candle (OHLC).
   Pattern depends on candle direction.
   - Bullish: Open -> Low -> High -> Close
   - Bearish: Open -> High -> Low -> Close
   Timestamps distributed: :00, :15, :45, :59"
  (let* ((ts (candle-timestamp candle))
         (o (candle-open candle))
         (h (candle-high candle))
         (l (candle-low candle))
         (c (candle-close candle))
         (ticks nil))
    
    (if (>= c o)
        ;; Bullish (O -> L -> H -> C)
        (setf ticks (list (make-tick-lite :timestamp ts :bid o :ask (+ o 0.01))
                          (make-tick-lite :timestamp (+ ts 15) :bid l :ask (+ l 0.01))
                          (make-tick-lite :timestamp (+ ts 45) :bid h :ask (+ h 0.01))
                          (make-tick-lite :timestamp (+ ts 59) :bid c :ask (+ c 0.01))))
        ;; Bearish (O -> H -> L -> C)
        (setf ticks (list (make-tick-lite :timestamp ts :bid o :ask (+ o 0.01))
                          (make-tick-lite :timestamp (+ ts 15) :bid h :ask (+ h 0.01))
                          (make-tick-lite :timestamp (+ ts 45) :bid l :ask (+ l 0.01))
                          (make-tick-lite :timestamp (+ ts 59) :bid c :ask (+ c 0.01)))))
    ticks))

(defun generate-tick-stream (candles)
  "Convert a list of candles into a continuous stream of interpolated ticks.
   Candles input should be Oldest-First for correct time sequence."
  (let ((stream nil))
    (dolist (c candles)
      (dolist (tick (interpolate-ticks c))
        (push tick stream)))
    (nreverse stream)))

;; Request backtest from Rust
;; Helper to convert numeric timeframe to string suffix
(defun get-tf-string (tf)
  "Convert a numeric timeframe (in minutes) or a timeframe string to a valid string.
   Returns the input if it's already a string (e.g., 'H1')."
  (cond ((stringp tf) tf) ; Already a string, return it
        ((not (numberp tf)) "M1") ; Not a number, default to M1
        ((= tf 1) "M1")
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
(defun request-backtest (strat &key (candles *candle-history*) (suffix "") (symbol nil))
  "Send strategy to Rust for high-speed backtesting. Resamples based on strategy's timeframe.
   Uses strategy's native symbol if not overridden."
  (let ((actual-symbol (or symbol (strategy-symbol strat) "USDJPY")))
  
  ;; V8.0: Multi-Timeframe Logic
  ;; Strategy decides its own destiny (timeframe)
  (let* ((tf-slot (if (slot-exists-p strat 'timeframe) (strategy-timeframe strat) 1))
         (timeframe (if (numberp tf-slot) tf-slot 1))
         (tf-str (get-tf-string timeframe))
         
         ;; P5.1: Use raw candles (M1) for data_id to ensure cache hits
         (target-candles candles)
         (len (length target-candles))
         (aux-candles-ht (make-hash-table :test 'equal))  ;; Initialize aux candles as hash table
         (msg nil)) ;; Initialize msg
    
    ;; V8.10: Fetch Aux Candles for MTF
    (when (and (slot-exists-p strat 'filter-enabled)
               (strategy-filter-enabled strat)
               (strategy-filter-tf strat))
      (let* ((ftf (strategy-filter-tf strat))
             (ftf-str (cond ((stringp ftf) ftf)
                            ((numberp ftf) (get-tf-string ftf)) 
                            (t "D1")))
             ;; FIXME: Symbol hardcoded to USDJPY in history logic too?
             ;; Ideally we fetch specific symbol history but for now we rely on *candle-histories-tf*
             (sym-hist (if (and (boundp '*candle-histories-tf*) *candle-histories-tf*)
                              (gethash symbol *candle-histories-tf*)
                              nil))
             (hist (if sym-hist
                       (gethash ftf-str sym-hist)
                       nil)))
        (when hist
           (format t "[L] 🧩 Attaching ~a Filter Data (~d bars)~%" ftf-str (length hist))
           (setf (gethash ftf-str aux-candles-ht) (candles-to-json hist)))))

    (format t "[L] 📊 Requesting backtest for ~a on ~a~a (Candles: ~d / TF: M~d)...~%" 
            (strategy-name strat) actual-symbol suffix len timeframe)
    
    (let* ((data-file (format nil "/home/swimmy/swimmy/data/historical/~a_M1.csv" actual-symbol))
           (use-file (and (string= suffix "") (probe-file data-file))))

      (if use-file
          (progn
             (format t "[L] 🚀 Using Direct CSV: ~a~%" data-file)
             (let* ((request `((action . "BACKTEST")
                               (strategy . ,(strategy-to-alist strat :name-suffix suffix))
                               (candles_file . ,data-file)
                               (symbol . ,actual-symbol)
                               (timeframe . ,timeframe)))
                    (msg (prin1-to-string request)))
               (send-zmq-msg msg)))

          ;; Fallback / WFV / In-Memory
          (let* ((base-id (generate-data-id target-candles (format nil "-~a" actual-symbol)))
                 (ftf-str (if (and (slot-exists-p strat 'filter-enabled)
                                   (strategy-filter-enabled strat)
                                   (strategy-filter-tf strat))
                              (get-tf-string (strategy-filter-tf strat))
                              "NONE"))
                 (data-id (format nil "~a-AUX~a-~a" base-id ftf-str suffix)))
                 
            ;; CACHE MISS: Send Data
            (unless (gethash data-id *sent-data-ids*)
              (format t "[L] 💾 Caching Data ID: ~a~%" data-id)
              (let* ((cache-request `((action . "CACHE_DATA")
                                      (data_id . ,data-id)
                                      (candles . ,(candles-to-alist target-candles))
                                      (aux_candles . ,(loop for k being the hash-keys of aux-candles-ht
                                                           using (hash-value v)
                                                           collect (cons k v)))))
                     (cache-msg (prin1-to-string cache-request)))
                (send-zmq-msg cache-msg)
                (setf (gethash data-id *sent-data-ids*) t)))
                
            ;; BACKTEST REQUEST
            (let* ((request `((action . "BACKTEST")
                              (strategy . ,(strategy-to-alist strat :name-suffix suffix))
                              (data_id . ,data-id)
                              (timeframe . ,timeframe)))
                   (msg (prin1-to-string request)))
              (send-zmq-msg msg)
              (format t "[L] 📤 Sent Backtest Request (ID: ~a / TF: M~d)~%" data-id timeframe))))))))

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
        (base 0.0) ;; Relaxed from 0.1 to 0.0 (The Laxative)
        (step 0.05))
    ;; Gen0: 0.0
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
       (notify-discord-alert (format nil "Valid Strategy: ~a (Gen ~d | OOS: ~,2f)" base-name gen oos-sharpe) :color 3066993)
       ;; V17c: Persist Promotion to Battlefield
       (swimmy.persistence:move-strategy strat :battlefield))
      
      ;; Scenario 2: Overfit (Good IS, Bad OOS)
      ((> degradation 0.7)
       (format t "[L] 🚮 OVERFIT: ~a discarded. (Good past, bad future)~%" base-name)
       (setf *evolved-strategies* 
             (remove-if (lambda (s) (string= (strategy-name s) base-name)) *evolved-strategies*))
       ;; V17c: Move to Graveyard
       (swimmy.persistence:move-strategy strat :graveyard))
       
      ;; Scenario 3: Weak (OOS < Required)
      (t
       (format t "[L] 🗑️ WEAK: ~a (OOS ~,2f < ~,2f) discarded.~%" base-name oos-sharpe required-oos)
       (setf *evolved-strategies* 
             (remove-if (lambda (s) (string= (strategy-name s) base-name)) *evolved-strategies*))
       ;; V17c: Move to Graveyard
       (setf *evolved-strategies* 
             (remove-if (lambda (s) (string= (strategy-name s) base-name)) *evolved-strategies*))
       ;; V17c: Move to Graveyard
       (swimmy.persistence:move-strategy strat :graveyard)))
    
    ;; Cleanup pending
    (remhash base-name *wfv-pending-strategies*)))

;; Clone check threshold (0.85 = 85% similar = reject, more diversity)
(defparameter *clone-threshold* 0.85)

;; V8.11: Pending Clone Checks Table (Async handling)
;; Key: Candidate Name, Value: Strategy Object
(defparameter *pending-clone-checks* (make-hash-table :test 'equal))

;; Track consecutive clones for stagnation warning
(defparameter *consecutive-clone-count* 0)

;; Convert existing evolved strategies to JSON for clone check
(defun evolved-strategies-to-json ()
  "Convert evolved strategies list to JSON for clone check"
  (mapcar #'strategy-to-json *evolved-strategies*))

;; Request clone check from Rust
(defun request-clone-check (new-strat callback)
  "Check if new strategy is a clone of existing ones"
  (declare (ignore callback)) ; Callback handled async via process-clone-check-result
  (when (and *evolved-strategies* (> (length *evolved-strategies*) 0))
    (format t "[L] 🧬 Checking for clones: ~a...~%" (strategy-name new-strat))
    
    ;; Register in pending table
    (setf (gethash (strategy-name new-strat) *pending-clone-checks*) new-strat)
    
    (let ((msg (jsown:to-json 
                 (jsown:new-js 
                   ("action" "CHECK_CLONE")
                   ("new_strategy" (strategy-to-json new-strat))
                   ("existing_strategies" (evolved-strategies-to-json))
                   ("threshold" *clone-threshold*)))))
      (pzmq:send *cmd-publisher* msg))))

(defun process-clone-check-result (candidate-name is-clone similar sim)
  "Handle async clone check result from Guardian"
  (let ((strat (gethash candidate-name *pending-clone-checks*)))
    (if strat
        (progn
          ;; Remove from pending
          (remhash candidate-name *pending-clone-checks*)
          
          (if is-clone
              (progn
                (incf *consecutive-clone-count*)
                (format t "[L] 🚫 CLONE REJECTED: ~a is ~d% similar to ~a (Streak: ~d)~%" 
                        candidate-name (round (* sim 100)) similar *consecutive-clone-count*)
                
                ;; Stagnation Warning (Every 10 clones)
                (when (zerop (mod *consecutive-clone-count* 10))
                  (notify-discord-alert 
                    (format nil "⚠️ Evolution Stagnation: ~d consecutive clones rejected." *consecutive-clone-count*)
                    :color 16776960)))
              
              (progn
                ;; UNIQUE STRATEGY FOUND!
                (setf *consecutive-clone-count* 0)
                (format t "[L] ✅ UNIQUE: ~a (Max Sim: ~d%) - Promoted to Backtest!~%" 
                        candidate-name (round (* sim 100)))
                
                ;; 1. Add to population
                (push strat *evolved-strategies*)
                
                ;; 2. Trigger Backtest (notification happens after backtest)
                (request-backtest strat))))
        
        (format t "[L] ⚠️ Received clone result for unknown candidate: ~a~%" candidate-name))))

;;; OOS Validation functions moved to school-validation.lisp (P10 SRP)

(defun prune-to-graveyard (strat reason)
  "V48.0 Fail Path: Move strategies to Graveyard."
  (let ((old-tier (strategy-tier strat))
        (old-rank (strategy-rank strat)))
    (setf (strategy-tier strat) :graveyard)
    (setf (strategy-rank strat) :graveyard)  ;; V48.0: Also set rank for Evolution Report
    (setf (strategy-status strat) :inactive)
    (setf (strategy-status-reason strat) reason)
    ;; Remove from active pools if present
    (let ((cat (categorize-strategy strat)))
      (setf (gethash cat *category-pools*) 
            (remove (strategy-name strat) (gethash cat *category-pools*) :key #'strategy-name :test #'string=)))
    
    (format t "[PRUNER] 🪦 GRAVEYARD: ~a (~a -> :graveyard) | Reason: ~a~%" 
            (strategy-name strat) old-tier reason)
    
    ;; Persist change (V47.8: immediate save for safety)
    (swimmy.persistence:save-strategy strat)))

(defun run-qualification-cycle ()
  "V47.8 Phase 3: Loop through strategies needing backtest."
  (format t "[QUALIFY] 🏁 Starting Qualification Cycle...~%")
  (let ((candidates (remove-if-not 
                     (lambda (s) 
                       (and (member (strategy-tier s) '(:incubator :scout))
                            (= (or (strategy-trades s) 0) 0)
                            (= (or (strategy-sharpe s) 0.0) 0.0)))
                     *strategy-knowledge-base*))
        (limit 50)
        (count 0))
    
    (format t "[QUALIFY] found ~d candidates.~%" (length candidates))
    
    (dolist (strat candidates)
      (when (< count limit)
        (incf count)
        ;; Request backtest (no suffix = standard phase 1)
        (request-backtest strat)
        ;; Sleep purely to not flood ZMQ buffer instantly
        (sleep 0.01)))
    
    (format t "[QUALIFY] 📤 Sent ~d backtest requests.~%" count)))

(defun init-backtest-zmq ()
  "Initialize ZMQ connection to backtest service for the school daemon.
   V47.8 Fix: Prevents crash when calling request-backtest in isolated service."
  (unless (and (boundp 'swimmy.globals:*backtest-requester*) swimmy.globals:*backtest-requester*)
    (handler-case
        (let* ((ctx (pzmq:ctx-new))
               (req (pzmq:socket ctx :pub)))
          (pzmq:connect req "tcp://localhost:5559")
          (setf swimmy.globals:*backtest-requester* req)
          (format t "[BACKTEST] 🔌 Connected to Guardian Service (PUB -> 5559)~%"))
      (error (e)
        (format t "[BACKTEST] ❌ Failed to initialize ZMQ: ~a~%" e)))))
