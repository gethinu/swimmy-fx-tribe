;;; school-backtest.lisp - Backtesting and Validation Logic
;;; Extracted from dreamer2.lisp

(in-package :swimmy.school)

;;; ==========================================
;;; UTILS
;;; ==========================================

;;; [UTILS AND PERSISTENCE MOVED TO school-backtest-utils.lisp]

(defun initialize-backtest-system ()
  "Initialize the backtest system."
  (swimmy.school::load-backtest-cache))

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
   Returns a symbol or nil."
  (when (and indicators (listp indicators))
    (let ((first-ind (if (listp (car indicators)) (caar indicators) (car indicators))))
      (cond
        ((member first-ind '(rsi RSI :rsi)) 'rsi)
        ((member first-ind '(bb BB :bb bollinger)) 'bb)
        ((member first-ind '(macd MACD :macd)) 'macd)
        ((member first-ind '(stoch STOCH :stoch stochastic)) 'stoch)
        (t 'sma)))))

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
      ,@(when (and (slot-exists-p strat 'filter-enabled) (strategy-filter-enabled strat))
          '((filter_enabled . t)))
      (filter_tf . ,(if (slot-exists-p strat 'filter-tf) (or (strategy-filter-tf strat) "") ""))
      (filter_period . ,(if (slot-exists-p strat 'filter-period) (or (strategy-filter-period strat) 0) 0))
      (filter_logic . ,(if (slot-exists-p strat 'filter-logic) (format nil "~a" (strategy-filter-logic strat)) "")))))

(defun strategy-to-json (strat &key (name-suffix ""))
  "Convert strategy struct to a jsown JSON object."
  (alist-to-json (strategy-to-alist strat :name-suffix name-suffix)))


;; Helper to resample M1 candles to arbitrary minutes (aligned buckets).
;; Input 'candles' MUST be Newest-First (same as Data Keeper responses).
;; This implementation aligns to Unix bucket boundaries, matching Guardian's resampler.
(defun resample-candles (candles minutes)
  "Resample M1 candles into MINUTES candles (newest-first), aligned to unix bucket start.

MINUTES is an integer number of minutes (e.g., 5, 60, 240, 3600)."
  (let ((m (if (numberp minutes) (round minutes) 1)))
    (cond
      ((or (null candles) (<= m 1)) candles)
      (t
       (let* ((tf-seconds (* m 60))
              ;; We scan oldest->newest to compute correct open/close, but return newest-first.
              (ordered (reverse candles))
              (result nil)
              (current-bucket-start nil)
              (open 0.0)
              (high most-negative-double-float)
              (low most-positive-double-float)
              (close 0.0)
              (vol 0.0)
              (has-data nil))
         (dolist (c ordered)
           (let* ((ts (candle-timestamp c))
                  (bucket-start (* (floor ts tf-seconds) tf-seconds)))
             (if (or (null current-bucket-start)
                     (/= bucket-start current-bucket-start))
                 (progn
                   (when has-data
                     (push (make-candle :timestamp current-bucket-start
                                        :open open
                                        :high high
                                        :low low
                                        :close close
                                        :volume vol)
                           result))
                   (setf current-bucket-start bucket-start
                         open (candle-open c)
                         high (candle-high c)
                         low (candle-low c)
                         close (candle-close c)
                         vol (candle-volume c)
                         has-data t))
                 (progn
                   (setf high (max high (candle-high c)))
                   (setf low (min low (candle-low c)))
                   (setf close (candle-close c))
                   (setf vol (+ vol (candle-volume c)))))))
         (when has-data
           (push (make-candle :timestamp current-bucket-start
                              :open open
                              :high high
                              :low low
                              :close close
                              :volume vol)
                 result))
         result)))))

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
  (cond
    ((stringp tf) (string-upcase tf))
    ((not (numberp tf)) "M1")
    (t
     (let ((m (round tf)))
       (cond
         ((<= m 1) "M1")
         ((= m 43200) "MN")
         ((and (>= m 10080) (zerop (mod m 10080))) (format nil "W~d" (/ m 10080)))
         ((and (>= m 1440) (zerop (mod m 1440))) (format nil "D~d" (/ m 1440)))
         ((and (>= m 60) (zerop (mod m 60))) (format nil "H~d" (/ m 60)))
         (t (format nil "M~d" m)))))))

(defun get-tf-minutes (tf)
  "Convert a numeric timeframe or a timeframe string to total minutes.
   Handles 'M1', 'M5', 'H1', 'H4', 'D1', 'W1', 'MN'."
  (labels ((all-digits-p (s)
             (and (stringp s)
                  (> (length s) 0)
                  (loop for ch across s always (digit-char-p ch))))
           (parse-int (s default)
             (handler-case (parse-integer s) (error () default))))
    (cond
      ((numberp tf) (round tf))
      ((stringp tf)
       (let* ((up (string-upcase tf)))
         (cond
           ((or (string= up "MN") (string= up "MN1")) 43200)
           ((and (>= (length up) 2) (char= (char up 0) #\M)
                 (all-digits-p (subseq up 1)))
            (parse-int (subseq up 1) 1))
           ((and (>= (length up) 2) (char= (char up 0) #\H)
                 (all-digits-p (subseq up 1)))
            (* 60 (parse-int (subseq up 1) 1)))
           ((and (>= (length up) 2) (char= (char up 0) #\D)
                 (all-digits-p (subseq up 1)))
            (* 1440 (parse-int (subseq up 1) 1)))
           ((and (>= (length up) 2) (char= (char up 0) #\W)
                 (all-digits-p (subseq up 1)))
            (* 10080 (parse-int (subseq up 1) 1)))
           ;; Legacy fixed labels
           ((string= up "M1") 1)
           ((string= up "M5") 5)
           ((string= up "M15") 15)
           ((string= up "M30") 30)
           ((string= up "H1") 60)
           ((string= up "H4") 240)
           ((string= up "D1") 1440)
           ((string= up "W1") 10080)
           (t 1))))
      (t 1))))

;; Finite timeframe buckets for category/correlation scoping.
;; We keep internal TF as minutes(int) per-strategy, but categories must remain bounded
;; even when exploring arbitrary TFs (M36/H2/H5/H60...).
(defparameter *tf-scope-buckets-minutes* '(5 15 30 60 240 1440 10080 43200)
  "Finite TF buckets (minutes) for category/correlation scoping.
M1 and other sub-5m timeframes are bucketed to M5.")

(defun get-tf-bucket-minutes (tf)
  "Normalize TF (minutes or label) into one of *tf-scope-buckets-minutes*."
  (let* ((m (get-tf-minutes tf))
         (m (if (and (numberp m) (> m 0)) (round m) 5)))
    (cond
      ((<= m 5) 5)
      (t
       (let ((best nil)
             (best-diff nil))
         (dolist (b *tf-scope-buckets-minutes*
                    (or best 5))
           (let ((d (abs (- m b))))
             ;; Tie-breaker: prefer larger bucket (more conservative scope).
             (when (or (null best-diff)
                       (< d best-diff)
                       (and (= d best-diff) (or (null best) (> b best))))
               (setf best b
                     best-diff d)))))))))

;; Bounded TF exploration set (minutes). Categories/correlation are bucketed separately via get-tf-bucket-minutes.
(defparameter *tf-mining-candidates-minutes*
  '(6 10 12 20 36 45 90 120 180 210 300 360 420 510 720 2880 3600)
  "Extra TF minutes to mine beyond the default TF buckets.
Keep this list bounded to avoid exploding the search space/data volume.")

(defparameter *tf-mutation-anchor-minutes* '(300 3600)
  "Proven non-standard TFs to keep in the mutation pool even under tight budget.")

(defparameter *tf-mutation-budget-mode* :auto
  "TF mutation pool budget mode.
 :AUTO = shrink options when backtest pending pressure is high.
 :FULL = always return full option set.
 INTEGER = explicit max option count (floored by core+anchor set).")

(defparameter *tf-mutation-budget-medium-ratio* 0.60d0
  "When pending/max-pending >= this, trim TF mutation options to medium budget.")

(defparameter *tf-mutation-budget-high-ratio* 0.80d0
  "When pending/max-pending >= this, trim TF mutation options to high-pressure budget.")

(defparameter *tf-mutation-budget-critical-ratio* 1.00d0
  "When pending/max-pending >= this, keep only core+anchor TF options.")

(defun %tf-mutation-priority-seed (all-options)
  "Prioritized seed list: 8 core buckets + proven anchors."
  (remove-duplicates
   (remove-if-not (lambda (m) (member m all-options :test #'eql))
                  (append *tf-scope-buckets-minutes* *tf-mutation-anchor-minutes*))
   :test #'eql))

(defun %resolve-tf-mutation-budget-size (all-options)
  "Resolve max TF option count based on budget mode and backtest pressure."
  (let* ((full (length all-options))
         (seed (length (%tf-mutation-priority-seed all-options)))
         (floor (min full (max 1 seed)))
         (mode *tf-mutation-budget-mode*))
    (cond
      ((or (eq mode :full) (eq mode :FULL)) full)
      ((and (integerp mode) (> mode 0))
       (max floor (min full mode)))
      (t
       (let* ((pending (if (fboundp 'backtest-pending-count)
                           (backtest-pending-count)
                           0))
              (max-pending (if (and (boundp 'swimmy.globals::*backtest-max-pending*)
                                    (numberp swimmy.globals::*backtest-max-pending*)
                                    (> swimmy.globals::*backtest-max-pending* 0))
                               swimmy.globals::*backtest-max-pending*
                               1))
              (ratio (/ (float pending 1.0d0)
                        (float max-pending 1.0d0))))
         (cond
           ((>= ratio *tf-mutation-budget-critical-ratio*)
            floor)
           ((>= ratio *tf-mutation-budget-high-ratio*)
            (max floor (min full (round (* full 0.50d0)))))
           ((>= ratio *tf-mutation-budget-medium-ratio*)
            (max floor (min full (round (* full 0.75d0)))))
           (t
            full)))))))

(defun get-tf-mutation-options ()
  "Return bounded TF minutes list for evolution/LLM timeframe exploration.
Budget is auto-shrunk under backtest pending pressure."
  (let* ((all-options (sort (remove-duplicates
                             (append *tf-scope-buckets-minutes*
                                     *tf-mining-candidates-minutes*)
                             :test #'eql)
                            #'<))
         (budget (%resolve-tf-mutation-budget-size all-options)))
    (if (>= budget (length all-options))
        all-options
        (let* ((seed (%tf-mutation-priority-seed all-options))
               (rest (remove-if (lambda (m) (member m seed :test #'eql))
                                all-options))
               (prioritized (append seed rest)))
          (subseq prioritized 0 (min budget (length prioritized)))))))


;; Request backtest from Rust
(defun %option-some (key value)
  "Encode Option<T> Some(value) for S-expression payloads."
  (list key value))

(defun request-backtest (strat &key (candles *candle-history*) (suffix "") (symbol nil) (request-id nil)
                               include-trades start-ts end-ts)
  "Send strategy to Rust for high-speed backtesting. Resamples based on strategy's timeframe.
   Uses strategy's native symbol if not overridden.
   When INCLUDE-TRADES is true, requests per-trade pnl `trade_list` in the result."
  (let* ((req-id (or request-id (swimmy.core::generate-uuid)))
         (actual-symbol (or symbol (strategy-symbol strat) "USDJPY"))
         (start-time (and (numberp start-ts) (truncate start-ts)))
         (end-time (and (numberp end-ts) (truncate end-ts))))
    (setf swimmy.globals:*backtest-submit-last-id* req-id)

    ;; V8.0: Multi-Timeframe Logic - strategy decides its own destiny (timeframe)
    (let* ((tf-slot (if (slot-exists-p strat 'timeframe) (strategy-timeframe strat) 1))
           (timeframe (get-tf-minutes tf-slot))
           ;; P5.1: Use raw candles (M1) for data_id to ensure cache hits
           (target-candles candles)
           (len (length target-candles))
           (aux-candles-ht (make-hash-table :test 'equal))
           (override swimmy.core::*backtest-csv-override*)
           (data-file (if (and override (> (length override) 0))
                          override
                          (format nil "~a"
                                  (swimmy.core::swimmy-path
                                   (format nil "data/historical/~a_M1.csv" actual-symbol)))))
           (wfv-suffix (or (search "_IS" suffix) (search "_OOS" suffix)))
           (use-file (and (probe-file data-file) (not wfv-suffix)))
           (strategy-alist (strategy-to-alist strat :name-suffix suffix))
           (msg nil))

      ;; V8.10: Fetch Aux Candles for MTF (optional)
      (when (and (slot-exists-p strat 'filter-enabled)
                 (strategy-filter-enabled strat)
                 (strategy-filter-tf strat))
        (let* ((ftf (strategy-filter-tf strat))
               (ftf-str (cond ((stringp ftf) ftf)
                              ((numberp ftf) (get-tf-string ftf))
                              (t "D1")))
               (sym-hist (and (boundp '*candle-histories-tf*) *candle-histories-tf*
                              (gethash actual-symbol *candle-histories-tf*)))
               (hist (and sym-hist (gethash ftf-str sym-hist))))
          (when hist
            (format t "[L] 🧩 Attaching ~a Filter Data (~d bars)~%" ftf-str (length hist))
            (setf (gethash ftf-str aux-candles-ht) (candles-to-json hist)))))

      (format t "[L] 📊 Requesting backtest for ~a on ~a~a (Candles: ~d / TF: ~a)...~%"
              (strategy-name strat) actual-symbol suffix len (get-tf-string timeframe))

      (if use-file
          ;; Fast path: CSV on disk (recommended)
          (let ((payload-sexpr `((action . "BACKTEST")
                                 (strategy . ,strategy-alist)
                                 (request_id . ,req-id)
                                 (data_id ,(format nil "~a_M1" actual-symbol))
                                 (candles_file ,data-file)
                                 (symbol . ,actual-symbol)
                                 (timeframe ,timeframe)
                                 ,@(when start-time (list (%option-some 'start_time start-time)))
                                 ,@(when end-time (list (%option-some 'end_time end-time)))
                                 ,@(when include-trades '((include_trades . t))))))
            (format t "[L] 🚀 Zero-Copy SXP: Using Data ID ~a_M1~%" actual-symbol)
            (let ((*print-case* :downcase)
                  (*print-pretty* nil)
                  (*print-right-margin* most-positive-fixnum)
                  (*print-escape* t)
                  (*package* (find-package :swimmy.school)))
              (setf msg (format nil "~s" payload-sexpr))))

          ;; Fallback / WFV: send candles inline (避けたいが必要時のみ)
          (let* ((aux-candles (loop for k being the hash-keys of aux-candles-ht
                                    using (hash-value v)
                                    collect (cons k v)))
                 (payload-sexpr `((action . "BACKTEST")
                                  (strategy . ,strategy-alist)
                                  (request_id . ,req-id)
                                  (candles . ,(candles-to-sexp target-candles))
                                  (aux_candles . ,aux-candles)
                                  (symbol . ,actual-symbol)
                                  (timeframe ,timeframe)
                                  ,@(when start-time (list (%option-some 'start_time start-time)))
                                  ,@(when end-time (list (%option-some 'end_time end-time)))
                                  ,@(when include-trades '((include_trades . t))))))
            (let ((*print-case* :downcase)
                  (*print-pretty* nil)
                  (*print-right-margin* most-positive-fixnum)
                  (*print-escape* t)
                  (*package* (find-package :swimmy.school)))
              (setf msg (format nil "~s" payload-sexpr)))
            (format t "[L] 📤 Sent Backtest Request (TF: ~a)~%" (get-tf-string timeframe))))

      (when msg
        (send-zmq-msg msg :target :backtest)))))

;;; ══════════════════════════════════════════════════════════════════
;;;  WALK-FORWARD VALIDATION (López de Prado)
;;; ══════════════════════════════════════════════════════════════════

(defparameter *wfv-pending-strategies* (make-hash-table :test 'equal)
  "Stores strategies currently undergoing WFV. Key: strategy-name")

(defun wfv-pending-stats (&key (now (get-universal-time)))
  (let ((count 0)
        (oldest nil))
    (maphash (lambda (_ entry)
               (declare (ignore _))
               (incf count)
               (let ((started (getf entry :started-at)))
                 (when started
                   (setf oldest (if oldest (min oldest started) started)))))
             *wfv-pending-strategies*)
    (values count (and oldest (- now oldest)))))

(defun start-walk-forward-validation (strat)
  "Initiate OOS validation for a strategy by splitting data."
  (unless (and *candle-history* (> (length *candle-history*) 100))
    (format t "[L] ⚠️ Not enough data for WFV~%")
    (return-from start-walk-forward-validation))

  (let* ((len (length *candle-history*))
         (split-ratio 0.2)
         (split-idx (floor (* len split-ratio))) ; Top 20% is OOS (Newest)
         ;; *candle-history* is Newest-First.
         ;; Index 0 to split-idx-1: Recent Data (OOS)
         ;; Index split-idx to End: Past Data (IS)
         (oos-candles (subseq *candle-history* 0 split-idx))
         (is-candles (subseq *candle-history* split-idx))
         (wfv-id (swimmy.core:generate-uuid)))
    
    (format t "[L] 🚦 Starting WFV for ~a. IS: ~d bars, OOS: ~d bars~%" 
            (strategy-name strat) (length is-candles) (length oos-candles))
            
    ;; Register in pending table
    (setf (gethash (strategy-name strat) *wfv-pending-strategies*) 
          (list :is-result nil :oos-result nil :strategy strat :started-at (get-universal-time)
                :wfv-id wfv-id :split-ratio split-ratio))

    (swimmy.core::emit-telemetry-event "wfv.started"
      :service "school"
      :severity "info"
      :correlation-id wfv-id
      :data (jsown:new-js
              ("wfv_id" wfv-id)
              ("split_ratio" split-ratio)
              ("is_bars" (length is-candles))
              ("oos_bars" (length oos-candles))))
    
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
      (let* ((wfv-id (getf entry :wfv-id))
             (part (if (search "_IS" name-with-suffix) "IS" "OOS")))
        (swimmy.core::emit-telemetry-event "wfv.part_received"
          :service "school"
          :severity "info"
          :correlation-id wfv-id
          :data (jsown:new-js
                  ("wfv_id" wfv-id)
                  ("part" part)
                  ("sharpe" (getf result-map :sharpe)))))
      
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
                          0.0))
         (wfv-id (getf entry :wfv-id))
         (split-ratio (getf entry :split-ratio))
         (decision nil)) ; Avoid div by zero
    
    (format t "[L] ⚖️ WFV VERDICT for ~a (Gen ~d): IS=~,2f OOS=~,2f (Req: ~,2f) Degradation: ~,1f%~%"
            base-name gen is-sharpe oos-sharpe required-oos (* degradation 100))
            
    (cond
      ;; Scenario 1: Robust Strategy (OOS > Threshold AND Degradation < 50%)
      ((and (> oos-sharpe required-oos) (< degradation 0.5))
       (setf decision "validated")
       (format t "[L] ✅ VALIDATED: ~a is robust! Promoted.~%" base-name)
       (notify-discord-alert (format nil "Valid Strategy: ~a (Gen ~d | OOS: ~,2f)" base-name gen oos-sharpe) :color 3066993)
       ;; V17c: Persist Promotion to Battlefield
       (swimmy.persistence:move-strategy strat :battlefield))
      
      ;; Scenario 2: Overfit (Good IS, Bad OOS)
      ((> degradation 0.7)
       (setf decision "overfit")
       (format t "[L] 🚮 OVERFIT: ~a discarded. (Good past, bad future)~%" base-name)
       (setf *evolved-strategies* 
             (remove-if (lambda (s) (string= (strategy-name s) base-name)) *evolved-strategies*))
       ;; V17c: Move to Graveyard
       (swimmy.persistence:move-strategy strat :graveyard))
       
      ;; Scenario 3: Weak (OOS < Required)
      (t
       (setf decision "weak")
       (format t "[L] 🗑️ WEAK: ~a (OOS ~,2f < ~,2f) discarded.~%" base-name oos-sharpe required-oos)
       (setf *evolved-strategies* 
             (remove-if (lambda (s) (string= (strategy-name s) base-name)) *evolved-strategies*))
       ;; V17c: Move to Graveyard
       (setf *evolved-strategies* 
             (remove-if (lambda (s) (string= (strategy-name s) base-name)) *evolved-strategies*))
       ;; V17c: Move to Graveyard
       (swimmy.persistence:move-strategy strat :graveyard)))

    (swimmy.core::emit-telemetry-event "wfv.result"
      :service "school"
      :severity "info"
      :correlation-id wfv-id
      :data (jsown:new-js
              ("wfv_id" wfv-id)
              ("split_ratio" split-ratio)
              ("generation" gen)
              ("required_oos" required-oos)
              ("degradation" degradation)
              ("decision" decision)
              ("is_sharpe" is-sharpe)
              ("oos_sharpe" oos-sharpe)))
    
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
    
    (let* ((*print-case* :downcase)
           (payload `((action . "CHECK_CLONE")
                      (new_strategy . ,(strategy-to-alist new-strat)) ;; Use alist, assuming strategy-to-alist returns one
                      (existing_strategies . ,(mapcar #'strategy-to-alist *evolved-strategies*))
                      (threshold . ,*clone-threshold*)))
           (msg (format nil "~s" payload)))
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
  "V48.1 Expert Panel P0: Delete strategy from KB immediately.
   Pattern is saved to graveyard.sexp for Q-Learning before deletion."
  (let ((name (strategy-name strat)))
    (when (oos-request-pending-p name)
      (format t "[PRUNER] ⏳ Skip prune (OOS pending): ~a~%" name)
      (return-from prune-to-graveyard nil))
    (ignore-errors (cancel-oos-request-for-strategy name "pruned"))
    
    ;; 1. Save pattern for Q-Learning (before deletion)
    (handler-case
        (save-failure-pattern strat reason)
      (error (e)
        (format t "[PRUNER] ⚠️ Pattern save failed: ~a~%" e)))
    ;; 2. Atomic removal from KB and pools
    (bt:with-lock-held (*kb-lock*)
      (let ((cat (categorize-strategy strat)))
        (setf (gethash cat *category-pools*) 
              (remove name (gethash cat *category-pools*) :key #'strategy-name :test #'string=)))
      (setf *strategy-knowledge-base* 
            (remove name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))

    ;; 3. File System Persistence: Move to Graveyard
    (handler-case
        (swimmy.persistence:move-strategy strat :graveyard)
      (error (e)
        (format t "[PRUNER] ⚠️ File move failed: ~a~%" e)))
    
    (format t "[PRUNER] 🗑️ DELETED: ~a | Reason: ~a~%" name reason)))

(defparameter *last-qual-cycle* 0)
(defparameter *qual-cycle-interval* 60
  "Minimum seconds between qualification batches.")
(defparameter *qualification-rename-seq* 0
  "Monotonic suffix used when QUAL candidates must be renamed.")

(defun %normalize-db-rank-token (rank)
  "Normalize DB rank token to uppercase without leading colon."
  (let* ((raw (cond
                ((null rank) "")
                ((symbolp rank) (symbol-name rank))
                (t (format nil "~a" rank))))
         (trimmed (string-upcase (string-trim '(#\Space #\Tab #\Newline) raw))))
    (if (and (> (length trimmed) 0)
             (char= (char trimmed 0) #\:))
        (subseq trimmed 1)
        trimmed)))

(defun %archived-db-rank-p (rank)
  "Return T when DB rank is archived and should not be reused for new QUAL candidates."
  (member (%normalize-db-rank-token rank)
          '("RETIRED" "GRAVEYARD")
          :test #'string=))

(defun ensure-qualification-candidate-name-unique (strat)
  "Rename candidate when its current name collides with archived DB rows."
  (let* ((current-name (strategy-name strat))
         (db-rank (ignore-errors
                    (execute-single "SELECT rank FROM strategies WHERE name = ?"
                                    current-name))))
    (when (%archived-db-rank-p db-rank)
      (incf *qualification-rename-seq*)
      (let ((new-name (format nil "~a-QUAL-~a-~d"
                              current-name
                              (get-universal-time)
                              *qualification-rename-seq*)))
        (setf (strategy-name strat) new-name)
        (format t "[QUALIFY] ♻️ Renamed archived collision: ~a -> ~a (rank=~a)~%"
                current-name new-name db-rank))))
  strat)

(defun incubator-has-phase1-metrics-p (strategy)
  "Return T when incubator already has Phase1 metrics and can be evaluated immediately."
  (and (eq (strategy-rank strategy) :incubator)
       (or (> (or (strategy-trades strategy) 0) 0)
           (not (zerop (float (or (strategy-sharpe strategy) 0.0))))
           (> (float (or (strategy-profit-factor strategy) 0.0)) 0.0)
           (> (float (or (strategy-win-rate strategy) 0.0)) 0.0))))

(defun run-qualification-cycle ()
  "V47.8 Phase 3: Loop through strategies needing backtest."
  (format t "[QUALIFY] 🏁 Starting Qualification Cycle...~%")
  (let ((now (get-universal-time)))
    (when (< (- now *last-qual-cycle*) *qual-cycle-interval*)
      (format t "[QUALIFY] ⏳ Skipping (cooldown ~d sec)~%" *qual-cycle-interval*)
      (return-from run-qualification-cycle nil))
    (setf *last-qual-cycle* now))
  (let* ((incubator-strategies (remove-if-not (lambda (s) (eq (strategy-rank s) :incubator))
                                               *strategy-knowledge-base*))
         (scored-incubators (remove-if-not #'incubator-has-phase1-metrics-p
                                           incubator-strategies))
         (incubator-candidates (remove-if #'incubator-has-phase1-metrics-p
                                          incubator-strategies))
         (unranked-candidates (remove-if-not
                               (lambda (s)
                                 (and (null (strategy-rank s))
                                      (= (or (strategy-trades s) 0) 0)
                                      (= (or (strategy-sharpe s) 0.0) 0.0)))
                               *strategy-knowledge-base*))
         (all-candidates (append incubator-candidates unranked-candidates))
         ;; Prioritize incubator backlog so newborns are not starved by unranked candidates.
         (candidates all-candidates)
	        (limit 50)
	        (attempted 0)
	        (accepted 0)
         (reconciled 0))
    
    ;; Incubators with already available metrics should not wait for redispatch.
    (dolist (strat scored-incubators)
      (handler-case
          (when (fboundp 'evaluate-new-strategy)
            (evaluate-new-strategy strat)
            (incf reconciled))
        (error (e)
          (format t "[QUALIFY] ⚠️ Failed to reconcile scored incubator ~a: ~a~%"
                  (strategy-name strat) e))))

    (format t "[QUALIFY] found ~d candidates (reconciled ~d scored incubators).~%"
            (length all-candidates)
            reconciled)
    (when (> (length incubator-candidates) 0)
      (format t "[QUALIFY] 🎯 Prioritizing ~d incubator candidates before unranked (~d).~%"
              (length incubator-candidates)
              (length unranked-candidates)))
    
    ;; Expected count tracks only accepted dispatches.
    (setf swimmy.globals:*qual-expected-backtest-count* 0)
    (setf swimmy.globals:*qual-backtest-results-buffer* nil)
    (setf swimmy.globals:*qual-backtest-start-time* (get-universal-time))
    
    (dolist (strat candidates)
      (when (< attempted limit)
        (incf attempted)
        (ensure-qualification-candidate-name-unique strat)
        ;; Request backtest with -QUAL suffix to route results correctly
        (let ((dispatch-state (request-backtest strat :suffix "-QUAL")))
          (if (backtest-dispatch-accepted-p dispatch-state)
              (incf accepted)
              (format t "[QUALIFY] ⚠️ Dispatch rejected: ~a (state=~a)~%"
                      (strategy-name strat) dispatch-state)))
        ;; Sleep purely to not flood ZMQ buffer instantly
        (sleep 0.01)))

    (setf swimmy.globals:*qual-expected-backtest-count* accepted)
    (format t "[QUALIFY] 📤 Accepted ~d backtest requests (attempted ~d).~%"
            accepted attempted)))

(defun init-backtest-zmq ()
  "Initialize ZMQ connection to backtest service for the school daemon.
   Connects PUSH -> *port-backtest-req* (Backtest Service PULL)."
  (unless *backtest-service-enabled*
    (format t "[BACKTEST] ℹ️ Backtest Service disabled. Using Guardian direct mode.~%")
    (return-from init-backtest-zmq nil))
  (unless (and (boundp 'swimmy.globals:*backtest-requester*) swimmy.globals:*backtest-requester*)
    (handler-case
        (let* ((ctx (pzmq:ctx-new))
               (req (pzmq:socket ctx :push)))
          (pzmq:connect req (zmq-connect-endpoint *port-backtest-req*))
          (setf swimmy.globals:*backtest-requester* req)
          (format t "[BACKTEST] 🔌 Connected to Backtest Service (PUSH -> ~d)~%" *port-backtest-req*)
          (flush-backtest-queue))
      (error (e)
        (format t "[BACKTEST] ❌ Failed to initialize ZMQ: ~a~%" e)
        (error e))))
  (unless (and (boundp 'swimmy.globals:*backtest-requester*) swimmy.globals:*backtest-requester*)
    (error "Backtest service enabled but requester not initialized")))
