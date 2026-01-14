;;; school-execution.lisp - Trade Execution & Strategy Management
;;; Part of the Swimmy School System
;;; Extracted from school.lisp to comply with SRP (Expert Panel 2026-01-13)

(in-package :swimmy.school)

;;; ==========================================
;;; PARAMETERS & STATE
;;; ==========================================

(defparameter *category-positions* (make-hash-table :test 'eq))
(defparameter *total-capital* 0.10)
(defparameter *lstm-threshold* 0.60)

(defparameter *last-clan-trade-time* (make-hash-table :test 'eq))
(defparameter *min-trade-interval* 300)  ; 5 min cooldown to reduce Discord spam
(defvar *last-swarm-consensus* 0)
(defparameter *category-entries* (make-hash-table :test 'eq))
;;; V19: Stale Allocation Detection and Cleanup - MOVED TO school-allocation.lisp
;;; V19: MT5 Position Synchronization - MOVED TO school-allocation.lisp


;;; ==========================================
;;; STRATEGY SELECTION & RECRUITMENT
;;; ==========================================

;;; Logic moved to school-strategy.lisp for SRP


;;; ==========================================
;;; SIGNALS & EVALUATION
;;; ==========================================

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
                                          (,(intern "BB-WIDTH" pkg) (- u l))
                                          (,(intern "BB-MIDDLE-PREV" pkg) ,pm) (,(intern "BB-UPPER-PREV" pkg) ,pu) (,(intern "BB-LOWER-PREV" pkg) ,pl))))))
                              (stoch (let ((k (ind-stoch (first p) (second p) history))
                                           (pk (ind-stoch (first p) (second p) rest-hist)))
                                      `((,(intern "STOCH-K" pkg) ,k) (,(intern "STOCH-K-PREV" pkg) ,pk) 
                                        (,(intern "STOCH-D" pkg) 50) (,(intern "STOCH-D-PREV" pkg) 50))))
                              (session-high `((,(intern (format nil "SESSION-HIGH-~d-~d" (first p) (second p)) pkg) 
                                               ,(ind-session-high (first p) (second p) history))))
                              (session-low `((,(intern (format nil "SESSION-LOW-~d-~d" (first p) (second p)) pkg) 
                                              ,(ind-session-low (first p) (second p) history))))
                              ;; Placeholder bindings for unimplemented indicators (prevent compile errors)
                              (ichimoku `((,(intern "SENKOU-A" pkg) ,(candle-close (first history)))
                                          (,(intern "SENKOU-B" pkg) ,(candle-close (first history)))
                                          (,(intern "TENKAN" pkg) ,(candle-close (first history)))
                                          (,(intern "KIJUN" pkg) ,(candle-close (first history)))))
                              (donchian (let* ((period (first p))
                                               (highs (mapcar #'candle-high (subseq history 0 (min period (length history)))))
                                               (lows (mapcar #'candle-low (subseq history 0 (min period (length history)))))
                                               (upper (if highs (apply #'max highs) 0))
                                               (lower (if lows (apply #'min lows) 0))
                                               (mid (/ (+ upper lower) 2)))
                                          `((,(intern "DONCHIAN-UPPER" pkg) ,upper)
                                            (,(intern "DONCHIAN-LOWER" pkg) ,lower)
                                            (,(intern "DONCHIAN-MID" pkg) ,mid))))
                              (t nil))))))
      (let ((pkg (find-package :swimmy.school))
            (current-close (candle-close (first history)))
            (prev-close (candle-close (second history)))
            (vol (candle-volume (first history))))
        (push `(,(intern "CLOSE" pkg) ,current-close) bindings)
        (push `(,(intern "CLOSE-PREV" pkg) ,prev-close) bindings)
        (push `(,(intern "HIGH" pkg) ,(candle-high (first history))) bindings)
        (push `(,(intern "HIGH-PREV" pkg) ,(candle-high (second history))) bindings)
        (push `(,(intern "LOW" pkg) ,(candle-low (first history))) bindings)
        (push `(,(intern "LOW-PREV" pkg) ,(candle-low (second history))) bindings)
        (push `(,(intern "OPEN" pkg) ,(candle-open (first history))) bindings)
        
        ;; V15.2: Fix undefined variables (RSI, EMA, VOLUME, PNL, HISTORY, TP, SL)
        ;; Bind defaults for generic variable names used in generated strategies
        (push `(,(intern "VOLUME" pkg) ,vol) bindings)
        (push `(,(intern "HISTORY" pkg) ',history) bindings)
        (push `(,(intern "PNL" pkg) 0.0) bindings) ; Placeholder as PnL is strategy-specific
        (push `(,(intern "TP" pkg) ,(strategy-tp strat)) bindings)
        (push `(,(intern "SL" pkg) ,(strategy-sl strat)) bindings)
        
        ;; Default indicator bindings (alias to standard params if not specified)
        ;; Note: These are rough defaults to preventing crashing. 
        ;; Ideal fix is to re-generate strategies with correct params.
        (when (fboundp 'ind-rsi)
          (push `(,(intern "RSI" pkg) ,(ind-rsi 14 history)) bindings))
        (when (fboundp 'ind-ema)
          (push `(,(intern "EMA" pkg) ,(ind-ema 20 history)) bindings))
        (when (fboundp 'ind-bb)
           (multiple-value-bind (m u l) (ind-bb 20 2.0 history)
             (push `(,(intern "BB-WIDTH" pkg) ,(- u l)) bindings)))
        
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
          (error (e) :hold))))))

(defun collect-strategy-signals (symbol history)
  "Evaluate ALL strategies and return triggered signals"
  (let ((signals nil))
    (dolist (strat *strategy-knowledge-base*)
      (handler-case
          (let* ((name (strategy-name strat))
                 (benched (and (fboundp 'strategy-benched-p) (strategy-benched-p name))))
            (unless benched
              (let ((sig (evaluate-strategy-signal strat history)))
                (when (member sig '(:buy :sell))
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

;;; ==========================================
;;; TRADING RULES & CHECKS
;;; ==========================================

(defun record-clan-trade-time (category)
  (setf (gethash category *last-clan-trade-time*) (get-universal-time)))

(defun can-clan-trade-p (category)
  (let ((last-time (gethash category *last-clan-trade-time* 0)))
    (> (- (get-universal-time) last-time) *min-trade-interval*)))

(defun is-safe-trading-time-p (strategy-name)
  "Check if current time is safe for trading (JST)"
  (multiple-value-bind (s m h d mo y dow) (decode-universal-time (get-universal-time))
    (declare (ignore s d mo y))
    (cond
      ((= dow 6) (return-from is-safe-trading-time-p nil))                 ; Sunday = CLOSED
      ((and (= dow 5) (>= h 7)) (return-from is-safe-trading-time-p nil))  ; Saturday after 7:00 = CLOSED
      ((and (= dow 0) (< h 5)) (return-from is-safe-trading-time-p nil)))   ; Monday before 5:00 = CLOSED
    (when (search "Gotobi" strategy-name)
      (return-from is-safe-trading-time-p t))
    (cond
      ((= h 6) nil) 
      ((and (= h 7) (< m 5)) nil)
      ((or (= h 12) (= h 13)) nil)
      ((and (= h 15) (< m 30)) nil)
      ((and (= dow 5) (>= h 23)) nil)
      (t t))))

(defun get-category-lot (category)
  (let ((alloc (cdr (assoc category (get-regime-weights)))))
    (if alloc (max 0.01 (* *total-capital* alloc)) 0.01)))

;;; ==========================================
;;; EXECUTION & WARRIORS
;;; ==========================================


;; NOTE: get-warrior-magic is defined in school-allocation.lisp
;; Do not duplicate here - it uses deterministic format: 1[CatID][Slot] (e.g., 110000)

(defun find-free-warrior-slot (category)
  (loop for i from 0 to 3
        for key = (format nil "~a-~d" category i)
        when (null (gethash key *warrior-allocation*))
        return i))

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
           ;; Logging & Recording
           (format t "[L] 🔄 DOTEN: Closing ~a ~a for ~a signal (PnL: ~5f)~%" category opposing-direction new-direction pnl)
           (incf *daily-pnl* (round (* pnl 1000 100)))
           (record-trade-result (if (> pnl 0) :win :loss))
           ;; V17: Record prediction outcome for feedback loop (Issue 2)
           (record-prediction-outcome symbol (if (eq opposing-direction :long) :buy :sell) (if (> pnl 0) :win :loss))
           (record-trade-outcome symbol (if (eq opposing-direction :long) :buy :sell) category "Doten" pnl)
           (when (fboundp 'record-strategy-trade)
             (let ((lead-strat (first (gethash category *active-team*))))
                 (when lead-strat (record-strategy-trade (strategy-name lead-strat) (if (> pnl 0) :win :loss) pnl))))
           ;; Discord Notification
           (swimmy.shell:notify-discord-symbol symbol (format nil "🔄 **DOTEN** ~a ~a closed ~,2f" (if (> pnl 0) "✅" "❌") category pnl) 
                                  :color (if (> pnl 0) 3066993 15158332))
           (incf closed-count))))
     *warrior-allocation*)
    closed-count))


(defparameter *processed-candle-time* (make-hash-table :test 'equal)
  "Tracks the timestamp of the last processed candle for each strategy/symbol pair.")


(defun execute-category-trade (category direction symbol bid ask)
  (format t "[TRACE] execute-category-trade ~a ~a symbol=~a bid=~a ask=~a~%" category direction symbol bid ask)
  (handler-case
    (when (and (numberp bid) (numberp ask) (total-exposure-allowed-p))  ; Safety + exposure check
      (let* ((strategies (gethash category *active-team*))
             (lead-strat (first strategies))
             (lead-name (when lead-strat (strategy-name lead-strat)))
             ;; V15.3: Respect Strategy Timeframe (Fix M1 Spam)
             (timeframe (if lead-strat (strategy-timeframe lead-strat) 1))
             (timeframe-key (cond ((= timeframe 5) "M5")
                                 ((= timeframe 15) "M15")
                                 ((= timeframe 30) "M30")
                                 ((= timeframe 60) "H1")
                                 ((= timeframe 240) "H4")
                                 ((= timeframe 1440) "D1")
                                 (t "M1")))
             
             (rank-data (when lead-name (get-strategy-rank lead-name)))
             (rank (if rank-data (strategy-rank-rank rank-data) :scout))
             (rank-mult (calculate-rank-multiplier rank))
             (base-lot (get-category-lot category))
             
             ;; Fetch correct history for timeframe
             ;; V15.4: Fallback Synthesis for H1 from M1 (Fixes Silence/Dead Air)
             (history (if (= timeframe 1)
                          (gethash symbol *candle-histories*)
                          (let ((tf-map (gethash symbol *candle-histories-tf*)))
                            (or (and tf-map (gethash timeframe-key tf-map))
                                ;; Synthesis Fallback
                                (when (and (= timeframe 60) (gethash symbol *candle-histories*))
                                   (format t "[L] ⚠️ Synthesizing H1 from M1 (~d bars)...~%" (length (gethash symbol *candle-histories*)))
                                   (resample-candles (gethash symbol *candle-histories*) 60))))))
             
             (latest-candle (and history (first history)))
             (latest-ts (if latest-candle (candle-timestamp latest-candle) 0))
             (idempotency-key (format nil "~a-~a-~a" category symbol timeframe-key))
             (last-processed (gethash idempotency-key *processed-candle-time* 0))

             ;; Logic wrapped in binding to maintain let* structure
             (guard-check (progn
                            ;; Guard 1: Missing Data
                            (unless history
                              (format t "[L] ⚠️ Skipping ~a (~a) - No history found~%" lead-name timeframe-key)
                              (return-from execute-category-trade nil))
                            
                            ;; Guard 2: Idempotency
                            (when (<= latest-ts last-processed)
                              ;; (format t "[TRACE] Skipping ~a (~a) - Candle ~a already processed~%" lead-name timeframe-key latest-ts)
                              (return-from execute-category-trade nil))
                            
                            ;; Update processed time (Commit execution)
                            (setf (gethash idempotency-key *processed-candle-time*) latest-ts)
                            (format t "[L] 🕰️ New Candle Detected: ~a (~a) TS=~d. Analysing...~%" 
                                    symbol timeframe-key latest-ts)
                            t))

             (vol-scaled-lot (if (and (fboundp 'volatility-scaled-lot) history)
                                 (volatility-scaled-lot base-lot history)
                                 base-lot))
                    (vol-mult (handler-case (get-volatility-lot-multiplier) (error () 1.0)))
                    (rp-lot (handler-case (get-risk-parity-lot category) (error () base-lot)))
                    (hdrl-lot (handler-case (hdrl-adjusted-lot symbol base-lot) (error () base-lot)))
                    (kelly-adj (if lead-name (get-strategy-kelly-lot lead-name base-lot) base-lot))
                    (lot (max 0.01 (* rank-mult vol-mult 
                                      (min (correlation-adjusted-lot symbol vol-scaled-lot) rp-lot hdrl-lot kelly-adj))))
                    (conf (or (and (boundp '*last-confidence*) *last-confidence*) 0.0))
                    (swarm-consensus (or (and (boundp '*last-swarm-consensus*) *last-swarm-consensus*) 0))
                    (sl-pips 0.15) (tp-pips 0.40)
                    (warmup-p (< *category-trades* 50))
                    (large-lot-p (> lot 0.05))
                    (high-rank-p (member rank '(:veteran :legend)))
                    (danger-p (and (boundp '*danger-level*) (> *danger-level* 2))))
      
      (setf conf (if (numberp conf) conf 0.0))
      
      (let ((now (get-universal-time)))
        (when (and (eq *system-state* :warmup) (> now *warmup-end-time*))
          (setf *system-state* :trading)
          (format t "[L] ✅ P0 WARMUP COMPLETE: Trading ENABLED~%"))
        (when (eq *system-state* :warmup) (return-from execute-category-trade nil))
        (when (< (- now *last-entry-time*) *min-entry-interval-seconds*) (return-from execute-category-trade nil))
        
        (let ((startup-period-end (+ *warmup-end-time* 60)))
          (when (and (< now startup-period-end) (> (hash-table-count *warrior-allocation*) 0))
            (return-from execute-category-trade nil)))
            
        (unless (market-open-p) (return-from execute-category-trade nil))

        (when *circuit-breaker-active*
          (if (> now *breaker-cooldown-end*)
              (progn (setf *circuit-breaker-active* nil) (setf *recent-losses* nil))
              (return-from execute-category-trade nil))))
      
      ;; V44.2: ATOMIC RESERVATION (Expert Panel Approved)
      ;; Reserve slot BEFORE any heavy lifting to prevent race conditions.
      (multiple-value-bind (slot-index magic) 
          (try-reserve-warrior-slot category lead-name symbol direction)
        
        (unless slot-index 
          (format t "[ALLOC] ⚠️ Clan ~a is fully deployed (4/4 warriors)!~%" category)
          (return-from execute-category-trade nil))
        
        ;; CRITICAL SECTION: We hold the reservation. Must ensure cleanup or commit.
        (let ((trade-committed nil))
          (unwind-protect
               (progn
                 (when (and (not warmup-p) (or large-lot-p high-rank-p danger-p) (fboundp 'convene-high-council))
                   (let* ((proposal (format nil "~a ~a ~,2f lot (~a戦略: ~a)" symbol direction lot rank lead-name))
                          (urgency (cond (danger-p :critical) (large-lot-p :high) (t :normal)))
                          (council-result (convene-high-council proposal category :urgency urgency)))
                     (when (eq council-result :rejected) (return-from execute-category-trade nil))))

                 (unless (should-block-trade-p symbol direction category)
                   (let* ((prediction (handler-case (predict-trade-outcome symbol direction) (error (e) nil)))
                          (should-trade (if prediction (should-take-trade-p prediction) t)))
                     
                     (when (global-panic-active-p) (format t "[L] 💉 SOROS: Elevated volatility~%"))
                     (when (should-unlearn-p symbol) (setf should-trade nil))
                     (when should-trade
                       (unless (verify-parallel-scenarios symbol direction category) (setf should-trade nil)))
                         
                     (when should-trade
                       (sleep (/ (random 2000) 1000.0))
                       (when (< (random 100) 5) (return-from execute-category-trade nil))
                       (close-opposing-clan-positions category direction symbol (if (eq direction :buy) bid ask) "Doten")

                       (let* ((strat (or (find lead-name *evolved-strategies* :key #'strategy-name :test #'string=)
                                         (find lead-name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
                              (sharpe (if strat (or (strategy-sharpe strat) 0.0) 0.0))
                              (lot (if (and (>= sharpe 0.0) (< sharpe 0.3)) 0.01 lot)))
                         (cond
                           ((eq direction :buy)
                            (let ((sl (- bid sl-pips)) (tp (+ bid tp-pips)))
                              (log-why-trade symbol :buy category :strategy lead-name 
                                            :tribe-cons (if (boundp '*tribe-consensus*) *tribe-consensus* 0)
                                            :swarm-cons swarm-consensus :parallel-score 2
                                            :elder-ok (not (should-block-trade-p symbol :buy category)))
                              (when (safe-order "BUY" symbol lot sl tp magic lead-name)
                                (setf trade-committed t) ;; LEGALLY COMMITTED
                                (update-symbol-exposure symbol lot :open)
                                (incf *category-trades*)
                                (setf *last-entry-time* (get-universal-time))
                                (format t "[L] ⏳ ORDER PENDING: ~a -> ~a BUY (Magic ~d)~%" category symbol magic)
                                (request-mt5-positions)
                                (return-from execute-category-trade t))))
                           ((eq direction :sell)
                            (let ((sl (+ ask sl-pips)) (tp (- ask tp-pips)))
                              (when (safe-order "SELL" symbol lot sl tp magic lead-name)
                                (setf trade-committed t) ;; LEGALLY COMMITTED
                                (update-symbol-exposure symbol lot :open)
                                (incf *category-trades*)
                                (setf *last-entry-time* (get-universal-time))
                                (format t "[L] ⏳ ORDER PENDING: ~a -> ~a SELL (Magic ~d)~%" category symbol magic)
                                (request-mt5-positions)
                                (return-from execute-category-trade t))))))))))
            ;; UNWIND-PROTECT CLEANUP
            (unless trade-committed
              (remhash magic *pending-orders*)
              (format t "[ALLOC] ♻️ Reservation Cancelled: Slot ~d (Magic ~d) Released~%" slot-index magic)))))))
    (error (e) (format t "[TRACE] 🚨 ERROR in execute-category-trade: ~a~%" e))))

(defun close-category-positions (symbol bid ask)
  "V5.2: Close warrior positions at SL/TP using warrior-allocation"
  (maphash 
   (lambda (key warrior)
     (when (and warrior (equal (getf warrior :symbol) symbol))
       (let* ((category (getf warrior :category))
              (pos (getf warrior :direction))
              (entry (getf warrior :entry))
               (magic (getf warrior :magic))
               (lot (or (getf warrior :lot) 0.01))
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
             (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" symbol) ("magic" magic))))
             (remhash key *warrior-allocation*)
             (update-symbol-exposure symbol lot :close)
             (incf *daily-pnl* (round (* pnl 1000 100)))
             (record-trade-result (if (> pnl 0) :win :loss))
             ;; V17: Record prediction outcome for feedback loop (Issue 2)
             (record-prediction-outcome symbol (if (eq pos :long) :buy :sell) (if (> pnl 0) :win :loss))
             (record-trade-outcome symbol (if (eq pos :long) :buy :sell) category "Warriors" pnl)
             (when (fboundp 'record-strategy-trade)
                (let ((lead-strat (first (gethash category *active-team*))))
                    (when lead-strat (record-strategy-trade (strategy-name lead-strat) (if (> pnl 0) :win :loss) pnl))))
             (contribute-to-treasury category pnl :trade (format nil "Trade ~a" (if (> pnl 0) "win" "loss")))
             (swimmy.shell:notify-discord-symbol symbol (format nil "~a ~a closed ~,2f" (if (> pnl 0) "✅" "❌") category pnl) 
                            :color (if (> pnl 0) 3066993 15158332)))))))
   *warrior-allocation*))

(defun process-category-trades (symbol bid ask)
  ;; V19: Periodic stale allocation cleanup
  (cleanup-stale-allocations)
  (when (and (trading-allowed-p) *candle-history* (> (length *candle-history*) 100))
    (close-category-positions symbol bid ask)
    (unless (is-safe-to-trade-p) (return-from process-category-trades nil))
    (unless (volatility-allows-trading-p) (return-from process-category-trades nil))
    
    (let ((research-analysis nil))
      (when (>= (length *candle-history*) 50)
        (setf research-analysis (research-enhanced-analysis *candle-history*))
        (select-optimal-model *candle-history*)
        (detect-regime-hmm *candle-history*))
    
    (let* ((swarm-decision (swarm-trade-decision symbol *candle-history*))
           (consensus (swarm-decision-consensus-strength swarm-decision))
           (swarm-direction (swarm-decision-direction swarm-decision))
           (memory-suggestion (memory-suggests-direction symbol))
           (dual-trend (when research-analysis (getf research-analysis :dual-trend)))
           (trend-agrees (or (null dual-trend) (not (listp dual-trend))
                            (eq (getf dual-trend :agreement) :aligned)
                            (eq (getf dual-trend :direction) (case swarm-direction (:BUY :UP) (:SELL :DOWN) (t :FLAT))))))
      (setf *last-swarm-consensus* consensus)
      (elect-leader)
      (let ((boosted-decision (get-leader-boosted-decision swarm-decision)))
        (setf swarm-decision boosted-decision))
      (handler-case
        (let* ((min-consensus-to-trade 0.25)
               (any-strong-signal nil))
        (when (or t (> consensus min-consensus-to-trade))
            (format t "[L] 🎯 61-STRATEGY SIGNAL SCAN~%")
            (let ((strat-signals (collect-strategy-signals symbol *candle-history*)))
              (setf any-strong-signal (and strat-signals t))
              (when strat-signals
                (format t "[L] 📊 ~d strategies triggered signals~%" (length strat-signals))
                (let ((by-category (make-hash-table :test 'eq)))
                  (dolist (sig strat-signals)
                    (let ((cat (getf sig :category))) (push sig (gethash cat by-category))))
                  (dolist (category '(:trend :reversion :breakout :scalp))
                    (let ((cat-sigs (gethash category by-category)))
                      (when cat-sigs
                        (let ((top-sigs (subseq cat-sigs 0 (min 4 (length cat-sigs)))))
                          (dolist (sig top-sigs)
                            (let* ((strat-name (getf sig :strategy-name))
                                   (direction (getf sig :direction))
                                   (strat-key (intern (format nil "~a-~a" category strat-name) :keyword)))
                              (when (can-clan-trade-p strat-key)
                                ;; V15.5 FIX: Only send Discord notification AFTER trade is approved and executed
                                ;; V42.0 FIX: Generate narrative AFTER execution so the Battlefield footer includes the new trade
                                (let ((trade-executed (execute-category-trade category direction symbol bid ask)))
                                  (when trade-executed
                                    (let ((narrative (generate-dynamic-narrative sig symbol bid)))
                                      (format t "~a~%" narrative)
                                      ;; V44.0: Narrative notification moved to POSITIONS handler (after MT5 confirms)
                                      ;; Discord notification removed here to prevent false positives
                                      (record-clan-trade-time strat-key)
                                      (when (fboundp 'record-strategy-trade) (record-strategy-trade strat-name :trade 0)))))))))))))))))
        (error (e) nil))))))

;;; ==========================================
;;; SYSTEM LOADING
;;; ==========================================



(defun force-recruit-strategy (name)
  (let ((strat (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
    (if strat
        (progn
          (pushnew strat *evolved-strategies* :test #'string= :key #'strategy-name)
          (let ((cat (categorize-strategy strat)))
            (setf (gethash cat *category-pools*) 
                  (cons strat (remove (strategy-name strat) (gethash cat *category-pools*) :key #'strategy-name :test #'string=))))
          (format t "[L] 🎖️ Special Force Recruited: ~a~%" name)
          t)
        (format t "[L] ⚠️ Special Force NOT FOUND: ~a~%" name))))

(defun recruit-special-forces ()
  (force-recruit-strategy "T-Nakane-Gotobi")
  (maphash (lambda (key val) (declare (ignore val)) (recruit-founder key)) *founder-registry*))

(defun safely-load-hunter-strategies ()
  (let ((path (merge-pathnames "src/lisp/school/school-hunter.lisp" (uiop:getcwd))))
    (handler-case
        (progn
          (load path)
          (format t "[HUNTER] ✅ Successfully loaded strategies from ~a~%" path)
          (when (boundp '*founder-registry*)
            (let ((count 0))
              (maphash (lambda (key maker-fn)
                         (declare (ignore key))
                         (handler-case
                             (let ((strat (funcall maker-fn)))
                               (unless (find (strategy-name strat) *strategy-knowledge-base* 
                                             :key #'strategy-name :test #'string=)
                                 (push strat *strategy-knowledge-base*)
                                 (incf count)))
                           (error (e) (format t "[HUNTER] ⚠️ Failed to instantiate ~a: ~a~%" key e))))
                       *founder-registry*)
              (when (> count 0)
                (format t "[HUNTER] ➕ Onboarded ~d new strategies from Registry to KB~%" count))))
          t)
      (error (e)
        (format t "[HUNTER] 🚨 CRITICAL LOAD ERROR: ~a~%" e)
        nil))))
