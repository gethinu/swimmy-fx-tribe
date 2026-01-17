;;; school-execution.lisp - Trade Execution & Strategy Management (SRP)
(in-package :swimmy.school)

;;; PARAMETERS
(defparameter *category-positions* (make-hash-table :test 'eq))
(defparameter *total-capital* 0.10)
(defparameter *lstm-threshold* 0.60)
(defparameter *last-clan-trade-time* (make-hash-table :test 'eq))
(defparameter *min-trade-interval* 300)
(defvar *last-swarm-consensus* 0)
(defparameter *category-entries* (make-hash-table :test 'eq))

;;; SIGNALS & EVALUATION moved to school-evaluation.lisp

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
                              (when (safe-order "BUY" symbol lot sl tp magic 
                                                (format nil "~a|~a" lead-name timeframe-key))
                                (setf trade-committed t) ;; LEGALLY COMMITTED
                                (update-symbol-exposure symbol lot :open)
                                (incf *category-trades*)
                                (setf *last-entry-time* (get-universal-time))
                                (format t "[L] ⏳ ORDER PENDING: ~a -> ~a BUY (Magic ~d)~%" category symbol magic)
                                (request-mt5-positions)
                                (return-from execute-category-trade t))))
                           ((eq direction :sell)
                            (let ((sl (+ ask sl-pips)) (tp (- ask tp-pips)))
                              (when (safe-order "SELL" symbol lot sl tp magic 
                                                (format nil "~a|~a" lead-name timeframe-key))
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
                ;; V44.7: Find GLOBAL best across ALL categories (Expert Panel)
                ;; V44.9: Shuffle first to randomize ties (Expert Panel Action 1)
                (let* ((all-sorted 
                        (sort (shuffle-list strat-signals)
                              (lambda (a b)
                                (let* ((name-a (getf a :strategy-name))
                                       (name-b (getf b :strategy-name))
                                       (cache-a (get-cached-backtest name-a))
                                       (cache-b (get-cached-backtest name-b))
                                       (sharpe-a (if cache-a (or (getf cache-a :sharpe) 0) 0))
                                       (sharpe-b (if cache-b (or (getf cache-b :sharpe) 0) 0)))
                                  (> sharpe-a sharpe-b)))))
                       (top-sig (first all-sorted))
                       (top-name (when top-sig (getf top-sig :strategy-name)))
                       (top-cat (when top-sig (getf top-sig :category)))
                       (top-cache (when top-name (get-cached-backtest top-name)))
                       (top-sharpe (if top-cache (or (getf top-cache :sharpe) 0) 0)))
                  (when top-sig
                    (format t "[L] 🏆 GLOBAL BEST: ~a (~a) Sharpe: ~,2f from ~d strategies~%"
                            top-name top-cat top-sharpe (length strat-signals))
                    (let* ((direction (getf top-sig :direction))
                           (strat-key (intern (format nil "~a-~a" top-cat top-name) :keyword)))
                      (when (can-clan-trade-p strat-key)
                        (let ((trade-executed (execute-category-trade top-cat direction symbol bid ask)))
                          (when trade-executed
                            (format t "~a~%" (generate-dynamic-narrative top-sig symbol bid))
                            (record-clan-trade-time strat-key)
                            (when (fboundp 'record-strategy-trade) 
                              (record-strategy-trade top-name :trade 0))))))))))))
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
