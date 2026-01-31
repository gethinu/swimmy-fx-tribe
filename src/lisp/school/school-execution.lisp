;;; school-execution.lisp - Trade Execution & Strategy Management (SRP)
(in-package :swimmy.school)

;;; PARAMETERS
(defparameter *category-positions* (make-hash-table :test 'equal))
(defparameter *total-capital* 0.10)
(defparameter *lstm-threshold* 0.60)
(defparameter *last-clan-trade-time* (make-hash-table :test 'equal))
(defparameter *min-trade-interval* 300)
(defvar *last-swarm-consensus* 0)
(defparameter *category-entries* (make-hash-table :test 'equal))

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
  (declare (ignore reason))
  (let ((opposing-direction (if (eq new-direction :buy) :short :long))
        (closed-count 0))
    (maphash 
     (lambda (key warrior)
       (when (and warrior 
                  (equal (getf warrior :category) category)
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
                  (when lead-strat 
                    (record-strategy-trade (strategy-name lead-strat) (if (> pnl 0) :win :loss) pnl)
                    ;; V47.5: Live Trade Audit + RL Reward hooks
                    (when (fboundp 'run-live-trade-audit)
                      (run-live-trade-audit lead-strat pnl))
                    (when (fboundp 'record-rl-reward)
                      (record-rl-reward lead-strat pnl)))))
           ;; Discord Notification
           (swimmy.shell:notify-discord-symbol symbol (format nil "🔄 **DOTEN** ~a ~a closed ~,2f" (if (> pnl 0) "✅" "❌") category pnl) 
                                  :color (if (> pnl 0) 3066993 15158332))
           (incf closed-count))))
     *warrior-allocation*)
    closed-count))


(defparameter *processed-candle-time* (make-hash-table :test 'equal)
  "Tracks the timestamp of the last processed candle for each strategy/symbol pair.")

;;; P2 Refactor: Helper Functions

(defun guard-execution-status (symbol now)
  "Check system state, market hours, and circuit breakers. Returns T if safe."
  (declare (ignore symbol))
  (cond
    ;; 1. Warmup Check
    ((eq *system-state* :warmup)
     (if (> now *warmup-end-time*)
         (progn (setf *system-state* :trading) t)
         nil))
    ;; 2. Entry Interval
    ((< (- now *last-entry-time*) *min-entry-interval-seconds*) nil)
    ;; 3. Startup Safety
    ((and (< now (+ *warmup-end-time* 60)) (> (hash-table-count *warrior-allocation*) 0)) nil)
    ;; 4. Market Hours
    ((not (market-open-p)) nil)
    ;; 5. Circuit Breaker
    (*circuit-breaker-active*
     (if (> now *breaker-cooldown-end*)
         (progn (setf *circuit-breaker-active* nil) (setf *recent-losses* nil) t)
         nil))
    (t t)))

(defun calc-execution-lot (category symbol history rank base-lot lead-name direction)
  "Calculate dynamic lot size based on Volatility, Risk Parity, and Rank."
  (let* ((rank-mult (calculate-rank-multiplier rank))
         (vol-scaled (if (and (fboundp 'swimmy.core::volatility-scaled-lot) history)
                         (swimmy.core::volatility-scaled-lot base-lot history)
                         base-lot))
         (vol-mult (handler-case (get-volatility-lot-multiplier) (error () 1.0)))
         (rp-lot (handler-case (get-risk-parity-lot category) (error () base-lot)))
         (hdrl-lot (handler-case (hdrl-adjusted-lot symbol base-lot) (error () base-lot)))
     (kelly-adj (if lead-name (get-strategy-kelly-lot lead-name base-lot) base-lot))
         (penalty (let ((strat (when lead-name 
                                 (or (find lead-name *evolved-strategies* :key #'strategy-name :test #'string=)
                                     (find lead-name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))))
                    (get-failure-penalty symbol direction category strat))))
    
    (max 0.01 (* rank-mult vol-mult penalty
                 (min (correlation-adjusted-lot symbol vol-scaled) 
                      rp-lot hdrl-lot kelly-adj)))))


;;; P3 Refactor: Decomposed Execution Helpers (Expert Panel 2026-01-20)

(defun prepare-trade-context (category symbol)
  "Helper: Resolve strategy, timeframe, and history context."
  (let* ((strategies (gethash category *active-team*))
         (lead-strat (first strategies))
         (lead-name (when lead-strat (strategy-name lead-strat)))
         (timeframe (if lead-strat (strategy-timeframe lead-strat) 1))
         (timeframe-key (cond ((= timeframe 5) "M5") ((= timeframe 15) "M15") ((= timeframe 30) "M30")
                             ((= timeframe 60) "H1") ((= timeframe 240) "H4") ((= timeframe 1440) "D1") (t "M1")))
         (history (if (= timeframe 1) (gethash symbol *candle-histories*)
                      (let ((tf-map (gethash symbol *candle-histories-tf*)))
                        (or (and tf-map (gethash timeframe-key tf-map))
                            (when (and (= timeframe 60) (gethash symbol *candle-histories*))
                               (resample-candles (gethash symbol *candle-histories*) 60)))))))
    (values lead-name timeframe-key history)))

(defun validate-trade-opportunity (category symbol timeframe-key history)
  "Helper: Check integrity, idempotency, and guardian status."
  (let* ((latest-candle (first history))
         (latest-ts (if latest-candle (candle-timestamp latest-candle) 0))
         (idempotency-key (format nil "~a-~a-~a" category symbol timeframe-key))
         (last-processed (gethash idempotency-key *processed-candle-time* 0)))
    (cond
      ((null history) nil)
      ((<= latest-ts last-processed) nil)
      ((not (guard-execution-status symbol (get-universal-time))) nil)
      (t 
       ;; Commit State (Side Effect required to prevent double processing)
       (setf (gethash idempotency-key *processed-candle-time*) latest-ts)
       (format t "[L] 🕰️ New Candle: ~a (~a) TS=~d~%" symbol timeframe-key latest-ts)
       t))))

(defun verify-signal-authority (symbol direction category lot rank lead-name)
  "Helper: Verify signal with Council, AI, and Blocking rules."
  (declare (ignore lead-name))
  (cond
    ((should-block-trade-p symbol direction category) nil)
    ((should-unlearn-p symbol) nil)
    ((not (verify-parallel-scenarios symbol direction category)) nil)
    (t 
     ;; High Council Check
     (let ((danger-p (and (boundp '*danger-level*) (> *danger-level* 2)))
           (large-lot-p (> lot 0.05))
           (high-rank-p (member rank '(:veteran :legend))))
       (if (and (or large-lot-p high-rank-p danger-p) (fboundp 'convene-high-council))
           (let ((res (convene-high-council (format nil "~a ~a ~,2f" symbol direction lot) category 
                                            :urgency (if danger-p :critical :normal))))
             (not (eq res :rejected)))
           t)))))

(defun execute-order-sequence (category direction symbol bid ask lot lead-name timeframe-key magic-override)
  "Helper: atomic reservation and execution."
  (declare (ignore magic-override))
  ;; Reservation
  (multiple-value-bind (slot-index magic) 
      (try-reserve-warrior-slot category lead-name symbol direction)
    (unless slot-index 
      (format t "[ALLOC] ⚠️ Clan ~a Full (4/4)!~%" category)
      (return-from execute-order-sequence nil))
    
    (let ((committed nil))
      (unwind-protect
           (progn 
             (close-opposing-clan-positions category direction symbol (if (eq direction :buy) bid ask) "Doten")
             (let ((sl-pips *default-sl-pips*) (tp-pips *default-tp-pips*)) ;; Constants (Phase 3.2)
               (cond
                 ((eq direction :buy)
                  (let ((sl (- bid sl-pips)) (tp (+ bid tp-pips)))
                    (when (safe-order "BUY" symbol lot sl tp magic (format nil "~a|~a" lead-name timeframe-key))
                      (setf committed t))))
                 ((eq direction :sell)
                  (let ((sl (+ ask sl-pips)) (tp (- ask tp-pips)))
                    (when (safe-order "SELL" symbol lot sl tp magic (format nil "~a|~a" lead-name timeframe-key))
                      (setf committed t))))))
             (when committed
                 (update-symbol-exposure symbol lot :open)
                 (incf *category-trades*)
                 (setf *last-entry-time* (get-universal-time))
                 (format t "[EXEC] ✅ Committed: ~a ~a~%" category symbol)
                 (request-mt5-positions)
                 t))
        ;; Cleanup
        (unless committed
           (remhash magic *pending-orders*)
           (remhash (format nil "~a-~d" category slot-index) *warrior-allocation*)
           (format t "[ALLOC] ♻️ Released Slot ~d~%" slot-index))))))

(defun execute-category-trade (category direction symbol bid ask)
  (format t "[TRACE] execute-category-trade ~a ~a~%" category direction)
  (handler-case
      (when (and (numberp bid) (numberp ask) (total-exposure-allowed-p))
        (multiple-value-bind (lead-name timeframe-key history) (prepare-trade-context category symbol)
          (when (validate-trade-opportunity category symbol timeframe-key history)
             (let* ((rank-data (when lead-name (get-strategy-rank lead-name)))
                    (rank (if rank-data (strategy-rank-rank rank-data) :scout))
                    (base-lot (get-category-lot category))
                    (lot (calc-execution-lot category symbol history rank base-lot lead-name direction)))
               
               (when (verify-signal-authority symbol direction category lot rank lead-name)
                  ;; Sleep Randomization (Anti-Gaming)
                  (sleep (/ (random 2000) 1000.0))
                  (execute-order-sequence category direction symbol bid ask lot lead-name timeframe-key nil))))))
    (error (e) (format t "[EXEC] 🚨 Error: ~a~%" e))))

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
               (sl-pips *default-sl-pips*) (tp-pips *default-tp-pips*)
               (pnl 0) (closed nil))
         (when (and entry (numberp bid) (numberp ask))
           (cond
             ((eq pos :long)
              (let ((sl (- entry sl-pips)) (tp (+ entry tp-pips)))
                (when (or (<= bid sl) (>= bid tp))
                  (setf pnl (- bid entry) closed t))))
             ((eq pos :short)
              (let ((sl (+ ask sl-pips)) (tp (- ask tp-pips)))
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
                     (when lead-strat 
                       (record-strategy-trade (strategy-name lead-strat) (if (> pnl 0) :win :loss) pnl)
                       ;; V47.5: Live Trade Audit + RL Reward hooks
                       (when (fboundp 'run-live-trade-audit)
                         (run-live-trade-audit lead-strat pnl))
                       (when (fboundp 'record-rl-reward)
                         (record-rl-reward lead-strat pnl)))))
              (contribute-to-treasury category pnl :trade (format nil "Trade ~a" (if (> pnl 0) "win" "loss")))
             (swimmy.shell:notify-discord-symbol symbol (format nil "~a ~a closed ~,2f" (if (> pnl 0) "✅" "❌") category pnl) 
                            :color (if (> pnl 0) 3066993 15158332)))))))
   *warrior-allocation*))


(defun s-rank-gate-passed-p ()
  "V49.0: Musk's Iron Gate. Require 5+ S-Rank strategies for Demo Trading."
  (let ((s-count (count-if (lambda (s) (eq (strategy-rank s) :S)) *strategy-knowledge-base*)))
    (if (>= s-count 5)
        t
        (progn
          ;; Throttled logging (every 5 mins)
          (when (= (mod (get-universal-time) 300) 0)
             (format t "[GATE] 🛑 Demo Trading Blocked: Only ~d/5 S-Rank strategies ready.~%" s-count))
          nil))))

(defun process-category-trades (symbol bid ask)
  ;; V19: Periodic stale allocation cleanup
  (cleanup-stale-allocations)
  ;; V45: Use per-symbol history for regime detection (Fix: Opus 2026-01-20)
  (let ((history (or (gethash symbol *candle-histories*) *candle-history*)))
    ;; V49.0: Added S-Rank Gate (Musk)
    (when (and (trading-allowed-p) (s-rank-gate-passed-p) history (> (length history) 100))
      (close-category-positions symbol bid ask)
      (unless (is-safe-to-trade-p) (return-from process-category-trades nil))
      (unless (volatility-allows-trading-p) (return-from process-category-trades nil))
    
      (when (>= (length history) 50)
        (research-enhanced-analysis history)
        (select-optimal-model history)
        (detect-regime-hmm history))
    
      (elect-leader) ;; Keep leader election if relevant, otherwise remove if tied to Swarm
      ;; Swarm Logic Removed (Center of gravity restored to individual strategies)
      (handler-case
          (when t ;; Swarm Consensus removed
            (format t "[L] 🎯 61-STRATEGY SIGNAL SCAN~%")
            (let ((strat-signals (collect-strategy-signals symbol history)))
              (when strat-signals
                (format t "[L] 📊 ~d strategies triggered signals~%" (length strat-signals))
                ;; V44.7: Find GLOBAL best across ALL categories (Expert Panel)
                ;; V44.9: Shuffle first to randomize ties (Expert Panel Action 1)
                (let* ((all-sorted 
                        (sort (copy-list strat-signals)
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
                              (record-strategy-trade top-name :trade 0)))))))))))
        (error (e)
          (declare (ignore e))
          nil)))))

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
  "Load Hunter strategies. P9: Split into core + auto files."
  (let ((core-path (merge-pathnames "src/lisp/school/school-hunter.lisp" (uiop:getcwd)))
        (auto-path (merge-pathnames "src/lisp/school/school-hunter-auto.lisp" (uiop:getcwd))))
    (handler-case
        (progn
          ;; P9: Load core strategies (manual Hunted)
          (load core-path)
          (format t "[HUNTER] ✅ Loaded core strategies~%")
          ;; P9: Load auto-generated strategies
          (when (probe-file auto-path)
            (load auto-path)
            (format t "[HUNTER] ✅ Loaded auto-generated strategies~%"))
          t)
      (error (e)
        (format t "[HUNTER] 🚨 CRITICAL LOAD ERROR: ~a~%" e)
        nil))))
