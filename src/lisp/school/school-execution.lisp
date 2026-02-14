;;; school-execution.lisp - Trade Execution & Strategy Management (SRP)
(in-package :swimmy.school)

;;; PARAMETERS
(defparameter *category-positions* (make-hash-table :test 'equal))
(defparameter *total-capital* 0.10)
(defparameter *lstm-threshold* 0.60)
(defparameter *last-category-trade-time* (make-hash-table :test 'equal))
(defparameter *min-trade-interval* 300)
(defparameter *category-entries* (make-hash-table :test 'equal))
(defparameter *pattern-gate-threshold* 0.60
  "Minimum directional probability to accept pattern alignment.")
(defparameter *pattern-gate-lot-multiplier* 0.70
  "Soft gate multiplier when pattern direction mismatches.")
(defparameter *pattern-gate-k* 30
  "Top-k neighbors for Pattern Similarity query.")
(defparameter *pattern-gate-timeframes* '("H1" "H4" "D1" "W1" "MN1")
  "Timeframes where pattern soft-gate is enabled.")
(defparameter *signal-confidence-entry-threshold* 0.20
  "Minimum signal confidence required to allow a new entry.")
(defparameter *signal-confidence-soft-threshold* 0.35
  "Confidence threshold to restore full lot size.")
(defparameter *signal-confidence-soft-lot-multiplier* 0.55
  "Lot multiplier for medium-confidence entries.")
(defparameter *min-s-rank-strategies-for-live* 2
  "Minimum number of S-rank strategies required before live execution starts.")

;;; SIGNALS & EVALUATION moved to school-evaluation.lisp

;;; ==========================================
;;; TRADING RULES & CHECKS
;;; ==========================================

(defun record-category-trade-time (category)
  (setf (gethash category *last-category-trade-time*) (get-universal-time)))

(defun can-category-trade-p (category)
  (let ((last-time (gethash category *last-category-trade-time* 0)))
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

(defun close-opposing-category-positions (category new-direction symbol price reason)
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
           (let ((msg (swimmy.core:encode-sexp `((type . "CLOSE")
                                                 (symbol . ,symbol)
                                                 (magic . ,magic)))))
             (pzmq:send *cmd-publisher* msg))
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

(defun pattern-gate-enabled-timeframe-p (timeframe-key)
  "Return T when Pattern gate should run for TIMEFRAME-KEY."
  (member (string-upcase (format nil "~a" timeframe-key))
          *pattern-gate-timeframes*
          :test #'string=))

(defun %pattern-gate-coerce-float (value &optional (default 0.0))
  "Best-effort conversion to float."
  (cond
    ((numberp value) (float value))
    ((stringp value)
     (handler-case
         (let ((*read-eval* nil))
           (multiple-value-bind (obj _pos) (read-from-string value nil nil)
             (declare (ignore _pos))
             (if (numberp obj) (float obj) default)))
       (error () default)))
    (t default)))

(defun %pattern-best-label (p-up p-down p-flat)
  "Return :UP/:DOWN/:FLAT from probabilities."
  (cond
    ((and (>= p-up p-down) (>= p-up p-flat)) :up)
    ((and (>= p-down p-up) (>= p-down p-flat)) :down)
    (t :flat)))

(defun %pattern-gate-get (gate key &optional default)
  "Fetch KEY from gate alist."
  (let ((cell (assoc key gate)))
    (if cell (cdr cell) default)))

(defun normalize-signal-confidence (confidence &optional (default 1.0))
  "Normalize confidence to [0,1]. Missing/invalid values fallback to DEFAULT."
  (let* ((raw (cond
                ((numberp confidence) (float confidence))
                ((stringp confidence)
                 (handler-case
                     (let ((*read-eval* nil))
                       (multiple-value-bind (obj _pos) (read-from-string confidence nil nil)
                         (declare (ignore _pos))
                         (if (numberp obj) (float obj) default)))
                   (error () default)))
                (t default))))
    (max 0.0 (min 1.0 raw))))

(defun signal-confidence-lot-multiplier (confidence)
  "Return lot multiplier by signal confidence. 0.0 means no entry."
  (let ((conf (normalize-signal-confidence confidence)))
    (cond
      ((< conf *signal-confidence-entry-threshold*) 0.0)
      ((< conf *signal-confidence-soft-threshold*)
       (max 0.01 (float *signal-confidence-soft-lot-multiplier*)))
      (t 1.0))))

(defun apply-pattern-soft-gate (category symbol direction timeframe-key history lot lead-name)
  "Apply pattern soft gate. Returns (values adjusted-lot gate-info-alist)."
  (let ((base-lot (max 0.01 (float lot))))
    (unless (pattern-gate-enabled-timeframe-p timeframe-key)
      (return-from apply-pattern-soft-gate
        (values base-lot
                `((:applied . nil)
                  (:reason . "SKIP_TF")
                  (:category . ,category)
                  (:symbol . ,symbol)
                  (:timeframe . ,timeframe-key)
                  (:strategy . ,lead-name)
                  (:lot_before . ,base-lot)
                  (:lot_after . ,base-lot)))))

    (multiple-value-bind (result err)
        (swimmy.core:query-pattern-similarity symbol timeframe-key history :k *pattern-gate-k*)
      (when err
        (return-from apply-pattern-soft-gate
          (values base-lot
                  `((:applied . nil)
                    (:reason . "UNAVAILABLE")
                    (:error . ,err)
                    (:category . ,category)
                    (:symbol . ,symbol)
                    (:timeframe . ,timeframe-key)
                    (:strategy . ,lead-name)
                    (:lot_before . ,base-lot)
                    (:lot_after . ,base-lot)))))

      (let* ((p-up (%pattern-gate-coerce-float (swimmy.core:sexp-alist-get result 'p_up) (/ 1.0 3.0)))
             (p-down (%pattern-gate-coerce-float (swimmy.core:sexp-alist-get result 'p_down) (/ 1.0 3.0)))
             (p-flat (%pattern-gate-coerce-float (swimmy.core:sexp-alist-get result 'p_flat) (/ 1.0 3.0)))
             (expected (if (eq direction :buy) :up :down))
             (expected-prob (if (eq expected :up) p-up p-down))
             (best-label (%pattern-best-label p-up p-down p-flat))
             (match-p (and (>= expected-prob *pattern-gate-threshold*)
                           (eq best-label expected)))
             (applied (not match-p))
             (adjusted (if applied
                           (max 0.01 (* base-lot *pattern-gate-lot-multiplier*))
                           base-lot)))
        (values adjusted
                `((:applied . ,applied)
                  (:reason . ,(if applied "MISMATCH" "MATCH"))
                  (:category . ,category)
                  (:symbol . ,symbol)
                  (:timeframe . ,timeframe-key)
                  (:strategy . ,lead-name)
                  (:direction . ,(if (eq direction :buy) "BUY" "SELL"))
                  (:expected_label . ,(string-upcase (symbol-name expected)))
                  (:best_label . ,(string-upcase (symbol-name best-label)))
                  (:expected_prob . ,expected-prob)
                  (:p_up . ,p-up)
                  (:p_down . ,p-down)
                  (:p_flat . ,p-flat)
                  (:threshold . ,*pattern-gate-threshold*)
                  (:k . ,*pattern-gate-k*)
                  (:lot_before . ,base-lot)
                  (:lot_after . ,adjusted)))))))

(defun emit-pattern-gate-telemetry (symbol timeframe-key gate-info)
  "Emit structured telemetry for pattern gate decision."
  (when (fboundp 'swimmy.core::emit-telemetry-event)
    (swimmy.core::emit-telemetry-event "execution.pattern_gate"
      :service "school"
      :severity "info"
      :data (jsown:new-js
              ("symbol" symbol)
              ("timeframe" timeframe-key)
              ("strategy" (%pattern-gate-get gate-info :strategy "unknown"))
              ("direction" (%pattern-gate-get gate-info :direction "UNKNOWN"))
              ("applied" (if (%pattern-gate-get gate-info :applied nil) t nil))
              ("reason" (%pattern-gate-get gate-info :reason "UNKNOWN"))
              ("error" (%pattern-gate-get gate-info :error ""))
              ("expected_label" (%pattern-gate-get gate-info :expected_label ""))
              ("best_label" (%pattern-gate-get gate-info :best_label ""))
              ("expected_prob" (%pattern-gate-get gate-info :expected_prob 0.0))
              ("p_up" (%pattern-gate-get gate-info :p_up 0.0))
              ("p_down" (%pattern-gate-get gate-info :p_down 0.0))
              ("p_flat" (%pattern-gate-get gate-info :p_flat 0.0))
              ("threshold" (%pattern-gate-get gate-info :threshold *pattern-gate-threshold*))
              ("k" (%pattern-gate-get gate-info :k *pattern-gate-k*))
              ("lot_before" (%pattern-gate-get gate-info :lot_before 0.0))
              ("lot_after" (%pattern-gate-get gate-info :lot_after 0.0))))))


;;; P3 Refactor: Decomposed Execution Helpers (Expert Panel 2026-01-20)

(defun prepare-trade-context (category symbol)
  "Helper: Resolve strategy, timeframe, and history context."
  (let* ((strategies (gethash category *active-team*))
         (lead-strat (first strategies))
         (lead-name (when lead-strat (strategy-name lead-strat)))
         (timeframe (if lead-strat (strategy-timeframe lead-strat) 1))
         (timeframe-key (cond ((= timeframe 5) "M5") ((= timeframe 15) "M15") ((= timeframe 30) "M30")
                             ((= timeframe 60) "H1") ((= timeframe 240) "H4") ((= timeframe 1440) "D1")
                             ((= timeframe 10080) "W1") ((= timeframe 43200) "MN1")
                             (t "M1")))
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
    (t t)))

(defun execute-order-sequence (category direction symbol bid ask lot lead-name timeframe-key magic-override &key pair-id)
  "Helper: atomic reservation and execution."
  (declare (ignore magic-override))
  (let* ((entry-bid bid)
         (entry-ask ask)
         (entry-spread-pips (spread-pips-from-bid-ask symbol bid ask))
         (entry-cost-pips entry-spread-pips))
    ;; Reservation
    (multiple-value-bind (slot-index magic)
        (try-reserve-warrior-slot category lead-name symbol direction
                                  :pair-id pair-id
                                  :lot lot
                                  :entry-bid entry-bid
                                  :entry-ask entry-ask
                                  :entry-spread-pips entry-spread-pips
                                  :entry-cost-pips entry-cost-pips)
      (unless slot-index
        (format t "[ALLOC] ⚠️ Category ~a Full (4/4)!~%" category)
        (return-from execute-order-sequence nil))

      (let ((committed nil))
        (unwind-protect
            (progn
              (close-opposing-category-positions category direction symbol (if (eq direction :buy) bid ask) "Doten")
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
                (when (fboundp 'swimmy.core::emit-telemetry-event)
                  (let* ((dir (if (eq direction :buy) "BUY" "SELL"))
                         (cat (if (keywordp category) (string-downcase (symbol-name category)) (format nil "~a" category)))
                         (pip-size (get-pip-size symbol)))
                    (swimmy.core::emit-telemetry-event "execution.order_submitted"
                      :service "school"
                      :severity "info"
                      :data (jsown:new-js
                              ("symbol" symbol)
                              ("direction" dir)
                              ("category" cat)
                              ("strategy" (or lead-name "unknown"))
                              ("timeframe" timeframe-key)
                              ("lot" lot)
                              ("magic" magic)
                              ("entry_bid" entry-bid)
                              ("entry_ask" entry-ask)
                              ("spread_pips" entry-spread-pips)
                              ("cost_pips" entry-cost-pips)
                              ("pip_size" pip-size))))
                (request-mt5-positions)
                t))
          ;; Cleanup
          (unless committed
            (remhash magic *allocation-pending-orders*)
            (remhash (format nil "~a-~d" category slot-index) *warrior-allocation*)
            (format t "[ALLOC] ♻️ Released Slot ~d~%" slot-index))))))))

(defun execute-category-trade (category direction symbol bid ask
                               &key (lot-multiplier 1.0) signal-confidence)
  (format t "[TRACE] execute-category-trade ~a ~a~%" category direction)
  (handler-case
      (when (and (numberp bid) (numberp ask) (total-exposure-allowed-p))
        (let ((spread-pips (spread-pips-from-bid-ask symbol bid ask)))
          (when (> spread-pips *max-spread-pips*)
            (format t "[EXEC] 🚫 Spread Reject: ~a spread=~,2f pips (max=~,2f)~%"
                    symbol spread-pips *max-spread-pips*)
            (when (fboundp 'swimmy.core::emit-telemetry-event)
              (swimmy.core::emit-telemetry-event "execution.spread_reject"
                :service "school"
                :severity "info"
                :data (jsown:new-js
                        ("symbol" symbol)
                        ("bid" bid)
                        ("ask" ask)
                        ("spread_pips" spread-pips)
                        ("max_spread_pips" *max-spread-pips*)
                        ("pip_size" (get-pip-size symbol)))))
            (return-from execute-category-trade nil)))
        (multiple-value-bind (lead-name timeframe-key history) (prepare-trade-context category symbol)
          (when (validate-trade-opportunity category symbol timeframe-key history)
             (let* ((rank-data (when lead-name (get-strategy-rank lead-name)))
                    (rank (if rank-data (strategy-rank-rank rank-data) :incubator))
                    (base-lot (get-category-lot category))
                    (lot (calc-execution-lot category symbol history rank base-lot lead-name direction))
                    (overlay (apply-pair-overlay lead-name direction symbol lot))
                    (final-lot (first overlay))
                    (pair-id (second overlay))
                    (confidence (normalize-signal-confidence signal-confidence))
                    (safe-lot-mult (if (numberp lot-multiplier)
                                       (max 0.0 (float lot-multiplier))
                                       (signal-confidence-lot-multiplier confidence)))
                    (confidence-lot (max 0.01 (* final-lot safe-lot-mult))))
               (multiple-value-bind (gated-lot gate-info)
                   (apply-pattern-soft-gate category symbol direction timeframe-key history confidence-lot lead-name)
                 (emit-pattern-gate-telemetry symbol timeframe-key gate-info)
                 (when (verify-signal-authority symbol direction category gated-lot rank lead-name)
                    ;; Sleep Randomization (Anti-Gaming)
                    (sleep (/ (random 2000) 1000.0))
                    (execute-order-sequence category direction symbol bid ask gated-lot lead-name timeframe-key nil
                                            :pair-id pair-id)))))))
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
             (let ((msg (swimmy.core:encode-sexp `((type . "CLOSE")
                                                   (symbol . ,symbol)
                                                   (magic . ,magic)))))
               (pzmq:send *cmd-publisher* msg))
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
  "V49.0+: Guard live execution until enough S-rank strategies are available."
  (let* ((required (if (and (integerp *min-s-rank-strategies-for-live*)
                            (> *min-s-rank-strategies-for-live* 0))
                       *min-s-rank-strategies-for-live*
                       1))
         (s-count (count-if (lambda (s) (eq (strategy-rank s) :S))
                            *strategy-knowledge-base*)))
    (if (>= s-count required)
        t
        (progn
          ;; Throttled logging (every 5 mins)
          (when (= (mod (get-universal-time) 300) 0)
            (format t "[GATE] 🛑 Live Trading Blocked: Only ~d/~d S-Rank strategies ready.~%"
                    s-count required))
          nil))))

(defun process-category-trades (symbol bid ask)
  ;; V19: Periodic stale allocation cleanup
  (when (boundp 'swimmy.main::*dispatch-step*)
    (setf swimmy.main::*dispatch-step* :tick/cleanup-stale-allocations))
  (cleanup-stale-allocations)
  ;; V45: Use per-symbol history for regime detection (Fix: Opus 2026-01-20)
  (when (boundp 'swimmy.main::*dispatch-step*)
    (setf swimmy.main::*dispatch-step* :tick/load-history))
  (let ((history (or (gethash symbol *candle-histories*) *candle-history*)))
    ;; V49.0: Added S-Rank Gate (Musk)
    ;; NOTE: Set dispatch-step before each gate so Msg Error logs point to the true failing step
    ;; even when earlier helpers leave a stale step value behind.
    (when (boundp 'swimmy.main::*dispatch-step*)
      (setf swimmy.main::*dispatch-step* :tick/trading-allowed-p))
    (let ((allowed (trading-allowed-p)))
      (when (and allowed history (> (length history) 100))
        (when (boundp 'swimmy.main::*dispatch-step*)
          (setf swimmy.main::*dispatch-step* :tick/s-rank-gate-passed-p))
        (when (s-rank-gate-passed-p)
      (when (boundp 'swimmy.main::*dispatch-step*)
        (setf swimmy.main::*dispatch-step* :tick/close-category-positions))
      (close-category-positions symbol bid ask)
      (when (boundp 'swimmy.main::*dispatch-step*)
        (setf swimmy.main::*dispatch-step* :tick/is-safe-to-trade-p))
      (unless (is-safe-to-trade-p) (return-from process-category-trades nil))
      (when (boundp 'swimmy.main::*dispatch-step*)
        (setf swimmy.main::*dispatch-step* :tick/volatility-allows-trading-p))
      (unless (volatility-allows-trading-p) (return-from process-category-trades nil))
    
      (when (>= (length history) 50)
        (when (boundp 'swimmy.main::*dispatch-step*)
          (setf swimmy.main::*dispatch-step* :tick/research-enhanced-analysis))
        (research-enhanced-analysis history)
        (when (boundp 'swimmy.main::*dispatch-step*)
          (setf swimmy.main::*dispatch-step* :tick/detect-regime-hmm))
        (detect-regime-hmm history))
    
      (when (boundp 'swimmy.main::*dispatch-step*)
        (setf swimmy.main::*dispatch-step* :tick/elect-leader))
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
                           (signal-confidence (normalize-signal-confidence (getf top-sig :confidence)))
                           (confidence-lot-mult (signal-confidence-lot-multiplier signal-confidence))
                           (strat-key (intern (format nil "~a-~a" top-cat top-name) :keyword)))
                      (when (<= confidence-lot-mult 0.0)
                        (format t "[L] ⏭️ SKIP LOW CONF: ~a conf=~,2f (min=~,2f)~%"
                                top-name signal-confidence *signal-confidence-entry-threshold*))
                      (when (and (> confidence-lot-mult 0.0)
                                 (can-category-trade-p strat-key))
                        (let ((trade-executed
                                (execute-category-trade top-cat direction symbol bid ask
                                                        :lot-multiplier confidence-lot-mult
                                                        :signal-confidence signal-confidence)))
                          (when trade-executed
                            (format t "~a~%" (generate-dynamic-narrative top-sig symbol bid))
                            (record-category-trade-time strat-key)
                            (when (fboundp 'record-strategy-trade)
                              (record-strategy-trade top-name :trade 0)))))))))))
        (error (e)
          (declare (ignore e))
          nil)))))))

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

(defparameter *special-force-skip-hunter-auto-founders* t
  "When T, recruit-special-forces skips founders sourced from school-hunter-auto.lisp.")

(defun hunter-auto-founder-key-p (key)
  "True when founder key metadata indicates school-hunter-auto source."
  (let* ((meta (and (boundp '*founder-registry-meta*)
                    *founder-registry-meta*
                    (gethash key *founder-registry-meta*)))
         (src (and (listp meta) (getf meta :source-file))))
    (and (stringp src)
         (search "school-hunter-auto.lisp" src :test #'char-equal))))

(defun recruit-special-forces ()
  (force-recruit-strategy "T-Nakane-Gotobi")
  (let ((symbols (if (and (boundp 'swimmy.core::*supported-symbols*)
                          swimmy.core::*supported-symbols*)
                     swimmy.core::*supported-symbols*
                     '("USDJPY")))
        (known-names (make-hash-table :test 'equal))
        (recruited 0)
        (auto-skipped 0))
    (dolist (s *strategy-knowledge-base*)
      (let ((name (and s (strategy-name s))))
        (when (and name (stringp name))
          (setf (gethash name known-names) t))))
    (maphash
     (lambda (key maker-func)
       (when (functionp maker-func)
         (if (and *special-force-skip-hunter-auto-founders*
                  (hunter-auto-founder-key-p key))
             (incf auto-skipped)
             (let ((proto (handler-case (funcall maker-func)
                            (error () nil))))
               (when proto
                 (let ((base-name (strategy-name proto)))
                   (when (and base-name (stringp base-name))
                     ;; Multi-symbol evolution: recruit the same founder archetype per
                     ;; supported symbol, with distinct names (P12.5 contract).
                     (dolist (sym symbols)
                       (let ((name (rewrite-strategy-name-for-symbol base-name sym)))
                         (unless (and name (gethash name known-names))
                           (when (recruit-founder key :symbol sym)
                             (incf recruited))
                           (when name
                             (setf (gethash name known-names) t))))))))))))
     *founder-registry*)
    (format t "[L] 🎖️ Special Force recruit run complete: ~d new founder attempts (auto-skipped ~d)~%"
            recruited auto-skipped)))

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
