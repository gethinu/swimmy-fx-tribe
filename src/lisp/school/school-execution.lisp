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
(defparameter *pattern-gate-fade-lot-multiplier* 0.55
  "Soft gate multiplier when Pattern decision_action is FADE.")
(defparameter *pattern-gate-no-trade-lot-multiplier* 0.35
  "Soft gate multiplier when Pattern decision_action is NO-TRADE.")
(defparameter *pattern-gate-k* 30
  "Top-k neighbors for Pattern Similarity query.")
(defparameter *pattern-gate-timeframes* '("H1" "H4" "D1" "W1" "MN")
  "Timeframes where pattern soft-gate is enabled.")
(defparameter *signal-confidence-entry-threshold* 0.20
  "Minimum signal confidence required to allow a new entry.")
(defparameter *signal-confidence-soft-threshold* 0.35
  "Confidence threshold to restore full lot size.")
(defparameter *signal-confidence-soft-lot-multiplier* 0.55
  "Lot multiplier for medium-confidence entries.")
(defparameter *min-s-rank-strategies-for-live* 2
  "Legacy threshold (monitoring-only). Live execution path no longer blocks on S-rank count.")
(defparameter *live-edge-guard-enabled* t
  "When T, block LIVE execution for strategies with degraded recent LIVE edge.")
(defparameter *live-edge-guard-lookback-trades* 40
  "Latest LIVE trade rows used for runtime edge guard checks.")
(defparameter *live-edge-guard-min-trades* 20
  "Minimum LIVE trades required before runtime edge guard can block execution.")
(defparameter *live-edge-guard-pf-min* 1.05
  "Runtime edge guard minimum PF floor on latest LIVE trades.")
(defparameter *live-edge-guard-wr-min* 0.35
  "Runtime edge guard minimum WR floor on latest LIVE trades.")
(defparameter *live-edge-guard-net-pnl-min* 0.0
  "Runtime edge guard minimum net pnl floor on latest LIVE trades.")
(defparameter *live-edge-guard-max-latest-loss-streak* 3
  "Runtime edge guard maximum allowed consecutive losses from the latest LIVE trade.")
(defparameter *live-edge-guard-alert-dedupe-seconds* 900
  "Minimum interval between repeated live-edge block alerts for same strategy/reason.")
(defparameter *live-edge-guard-last-alert-at* (make-hash-table :test 'equal)
  "Per strategy/reason timestamp for live-edge block alert deduplication.")
(defparameter *runtime-selection-stability-weight* 0.30
  "Weight applied to recent stability score in runtime candidate scoring.")
(defparameter *runtime-selection-rank-weight* 0.08
  "Weight applied to rank score in runtime candidate scoring.")
(defparameter *runtime-selection-diversification-penalty-weight* 0.40
  "Weight applied to diversification/correlation penalty in runtime candidate scoring.")
(defparameter *runtime-selection-gate-penalty* 0.60
  "Penalty applied when deployment gate is not LIVE_READY.")
(defparameter *runtime-selection-live-edge-penalty* 0.45
  "Penalty applied when live-edge guard is currently failing.")
(defparameter *runtime-selection-high-diversification-threshold* 0.45
  "Penalty threshold where diversification pressure is considered high.")
(defparameter *runtime-selection-max-diversification-penalty* 1.20
  "Maximum diversification penalty cap in runtime scoring.")
(defparameter *symbol-sl-pips-overrides* (make-hash-table :test 'equal)
  "Per-symbol SL pips overrides keyed by uppercase symbol string.")
(defparameter *symbol-tp-pips-overrides* (make-hash-table :test 'equal)
  "Per-symbol TP pips overrides keyed by uppercase symbol string.")
(defparameter *symbol-sltp-overrides-path*
  (let* ((raw (ignore-errors
                (swimmy.core::getenv-or-dotenv "SWIMMY_SYMBOL_SLTP_OVERRIDES_PATH")))
         (trimmed (and (stringp raw)
                       (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))
    (if (and (stringp trimmed) (> (length trimmed) 0))
        (namestring
         (merge-pathnames trimmed (swimmy.core::resolve-swimmy-home)))
        (namestring (swimmy.core::swimmy-path ".opus/symbol_sltp_overrides.sexp"))))
  "Path to persisted per-symbol SL/TP override S-expression.")

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
;;; EXECUTION & SLOTS
;;; ==========================================


;; NOTE: get-slot-magic is defined in school-allocation.lisp
;; Do not duplicate here - it uses deterministic format: 1[CatID][Slot] (e.g., 110000)

(defun find-free-slot (category)
  (loop for i from 0 to 3
        for key = (format nil "~a-~d" category i)
        when (null (gethash key *slot-allocation*))
        return i))

(defun find-free-warrior-slot (category)
  "Deprecated compatibility alias. Use FIND-FREE-SLOT."
  (find-free-slot category))

(defun close-opposing-category-positions (category new-direction symbol price reason)
  "Close positions in the opposite direction for Doten (Stop and Reverse) logic"
  (declare (ignore reason))
  (let ((opposing-direction (if (eq new-direction :buy) :short :long))
        (closed-count 0))
    (maphash 
     (lambda (key slot)
       (when (and slot 
                  (equal (getf slot :category) category)
                  (eq (getf slot :direction) opposing-direction)
                  (equal (getf slot :symbol) symbol))
         ;; Close it
         (let* ((magic (getf slot :magic))
                (entry (getf slot :entry))
                (lot (or (getf slot :lot) 0.01))
                (pnl (if (eq opposing-direction :long)
                         (- price entry)
                         (- entry price))))
           ;; Send CLOSE command
           (let ((msg (swimmy.core:encode-sexp `((type . "CLOSE")
                                                 (symbol . ,symbol)
                                                 (magic . ,magic)))))
             (pzmq:send *cmd-publisher* msg))
           ;; Free slot
           (remhash key *slot-allocation*)
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
     *slot-allocation*)
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
    ((and (< now (+ *warmup-end-time* 60)) (> (hash-table-count *slot-allocation*) 0)) nil)
    ;; 4. Market Hours
    ((not (market-open-p)) nil)
    ;; 4.5 Armada runtime kill switch (fail-close)
    ((and (fboundp 'enforce-armada-kill-switch)
          (enforce-armada-kill-switch now))
     nil)
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

(defun %normalize-pattern-gate-timeframe-label (timeframe-key)
  "Normalize arbitrary TF key to finite bucket label (e.g., H5 -> H4)."
  (let* ((raw-min (if (fboundp 'get-tf-minutes)
                      (get-tf-minutes timeframe-key)
                      (if (numberp timeframe-key) (round timeframe-key) 1)))
         (bucket-min (if (fboundp 'get-tf-bucket-minutes)
                         (get-tf-bucket-minutes raw-min)
                         raw-min)))
    (if (fboundp 'get-tf-string)
        (get-tf-string bucket-min)
        (string-upcase (format nil "~a" timeframe-key)))))

(defun pattern-gate-enabled-timeframe-p (timeframe-key)
  "Return T when Pattern gate should run for TIMEFRAME-KEY."
  (member (%normalize-pattern-gate-timeframe-label timeframe-key)
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

(defun %pattern-gate-coerce-bool (value &optional (default nil))
  "Best-effort conversion to boolean."
  (cond
    ((eq value t) t)
    ((null value) nil)
    ((numberp value) (not (zerop value)))
    ((stringp value)
     (let ((v (string-downcase (string-trim '(#\Space #\Tab #\Newline) value))))
       (cond
         ((member v '("true" "#t" "t" "1" "yes" "on") :test #'string=) t)
         ((member v '("false" "#f" "nil" "0" "no" "off" "") :test #'string=) nil)
         (t default))))
    (t default)))

(defun %pattern-gate-normalize-action (value)
  "Normalize decision action text into \"follow\"/\"fade\"/\"no-trade\"/\"unknown\"."
  (let ((txt (string-downcase (string-trim '(#\Space #\Tab #\Newline)
                                           (format nil "~a" (or value ""))))))
    (cond
      ((string= txt "follow") "follow")
      ((string= txt "fade") "fade")
      ((string= txt "no-trade") "no-trade")
      (t "unknown"))))

(defun %pattern-gate-decision-multiplier (decision-action enforce-no-trade)
  "Return (values multiplier reason-code) for decision-driven soft gate.
Multiplier NIL means fallback to direction mismatch logic."
  (cond
    (enforce-no-trade
     (values (max 0.01 (float *pattern-gate-no-trade-lot-multiplier*))
             "DECISION_NO_TRADE"))
    ((string= decision-action "no-trade")
     (values (max 0.01 (float *pattern-gate-no-trade-lot-multiplier*))
             "DECISION_NO_TRADE"))
    ((string= decision-action "fade")
     (values (max 0.01 (float *pattern-gate-fade-lot-multiplier*))
             "DECISION_FADE"))
    (t
     (values nil nil))))

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
  (let* ((base-lot (max 0.01 (float lot)))
         (tf-label (%normalize-pattern-gate-timeframe-label timeframe-key)))
    (unless (pattern-gate-enabled-timeframe-p timeframe-key)
      (return-from apply-pattern-soft-gate
        (values base-lot
                `((:applied . nil)
                  (:reason . "SKIP_TF")
                  (:category . ,category)
                  (:symbol . ,symbol)
                  (:timeframe . ,tf-label)
                  (:strategy . ,lead-name)
                  (:lot_before . ,base-lot)
                  (:lot_after . ,base-lot)))))

    (multiple-value-bind (result err)
        (swimmy.core:query-pattern-similarity symbol tf-label history :k *pattern-gate-k* :direction direction)
      (when err
        (return-from apply-pattern-soft-gate
          (values base-lot
                  `((:applied . nil)
                    (:reason . "UNAVAILABLE")
                    (:error . ,err)
                    (:category . ,category)
                    (:symbol . ,symbol)
                    (:timeframe . ,tf-label)
                    (:strategy . ,lead-name)
                    (:lot_before . ,base-lot)
                    (:lot_after . ,base-lot)))))

      (let* ((p-up (%pattern-gate-coerce-float (swimmy.core:sexp-alist-get result 'p_up) (/ 1.0 3.0)))
             (p-down (%pattern-gate-coerce-float (swimmy.core:sexp-alist-get result 'p_down) (/ 1.0 3.0)))
             (p-flat (%pattern-gate-coerce-float (swimmy.core:sexp-alist-get result 'p_flat) (/ 1.0 3.0)))
             (distortion-passed (%pattern-gate-coerce-bool (swimmy.core:sexp-alist-get result 'distortion_passed) t))
             (distortion-score (%pattern-gate-coerce-float (swimmy.core:sexp-alist-get result 'distortion_score) 0.0))
             (distortion-threshold (%pattern-gate-coerce-float (swimmy.core:sexp-alist-get result 'distortion_threshold) 0.0))
             (policy-mode (or (swimmy.core:sexp-alist-get result 'policy_mode) "shadow"))
             (decision-action (%pattern-gate-normalize-action
                               (swimmy.core:sexp-alist-get result 'decision_action)))
             (decision-reason (or (swimmy.core:sexp-alist-get result 'decision_reason) "unknown"))
             (enforce-no-trade (%pattern-gate-coerce-bool
                                (swimmy.core:sexp-alist-get result 'enforce_no_trade)
                                nil))
             (ev-follow (%pattern-gate-coerce-float (swimmy.core:sexp-alist-get result 'ev_follow) 0.0))
             (ev-fade (%pattern-gate-coerce-float (swimmy.core:sexp-alist-get result 'ev_fade) 0.0))
             (vector-weight-applied (let ((raw (swimmy.core:sexp-alist-get result 'vector_weight_applied)))
                                      (if raw (%pattern-gate-coerce-float raw 0.0) nil)))
             (weight-source (or (swimmy.core:sexp-alist-get result 'weight_source) "unknown"))
             (backend-used (or (swimmy.core:sexp-alist-get result 'backend_used) "unknown"))
             (expected (if (eq direction :buy) :up :down))
             (expected-prob (if (eq expected :up) p-up p-down))
             (best-label (%pattern-best-label p-up p-down p-flat))
             (match-p (and (>= expected-prob *pattern-gate-threshold*)
                           (eq best-label expected))))
        (multiple-value-bind (decision-mult decision-reason-code)
            (%pattern-gate-decision-multiplier decision-action enforce-no-trade)
          (let* ((decision-applied-p (not (null decision-mult)))
                 (applied (and distortion-passed (or decision-applied-p (not match-p))))
                 (adjusted (cond
                             ((not distortion-passed) base-lot)
                             (decision-mult (max 0.01 (* base-lot decision-mult)))
                             ((not match-p) (max 0.01 (* base-lot *pattern-gate-lot-multiplier*)))
                             (t base-lot))))
        (values adjusted
                `((:applied . ,applied)
                  (:reason . ,(cond
                                ((not distortion-passed) "LOW_DISTORTION")
                                (decision-reason-code decision-reason-code)
                                (applied "MISMATCH")
                                (t "MATCH")))
                  (:category . ,category)
                  (:symbol . ,symbol)
                  (:timeframe . ,tf-label)
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
                  (:distortion_passed . ,distortion-passed)
                  (:distortion_score . ,distortion-score)
                  (:distortion_threshold . ,distortion-threshold)
                  (:policy_mode . ,policy-mode)
                  (:decision_action . ,decision-action)
                  (:decision_reason . ,decision-reason)
                  (:enforce_no_trade . ,enforce-no-trade)
                  (:ev_follow . ,ev-follow)
                  (:ev_fade . ,ev-fade)
                  (:vector_weight_applied . ,vector-weight-applied)
                  (:weight_source . ,weight-source)
                  (:backend_used . ,backend-used)
                  (:lot_before . ,base-lot)
                  (:lot_after . ,adjusted)))))))))

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
              ("distortion_passed" (if (%pattern-gate-get gate-info :distortion_passed nil) t nil))
              ("distortion_score" (%pattern-gate-get gate-info :distortion_score 0.0))
              ("distortion_threshold" (%pattern-gate-get gate-info :distortion_threshold 0.0))
              ("policy_mode" (%pattern-gate-get gate-info :policy_mode "shadow"))
              ("decision_action" (%pattern-gate-get gate-info :decision_action "unknown"))
              ("decision_reason" (%pattern-gate-get gate-info :decision_reason "unknown"))
              ("enforce_no_trade" (if (%pattern-gate-get gate-info :enforce_no_trade nil) t nil))
              ("ev_follow" (%pattern-gate-get gate-info :ev_follow 0.0))
              ("ev_fade" (%pattern-gate-get gate-info :ev_fade 0.0))
              ("vector_weight_applied" (%pattern-gate-get gate-info :vector_weight_applied 0.0))
              ("weight_source" (%pattern-gate-get gate-info :weight_source "unknown"))
              ("backend_used" (%pattern-gate-get gate-info :backend_used "unknown"))
              ("lot_before" (%pattern-gate-get gate-info :lot_before 0.0))
              ("lot_after" (%pattern-gate-get gate-info :lot_after 0.0))))))


;;; P3 Refactor: Decomposed Execution Helpers (Expert Panel 2026-01-20)

(defun %normalize-strategy-token (value)
  "Normalize VALUE into uppercase trimmed token, or NIL."
  (when value
    (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (format nil "~a" value))))
      (when (> (length text) 0)
        (string-upcase text)))))

(defun %nil-like-strategy-name-p (value)
  "True when VALUE is missing or one of the reserved NIL-like strategy tokens."
  (let ((token (%normalize-strategy-token value)))
    (or (null token)
        (member token '("NIL" "UNKNOWN" "NULL" "NONE") :test #'string=))))

(defun resolve-strategy-by-name (name)
  "Best-effort resolve strategy object by NAME from KB/evolved pools."
  (when (and (stringp name) (not (%nil-like-strategy-name-p name)))
    (or (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=)
        (find name *evolved-strategies* :key #'strategy-name :test #'string=))))

<<<<<<< HEAD
(defun resolve-live-effective-sltp (strategy-name)
  "Resolve live SL/TP pips and source with strategy-first fallback.
   Current model supports strategy value -> global default precedence."
  (let* ((strat (and (stringp strategy-name)
                     (not (%nil-like-strategy-name-p strategy-name))
                     (resolve-strategy-by-name strategy-name)))
         (raw-sl (and strat (strategy-sl strat)))
         (raw-tp (and strat (strategy-tp strat)))
         (strategy-sl (and (numberp raw-sl) (> raw-sl 0.0) (float raw-sl 1.0)))
         (strategy-tp (and (numberp raw-tp) (> raw-tp 0.0) (float raw-tp 1.0))))
    (if (and strategy-sl strategy-tp)
        (values strategy-sl strategy-tp "strategy")
        (values (float *default-sl-pips* 1.0)
                (float *default-tp-pips* 1.0)
                "global_default"))))
=======
(defun %normalize-sltp-pips-value (value)
  (and (numberp value)
       (> value 0.0)
       (float value 1.0)))

(defun %normalize-symbol-override-key (symbol)
  (when symbol
    (let ((raw (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           (format nil "~a" symbol)))))
      (and (> (length raw) 0) raw))))

(defun %lookup-symbol-pips-override (table symbol)
  (let ((key (%normalize-symbol-override-key symbol)))
    (when (and key (hash-table-p table))
      (%normalize-sltp-pips-value (gethash key table nil)))))

(defun %symbol-sltp-entry-field (entry key)
  (or (and (listp entry) (getf entry key))
      (and (consp entry)
           (listp (rest entry))
           (or (getf (rest entry) key)
               (let ((payload (second entry)))
                 (and (listp payload) (getf payload key)))))))

(defun %symbol-sltp-entries (form)
  (cond
    ((null form) '())
    ((and (listp form) (keywordp (first form)))
     (let ((entries (getf form :symbols)))
       (if (listp entries) entries '())))
    ((listp form) form)
    (t '())))

(defun load-symbol-sltp-overrides (&key (path *symbol-sltp-overrides-path*))
  "Load persisted symbol SL/TP overrides from PATH into current hash tables.
   Accepted format:
   (:schema-version 1 :symbols ((:symbol \"USDJPY\" :sl 0.33 :tp 0.55) ...))"
  (setf *symbol-sl-pips-overrides* (make-hash-table :test 'equal)
        *symbol-tp-pips-overrides* (make-hash-table :test 'equal))
  (when (and path (probe-file path))
    (handler-case
        (let* ((form (swimmy.core:read-sexp-file path :package :swimmy.school))
               (loaded 0))
          (dolist (entry (%symbol-sltp-entries form) loaded)
            (let* ((symbol (%normalize-symbol-override-key
                            (or (%symbol-sltp-entry-field entry :symbol)
                                (and (consp entry) (first entry)))))
                   (sl (%normalize-sltp-pips-value (%symbol-sltp-entry-field entry :sl)))
                   (tp (%normalize-sltp-pips-value (%symbol-sltp-entry-field entry :tp))))
              (when (and symbol sl tp)
                (setf (gethash symbol *symbol-sl-pips-overrides*) sl
                      (gethash symbol *symbol-tp-pips-overrides*) tp)
                (incf loaded)))))
      (error (e)
        (format t "[EXEC] ⚠️ Failed to load symbol SL/TP overrides from ~a: ~a~%"
                path e)
        nil))))

(defun resolve-live-effective-sltp (strategy-name &key symbol)
  "Resolve execution SL/TP pips with precedence:
   strategy value -> symbol override -> global default.
   Partial pairs are ignored; source changes only when both SL and TP are valid."
  (let* ((strat (and (stringp strategy-name)
                     (not (%nil-like-strategy-name-p strategy-name))
                     (resolve-strategy-by-name strategy-name)))
         (resolved-symbol (or symbol
                              (and strat (strategy-symbol strat))))
         (strategy-sl (%normalize-sltp-pips-value (and strat (strategy-sl strat))))
         (strategy-tp (%normalize-sltp-pips-value (and strat (strategy-tp strat))))
         (symbol-sl (%lookup-symbol-pips-override *symbol-sl-pips-overrides* resolved-symbol))
         (symbol-tp (%lookup-symbol-pips-override *symbol-tp-pips-overrides* resolved-symbol)))
    (if (and strategy-sl strategy-tp)
        (values strategy-sl strategy-tp "strategy")
        (if (and symbol-sl symbol-tp)
            (values symbol-sl symbol-tp "symbol_override")
            (values (float *default-sl-pips* 1.0)
                    (float *default-tp-pips* 1.0)
                    "global_default")))))

(eval-when (:load-toplevel :execute)
  (ignore-errors (load-symbol-sltp-overrides)))
>>>>>>> da28fca8275070012eeaccbc387481dbbff3ad83

(defun %digits-only-p (text)
  "Return T when TEXT consists of one or more ASCII digits."
  (and (stringp text)
       (> (length text) 0)
       (loop for ch across text always (digit-char-p ch))))

(defun resolve-execution-timeframe-minutes (value)
  "Strictly parse VALUE into timeframe minutes for live execution.
Returns NIL for missing/invalid labels instead of silently coercing to M1."
  (labels ((parse-positive-int (text)
             (when (%digits-only-p text)
               (handler-case
                   (let ((n (parse-integer text)))
                     (and (> n 0) n))
                 (error () nil))))
           (parse-string (raw)
             (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw))
                    (up (string-upcase trimmed)))
               (cond
                 ((or (string= up "") (string= up "NIL")) nil)
                 ((string= up "MN") 43200)
                 ((string= up "MN1") 43200)
                 ((%digits-only-p up) (parse-positive-int up))
                 ((and (>= (length up) 2)
                       (member (char up 0) '(#\M #\H #\D #\W) :test #'char=))
                  (let ((mag (parse-positive-int (subseq up 1))))
                    (when mag
                      (case (char up 0)
                        (#\M mag)
                        (#\H (* 60 mag))
                        (#\D (* 1440 mag))
                        (#\W (* 10080 mag))
                        (otherwise nil)))))
                 (t nil)))))
    (cond
      ((null value) nil)
      ((numberp value)
       (let ((minutes (round value)))
         (and (> minutes 0) minutes)))
      ((stringp value) (parse-string value))
      ((symbolp value) (parse-string (symbol-name value)))
      (t nil))))

(defun prepare-trade-context (category symbol &key strategy-name strategy-timeframe)
  "Helper: Resolve strategy, timeframe, and history context.
   Optional STRATEGY-NAME/STRATEGY-TIMEFRAME override active-team fallback."
  (let* ((strategies (gethash category *active-team*))
         (lead-strat (first strategies))
         (signal-strat (resolve-strategy-by-name strategy-name))
         (context-strat (or signal-strat lead-strat))
         (lead-name (or strategy-name (and context-strat (strategy-name context-strat))))
         ;; Prefer canonical strategy TF when strategy object is resolvable.
         ;; Caller-provided TF can be stale (e.g., cached signal payload from pre-migration state).
         (tf-slot (or (and context-strat (strategy-timeframe context-strat))
                      strategy-timeframe))
         (timeframe (resolve-execution-timeframe-minutes tf-slot))
         ;; Never silently fall back to M1 for unknown TF minutes.
         ;; If DataKeeper doesn't provide that TF, we resample from M1 using aligned buckets.
         (timeframe-key (and timeframe
                             (if (fboundp 'get-tf-string) (get-tf-string timeframe) nil)))
         (history (cond
                    ((null timeframe) nil)
                    ((= timeframe 1) (gethash symbol *candle-histories*))
                    (t
                     (let ((tf-map (gethash symbol *candle-histories-tf*)))
                       (or (and tf-map
                                (or (gethash timeframe-key tf-map)
                                    (and (stringp timeframe-key)
                                         (string= timeframe-key "MN")
                                         (gethash "MN1" tf-map))
                                    (and (stringp timeframe-key)
                                         (string= timeframe-key "MN1")
                                         (gethash "MN" tf-map))))
                           (when (gethash symbol *candle-histories*)
                             (resample-candles (gethash symbol *candle-histories*) timeframe))))))))
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

(defun report-execution-context-missing (symbol category reason &key strategy-name timeframe-key)
  "Emit fail-closed observability signals (Discord + telemetry) for context-missing skips."
  (let* ((cat (if (keywordp category) (string-downcase (symbol-name category)) (format nil "~a" category)))
         (reason-label (case reason
                         (:missing-strategy "Missing strategy context")
                         (:missing-timeframe "Missing timeframe context")
                         (otherwise "Missing execution context")))
         (strategy-text (if (%nil-like-strategy-name-p strategy-name)
                            "N/A"
                            (format nil "~a" strategy-name)))
         (timeframe-text (if (and timeframe-key
                                  (not (string= (string-upcase (format nil "~a" timeframe-key)) "NIL")))
                             (format nil "~a" timeframe-key)
                             "N/A"))
         (msg (format nil "🚫 EXECUTION BLOCKED (Context Missing)~%Symbol: ~a~%Category: ~a~%Reason: ~a~%Strategy: ~a~%Timeframe: ~a"
                      symbol cat reason-label strategy-text timeframe-text)))
    (when (fboundp 'swimmy.core::emit-telemetry-event)
      (swimmy.core::emit-telemetry-event "execution.context_missing"
        :service "school"
        :severity "warn"
        :data (jsown:new-js
                ("symbol" symbol)
                ("category" cat)
                ("reason" reason-label)
                ("strategy" strategy-text)
                ("timeframe" timeframe-text))))
    (when (fboundp 'swimmy.core:notify-discord-alert)
      (swimmy.core:notify-discord-alert msg :color 15158332))
    nil))

(defun %deployment-gate-live-ready-p (gate-status)
  "Return T only when GATE-STATUS decision is LIVE_READY."
  (let ((decision (%normalize-strategy-token (and (listp gate-status)
                                                  (getf gate-status :decision nil)))))
    (and decision (string= decision "LIVE_READY"))))

(defun report-deployment-gate-blocked (symbol category strategy-name gate-status)
  "Emit observability signals when deployment gate blocks live execution."
  (let* ((cat (if (keywordp category) (string-downcase (symbol-name category)) (format nil "~a" category)))
         (strategy-text (if (%nil-like-strategy-name-p strategy-name)
                            "N/A"
                            (format nil "~a" strategy-name)))
         (decision-token (%normalize-strategy-token (and (listp gate-status)
                                                         (getf gate-status :decision nil))))
         (decision-text (or decision-token "UNSET"))
         (reason-text (let ((raw (and (listp gate-status)
                                      (getf gate-status :reason nil))))
                        (if (and raw (> (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                              (format nil "~a" raw)))
                                        0))
                            (format nil "~a" raw)
                            "deployment gate row missing")))
         (msg (format nil "🚫 EXECUTION BLOCKED (Deployment Gate)~%Symbol: ~a~%Category: ~a~%Strategy: ~a~%Decision: ~a~%Reason: ~a"
                      symbol cat strategy-text decision-text reason-text)))
    (when (fboundp 'swimmy.core::emit-telemetry-event)
      (swimmy.core::emit-telemetry-event "execution.deployment_gate_blocked"
        :service "school"
        :severity "warn"
        :data (jsown:new-js
                ("symbol" symbol)
                ("category" cat)
                ("strategy" strategy-text)
                ("decision" decision-text)
                ("reason" reason-text))))
    (when (fboundp 'swimmy.core:notify-discord-alert)
      (swimmy.core:notify-discord-alert msg :color 15158332))
    nil))

(defun %live-edge-guard-pass-p (strategy-name)
  "Return (values PASS-P FAIL-REASON METRICS) for runtime LIVE edge guard."
  (cond
    ((not *live-edge-guard-enabled*) (values t :disabled nil))
    ((or (null strategy-name) (not (stringp strategy-name)) (= (length strategy-name) 0))
     (values t :invalid-name nil))
    ((not (fboundp 'fetch-recent-live-trade-metrics))
     (values t :metrics-fn-missing nil))
    (t
     (let ((metrics (ignore-errors
                      (fetch-recent-live-trade-metrics
                       strategy-name
                       :limit *live-edge-guard-lookback-trades*))))
       (if (null metrics)
           (values t :metrics-unavailable nil)
           (let* ((trades (if (numberp (getf metrics :trades))
                              (round (getf metrics :trades))
                              0))
                  (pf (float (or (getf metrics :profit-factor) 0.0) 1.0))
                  (wr (float (or (getf metrics :win-rate) 0.0) 1.0))
                  (net (float (or (getf metrics :net-pnl) 0.0) 1.0))
                  (latest-loss-streak (if (numberp (getf metrics :latest-loss-streak))
                                          (max 0 (round (getf metrics :latest-loss-streak)))
                                          0))
                  (min-trades (max 1 (round *live-edge-guard-min-trades*)))
                  (max-latest-loss-streak (max 0 (round *live-edge-guard-max-latest-loss-streak*))))
             (cond
               ((< trades min-trades) (values t :insufficient-trades metrics))
               ((> latest-loss-streak max-latest-loss-streak)
                (values nil :loss-streak metrics))
               ((< pf (float *live-edge-guard-pf-min* 1.0)) (values nil :pf metrics))
               ((< wr (float *live-edge-guard-wr-min* 1.0)) (values nil :wr metrics))
               ((< net (float *live-edge-guard-net-pnl-min* 1.0)) (values nil :net-pnl metrics))
               (t (values t :ok metrics)))))))))

(defun %live-edge-fail-reason-label (reason)
  "Human readable reason text for live-edge runtime block."
  (case reason
    (:loss-streak (format nil "latest_loss_streak > ~d" *live-edge-guard-max-latest-loss-streak*))
    (:pf (format nil "pf < ~,2f" *live-edge-guard-pf-min*))
    (:wr (format nil "wr < ~,1f%%" (* 100.0 *live-edge-guard-wr-min*)))
    (:net-pnl (format nil "net_pnl < ~,2f" *live-edge-guard-net-pnl-min*))
    (otherwise "live edge guard failed")))

(defun %should-notify-live-edge-block-p (strategy-name reason)
  "Deduplicate repetitive live-edge block alerts."
  (let* ((key (format nil "~a|~a" (or strategy-name "N/A") reason))
         (now (get-universal-time))
         (last-ts (or (gethash key *live-edge-guard-last-alert-at*) 0))
         (dedupe-sec (max 0 (round *live-edge-guard-alert-dedupe-seconds*))))
    (if (or (<= dedupe-sec 0)
            (>= (- now last-ts) dedupe-sec))
        (progn
          (setf (gethash key *live-edge-guard-last-alert-at*) now)
          t)
        nil)))

(defun report-live-edge-guard-blocked (symbol category strategy-name reason metrics)
  "Emit observability signals when runtime LIVE edge guard blocks execution."
  (let* ((cat (if (keywordp category) (string-downcase (symbol-name category)) (format nil "~a" category)))
         (strategy-text (if (%nil-like-strategy-name-p strategy-name)
                            "N/A"
                            (format nil "~a" strategy-name)))
         (trades (if (numberp (getf metrics :trades)) (round (getf metrics :trades)) 0))
         (pf (float (or (getf metrics :profit-factor) 0.0) 1.0))
         (wr (float (or (getf metrics :win-rate) 0.0) 1.0))
         (net (float (or (getf metrics :net-pnl) 0.0) 1.0))
         (latest-loss-streak (if (numberp (getf metrics :latest-loss-streak))
                                 (max 0 (round (getf metrics :latest-loss-streak)))
                                 0))
         (reason-text (%live-edge-fail-reason-label reason))
         (msg (format nil "🚫 EXECUTION BLOCKED (Live Edge)~%Symbol: ~a~%Category: ~a~%Strategy: ~a~%Reason: ~a~%Recent LIVE: n=~d PF=~,2f WR=~,1f%% NET=~,2f latest_loss_streak=~d~%Thresholds: n>=~d PF>=~,2f WR>=~,1f%% NET>=~,2f latest_loss_streak<=~d"
                      symbol cat strategy-text reason-text
                      trades pf (* 100.0 wr) net latest-loss-streak
                      *live-edge-guard-min-trades*
                      *live-edge-guard-pf-min*
                      (* 100.0 *live-edge-guard-wr-min*)
                      *live-edge-guard-net-pnl-min*
                      *live-edge-guard-max-latest-loss-streak*)))
    (when (fboundp 'swimmy.core::emit-telemetry-event)
      (swimmy.core::emit-telemetry-event "execution.live_edge_blocked"
        :service "school"
        :severity "warn"
        :data (jsown:new-js
                ("symbol" symbol)
                ("category" cat)
                ("strategy" strategy-text)
                ("reason" reason-text)
                ("trades" trades)
                ("pf" pf)
                ("wr" wr)
                ("net_pnl" net)
                ("latest_loss_streak" latest-loss-streak)
                ("min_trades" *live-edge-guard-min-trades*)
                ("pf_min" *live-edge-guard-pf-min*)
                ("wr_min" *live-edge-guard-wr-min*)
                ("net_pnl_min" *live-edge-guard-net-pnl-min*)
                ("max_latest_loss_streak" *live-edge-guard-max-latest-loss-streak*))))
    (when (and (fboundp 'swimmy.core:notify-discord-alert)
               (%should-notify-live-edge-block-p strategy-text reason))
      (swimmy.core:notify-discord-alert msg :color 15158332))
    nil))

(defun verify-signal-authority (symbol direction category lot rank lead-name)
  "Helper: Verify signal with Council, AI, and Blocking rules."
  (declare (ignore lot rank))
  (cond
    ((should-block-trade-p symbol direction category) nil)
    ((should-unlearn-p symbol) nil)
    ((%nil-like-strategy-name-p lead-name)
     (report-execution-context-missing symbol category :missing-strategy
                                       :strategy-name lead-name))
    ((let ((gate-status (and (stringp lead-name)
                             (fetch-deployment-gate-status lead-name))))
       (unless (%deployment-gate-live-ready-p gate-status)
         (report-deployment-gate-blocked symbol category lead-name gate-status)
         t))
     nil)
    ((multiple-value-bind (pass-p fail-reason metrics)
         (%live-edge-guard-pass-p lead-name)
       (unless pass-p
         (report-live-edge-guard-blocked symbol category lead-name fail-reason metrics)
         t))
     nil)
    ((not (verify-parallel-scenarios symbol direction category)) nil)
    (t t)))

(defun %runtime-selection-coerce-float (value &optional (default 0.0))
  "Best-effort float conversion used in runtime candidate scoring."
  (cond
    ((numberp value) (float value 1.0))
    ((stringp value)
     (handler-case
         (let ((*read-eval* nil))
           (multiple-value-bind (obj _pos) (read-from-string value nil nil)
             (declare (ignore _pos))
             (if (numberp obj) (float obj 1.0) (float default 1.0))))
       (error () (float default 1.0))))
    (t (float default 1.0))))

(defun %runtime-selection-clamp (value lo hi)
  "Clamp VALUE into [LO,HI]."
  (min hi (max lo value)))

(defun %runtime-selection-normalize-rank (rank)
  "Normalize RANK into keyword token for runtime scoring."
  (cond
    ((keywordp rank) rank)
    ((symbolp rank) (intern (string-upcase (symbol-name rank)) :keyword))
    ((stringp rank)
     (let* ((up (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return) rank)))
            (plain (if (and (> (length up) 0) (char= (char up 0) #\:))
                       (subseq up 1)
                       up)))
       (if (> (length plain) 0)
           (intern plain :keyword)
           nil)))
    (t nil)))

(defun %runtime-selection-rank-score (rank)
  "Return rank contribution score in [0,1]."
  (case (%runtime-selection-normalize-rank rank)
    (:legend 1.00)
    (:s 0.90)
    (:a 0.75)
    (:b 0.60)
    (:warrior 0.55)
    (:veteran 0.65)
    (:incubator 0.45)
    (otherwise 0.50)))

(defun %runtime-selection-timeframe-label (timeframe)
  "Normalize runtime timeframe value into printable label."
  (cond
    ((null timeframe) "N/A")
    ((stringp timeframe) timeframe)
    ((numberp timeframe)
     (let ((minutes (round timeframe)))
       (if (and (fboundp 'get-tf-string) (> minutes 0))
           (or (ignore-errors (get-tf-string minutes))
               (format nil "~a" minutes))
           (format nil "~a" minutes))))
    ((symbolp timeframe) (symbol-name timeframe))
    (t (format nil "~a" timeframe))))

(defun %runtime-selection-normalize-direction (value)
  "Normalize direction token into :BUY/:SELL, or NIL."
  (let ((token (%normalize-strategy-token value)))
    (cond
      ((member token '("BUY" "LONG" ":BUY" ":LONG") :test #'string=) :buy)
      ((member token '("SELL" "SHORT" ":SELL" ":SHORT") :test #'string=) :sell)
      (t nil))))

(defun %runtime-selection-evidence-score (raw-sharpe trades profit-factor win-rate max-dd)
  "Compute evidence-aware quality score from cached metrics."
  (let* ((sharpe (%runtime-selection-coerce-float raw-sharpe 0.0))
         (trades-n (max 0 (round (%runtime-selection-coerce-float trades 0.0))))
         (pf (%runtime-selection-coerce-float profit-factor 0.0))
         (wr (%runtime-selection-coerce-float win-rate 0.0))
         (dd (%runtime-selection-coerce-float max-dd 1.0))
         (metrics (list :sharpe sharpe
                        :trades trades-n
                        :profit-factor pf
                        :win-rate wr
                        :max-dd dd)))
    (cond
      ((fboundp 'score-from-metrics)
       (%runtime-selection-coerce-float (ignore-errors (score-from-metrics metrics)) sharpe))
      ((fboundp 'evidence-adjusted-sharpe)
       (%runtime-selection-coerce-float (ignore-errors (evidence-adjusted-sharpe sharpe trades-n)) sharpe))
      (t sharpe))))

(defun %runtime-selection-stability-score (metrics)
  "Compute recent stability score from live-edge metrics."
  (if (not (listp metrics))
      0.50
      (let* ((trades (max 0.0 (%runtime-selection-coerce-float (getf metrics :trades) 0.0)))
             (pf (%runtime-selection-coerce-float (getf metrics :profit-factor) 0.0))
             (wr (%runtime-selection-coerce-float (getf metrics :win-rate) 0.0))
             (net (%runtime-selection-coerce-float (getf metrics :net-pnl) 0.0))
             (loss-streak (max 0.0 (%runtime-selection-coerce-float (getf metrics :latest-loss-streak) 0.0)))
             (pf-score (%runtime-selection-clamp (/ (- pf 1.0) 1.0) 0.0 1.0))
             (wr-floor (%runtime-selection-coerce-float *live-edge-guard-wr-min* 0.35))
             (wr-score (%runtime-selection-clamp (/ (- wr wr-floor)
                                                    (max 0.05 (- 0.70 wr-floor)))
                                                 0.0 1.0))
             (net-score (cond
                          ((> net 0.0) 1.0)
                          ((= net 0.0) 0.5)
                          (t 0.0)))
             (streak-limit (max 1.0 (+ 1.0 (float *live-edge-guard-max-latest-loss-streak* 1.0))))
             (streak-score (%runtime-selection-clamp (- 1.0 (/ loss-streak streak-limit)) 0.0 1.0))
             (min-trades (max 1.0 (%runtime-selection-coerce-float *live-edge-guard-min-trades* 20.0)))
             (evidence-factor (%runtime-selection-clamp (/ trades min-trades) 0.0 1.0))
             (base (+ (* 0.35 pf-score)
                      (* 0.30 wr-score)
                      (* 0.20 streak-score)
                      (* 0.15 net-score))))
        (* base (+ 0.35 (* 0.65 evidence-factor))))))

(defun %runtime-selection-diversification-penalty (symbol category timeframe direction)
  "Compute diversification/correlation penalty from currently open slots."
  (let* ((norm-symbol (string-upcase (or symbol "")))
         (norm-dir (%runtime-selection-normalize-direction direction))
         (tf-min (resolve-execution-timeframe-minutes timeframe))
         (penalty 0.0))
    (when (hash-table-p *slot-allocation*)
      (maphash
       (lambda (_key slot)
         (declare (ignore _key))
         (when (listp slot)
           (let* ((slot-symbol (string-upcase (or (getf slot :symbol) "")))
                  (same-symbol (and (> (length norm-symbol) 0)
                                    (> (length slot-symbol) 0)
                                    (string= norm-symbol slot-symbol)))
                  (slot-category (getf slot :category))
                  (slot-direction (%runtime-selection-normalize-direction (getf slot :direction)))
                  (slot-strategy-name (getf slot :strategy))
                  (slot-strategy (and (stringp slot-strategy-name)
                                      (resolve-strategy-by-name slot-strategy-name)))
                  (slot-tf-min (resolve-execution-timeframe-minutes
                                (and slot-strategy (strategy-timeframe slot-strategy)))))
             (when same-symbol
               (incf penalty 0.15)
               (when (and category (eq slot-category category))
                 (incf penalty 0.25))
               (when (and norm-dir slot-direction (eq norm-dir slot-direction))
                 (incf penalty 0.05))
               (when (and tf-min slot-tf-min (= tf-min slot-tf-min))
                 (incf penalty 0.10)))
             (unless same-symbol
               (let ((corr (if (and (fboundp 'get-dynamic-correlation)
                                    (stringp symbol)
                                    (> (length symbol) 0)
                                    (> (length slot-symbol) 0))
                               (%runtime-selection-coerce-float
                                (abs (or (ignore-errors (get-dynamic-correlation symbol slot-symbol))
                                         0.0))
                                0.0)
                               0.0)))
                 (incf penalty (* 0.20 corr)))))))
       *slot-allocation*))
    (%runtime-selection-clamp penalty 0.0 *runtime-selection-max-diversification-penalty*)))

(defun %runtime-selection-reasons-text (reasons)
  "Encode REASONS list into a deterministic telemetry string."
  (if (and (listp reasons) reasons)
      (format nil "~{~a~^|~}" reasons)
      "none"))

(defun build-runtime-signal-candidate (symbol signal)
  "Build runtime selection candidate with evidence-aware score and reason map."
  (let* ((strategy-name (getf signal :strategy-name))
         (strategy (and (stringp strategy-name)
                        (resolve-strategy-by-name strategy-name)))
         (category (or (getf signal :category)
                       (and strategy (strategy-regime-class strategy))
                       :trend))
         (direction (or (getf signal :direction) :buy))
         (timeframe (or (getf signal :timeframe)
                        (and strategy (strategy-timeframe strategy))))
         (timeframe-label (%runtime-selection-timeframe-label timeframe))
         (cache (and (stringp strategy-name)
                     (get-cached-backtest strategy-name)))
         (raw-sharpe (%runtime-selection-coerce-float (and (listp cache) (getf cache :sharpe)) 0.0))
         (trades (max 0 (round (%runtime-selection-coerce-float (and (listp cache) (getf cache :trades)) 0.0))))
         (profit-factor (%runtime-selection-coerce-float (and (listp cache) (getf cache :profit-factor)) 0.0))
         (win-rate (%runtime-selection-coerce-float (and (listp cache) (getf cache :win-rate)) 0.0))
         (max-dd (%runtime-selection-coerce-float (and (listp cache) (getf cache :max-dd)) 1.0))
         (strategy-rank-token (%runtime-selection-normalize-rank
                               (or (and strategy (strategy-rank strategy))
                                   (let ((rank-data (and (stringp strategy-name)
                                                         (ignore-errors (get-strategy-rank strategy-name)))))
                                     (and rank-data (strategy-rank-rank rank-data)))
                                   :incubator)))
         (gate-status (and (stringp strategy-name)
                           (ignore-errors (fetch-deployment-gate-status strategy-name))))
         (gate-ready-p (%deployment-gate-live-ready-p gate-status))
         (gate-decision (or (%normalize-strategy-token (and (listp gate-status)
                                                            (getf gate-status :decision nil)))
                            "UNSET"))
         (gate-reason (let ((raw (and (listp gate-status) (getf gate-status :reason nil))))
                        (if (and raw (> (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                              (format nil "~a" raw)))
                                        0))
                            (format nil "~a" raw)
                            "deployment gate row missing")))
         (live-edge-pass-p t)
         (live-edge-fail-reason :ok)
         (live-edge-metrics nil)
         (evidence-score (%runtime-selection-evidence-score raw-sharpe trades profit-factor win-rate max-dd))
         (rank-score (%runtime-selection-rank-score strategy-rank-token))
         (diversification-penalty (%runtime-selection-diversification-penalty
                                   symbol category timeframe direction))
         (reasons '()))
    (when (and (stringp strategy-name) (> (length strategy-name) 0))
      (multiple-value-bind (pass-p fail-reason metrics)
          (%live-edge-guard-pass-p strategy-name)
        (setf live-edge-pass-p pass-p
              live-edge-fail-reason fail-reason
              live-edge-metrics metrics)))
    (let* ((stability-score (%runtime-selection-stability-score live-edge-metrics))
           (selection-score (- (+ evidence-score
                                  (* *runtime-selection-stability-weight* stability-score)
                                  (* *runtime-selection-rank-weight* rank-score))
                               (* *runtime-selection-diversification-penalty-weight*
                                  diversification-penalty)
                               (if gate-ready-p 0.0 *runtime-selection-gate-penalty*)
                               (if live-edge-pass-p 0.0 *runtime-selection-live-edge-penalty*)))
           (eligible-p (and gate-ready-p live-edge-pass-p)))
      (if gate-ready-p
          (push "DEPLOYMENT_GATE_LIVE_READY" reasons)
          (push (format nil "DEPLOYMENT_GATE_BLOCKED(~a)" gate-decision) reasons))
      (if live-edge-pass-p
          (push "LIVE_EDGE_PASS" reasons)
          (push (format nil "LIVE_EDGE_BLOCKED(~a)"
                        (%live-edge-fail-reason-label live-edge-fail-reason))
                reasons))
      (when (>= diversification-penalty *runtime-selection-high-diversification-threshold*)
        (push "DIVERSIFICATION_PENALTY_HIGH" reasons))
      (push (format nil "EVIDENCE=~,3f" evidence-score) reasons)
      (push (format nil "STABILITY=~,3f" stability-score) reasons)
      (list :signal signal
            :strategy-name strategy-name
            :category category
            :direction direction
            :timeframe timeframe
            :timeframe-label timeframe-label
            :rank strategy-rank-token
            :raw-sharpe raw-sharpe
            :trades trades
            :evidence-score evidence-score
            :stability-score stability-score
            :rank-score rank-score
            :diversification-penalty diversification-penalty
            :selection-score selection-score
            :gate-status gate-status
            :gate-ready-p gate-ready-p
            :gate-decision gate-decision
            :gate-reason gate-reason
            :live-edge-pass-p live-edge-pass-p
            :live-edge-fail-reason live-edge-fail-reason
            :live-edge-metrics live-edge-metrics
            :eligible-p eligible-p
            :reasons (nreverse reasons)))))

(defun %runtime-selection-candidate-better-p (a b)
  "Sort predicate for runtime candidates (higher score first)."
  (let ((score-a (%runtime-selection-coerce-float (getf a :selection-score) 0.0))
        (score-b (%runtime-selection-coerce-float (getf b :selection-score) 0.0)))
    (cond
      ((> score-a score-b) t)
      ((< score-a score-b) nil)
      (t
       (let ((evidence-a (%runtime-selection-coerce-float (getf a :evidence-score) 0.0))
             (evidence-b (%runtime-selection-coerce-float (getf b :evidence-score) 0.0)))
         (cond
           ((> evidence-a evidence-b) t)
           ((< evidence-a evidence-b) nil)
           (t
            (> (%runtime-selection-coerce-float (getf a :raw-sharpe) 0.0)
               (%runtime-selection-coerce-float (getf b :raw-sharpe) 0.0)))))))))

(defun rank-runtime-signal-candidates (symbol strat-signals)
  "Return STRAT-SIGNALS sorted by evidence-aware runtime score."
  (sort (mapcar (lambda (sig)
                  (build-runtime-signal-candidate symbol sig))
                (copy-list strat-signals))
        #'%runtime-selection-candidate-better-p))

(defun select-runtime-signal-candidate (symbol strat-signals)
  "Select runtime winner using evidence-aware scoring.
Returns (values selected-candidate ranked-candidates)."
  (let* ((ranked (if (and (listp strat-signals) strat-signals)
                     (rank-runtime-signal-candidates symbol strat-signals)
                     '()))
         (eligible (find-if (lambda (candidate) (getf candidate :eligible-p))
                            ranked))
         (selected (or eligible (first ranked))))
    (when selected
      (if (getf selected :eligible-p)
          (pushnew "SELECTED_HIGHEST_EVIDENCE_AWARE_SCORE"
                   (getf selected :reasons)
                   :test #'string=)
          (progn
            ;; Keep runtime flow alive for backward compatibility when no eligible candidate exists.
            (setf (getf selected :fallback-selected-p) t)
            (pushnew "FALLBACK_SELECTED_NO_ELIGIBLE_CANDIDATE"
                     (getf selected :reasons)
                     :test #'string=))))
    (values selected ranked)))

(defun emit-runtime-selection-telemetry (symbol ranked-candidates selected-strategy-name)
  "Emit runtime selection telemetry with selected/rejected reasons."
  (when (and (fboundp 'swimmy.core::emit-telemetry-event)
             (listp ranked-candidates))
    (let ((candidate-count (length ranked-candidates)))
      (dolist (candidate ranked-candidates)
        (let* ((strategy-name (or (getf candidate :strategy-name) "unknown"))
               (selected-p (and selected-strategy-name
                                (stringp selected-strategy-name)
                                (stringp strategy-name)
                                (string= selected-strategy-name strategy-name)))
               (status (if selected-p "selected" "rejected"))
               (reasons-text (%runtime-selection-reasons-text (getf candidate :reasons)))
               (reason (cond
                         (selected-p
                          (if (getf candidate :fallback-selected-p)
                              "selected:fallback-no-eligible-candidate"
                              "selected:highest-evidence-aware-score"))
                         ((not (getf candidate :eligible-p))
                          (format nil "rejected:ineligible|~a" reasons-text))
                         (t
                          (format nil "rejected:lower-score-than-selected(~a)"
                                  (or selected-strategy-name "none"))))))
          (swimmy.core::emit-telemetry-event "execution.runtime_selection_candidate"
            :service "school"
            :severity "info"
            :data (jsown:new-js
                    ("symbol" symbol)
                    ("strategy" strategy-name)
                    ("status" status)
                    ("reason" reason)
                    ("reasons" reasons-text)
                    ("candidate_count" candidate-count)
                    ("category" (format nil "~a" (or (getf candidate :category) :unknown)))
                    ("timeframe" (or (getf candidate :timeframe-label) "N/A"))
                    ("direction" (format nil "~a" (or (getf candidate :direction) :unknown)))
                    ("rank" (format nil "~a" (or (getf candidate :rank) :unknown)))
                    ("raw_sharpe" (%runtime-selection-coerce-float (getf candidate :raw-sharpe) 0.0))
                    ("selection_score" (%runtime-selection-coerce-float (getf candidate :selection-score) 0.0))
                    ("evidence_score" (%runtime-selection-coerce-float (getf candidate :evidence-score) 0.0))
                    ("stability_score" (%runtime-selection-coerce-float (getf candidate :stability-score) 0.0))
                    ("rank_score" (%runtime-selection-coerce-float (getf candidate :rank-score) 0.0))
                    ("diversification_penalty"
                     (%runtime-selection-coerce-float (getf candidate :diversification-penalty) 0.0))
                    ("deployment_ready" (if (getf candidate :gate-ready-p) t nil))
                    ("deployment_decision" (or (getf candidate :gate-decision) "UNSET"))
                    ("deployment_reason" (or (getf candidate :gate-reason) ""))
                    ("live_edge_pass" (if (getf candidate :live-edge-pass-p) t nil))
                    ("live_edge_reason" (format nil "~a" (or (getf candidate :live-edge-fail-reason) :ok)))))))
      (swimmy.core::emit-telemetry-event "execution.runtime_selection_summary"
        :service "school"
        :severity "info"
        :data (jsown:new-js
                ("symbol" symbol)
                ("candidate_count" candidate-count)
                ("selected_strategy" (or selected-strategy-name "none")))))))

(defun %normalize-shadow-direction (value)
  "Normalize direction token into :BUY/:SELL, or NIL."
  (let ((token (%normalize-strategy-token value)))
    (cond
      ((member token '("BUY" "LONG" ":BUY" ":LONG") :test #'string=) :buy)
      ((member token '("SELL" "SHORT" ":SELL" ":SHORT") :test #'string=) :sell)
      (t nil))))

(defun %normalize-shadow-rank (rank)
  "Normalize strategy rank token into keyword."
  (cond
    ((keywordp rank) rank)
    ((symbolp rank) (intern (string-upcase (symbol-name rank)) :keyword))
    ((stringp rank)
     (let* ((up (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return) rank)))
            (plain (if (and (> (length up) 0) (char= (char up 0) #\:))
                       (subseq up 1)
                       up)))
       (and (> (length plain) 0) (intern plain :keyword))))
    (t nil)))

(defun %shadow-rank-eligible-p (rank)
  "Return T when RANK is eligible for shadow paper execution."
  (member rank *shadow-paper-eligible-ranks* :test #'eq))

(defun %shadow-forward-running-p (strategy-name)
  "Return T when STRATEGY-NAME is currently in FORWARD_RUNNING deployment gate."
  (let* ((gate-status (and (stringp strategy-name)
                           (ignore-errors (fetch-deployment-gate-status strategy-name))))
         (decision (%normalize-strategy-token
                    (and (listp gate-status) (getf gate-status :decision nil)))))
    (and decision (string= decision "FORWARD_RUNNING"))))

(defun %shadow-strategy-eligible-p (strategy-name rank)
  "Return T when strategy is rank-eligible or actively collecting forward evidence."
  (or (%shadow-rank-eligible-p rank)
      (%shadow-forward-running-p strategy-name)))

(defun %shadow-slot-key (strategy-name symbol direction)
  (format nil "~a|~a|~a"
          (or strategy-name "UNKNOWN")
          (string-upcase (or symbol "USDJPY"))
          (if (eq direction :buy) "BUY" "SELL")))

(defun close-a-rank-shadow-positions (symbol bid ask)
  "Close open A-rank shadow positions on SL/TP and record learning rows."
  (let ((keys-to-close '())
        (now (get-universal-time)))
    (maphash
     (lambda (key slot)
       (when (and (listp slot)
                  (string= (string-upcase (or (getf slot :symbol) ""))
                           (string-upcase (or symbol ""))))
         (let* ((direction (or (%normalize-shadow-direction (getf slot :direction)) :buy))
                (entry (getf slot :entry))
                (entry-bid (getf slot :entry-bid))
                (entry-ask (getf slot :entry-ask))
                (sl (or (%normalize-sltp-pips-value (getf slot :effective-sl))
                        (%normalize-sltp-pips-value (getf slot :effective_sl))
                        (%normalize-sltp-pips-value (getf slot :sl))))
                (tp (or (%normalize-sltp-pips-value (getf slot :effective-tp))
                        (%normalize-sltp-pips-value (getf slot :effective_tp))
                        (%normalize-sltp-pips-value (getf slot :tp))))
                (category (or (getf slot :category) :trend))
                (strategy-name (getf slot :strategy))
                (pair-id (getf slot :pair-id))
                (opened-at (or (getf slot :opened-at) now))
                (shadow-age (max 0 (- now opened-at)))
                (max-hold-sec (if (and (numberp *shadow-max-hold-seconds*)
                                       (> *shadow-max-hold-seconds* 0))
                                  (round *shadow-max-hold-seconds*)
                                  0))
                (close-p nil)
                (hit :unknown)
                (pnl 0.0))
           (when (and (numberp entry) (numberp sl) (numberp tp) (numberp bid) (numberp ask))
             (cond
               ((eq direction :buy)
                (cond
                  ((<= bid sl) (setf close-p t hit :sl pnl (- bid entry)))
                  ((>= bid tp) (setf close-p t hit :tp pnl (- bid entry)))))
               ((eq direction :sell)
                (cond
                  ((>= ask sl) (setf close-p t hit :sl pnl (- entry ask)))
                  ((<= ask tp) (setf close-p t hit :tp pnl (- entry ask))))))
             (when (and (not close-p)
                        (> max-hold-sec 0)
                        (>= shadow-age max-hold-sec))
               ;; Prevent paper evidence starvation when SL/TP is too wide to close naturally.
               (setf close-p t
                     hit :timeout
                     pnl (if (eq direction :buy)
                             (- bid entry)
                             (- entry ask))))
             (when close-p
               (let ((dryrun-slip nil))
                 (when (and strategy-name
                            (numberp entry)
                            (numberp entry-bid)
                            (numberp entry-ask)
                            (fboundp 'slippage-pips-from-fill))
                   (setf dryrun-slip
                         (slippage-pips-from-fill symbol direction entry-bid entry-ask entry)))
                 (push (list key strategy-name direction category pnl hit pair-id
                             shadow-age dryrun-slip)
                       keys-to-close)))))))
     *shadow-slot-allocation*)
    (dolist (item keys-to-close)
      (destructuring-bind (key strategy-name direction category pnl hit pair-id hold-time dryrun-slip) item
        (remhash key *shadow-slot-allocation*)
        (handler-case
            (record-trade-outcome symbol direction category strategy-name pnl
                                  :hit hit
                                  :hold-time hold-time
                                  :pair-id pair-id
                                  :execution-mode :shadow)
          (error (e)
            (format t "[SHADOW] ⚠️ Failed to record shadow outcome for ~a: ~a~%" strategy-name e)))
        (when (and strategy-name
                   (numberp dryrun-slip)
                   (fboundp 'record-dryrun-slippage))
          (ignore-errors (record-dryrun-slippage strategy-name dryrun-slip))))))
  t)

(defun open-a-rank-shadow-trades (symbol bid ask strat-signals)
  "Open shadow positions for rank-eligible or FORWARD_RUNNING strategies."
  (when (and *a-rank-shadow-trading-enabled*
             (listp strat-signals)
             (numberp bid)
             (numberp ask))
    (dolist (sig strat-signals)
      (let* ((strategy-name (getf sig :strategy-name))
             (direction (%normalize-shadow-direction (getf sig :direction)))
             (strat (and strategy-name (resolve-strategy-by-name strategy-name)))
             (rank (and strat (%normalize-shadow-rank (strategy-rank strat)))))
	        (when (and strategy-name
	                   strat
	                   direction
	                   (%shadow-strategy-eligible-p strategy-name rank))
	          (let* ((slot-key (%shadow-slot-key strategy-name symbol direction))
	                 (entry (if (eq direction :buy) ask bid)))
	            (unless (gethash slot-key *shadow-slot-allocation*)
	              (multiple-value-bind (sl-pips tp-pips source-of-override)
	                  (resolve-live-effective-sltp strategy-name :symbol symbol)
	                (let* ((effective-sl (if (eq direction :buy)
	                                         (- entry sl-pips)
	                                         (+ entry sl-pips)))
	                       (effective-tp (if (eq direction :buy)
	                                         (+ entry tp-pips)
	                                         (- entry tp-pips))))
	                  (setf (gethash slot-key *shadow-slot-allocation*)
	                        (list :strategy strategy-name
	                              :symbol symbol
	                              :direction direction
	                              :category (or (getf sig :category) :trend)
	                              :entry-bid bid
	                              :entry-ask ask
	                              :entry entry
	                              :sl effective-sl
	                              :tp effective-tp
	                              :effective-sl effective-sl
	                              :effective-tp effective-tp
	                              :effective_sl effective-sl
	                              :effective_tp effective-tp
	                              :source_of_override source-of-override
	                              :pair-id (getf sig :pair-id)
	                              :opened-at (get-universal-time)))))))))))
  t)

(defun %forward-no-signal-probe-interval-ok-p (strategy-name now)
  "Return T when STRATEGY-NAME passed no-signal probe throttle interval."
  (let* ((interval (if (and (numberp *forward-no-signal-shadow-interval-seconds*)
                            (> *forward-no-signal-shadow-interval-seconds* 0))
                       (round *forward-no-signal-shadow-interval-seconds*)
                       0))
         (last-at (and (hash-table-p *forward-no-signal-shadow-last-at*)
                       (stringp strategy-name)
                       (gethash strategy-name *forward-no-signal-shadow-last-at* nil))))
    (or (<= interval 0)
        (not (numberp last-at))
        (>= (- now last-at) interval))))

(defun %forward-running-probe-strategy-p (strategy symbol)
  "Return T when STRATEGY should emit periodic probe for SYMBOL."
  (let* ((strategy-name (and strategy (strategy-name strategy)))
         (strategy-symbol (and strategy (strategy-symbol strategy)))
         (symbol-match (and (stringp strategy-symbol)
                            (stringp symbol)
                            (string= (string-upcase strategy-symbol)
                                     (string-upcase symbol)))))
    (and (stringp strategy-name)
         (> (length strategy-name) 0)
         symbol-match
         (%shadow-forward-running-p strategy-name)
         ;; Do not emit probe while real shadow slot is open.
         (not (gethash (%shadow-slot-key strategy-name symbol :buy) *shadow-slot-allocation*))
         (not (gethash (%shadow-slot-key strategy-name symbol :sell) *shadow-slot-allocation*)))))

(defun %forward-running-db-probe-strategy-names (symbol)
  "Return FORWARD_RUNNING strategy names for SYMBOL from deployment gate DB rows."
  (if (or (null symbol) (not (stringp symbol)) (<= (length symbol) 0))
      '()
      (let* ((rows (ignore-errors
                     (execute-to-list
                      "SELECT d.strategy_name
                         FROM deployment_gate_status d
                         LEFT JOIN strategies s
                           ON s.name = d.strategy_name
                        WHERE UPPER(COALESCE(d.decision, '')) = 'FORWARD_RUNNING'
                          AND (UPPER(COALESCE(s.symbol, '')) = UPPER(?)
                               OR INSTR(UPPER(COALESCE(d.strategy_name, '')), UPPER(?)) > 0)"
                      symbol
                      symbol)))
             (names (remove nil
                            (mapcar (lambda (row)
                                      (let ((name (first row)))
                                        (and (stringp name) (> (length name) 0) name)))
                                    rows))))
        (remove-duplicates names :test #'string=))))

(defun record-forward-running-periodic-shadow-probes (symbol bid ask)
  "Emit neutral SHADOW probe outcomes for FORWARD_RUNNING strategies.
This keeps FORWARD_RUNNING liveness telemetry active even when live execution is blocked."
  (let ((recorded 0))
    (when (and *forward-no-signal-shadow-enabled*
               (stringp symbol)
               (> (length symbol) 0)
               (numberp bid)
               (numberp ask)
               (listp *strategy-knowledge-base*))
      (let* ((now (get-universal-time))
             (direction :buy)
             (kb-candidates
               (remove nil
                       (mapcar (lambda (strategy)
                                 (when (%forward-running-probe-strategy-p strategy symbol)
                                   (strategy-name strategy)))
                               *strategy-knowledge-base*)))
             (db-candidates (%forward-running-db-probe-strategy-names symbol))
             (candidates (remove-duplicates (append kb-candidates db-candidates)
                                            :test #'string=)))
        (dolist (strategy-name candidates recorded)
          (when (and (stringp strategy-name)
                     (> (length strategy-name) 0)
                     (%shadow-forward-running-p strategy-name)
                     (not (gethash (%shadow-slot-key strategy-name symbol :buy) *shadow-slot-allocation*))
                     (not (gethash (%shadow-slot-key strategy-name symbol :sell) *shadow-slot-allocation*))
                     (%forward-no-signal-probe-interval-ok-p strategy-name now))
            (handler-case
                (progn
                  (record-trade-outcome symbol direction :trend strategy-name 0.0
                                        :hit :probe
                                        :hold-time 0
                                        :pair-id (format nil "NO-SIGNAL-PROBE-~d" now)
                                        :execution-mode :shadow)
                  (setf (gethash strategy-name *forward-no-signal-shadow-last-at*)
                        now)
                  (incf recorded))
              (error (e)
                (format t "[SHADOW] ⚠️ Failed to record no-signal probe for ~a: ~a~%"
                        strategy-name e))))))
      (when (> recorded 0)
        (format t "[SHADOW] 🧪 Recorded periodic probes for ~a: ~d~%" symbol recorded)))
    recorded))

(defun record-forward-running-no-signal-shadow-probes (symbol bid ask)
  "Backward-compatible alias for periodic FORWARD_RUNNING probe writer."
  (record-forward-running-periodic-shadow-probes symbol bid ask))

(defun execute-order-sequence (category direction symbol bid ask lot lead-name timeframe-key magic-override &key pair-id)
  "Helper: atomic reservation and execution."
  (declare (ignore magic-override))
  (when (%nil-like-strategy-name-p lead-name)
    (format t "[EXEC] 🚫 Missing strategy context before reservation for ~a (~a); skip trade.~%" symbol category)
    (report-execution-context-missing symbol category :missing-strategy
                                      :strategy-name lead-name
                                      :timeframe-key timeframe-key)
    (return-from execute-order-sequence nil))
  (when (or (null timeframe-key)
            (string= (string-upcase (format nil "~a" timeframe-key)) "NIL"))
    (format t "[EXEC] 🚫 Missing timeframe context before reservation for ~a (~a); skip trade.~%" symbol category)
    (report-execution-context-missing symbol category :missing-timeframe
                                      :strategy-name lead-name
                                      :timeframe-key timeframe-key)
    (return-from execute-order-sequence nil))
  (let* ((entry-bid bid)
         (entry-ask ask)
         (entry-spread-pips (spread-pips-from-bid-ask symbol bid ask))
         (entry-cost-pips entry-spread-pips))
    ;; Reservation
    (multiple-value-bind (slot-index magic)
        (try-reserve-slot category lead-name symbol direction
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
<<<<<<< HEAD
              (close-opposing-category-positions category direction symbol
                                                 (if (eq direction :buy) bid ask)
                                                 "Doten")
              (multiple-value-bind (sl-pips tp-pips source-of-override)
                  (resolve-live-effective-sltp lead-name)
                (let ((effective-sl nil)
                      (effective-tp nil))
=======
	              (close-opposing-category-positions category direction symbol
	                                                 (if (eq direction :buy) bid ask)
	                                                 "Doten")
	              (multiple-value-bind (sl-pips tp-pips source-of-override)
	                  (resolve-live-effective-sltp lead-name :symbol symbol)
	                (let ((effective-sl nil)
	                      (effective-tp nil))
>>>>>>> da28fca8275070012eeaccbc387481dbbff3ad83
                  (cond
                    ((eq direction :buy)
                     (setf effective-sl (- bid sl-pips)
                           effective-tp (+ bid tp-pips))
                     (when (safe-order "BUY" symbol lot effective-sl effective-tp magic
                                       (format nil "~a|~a" lead-name timeframe-key))
                       (setf committed t)))
                    ((eq direction :sell)
                     (setf effective-sl (+ ask sl-pips)
                           effective-tp (- ask tp-pips))
                     (when (safe-order "SELL" symbol lot effective-sl effective-tp magic
                                       (format nil "~a|~a" lead-name timeframe-key))
                       (setf committed t))))
                  (when committed
                    (let ((pending (gethash magic *allocation-pending-orders*)))
                      (setf (getf pending :effective-sl) effective-sl
                            (getf pending :effective-tp) effective-tp
                            (getf pending :effective_sl) effective-sl
                            (getf pending :effective_tp) effective-tp
                            (getf pending :source_of_override) source-of-override)
                      (setf (gethash magic *allocation-pending-orders*) pending)))
                  (when committed
                    (update-symbol-exposure symbol lot :open)
                    (incf *category-trades*)
                    (setf *last-entry-time* (get-universal-time))
                    (format t "[EXEC] ✅ Committed: ~a ~a~%" category symbol)
                    (when (fboundp 'swimmy.core::emit-telemetry-event)
                      (let* ((dir (if (eq direction :buy) "BUY" "SELL"))
                             (cat (if (keywordp category)
                                      (string-downcase (symbol-name category))
                                      (format nil "~a" category)))
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
                                  ("effective_sl" effective-sl)
                                  ("effective_tp" effective-tp)
                                  ("source_of_override" source-of-override)
                                  ("pip_size" pip-size))))
                    (request-mt5-positions)
                    t))))
          ;; Cleanup
          (unless committed
            (remhash magic *allocation-pending-orders*)
            (remhash (format nil "~a-~d" category slot-index) *slot-allocation*)
            (format t "[ALLOC] ♻️ Released Slot ~d~%" slot-index))))
        committed))))

(defun execute-category-trade (category direction symbol bid ask
                               &key (lot-multiplier 1.0) signal-confidence strategy-name strategy-timeframe)
  (format t "[TRACE] execute-category-trade ~a ~a~%" category direction)
  (handler-case
      (when (and (numberp bid) (numberp ask) (total-exposure-allowed-p))
        (let* ((strategy-name-text (and strategy-name
                                        (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                     (format nil "~a" strategy-name))))
               (strategy-name-provided (and strategy-name-text
                                            (> (length strategy-name-text) 0)
                                            (not (%nil-like-strategy-name-p strategy-name-text))))
               (resolved-signal-strat (and strategy-name-provided
                                           (resolve-strategy-by-name strategy-name-text))))
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
        (multiple-value-bind (lead-name timeframe-key history)
            (prepare-trade-context category symbol
                                   :strategy-name strategy-name
                                   :strategy-timeframe strategy-timeframe)
          (when (%nil-like-strategy-name-p lead-name)
            (format t "[EXEC] 🚫 Missing strategy context for ~a (~a); skip trade.~%" symbol category)
            (report-execution-context-missing symbol category :missing-strategy
                                              :strategy-name lead-name
                                              :timeframe-key timeframe-key)
            (return-from execute-category-trade nil))
          (when (or (null timeframe-key)
                    (string= (string-upcase (format nil "~a" timeframe-key)) "NIL"))
            (format t "[EXEC] 🚫 Missing timeframe context for ~a (~a); skip trade.~%" symbol category)
            (report-execution-context-missing symbol category :missing-timeframe
                                              :strategy-name lead-name
                                              :timeframe-key timeframe-key)
            (return-from execute-category-trade nil))
          ;; Fail-closed on unresolved explicit strategy identity once timeframe is valid.
          ;; This prevents trading under stale/unknown strategy names with synthetic TF inputs.
          (when (and strategy-name-provided
                     (null resolved-signal-strat))
            (format t "[EXEC] 🚫 Unresolved strategy context for ~a (~a): ~a~%"
                    symbol category strategy-name-text)
            (report-execution-context-missing symbol category :missing-strategy
                                              :strategy-name strategy-name
                                              :timeframe-key timeframe-key)
            (return-from execute-category-trade nil))
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
                                            :pair-id pair-id))))))))
    (error (e) (format t "[EXEC] 🚨 Error: ~a~%" e))))

(defun close-category-positions (symbol bid ask)
  "V5.2: Close slot positions at SL/TP using slot-allocation"
  (maphash 
   (lambda (key slot)
     (when (and slot (equal (getf slot :symbol) symbol))
       (let* ((category (getf slot :category))
              (pos (getf slot :direction))
              (entry (getf slot :entry))
<<<<<<< HEAD
               (magic (getf slot :magic))
               (lot (or (getf slot :lot) 0.01))
               (sl-pips *default-sl-pips*) (tp-pips *default-tp-pips*)
               (effective-sl (or (and (numberp (getf slot :effective-sl))
                                      (float (getf slot :effective-sl) 1.0))
                                 (and (numberp (getf slot :effective_sl))
                                      (float (getf slot :effective_sl) 1.0))))
               (effective-tp (or (and (numberp (getf slot :effective-tp))
                                      (float (getf slot :effective-tp) 1.0))
                                 (and (numberp (getf slot :effective_tp))
                                      (float (getf slot :effective_tp) 1.0))))
               (pnl 0) (closed nil))
=======
              (magic (getf slot :magic))
              (lot (or (getf slot :lot) 0.01))
              (resolved-sltp (multiple-value-list
                              (resolve-live-effective-sltp (getf slot :strategy)
                                                           :symbol (or (getf slot :symbol) symbol))))
              (sl-pips (or (first resolved-sltp)
                           (float *default-sl-pips* 1.0)))
              (tp-pips (or (second resolved-sltp)
                           (float *default-tp-pips* 1.0)))
              (effective-sl (or (and (numberp (getf slot :effective-sl))
                                     (float (getf slot :effective-sl) 1.0))
                                (and (numberp (getf slot :effective_sl))
                                     (float (getf slot :effective_sl) 1.0))))
              (effective-tp (or (and (numberp (getf slot :effective-tp))
                                     (float (getf slot :effective-tp) 1.0))
                                (and (numberp (getf slot :effective_tp))
                                     (float (getf slot :effective_tp) 1.0))))
              (pnl 0)
              (closed nil))
>>>>>>> da28fca8275070012eeaccbc387481dbbff3ad83
         (when (and entry (numberp bid) (numberp ask))
           (cond
             ((or (eq pos :long) (eq pos :buy))
              (let ((sl (or effective-sl (- entry sl-pips)))
                    (tp (or effective-tp (+ entry tp-pips))))
                (when (or (<= bid sl) (>= bid tp))
                  (setf pnl (- bid entry) closed t))))
             ((or (eq pos :short) (eq pos :sell))
<<<<<<< HEAD
              (let ((sl (or effective-sl (+ ask sl-pips)))
                    (tp (or effective-tp (- ask tp-pips))))
=======
              (let ((sl (or effective-sl (+ entry sl-pips)))
                    (tp (or effective-tp (- entry tp-pips))))
>>>>>>> da28fca8275070012eeaccbc387481dbbff3ad83
                (when (or (>= ask sl) (<= ask tp))
                  (setf pnl (- entry ask) closed t)))))
           (when closed
             (let ((msg (swimmy.core:encode-sexp `((type . "CLOSE")
                                                   (symbol . ,symbol)
                                                   (magic . ,magic)))))
               (pzmq:send *cmd-publisher* msg))
             (remhash key *slot-allocation*)
             (update-symbol-exposure symbol lot :close)
             (incf *daily-pnl* (round (* pnl 1000 100)))
             (record-trade-result (if (> pnl 0) :win :loss))
             ;; V17: Record prediction outcome for feedback loop (Issue 2)
             (record-prediction-outcome symbol (if (eq pos :long) :buy :sell) (if (> pnl 0) :win :loss))
             (record-trade-outcome symbol (if (eq pos :long) :buy :sell) category "Slots" pnl)
             (when (fboundp 'record-strategy-trade)
                 (let ((lead-strat (first (gethash category *active-team*))))
                     (when lead-strat 
                       (record-strategy-trade (strategy-name lead-strat) (if (> pnl 0) :win :loss) pnl)
                       ;; V47.5: Live Trade Audit + RL Reward hooks
                       (when (fboundp 'run-live-trade-audit)
                         (run-live-trade-audit lead-strat pnl))
                       (when (fboundp 'record-rl-reward)
                         (record-rl-reward lead-strat pnl)))))
              (swimmy.shell:notify-discord-symbol symbol (format nil "~a ~a closed ~,2f" (if (> pnl 0) "✅" "❌") category pnl) 
                             :color (if (> pnl 0) 3066993 15158332)))))))
   *slot-allocation*))


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
    ;; NOTE: Set dispatch-step before each gate so Msg Error logs point to the true failing step
    ;; even when earlier helpers leave a stale step value behind.
    (when (boundp 'swimmy.main::*dispatch-step*)
      (setf swimmy.main::*dispatch-step* :tick/trading-allowed-p))
    (let ((allowed (trading-allowed-p)))
      (when (and allowed history (> (length history) 100))
        (when (boundp 'swimmy.main::*dispatch-step*)
          (setf swimmy.main::*dispatch-step* :tick/close-category-positions))
        (close-category-positions symbol bid ask)
        ;; Shadow positions are closed regardless of live execution gate results.
        (close-a-rank-shadow-positions symbol bid ask)
        ;; Keep FORWARD_RUNNING evidence progressing even when strategy signals exist
        ;; or live guards block the current tick.
        (record-forward-running-periodic-shadow-probes symbol bid ask)
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
        (elect-leader)
        (handler-case
            (progn
              (format t "[L] 🎯 61-STRATEGY SIGNAL SCAN~%")
              (let ((strat-signals (collect-strategy-signals symbol history)))
                (if strat-signals
                    (progn
                      (format t "[L] 📊 ~d strategies triggered signals~%" (length strat-signals))
                      ;; A-rank shadow operation: keep collecting paper outcomes.
                      (open-a-rank-shadow-trades symbol bid ask strat-signals)
                      ;; Runtime routing policy:
                      ;; Keep single-winner order flow, but rank candidates by evidence-aware score
                      ;; instead of raw cached Sharpe-only global best.
                      (multiple-value-bind (selected-candidate ranked-candidates)
                          (select-runtime-signal-candidate symbol strat-signals)
                        (let* ((top-sig (and selected-candidate (getf selected-candidate :signal)))
                               (top-name (and selected-candidate (getf selected-candidate :strategy-name)))
                               (top-cat (and selected-candidate (getf selected-candidate :category)))
                               (top-timeframe (and selected-candidate (getf selected-candidate :timeframe)))
                               (top-score (if selected-candidate
                                              (%runtime-selection-coerce-float
                                               (getf selected-candidate :selection-score)
                                               0.0)
                                              0.0))
                               (top-sharpe (if selected-candidate
                                               (%runtime-selection-coerce-float
                                                (getf selected-candidate :raw-sharpe)
                                                0.0)
                                               0.0)))
                          (emit-runtime-selection-telemetry symbol ranked-candidates top-name)
                          (when top-sig
                            (format t "[L] 🏆 RUNTIME SELECTED: ~a (~a) score=~,3f raw_sharpe=~,2f from ~d strategies~%"
                                    top-name top-cat top-score top-sharpe (length strat-signals))
                            (when (getf selected-candidate :fallback-selected-p)
                              (format t "[L] ⚠️ Runtime selection fallback: no LIVE_READY/live-edge-passing candidate; selected highest score for legacy flow compatibility.~%"))
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
                                                                :signal-confidence signal-confidence
                                                                :strategy-name top-name
                                                                :strategy-timeframe top-timeframe)))
                                  (when trade-executed
                                    (format t "[L] 📣 TRADE EXECUTED: ~a ~a strat=~a conf=~,2f lot-mult=~,2f~%"
                                            symbol direction top-name signal-confidence confidence-lot-mult)
                                    (record-category-trade-time strat-key)
                                    (when (fboundp 'record-strategy-trade)
                                      (record-strategy-trade top-name :trade 0))))))))))
                    nil)))
          (error (e)
            (declare (ignore e))
            nil))))))

;;; ==========================================
;;; SYSTEM LOADING
;;; ==========================================



(defun force-recruit-strategy (name)
  (let ((strat (find name *strategy-knowledge-base* :key #'strategy-name :test #'string=)))
    (if strat
        (progn
          (pushnew strat *evolved-strategies* :test #'string= :key #'strategy-name)
          (if (or (not (fboundp 'strategy-active-pool-eligible-p))
                  (strategy-active-pool-eligible-p strat))
              (if (fboundp 'add-strategy-to-active-pools)
                  (add-strategy-to-active-pools strat)
                  (let ((cat (categorize-strategy strat)))
                    (setf (gethash cat *category-pools*)
                          (cons strat (remove (strategy-name strat) (gethash cat *category-pools*) :key #'strategy-name :test #'string=)))
                    (when (boundp '*regime-pools*)
                      (let ((regime-class (if (fboundp 'strategy-regime-class)
                                              (strategy-regime-class strat)
                                              (strategy-category strat))))
                        (setf (gethash regime-class *regime-pools*)
                              (cons strat
                                    (remove (strategy-name strat)
                                            (gethash regime-class *regime-pools*)
                                            :key #'strategy-name
                                            :test #'string=)))))))
              (format t "[L] ⏳ Special Force deferred until evaluated: ~a (Rank: ~s)~%"
                      name (strategy-rank strat)))
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

(defun normalize-founder-key-token (value)
  "Normalize VALUE into a founder keyword token."
  (cond
    ((keywordp value) value)
    ((symbolp value) (intern (string-upcase (symbol-name value)) :keyword))
    ((stringp value)
     (let* ((trimmed (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
            (plain (if (and (> (length trimmed) 0)
                            (char= (char trimmed 0) #\:))
                       (subseq trimmed 1)
                       trimmed)))
       (and (> (length plain) 0)
            (intern plain :keyword))))
    (t nil)))

(defun normalize-founder-key-list (raw)
  "Normalize RAW founder key list into unique keyword tokens."
  (let ((items (cond
                 ((null raw) nil)
                 ((listp raw) raw)
                 (t (list raw))))
        (out nil))
    (dolist (item items (nreverse out))
      (let ((token (normalize-founder-key-token item)))
        (when token
          (pushnew token out :test #'eq))))))

(defun string-suffix-ci-p (suffix value)
  "Case-insensitive suffix predicate."
  (let* ((suffix (or suffix ""))
         (value (or value ""))
         (ls (length suffix))
         (lv (length value)))
    (and (<= ls lv)
         (string-equal suffix value
                       :start1 0 :end1 ls
                       :start2 (- lv ls) :end2 lv))))

(defun founder-key-pinned-symbol (key)
  "Return pinned symbol when founder KEY is symbol-scoped, else NIL."
  (let ((token (and (symbolp key) (string-upcase (symbol-name key)))))
    (cond
      ;; Backward-compatible mapping for existing VWAPVR founder key.
      ((eq key :hunted-h12-vwapvr-50-150) "USDJPY")
      ((and token (string-suffix-ci-p "-USDJPY" token)) "USDJPY")
      ((and token (string-suffix-ci-p "-EURUSD" token)) "EURUSD")
      ((and token (string-suffix-ci-p "-GBPUSD" token)) "GBPUSD")
      (t nil))))

(defun founder-target-symbols (key symbols)
  "Resolve target symbols for founder KEY from supported SYMBOLS."
  (let ((pinned (founder-key-pinned-symbol key)))
    (if (and pinned
             (find pinned symbols :test #'string-equal))
        (list pinned)
        (if pinned nil symbols))))

(defun recruit-special-forces (&key max-attempts founder-keys)
  (force-recruit-strategy "T-Nakane-Gotobi")
  (let ((symbols (if (and (boundp 'swimmy.core::*supported-symbols*)
                          swimmy.core::*supported-symbols*)
                     swimmy.core::*supported-symbols*
                     '("USDJPY")))
        (attempt-limit (and (integerp max-attempts)
                            (>= max-attempts 0)
                            max-attempts))
        (selected-keys (normalize-founder-key-list founder-keys))
        (known-names (make-hash-table :test 'equal))
        (recruited 0)
        (attempted 0)
        (auto-skipped 0)
        (limit-reached nil))
    (dolist (s *strategy-knowledge-base*)
      (let ((name (and s (strategy-name s)))
            (competition-eligible-p
              (if (fboundp 'strategy-competition-eligible-p)
                  (strategy-competition-eligible-p s)
                  t)))
        (when (and competition-eligible-p
                   name
                   (stringp name))
          (setf (gethash name known-names) t))))
    (block special-force-loop
      (maphash
       (lambda (key maker-func)
         (when (and (functionp maker-func)
                    (or (null selected-keys)
                        (member key selected-keys :test #'eq)))
           (if (and *special-force-skip-hunter-auto-founders*
                    (hunter-auto-founder-key-p key))
               (incf auto-skipped)
               (let ((proto (handler-case (funcall maker-func)
                              (error () nil))))
                 (when proto
                   (let ((base-name (strategy-name proto)))
                     (when (and base-name (stringp base-name))
                       (let ((target-symbols (founder-target-symbols key symbols)))
                         (when target-symbols
                       ;; Multi-symbol evolution: recruit the same founder archetype per
                       ;; supported symbol, with distinct names (P12.5 contract).
                       (dolist (sym target-symbols)
                         (let ((name (rewrite-strategy-name-for-symbol base-name sym)))
                           (unless (and name (gethash name known-names))
                             (when (and attempt-limit
                                        (>= attempted attempt-limit))
                               (setf limit-reached t)
                               (return-from special-force-loop nil))
                             (incf attempted)
                             (when (recruit-founder key :symbol sym)
                               (incf recruited))
                             (when name
                               (setf (gethash name known-names) t))))))))))))))
       *founder-registry*))
    (format t "[L] 🎖️ Special Force recruit run complete: ~d new founder attempts (auto-skipped ~d, tried ~d~@[ /limit ~d~]~:[~; LIMIT REACHED~])~%"
            recruited auto-skipped attempted attempt-limit limit-reached)))

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
