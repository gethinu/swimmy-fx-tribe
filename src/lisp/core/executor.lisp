(in-package :swimmy.executor)

;;; ============================================================================
;;; SWIMMY EXECUTOR (executor.lisp)
;;; ============================================================================
;;; Responsibility: Use "Core" layer logic to execute trades and maintain system health.
;;; - Heartbeat (Keep Connection Alive)
;;; - Status Monitoring (Watchdog Food)
;;; - Trade Processing (PnL, State Updates)
;;; - Account Info Sync (Data Validity)
;;;
;;; "The Executor never sleeps, never dreams, and never dies."
;;; ============================================================================

(defparameter *total-ticks-count* 0)
(defvar *processed-tickets* (make-hash-table :test 'equal))
(defvar *current-drawdown* 0.0)
(defparameter *account-info-save-interval* 60)
(defparameter *last-account-save-time* 0)
(defparameter *monitoring-peak-rebase-on-restart* t
  "When T, stale monitoring peak can be rebased on first ACCOUNT_INFO after restart.")
(defparameter *monitoring-peak-stale-ratio-threshold* 0.50
  "If both persisted and incoming equity are <= this ratio of stale monitoring peak, rebase baseline.")

;;; --------------------------------------
;;; PAYLOAD HELPERS
;;; --------------------------------------

(defun %payload-val (payload key &optional default)
  "Fetch value from alist or jsown payload with KEY."
  (cond
    ((listp payload)
     (let ((cell (assoc key payload)))
       (if cell
           (values (cdr cell) t)
           (let ((key-name (cond ((symbolp key) (string-downcase (symbol-name key)))
                                 ((stringp key) (string-downcase key))
                                 (t nil))))
             (if (not key-name)
                 (values default nil)
                 (loop for (k . v) in payload
                       when (or (and (symbolp k)
                                     (string= (string-downcase (symbol-name k)) key-name))
                                (and (stringp k)
                                     (string= (string-downcase k) key-name)))
                       do (return (values v t))
                       finally (return (values default nil))))))))
    (t
     (let ((k (if (symbolp key) (string-downcase (symbol-name key)) key)))
       (if (jsown:keyp payload k)
           (values (jsown:val payload k) t)
           (values default nil))))))

(defun %payload-val* (payload keys &optional default)
  "Fetch first matching value from KEYS in payload."
  (dolist (k keys (values default nil))
    (multiple-value-bind (val ok) (%payload-val payload k)
      (when ok (return (values val t))))))

(defun %payload-first (payload keys &optional default)
  "Fetch first matching value from KEYS in payload (single value)."
  (multiple-value-bind (val ok) (%payload-val* payload keys default)
    (declare (ignore ok))
    val))

(defun %payload-string (payload keys &optional (default ""))
  "Fetch string value from payload for KEYS."
  (let ((val (%payload-first payload keys default)))
    (cond ((null val) default)
          ((symbolp val) (symbol-name val))
          ((stringp val) val)
          (t (format nil "~a" val)))))

(defun %payload-number (payload keys &optional default)
  "Fetch numeric value from payload for KEYS."
  (let ((val (%payload-first payload keys default)))
    (cond ((numberp val) val)
          ((stringp val) (or (ignore-errors (read-from-string val)) default))
          (t default))))

(defun %normalize-token-string (value)
  "Return trimmed string token for VALUE, or NIL when empty."
  (let* ((raw (cond ((null value) "")
                    ((stringp value) value)
                    ((symbolp value) (symbol-name value))
                    (t (format nil "~a" value))))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
    (when (> (length trimmed) 0) trimmed)))

(defun %nil-like-token-p (value &key (treat-unknown nil))
  "True when VALUE represents NIL-like token."
  (let ((token (%normalize-token-string value)))
    (if (null token)
        t
        (let ((up (string-upcase token)))
          (or (string= up "NIL")
              (string= up "NULL")
              (string= up "NONE")
              (and treat-unknown (string= up "UNKNOWN")))))))

(defun %direction-token->string (value)
  "Normalize VALUE into BUY/SELL string or empty string."
  (let* ((token (%normalize-token-string value))
         (up (if token (string-upcase token) "")))
    (cond ((or (string= up "") (string= up "TRADE_CLOSED")) "")
          ((search "BUY" up) "BUY")
          ((search "SELL" up) "SELL")
          (t ""))))

(defun %direction-token->keyword (value)
  "Normalize direction VALUE into :BUY/:SELL/:UNKNOWN."
  (let ((dir (%direction-token->string value)))
    (cond ((string= dir "BUY") :buy)
          ((string= dir "SELL") :sell)
          (t :unknown))))

(defun %normalize-category-token (value)
  "Normalize VALUE into category keyword, or NIL when missing/invalid token."
  (let ((token (%normalize-token-string value)))
    (when (and token (not (%nil-like-token-p token :treat-unknown t)))
      (intern (string-upcase token) :keyword))))

(defun %normalize-strategy-name-token (value)
  "Normalize strategy token; return NIL when missing/nil-like/unknown."
  (let ((token (%normalize-token-string value)))
    (when (and token (not (%nil-like-token-p token :treat-unknown t)))
      token)))

(defun %category-from-magic (magic)
  "Decode category keyword from encoded magic number, or NIL when unknown."
  (when (numberp magic)
    (let ((magic-int (truncate magic)))
      (when (and (>= magic-int 100000000) (< magic-int 200000000))
        (case (truncate (- magic-int 100000000) 10000000)
          (1 :trend)
          (2 :reversion)
          (3 :breakout)
          (4 :scalp)
          (otherwise nil))))))

(defun %format-signed-yen (amount)
  "Format AMOUNT as signed yen text without trailing decimals."
  (let* ((value (if (numberp amount) (float amount) 0.0))
         (sign (if (>= value 0.0) "+" "-"))
         (rounded (round (abs value))))
    (format nil "~a¥~d" sign rounded)))

(defun %format-trade-close-notification (symbol direction strategy-name category pnl entry-price exit-price
                                         &key lot open-time)
  "Build fixed-template trade-close notification text."
  (let* ((symbol-token (or (%normalize-token-string symbol) "UNKNOWN"))
         (direction-token (or (%normalize-token-string direction) "UNKNOWN"))
         (strategy-token (or (%normalize-strategy-name-token strategy-name) "Unknown"))
         (category-token (or (%normalize-token-string category) "unknown"))
         (pnl-value (if (numberp pnl) (float pnl) 0.0))
         (entry-value (if (numberp entry-price) (float entry-price) 0.0))
         (exit-value (if (numberp exit-price) (float exit-price) 0.0))
         (lot-line (if (and (numberp lot) (> lot 0.0))
                       (format nil "~%Lot: ~,2f" (float lot))
                       ""))
         (open-line (if (and (numberp open-time) (> open-time 0))
                        (format nil "~%OpenTime: ~d" (truncate open-time))
                        "")))
    (format nil "Trade Closed | ~a~%Result: ~a ~a~%Strategy: ~a (~a)~%PnL: ~a~%Entry: ~,5f~%Exit: ~,5f~a~a"
            (if (> pnl-value 0.0) "WIN" "LOSS")
            symbol-token
            (string-upcase direction-token)
            strategy-token
            (string-downcase category-token)
            (%format-signed-yen pnl-value)
            entry-value
            exit-value
            lot-line
            open-line)))

(defun stale-account-info-timestamp-p (timestamp)
  "True when TIMESTAMP belongs to a previous process/session."
  (and (numberp timestamp)
       (> timestamp 0)
       (boundp 'swimmy.globals::*system-start-time*)
       (numberp swimmy.globals::*system-start-time*)
       (< timestamp swimmy.globals::*system-start-time*)))

(defun should-rebase-monitoring-peak-p (prev-account-info-time prev-current incoming-equity prev-monitor-peak)
  "Return T when stale dynamic-DD baseline should be reset to incoming equity."
  (let* ((ratio (if (and (numberp *monitoring-peak-stale-ratio-threshold*)
                         (> *monitoring-peak-stale-ratio-threshold* 0.0)
                         (< *monitoring-peak-stale-ratio-threshold* 1.0))
                    *monitoring-peak-stale-ratio-threshold*
                    0.50))
         (peak (if (and (numberp prev-monitor-peak) (> prev-monitor-peak 0.0))
                   prev-monitor-peak
                   0.0))
         (cutoff (* peak ratio)))
    (and *monitoring-peak-rebase-on-restart*
         (stale-account-info-timestamp-p prev-account-info-time)
         (> peak 0.0)
         (numberp prev-current)
         (numberp incoming-equity)
         (<= prev-current cutoff)
         (<= incoming-equity cutoff))))

;;; --------------------------------------
;;; STATUS & MONITORING
;;; --------------------------------------

(defun update-status-file ()
  "Write system status to /tmp/swimmy_status"
  (ignore-errors
    (with-open-file (s "/tmp/swimmy_status" :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s "TIME: ~a~%" (swimmy.core:get-jst-str))
      (format s "TICKS: ~d~%" *total-ticks-count*)
      (format s "EQUITY: ~,2f~%" *current-equity*)
      (format s "PNL: ~,2f~%" *daily-pnl*)
      (format s "MT5: ~a~%" (if (> (- (get-universal-time) *last-guardian-heartbeat*) 60) "OFFLINE" "ONLINE")))))

(defun pulse-check ()
  "Print 1-line HUD pulse and update status file every 60 ticks"
  (incf *total-ticks-count*)
  (when (or (= (mod *total-ticks-count* 60) 0) (= *total-ticks-count* 1))
    (format t "~%[💓] ~a | Ticks: ~d | MT5: ~a | Equity: ~d~%"
            (swimmy.core:get-jst-str) 
            *total-ticks-count* 
            (if (> (- (get-universal-time) *last-guardian-heartbeat*) 60) "OFFLINE" "ONLINE")
            (round *current-equity*))
    (force-output) ; Ensure pulse is visible
    (update-status-file)))

;;; --------------------------------------
;;; HEARTBEAT
;;; --------------------------------------

(defun send-heartbeat ()
  (let ((now (get-universal-time)))
    ;; Heartbeat must be frequent enough to prevent Guardian/Watchdog false "brain silence".
    (when (> (- now *last-heartbeat-sent*) 10)
      (pulse-check) ; Added Pulse Check here
      (let* ((heartbeat-msg (make-heartbeat-message))
             (hb-id (ignore-errors (swimmy.core:sexp-alist-get heartbeat-msg "id")))
             (hb-status (ignore-errors (swimmy.core:sexp-alist-get heartbeat-msg "status")))
             (hb-source (ignore-errors (swimmy.core:sexp-alist-get heartbeat-msg "source"))))
        (swimmy.core::emit-telemetry-event "heartbeat.sent"
          :service "executor"
          :severity "info"
          :correlation-id hb-id
          :data (jsown:new-js
                  ("heartbeat_id" hb-id)
                  ("status" hb-status)
                  ("source" hb-source)))
        (when (and (boundp '*cmd-publisher*) *cmd-publisher*)
          (pzmq:send *cmd-publisher* (swimmy.core:encode-sexp heartbeat-msg))))
      ;; V43.0: Position Status Report every 5 minutes
      (when (and (= (mod (floor now 30) 10) 0)
                 (boundp 'swimmy.school::*slot-allocation*)
                 (> (hash-table-count swimmy.school::*slot-allocation*) 0))
        (handler-case (swimmy.school:report-active-positions) (error () nil)))
      ;; V8.5: ACCOUNT_INFO Monitoring
      (when (and (> *last-account-info-time* 0)
                 (> (- now *last-account-info-time*) 60)
                 (not *account-info-alert-sent*))
        (notify-discord-alert 
         (format nil "⚠️ ACCOUNT_INFO Timeout - MT5同期が~d秒間途絶" (- now *last-account-info-time*))
         :color 16776960) ; Yellow
        (setf *account-info-alert-sent* t))
      
      ;; Phase 7: Retry Logic
      (check-pending-orders)
      
      (setf *last-heartbeat-sent* now))))

;; Phase 7: Retry Logic Implementation
(defun execution-link-stale-reason-for-pending (&optional (now (get-universal-time)))
  "Return stale-link reason when pending retry/timeout should be paused; otherwise NIL."
  (when (fboundp 'swimmy.engine::execution-link-stale-reason)
    (ignore-errors (swimmy.engine::execution-link-stale-reason now))))

(defun check-pending-orders ()
  "Check pending orders for timeout and retry."
  (when (and (boundp 'swimmy.globals:*pending-orders*)
             (> (hash-table-count swimmy.globals:*pending-orders*) 0))
    (let* ((now (get-universal-time))
          (stale-reason (execution-link-stale-reason-for-pending now))
          (to-remove nil))
      (when stale-reason
        ;; Fail-closed: pause retries/timeouts while execution link is stale.
        (return-from check-pending-orders nil))
      (maphash (lambda (id data)
                 (destructuring-bind (ts retries msg-obj) data
                   (when (> (- now ts) 5) ;; 5 seconds timeout
                     (if (< retries 3)
                         (progn
                           ;; Retry
                           (format t "[L] 🔄 Resending Order ~a (Retry ~d)~%" id (1+ retries))
                           (setf (gethash id swimmy.globals:*pending-orders*) 
                                 (list now (1+ retries) msg-obj))
                           (pzmq:send *cmd-publisher* (swimmy.core:encode-sexp msg-obj)))
                         (progn
                           ;; Fail
                           (format t "[L] ❌ Order ~a TIMED OUT after 3 retries~%" id)
                           (notify-discord-alert (format nil "❌ Order Timed Out: ~a" id) :color 15158332)
                           (push id to-remove))))))
               swimmy.globals:*pending-orders*)
      (dolist (id to-remove)
        (remhash id swimmy.globals:*pending-orders*)))))

;;; --------------------------------------
;;; TRADE PROCESSING
;;; --------------------------------------

(defun train-nn-from-trade (symbol pnl direction)
  "Train NN from trade outcome - online learning"
  (declare (ignore symbol direction))
  (when (and *candle-history* (> (length *candle-history*) 30))
    (handler-case
        (let* ((target (cond
                         ((> pnl 0.3) 0)   ;; Clear win -> UP correct
                         ((< pnl -0.3) 1)  ;; Clear loss -> DOWN correct
                         (t 2)))           ;; Small -> FLAT
               (candles-json (swimmy.school:candles-to-json (subseq *candle-history* 0 (min 30 (length *candle-history*)))))
               (cmd (swimmy.core::sexp->string
                     `((type . "TRAIN")
                       (candles . ,candles-json)
                       (target . ,target))
                     :package *package*)))
          (pzmq:send *cmd-publisher* cmd)
          (format t "[L] 🧠 NN TRAIN: target=~a (~a)~%" target 
                  (cond ((= target 0) "UP") ((= target 1) "DOWN") (t "FLAT"))))
      (error (e) (format t "[L] NN train error: ~a~%" e)))))

(defun process-trade-closed (payload msg)
  ;; DEBUG: Log raw message to understand grouping
  (when (stringp msg)
    (format t "~%[L] 🔍 DEBUG TRADE_CLOSED: ~a~%"
            (subseq msg 0 (min 300 (length msg)))))
  (handler-case
      (multiple-value-bind (won won-ok) (%payload-val* payload '(won "won") nil)
        (let* ((ticket (%payload-first payload '(ticket "ticket") "?"))
               (symbol (%payload-string payload '(symbol "symbol") "UNKNOWN"))
               (pnl (%payload-number payload '(profit "profit" pnl "pnl" close_profit "close_profit") 0))
               (magic-from-json (%payload-number payload '(magic "magic") nil))
               (direction (let ((payload-dir (%direction-token->string
                                              (%payload-string payload '(action "action" side "side" direction "direction" type "type") ""))))
                            payload-dir))
               (dir-keyword (%direction-token->keyword direction))
               (alloc-dir-key (case dir-keyword
                                (:buy :long)
                                (:sell :short)
                                (otherwise :unknown)))
               (strategy-name
                (or (%normalize-strategy-name-token
                     (%payload-string payload '(strategy "strategy" strategy_name "strategy_name" "strategy-name") ""))
                    "Unknown"))
               (category (or (%normalize-category-token (%payload-first payload '(category "category") nil))
                             (%category-from-magic magic-from-json)
                             :trend))
               (raw-lot (%payload-number payload '(lot "lot") nil))
               (lot (if (and (numberp raw-lot) (> raw-lot 0)) (float raw-lot) 0.01))
               (raw-open-time (%payload-number payload '(open_time "open_time" open-time "open-time") nil))
               (open-time (if (and (numberp raw-open-time) (> raw-open-time 0)) raw-open-time 0))
               (is-win (if won-ok (and won t) (> pnl 0))))
          ;; V44.0: Ticket Deduplication (Expert Panel P1)
          ;; Prevent processing same TRADE_CLOSED twice
          (when (and (boundp '*processed-tickets*) (gethash ticket *processed-tickets*))
            (format t "[L] ⚠️ DUPLICATE TRADE_CLOSED ticket ~a - SKIPPING~%" ticket)
            (return-from process-trade-closed nil))
          (when (boundp '*processed-tickets*)
            (setf (gethash ticket *processed-tickets*) (get-universal-time)))

          ;; Update PnL tracking (Daily/Weekly/Monthly)
          (incf *daily-pnl* pnl)
          (incf *weekly-pnl* pnl)
          (incf *monthly-pnl* pnl)
          (incf *accumulated-pnl* pnl)

	          ;; V44.0 FIX: Cleanup ONLY ONE slot per TRADE_CLOSED (Expert Panel P1)
	          ;; Match by magic number if available, else by symbol+direction (remove only first match)
	          (when (boundp 'swimmy.school::*slot-allocation*)
	            (let ((removed nil))
	              (maphash (lambda (key slot)
	                         (when (and (not removed)
	                                    (or (and magic-from-json (= (getf slot :magic) magic-from-json))
	                                        (and (null magic-from-json)
	                                             (equal (getf slot :symbol) symbol)
	                                             (eq (getf slot :direction) alloc-dir-key))))
	                           (remhash key swimmy.school::*slot-allocation*)
	                           (setf removed t)
	                           (format t "[L] 🧹 SLOT CLEANUP: ~a freed (Ticket: ~a)~%" key ticket)))
	                       swimmy.school::*slot-allocation*)))

	          ;; Optional telemetry enrichment for slippage evidence.
	          ;; Learning context remains payload-canonical; this block is best-effort only.
	          (let ((entry-context (and magic-from-json
	                                    (ignore-errors
	                                      (swimmy.school:lookup-entry-context-by-magic magic-from-json)))))
	            (when (and entry-context (fboundp 'swimmy.core::emit-telemetry-event))
	              (let* ((raw-entry (%payload-number payload '(entry_price "entry_price" entry-price "entry-price") 0.0))
	                     (entry-price (if (> raw-entry 0.00001) raw-entry nil))
	                     (entry-bid (getf entry-context :entry-bid))
	                     (entry-ask (getf entry-context :entry-ask))
	                     (entry-spread-pips (getf entry-context :entry-spread-pips))
	                     (entry-cost-pips (getf entry-context :entry-cost-pips))
	                     (entry-direction (or (getf entry-context :direction) direction))
	                     (strategy-for-slip (or (%normalize-strategy-name-token strategy-name)
	                                            (%normalize-strategy-name-token (getf entry-context :strategy)))))
	                (when (and entry-price (numberp entry-bid) (numberp entry-ask))
	                  (let ((slip (swimmy.school:slippage-pips-from-fill
	                               symbol entry-direction entry-bid entry-ask entry-price)))
	                    (when (numberp slip)
	                      (when (and strategy-for-slip
	                                 (fboundp 'swimmy.school::record-dryrun-slippage))
	                        (swimmy.school::record-dryrun-slippage strategy-for-slip slip))
	                      (swimmy.core::emit-telemetry-event "execution.slippage"
	                        :service "executor"
	                        :severity "info"
	                        :data (jsown:new-js
	                                ("symbol" symbol)
	                                ("direction"
	                                 (let ((d1 (%direction-token->string entry-direction))
	                                       (d2 (%direction-token->string direction)))
	                                   (cond ((> (length d1) 0) d1)
	                                         ((> (length d2) 0) d2)
	                                         (t ""))))
	                                ("magic" magic-from-json)
	                                ("entry_expected_bid" entry-bid)
	                                ("entry_expected_ask" entry-ask)
	                                ("entry_actual" entry-price)
	                                ("slippage_pips" slip)
	                                ("entry_spread_pips" entry-spread-pips)
	                                ("entry_cost_pips" entry-cost-pips)
	                                ("pip_size" (swimmy.school:get-pip-size symbol))))))))))

	          ;; Record result for danger tracking
	          (swimmy.school:record-trade-result (if is-win :win :loss))

          ;; V5.1: Increment total trades
          (incf *total-trades*)

          ;; V3.0: Track success count
          (when is-win (incf *success-count*))

          ;; LEARNING: Record for dreamer analysis
          (let* ((pair-id (swimmy.school:lookup-pair-id-by-magic magic-from-json)))
            (handler-case
                (swimmy.school:record-trade-outcome symbol dir-keyword category strategy-name pnl :pair-id pair-id)
              (error (e) (format t "[L] Learning record error: ~a~%" e))))

          ;; NEURAL NETWORK ONLINE LEARNING
          (handler-case
              (train-nn-from-trade symbol pnl direction)
            (error (e) (format t "[L] NN train error: ~a~%" e)))

          ;; ELDER LEARNING
          (unless is-win
            (handler-case
                (let* ((history (gethash symbol *candle-histories*))
                       (rsi (when (and history (> (length history) 14))
                              (swimmy.school:ind-rsi 14 history)))
                       (price-pos (when (and history (fboundp 'swimmy.school:get-price-position))
                                    (swimmy.school:get-price-position history)))
                       (context (list :regime *market-regime*
                                      :volatility-state *current-volatility-state*
                                      :session (swimmy.school:current-trading-session)
                                      :rsi-value rsi
                                      :price-position price-pos
                                      :symbol symbol
                                      :direction dir-keyword)))
                  (when (fboundp 'swimmy.school:learn-from-failure)
                    (swimmy.school:learn-from-failure context pnl))
                  (format t "[L] 📚 長老会議に敗因を報告(6次元)~%"))
              (error (e) (format t "[L] Elder learning error: ~a~%" e))))

          ;; LEADER STATS + MEMORY
          (handler-case
              (progn
                (when (fboundp 'swimmy.school:update-leader-stats)
                  (swimmy.school:update-leader-stats pnl))
                (when (fboundp 'swimmy.school:store-memory)
                  (swimmy.school:store-memory symbol dir-keyword (if is-win :win :loss) pnl 0)))
            (error (e) (format t "[L] Leader/Memory error: ~a~%" e)))

	          ;; Fixed-template trade-close notification (payload-canonical, no narrative dependency).
	          (handler-case
	              (let* ((curr-candle (gethash symbol swimmy.school::*current-candles*))
	                     (curr-close (if curr-candle (swimmy.school::candle-close curr-candle) 0.0))
	                     (raw-exit (%payload-number payload '(exit_price "exit_price" exit-price "exit-price") 0.0))
	                     (exit-price (if (> raw-exit 0.00001) raw-exit curr-close))
	                     (raw-entry (%payload-number payload '(entry_price "entry_price" entry-price "entry-price") 0.0))
	                     (entry-price (if (> raw-entry 0.00001) raw-entry exit-price))
	                     (message (%format-trade-close-notification
	                               symbol direction strategy-name category pnl entry-price exit-price
	                               :lot lot
	                               :open-time open-time)))
	                (format t "[L] ~a~%" message)
	                (queue-discord-notification (gethash symbol *symbol-webhooks*) message
	                                            :color (if is-win 3066993 15158332)))
	            (error (e)
	              (format t "[L] Trade close notification error: ~a~%" e)))

          ;; Persist state immediately after trade close
          (when (fboundp 'swimmy.engine:save-state)
            (funcall 'swimmy.engine:save-state)
            (format t "[L] 💾 Global state saved after trade close~%"))))
    (error (e) (format t "[L] Trade close error: ~a~%" e))))

(defun process-account-info (payload)
  (handler-case
      (multiple-value-bind (equity equity-ok) (%payload-val* payload '(equity "equity") nil)
        (multiple-value-bind (balance balance-ok) (%payload-val* payload '(balance "balance") nil)
          (declare (ignore balance balance-ok))
          (let* ((now (get-universal-time))
                (prev-last-account-info *last-account-info-time*)
                (prev-current-equity *current-equity*)
                (prev-monitoring-peak *monitoring-peak-equity*))
          ;; ACCOUNT_INFO implies MT5 execution link is alive.
          (setf *last-account-info-time* now
                *last-guardian-heartbeat* now)
          ;; V8.5: Recovery notification
          (when *account-info-alert-sent*
            (notify-discord-alert "✅ ACCOUNT_INFO Recovered - MT5同期復旧" :color 3066993)
            (setf *account-info-alert-sent* nil))
          (when (and equity-ok (numberp equity) (> equity 0))
            (setf *current-equity* (float equity))
            (when (> *current-equity* *peak-equity*)
              (setf *peak-equity* *current-equity*))

            ;; V44.5+: Dynamic (Session) DD Logic
            (if (should-rebase-monitoring-peak-p prev-last-account-info
                                                 prev-current-equity
                                                 *current-equity*
                                                 prev-monitoring-peak)
                (progn
                  (setf *monitoring-peak-equity* *current-equity*)
                  (setf *monitoring-drawdown* 0.0)
                  (setf *monitoring-alert-sent-20* nil)
                  (format t "[L] ♻️ DynDD baseline reset after restart: Peak=¥~d -> ¥~d~%"
                          (round prev-monitoring-peak)
                          (round *monitoring-peak-equity*)))
                (progn
                  (when (zerop *monitoring-peak-equity*)
                    (setf *monitoring-peak-equity* *current-equity*))
                  (when (> *current-equity* *monitoring-peak-equity*)
                    (setf *monitoring-peak-equity* *current-equity*)
                    (setf *monitoring-alert-sent-20* nil))
                  (setf *monitoring-drawdown*
                        (* 100 (/ (- *monitoring-peak-equity* *current-equity*)
                                  *monitoring-peak-equity*)))))
                
            ;; Alert Logic (20% Threshold)
            (when (and (>= *monitoring-drawdown* 20.0)
                       (not *monitoring-alert-sent-20*))
              (notify-discord-alert (format nil "⚠️ DYNAMIC DRAWDOWN WARNING: ~,1f% (Peak: ¥~d)"
                                            *monitoring-drawdown* (round *monitoring-peak-equity*))
                                    :color 16776960)
              (setf *monitoring-alert-sent-20* t))

            ;; Legacy Stats
            (when (> *peak-equity* 0)
              (setf *current-drawdown* (* 100 (/ (- *peak-equity* *current-equity*) *peak-equity*))))
            (format t "[L] 💰 MT5 Sync: Equity=¥~d DynPK=¥~d DynDD=~,1f% (LegacyDD=~,1f%)~%"
                    (round *current-equity*) (round *monitoring-peak-equity*)
                    *monitoring-drawdown* *current-drawdown*)
            ;; Persist account metrics periodically so daily report survives restarts
            (let ((now (get-universal-time)))
              (when (> (- now *last-account-save-time*) *account-info-save-interval*)
                (setf *last-account-save-time* now)
                (when (fboundp 'swimmy.engine:save-state)
                  (swimmy.engine:save-state))))))))
    (error (e) (format t "[L] Account sync error: ~a~%" e))))

;;; --------------------------------------
;;; UTILS
;;; --------------------------------------

(defun get-system-metrics ()
  "Return basic system metrics for Cortex consumption"
  (list :ticks *total-ticks-count*
        :equity *current-equity*
        :last-heartbeat *last-heartbeat-sent*))
