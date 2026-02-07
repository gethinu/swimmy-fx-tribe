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
    (format t "~%[💓] ~a | Ticks: ~d | MT5: ~a | Equity: ~,0f~%" 
            (swimmy.core:get-jst-str) 
            *total-ticks-count* 
            (if (> (- (get-universal-time) *last-guardian-heartbeat*) 60) "OFFLINE" "ONLINE")
            *current-equity*)
    (force-output) ; Ensure pulse is visible
    (update-status-file)))

;;; --------------------------------------
;;; HEARTBEAT
;;; --------------------------------------

(defun send-heartbeat ()
  (let ((now (get-universal-time)))
    (when (> (- now *last-heartbeat-sent*) 30)
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
      ;; V43.0: Position Status Report every 5 minutes (10 heartbeats)
      (when (and (= (mod (floor now 30) 10) 0)
                 (boundp 'swimmy.school::*warrior-allocation*)
                 (> (hash-table-count swimmy.school::*warrior-allocation*) 0))
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
(defun check-pending-orders ()
  "Check pending orders for timeout and retry."
  (when (and (boundp 'swimmy.globals:*pending-orders*)
             (> (hash-table-count swimmy.globals:*pending-orders*) 0))
    (let ((now (get-universal-time))
          (to-remove nil))
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
               (direction-raw (%payload-string payload '(action "action" side "side" direction "direction" type "type") ""))
               (direction (if (string= (string-upcase direction-raw) "TRADE_CLOSED") "" direction-raw))
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

          ;; V44.0 FIX: Cleanup ONLY ONE warrior per TRADE_CLOSED (Expert Panel P1)
          ;; Match by magic number if available, else by symbol+direction (remove only first match)
          (when (boundp 'swimmy.school::*warrior-allocation*)
            (let ((magic-from-json (%payload-number payload '(magic "magic") nil))
                  (dir-key (cond ((search "BUY" (string-upcase direction)) :long)
                                 ((search "SELL" (string-upcase direction)) :short)
                                 (t :unknown)))
                  (removed nil))
              (maphash (lambda (key warrior)
                         (when (and (not removed)
                                    (or (and magic-from-json (= (getf warrior :magic) magic-from-json))
                                        (and (null magic-from-json)
                                             (equal (getf warrior :symbol) symbol)
                                             (eq (getf warrior :direction) dir-key))))
                           (remhash key swimmy.school::*warrior-allocation*)
                           (setf removed t)
                           (format t "[L] 🧹 WARRIOR CLEANUP: Slot ~a freed (Ticket: ~a)~%" key ticket)))
                       swimmy.school::*warrior-allocation*)))

          ;; Record result for danger tracking
          (swimmy.school:record-trade-result (if is-win :win :loss))

          ;; V5.1: Increment total trades
          (incf *total-trades*)

          ;; V3.0: Track success count
          (when is-win (incf *success-count*))

          ;; LEARNING: Record for dreamer analysis
          (let* ((dir-keyword (cond ((search "BUY" (string-upcase direction)) :buy)
                                    ((search "SELL" (string-upcase direction)) :sell)
                                    (t :unknown)))
                 (category-raw (%payload-first payload '(category "category") nil))
                 (category (cond ((keywordp category-raw) category-raw)
                                 ((symbolp category-raw)
                                  (intern (string-upcase (symbol-name category-raw)) :keyword))
                                 ((stringp category-raw)
                                  (intern (string-upcase category-raw) :keyword))
                                 (t :trend)))
                 (strategy-name (%payload-string payload '(strategy "strategy") "unknown"))
                 (magic (%payload-number payload '(magic "magic") nil))
                 (pair-id (swimmy.school:lookup-pair-id-by-magic magic)))
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
                                      :direction (cond ((search "BUY" (string-upcase direction)) :buy)
                                                       ((search "SELL" (string-upcase direction)) :sell)
                                                       (t :unknown)))))
                  (when (fboundp 'swimmy.school:learn-from-failure)
                    (swimmy.school:learn-from-failure context pnl))
                  (format t "[L] 📚 長老会議に敗因を報告(6次元)~%"))
              (error (e) (format t "[L] Elder learning error: ~a~%" e))))

          ;; LEADER STATS + MEMORY
          (handler-case
              (progn
                (when (fboundp 'swimmy.school:update-leader-stats)
                  (swimmy.school:update-leader-stats pnl))
                (let ((dir-keyword (cond ((search "BUY" (string-upcase direction)) :buy)
                                         ((search "SELL" (string-upcase direction)) :sell)
                                         (t :unknown))))
                  (when (fboundp 'swimmy.school:store-memory)
                    (swimmy.school:store-memory symbol dir-keyword (if is-win :win :loss) pnl 0))))
            (error (e) (format t "[L] Leader/Memory error: ~a~%" e)))

          ;; Victory/Funeral Ceremony using RICH narrative (P9 Fix)
          (handler-case
              (let* ((magic (%payload-number payload '(magic "magic") nil))
                     (lot (%payload-number payload '(lot "lot") 0.01))
                     ;; V44.10: Price Fallback Logic (Fix for 0.0 notification)
                     ;; If MT5 sends 0, use current market price for Exit, and Exit for Entry default.
                     (curr-candle (gethash symbol swimmy.school::*current-candles*))
                     (curr-close (if curr-candle (swimmy.school::candle-close curr-candle) 0.0))
                     (raw-exit (%payload-number payload '(exit_price "exit_price" exit-price "exit-price") 0.0))
                     (exit-price (if (> raw-exit 0.00001) raw-exit curr-close))
                     (raw-entry (%payload-number payload '(entry_price "entry_price" entry-price "entry-price") 0.0))
                     (entry-price (if (> raw-entry 0.00001) raw-entry exit-price))
                     ;; V44.8: Try to resolve Unknown strategy using Magic Number
                     (strategy-name
                      (let ((raw-name (%payload-string payload '(strategy "strategy" strategy_name "strategy_name" "strategy-name") "Unknown")))
                        (if (or (string= raw-name "Unknown") (string= raw-name "") (search "Swimmy" raw-name))
                            (or (swimmy.school:lookup-strategy-by-magic magic) "Unknown")
                            raw-name)))
                     (open-time (%payload-number payload '(open_time "open_time" open-time "open-time") 0))
                     (category-raw (%payload-first payload '(category "category") nil))
                     (category (cond ((keywordp category-raw) category-raw)
                                     ((symbolp category-raw)
                                      (intern (string-upcase (symbol-name category-raw)) :keyword))
                                     ((stringp category-raw)
                                      (intern (string-upcase category-raw) :keyword))
                                     (t :trend)))
                     (duration-seconds (if (and (numberp open-time) (> open-time 0))
                                           (- (get-universal-time) open-time)
                                           0))
                     (dir-keyword (cond ((search "BUY" (string-upcase direction)) :buy)
                                        ((search "SELL" (string-upcase direction)) :sell)
                                        (t :unknown)))
                     (narrative (swimmy.school:generate-trade-result-narrative
                                 symbol dir-keyword pnl pnl entry-price exit-price lot strategy-name duration-seconds category)))
                (format t "[L] ~a~%" narrative)
                (queue-discord-notification (gethash symbol *symbol-webhooks*) narrative
                                            :color (if is-win 3066993 15158332)))
            (error (e)
              ;; Fallback to simple notification if narrative generation fails
              (format t "[L] Narrative generation error: ~a. Using fallback.~%" e)
              (if is-win
                  (queue-discord-notification (gethash symbol *symbol-webhooks*)
                                              (format nil "🎉 WIN: ~a ~a +¥~,0f" symbol direction pnl) :color 3066993)
                  (queue-discord-notification (gethash symbol *symbol-webhooks*)
                                              (format nil "💀 LOSS: ~a ~a ¥~,0f" symbol direction (abs pnl)) :color 15158332))))

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
          ;; V8.5: Track last ACCOUNT_INFO time for monitoring
          (setf *last-account-info-time* (get-universal-time))
          ;; V8.5: Recovery notification
          (when *account-info-alert-sent*
            (notify-discord-alert "✅ ACCOUNT_INFO Recovered - MT5同期復旧" :color 3066993)
            (setf *account-info-alert-sent* nil))
          (when (and equity-ok (numberp equity) (> equity 0))
            (setf *current-equity* (float equity))
            (when (> *current-equity* *peak-equity*)
              (setf *peak-equity* *current-equity*))
            
            ;; V44.5: Dynamic (Session) DD Logic
            (when (zerop *monitoring-peak-equity*)
              (setf *monitoring-peak-equity* *current-equity*))
            
            (when (> *current-equity* *monitoring-peak-equity*)
              (setf *monitoring-peak-equity* *current-equity*)
              (setf *monitoring-alert-sent-20* nil))
            
            (setf *monitoring-drawdown* 
                  (* 100 (/ (- *monitoring-peak-equity* *current-equity*) *monitoring-peak-equity*)))
                
            ;; Alert Logic (20% Threshold)
            (when (and (>= *monitoring-drawdown* 20.0)
                       (not *monitoring-alert-sent-20*))
              (notify-discord-alert (format nil "⚠️ DYNAMIC DRAWDOWN WARNING: ~,1f% (Peak: ¥~,0f)" 
                                            *monitoring-drawdown* *monitoring-peak-equity*)
                                    :color 16776960)
              (setf *monitoring-alert-sent-20* t))

            ;; Legacy Stats
            (when (> *peak-equity* 0)
              (setf *current-drawdown* (* 100 (/ (- *peak-equity* *current-equity*) *peak-equity*))))
            (format t "[L] 💰 MT5 Sync: Equity=¥~,0f DynPK=¥~,0f DynDD=~,1f% (LegacyDD=~,1f%)~%"
                    *current-equity* *monitoring-peak-equity* *monitoring-drawdown* *current-drawdown*)
            ;; Persist account metrics periodically so daily report survives restarts
            (let ((now (get-universal-time)))
              (when (> (- now *last-account-save-time*) *account-info-save-interval*)
                (setf *last-account-save-time* now)
                (when (fboundp 'swimmy.engine:save-state)
                  (swimmy.engine:save-state)))))))
    (error (e) (format t "[L] Account sync error: ~a~%" e))))

;;; --------------------------------------
;;; UTILS
;;; --------------------------------------

(defun get-system-metrics ()
  "Return basic system metrics for Cortex consumption"
  (list :ticks *total-ticks-count*
        :equity *current-equity*
        :last-heartbeat *last-heartbeat-sent*))
