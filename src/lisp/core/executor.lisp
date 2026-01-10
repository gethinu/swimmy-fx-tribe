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
      (when (and (boundp '*cmd-publisher*) *cmd-publisher*)
        (pzmq:send *cmd-publisher* (jsown:to-json (jsown:new-js ("action" "HEARTBEAT")))))
      ;; V8.5: ACCOUNT_INFO Monitoring
      (when (and (> *last-account-info-time* 0)
                 (> (- now *last-account-info-time*) 60)
                 (not *account-info-alert-sent*))
        (notify-discord-alert 
         (format nil "⚠️ ACCOUNT_INFO Timeout - MT5同期が~d秒間途絶" (- now *last-account-info-time*))
         :color 16776960) ; Yellow
        (setf *account-info-alert-sent* t))
      (setf *last-heartbeat-sent* now))))

;;; --------------------------------------
;;; TRADE PROCESSING
;;; --------------------------------------

(defun train-nn-from-trade (symbol pnl direction)
  "Train NN from trade outcome - online learning"
  (when (and *candle-history* (> (length *candle-history*) 30))
    (handler-case
        (let* ((target (cond
                         ((> pnl 0.3) 0)   ;; Clear win -> UP correct
                         ((< pnl -0.3) 1)  ;; Clear loss -> DOWN correct
                         (t 2)))           ;; Small -> FLAT
               (candles-json (candles-to-json (subseq *candle-history* 0 (min 30 (length *candle-history*)))))
               (cmd (jsown:to-json (jsown:new-js
                      ("action" "TRAIN")
                      ("candles" candles-json)
                      ("target" target)))))
          (pzmq:send *cmd-publisher* cmd)
          (format t "[L] 🧠 NN TRAIN: target=~a (~a)~%" target 
                  (cond ((= target 0) "UP") ((= target 1) "DOWN") (t "FLAT"))))
      (error (e) (format t "[L] NN train error: ~a~%" e)))))

(defun process-trade-closed (json msg)
  ;; DEBUG: Log raw message to understand grouping
  (format t "~%[L] 🔍 DEBUG TRADE_CLOSED: ~a~%" (subseq msg 0 (min 300 (length msg))))
  (handler-case
      (let* ((ticket (if (jsown:keyp json "ticket") (jsown:val json "ticket") "?"))
             (symbol (if (jsown:keyp json "symbol") (jsown:val json "symbol") "UNKNOWN"))
             (pnl (cond 
                    ((jsown:keyp json "profit") (jsown:val json "profit"))
                    ((jsown:keyp json "pnl") (jsown:val json "pnl"))
                    ((jsown:keyp json "close_profit") (jsown:val json "close_profit"))
                    (t 0)))
             (direction (if (jsown:keyp json "type") (jsown:val json "type") ""))
             (is-win (> pnl 0)))
        ;; Update PnL tracking
        (incf *daily-pnl* pnl)
        (incf *accumulated-pnl* pnl)
        
        ;; V9.0 FIX: Cleanup warrior allocation on external close (TP/SL)
        (when (boundp 'swimmy.school::*warrior-allocation*)
          (let ((dir-key (if (search "BUY" (string-upcase direction)) :long :short))
                (removed-count 0))
            (maphash (lambda (key warrior)
                       (when (and (equal (getf warrior :symbol) symbol)
                                  (eq (getf warrior :direction) dir-key))
                         (remhash key swimmy.school::*warrior-allocation*)
                         (incf removed-count)
                         (format t "[L] 🧹 WARRIOR CLEANUP: Slot ~a freed (External Close: ~a ~a)~%" key symbol direction)))
                     swimmy.school::*warrior-allocation*)
            (when (> removed-count 0)
              (format t "[L] 🧹 Freed ~d warrior slots total~%" removed-count))))
        
        ;; Record result for danger tracking
        (swimmy.school:record-trade-result (if is-win :win :loss))
        
        ;; V5.1: Increment total trades
        (incf *total-trades*)
        
        ;; V3.0: Track success count
        (when is-win (incf *success-count*))
        
        ;; LEARNING: Record for dreamer analysis
        (let ((dir-keyword (if (search "BUY" (string-upcase direction)) :buy :sell))
              (category (cond 
                          ((jsown:keyp json "category") 
                           (intern (string-upcase (jsown:val json "category")) :keyword))
                          (t :trend)))
              (strategy-name (if (jsown:keyp json "strategy") 
                                (jsown:val json "strategy") 
                                "unknown")))
          (handler-case
              (swimmy.school:record-trade-outcome symbol dir-keyword category strategy-name pnl)
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
                     (price-pos (when (and history (fboundp 'swimmy.school:get-price-position)) (swimmy.school:get-price-position history)))
                     (context (list :regime *market-regime*
                                   :volatility-state *current-volatility-state*
                                   :session (swimmy.school:current-trading-session)
                                   :rsi-value rsi
                                   :price-position price-pos
                                   :symbol symbol
                                   :direction (if (search "BUY" (string-upcase direction)) :buy :sell))))
                (when (fboundp 'swimmy.school:learn-from-failure) (swimmy.school:learn-from-failure context pnl))
                (format t "[L] 📚 長老会議に敗因を報告(6次元)~%"))
            (error (e) (format t "[L] Elder learning error: ~a~%" e))))
        
        ;; LEADER STATS + MEMORY
        (handler-case
            (progn
              (when (fboundp 'swimmy.school:update-leader-stats) (swimmy.school:update-leader-stats pnl))
              (let ((dir-keyword (if (search "BUY" (string-upcase direction)) :buy :sell)))
                (when (fboundp 'swimmy.school:store-memory) (swimmy.school:store-memory symbol dir-keyword (if is-win :win :loss) pnl 0))))
          (error (e) (format t "[L] Leader/Memory error: ~a~%" e)))
        
        ;; Funeral/Victory Ceremony
        (if is-win
            ;; VICTORY
            (let ((msg (format nil "
⚔️ ═══════════════════════════════ ⚔️
  🎉 戦士凱旋！
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📈 ~a | ~a
💰 利益: +¥~,0f

🏆 「勝利は準備の結果である」
⚔️ ═══════════════════════════════ ⚔️" symbol direction pnl)))
              (format t "[L] ~a~%" msg)
              (queue-discord-notification (gethash symbol *symbol-webhooks*) msg :color 3066993))
            
            ;; FUNERAL
            (let ((msg (format nil "
🕯️ ═══════════════════════════════ 🕯️
  ⚰️ 戦士追悼
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📉 ~a | ~a
💸 損失: ¥~,0f

🙏 「敗北もまた師である」
🕯️ ═══════════════════════════════ 🕯️" symbol direction (abs pnl))))
              (format t "[L] ~a~%" msg)
              (queue-discord-notification (gethash symbol *symbol-webhooks*) msg :color 15158332)))
        
        ;; Persist state immediately after trade close
        (when (fboundp 'swimmy.engine:save-state)
          (funcall 'swimmy.engine:save-state)
          (format t "[L] 💾 Global state saved after trade close~%")))
    (error (e) (format t "[L] Trade close error: ~a~%" e))))

(defun process-account-info (json)
  (handler-case
      (let ((equity (if (jsown:keyp json "equity") (jsown:val json "equity") nil))
            (balance (if (jsown:keyp json "balance") (jsown:val json "balance") nil)))
        ;; V8.5: Track last ACCOUNT_INFO time for monitoring
        (setf *last-account-info-time* (get-universal-time))
        ;; V8.5: Recovery notification
        (when *account-info-alert-sent*
          (notify-discord-alert "✅ ACCOUNT_INFO Recovered - MT5同期復旧" :color 3066993)
          (setf *account-info-alert-sent* nil))
        (when (and equity (numberp equity) (> equity 0))
          (setf *current-equity* (float equity))
          (when (> *current-equity* *peak-equity*)
            (setf *peak-equity* *current-equity*))
          ;; Calculate current drawdown
          (when (> *peak-equity* 0)
            (setf *current-drawdown* (* 100 (/ (- *peak-equity* *current-equity*) *peak-equity*))))
          (format t "[L] 💰 MT5 Sync: Equity=¥~,0f Peak=¥~,0f DD=~,1f%~%"
                  *current-equity* *peak-equity* *current-drawdown*)))
    (error (e) (format t "[L] Account sync error: ~a~%" e))))

;;; --------------------------------------
;;; UTILS
;;; --------------------------------------

(defun get-system-metrics ()
  "Return basic system metrics for Cortex consumption"
  (list :ticks *total-ticks-count*
        :equity *current-equity*
        :last-heartbeat *last-heartbeat-sent*))
