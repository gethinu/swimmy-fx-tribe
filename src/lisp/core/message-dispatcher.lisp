(in-package :swimmy.main)

;;; ==========================================
;;; MESSAGE DISPATCHER - Extracted from tick-handler.lisp (SRP Refactor)
;;; ==========================================
;;; Handles:
;;; - ZeroMQ Message Parsing
;;; - Dispatch to Tick, Heartbeat, Account, etc.
;;; - System Commands (REPORT, RELOAD, RESET)

(defun internal-process-msg (msg)
  (handler-case
      (let* ((json (jsown:parse msg)) (type (jsown:val json "type")))
        (cond
          ((string= type swimmy.core:+MSG-TICK+) 
           (update-candle (jsown:val json "bid") (jsown:val json "symbol"))
           ;; V41.2: Throttled operations for performance
           ;; Only save status every 60 seconds (already implemented in save-live-status)
           (when (fboundp 'save-live-status) (save-live-status))
           ;; Only report every hour (already implemented in send-periodic-status-report)
           (when (fboundp 'send-periodic-status-report)
             (send-periodic-status-report (jsown:val json "symbol") (jsown:val json "bid")))
           ;; Learning step is already throttled by cycle count
           (handler-case (when (fboundp 'continuous-learning-step) (continuous-learning-step)) (error () nil)))
          ;; V5.0: Guardian Heartbeat
          ((string= type swimmy.core:+MSG-HEARTBEAT+)
           (unless (numberp *last-guardian-heartbeat*) (setf *last-guardian-heartbeat* 0))
           (setf *last-guardian-heartbeat* (get-universal-time)))
           
          ;; V41.6: MT5 Account Sync - Update equity from MT5
           ((string= type swimmy.core:+MSG-ACCOUNT-INFO+)
            (swimmy.executor:process-account-info json))
            
          ((string= type swimmy.core:+MSG-HISTORY+)
           ;; V15.8: Throttle HISTORY processing to prevent tick blocking
           ;; Only process HISTORY once per 60 seconds per symbol
           (let* ((symbol (if (jsown:keyp json "symbol") (jsown:val json "symbol") "USDJPY"))
                  (tf (if (jsown:keyp json "tf") (jsown:val json "tf") "M1"))
                  (cache-key (format nil "~a-~a" symbol tf))
                  (now (get-universal-time))
                  (last-process (gethash cache-key *history-process-cache* 0)))
             (when (> (- now last-process) 60) ; Only process every 60 seconds
               (setf (gethash cache-key *history-process-cache*) now)
               
               ;; Self-healing: Ensure *candle-histories* is initialized
               (unless (and (boundp '*candle-histories*) *candle-histories*)
                 (setf *candle-histories* (make-hash-table :test 'equal))
                 (format t "[L] 🩹 Patched NIL *candle-histories*~%"))
               
               ;; Initialize MTF storage if needed
               (unless (and (boundp '*candle-histories-tf*) *candle-histories-tf*)
                 (setf *candle-histories-tf* (make-hash-table :test 'equal))
                 (format t "[L] 🩹 Patched NIL *candle-histories-tf*~%"))
             
               (let ((bars nil))
                  
                 (dolist (b (jsown:val json "data"))
                  (let ((c (jsown:val b "c")))
                    (push (make-candle :timestamp (jsown:val b "t") 
                                       :open c :high c :low c :close c :volume 1) bars)))
                                    
                 ;; Store in MTF structure (Merge Strategy)
                 (unless (gethash symbol *candle-histories-tf*)
                   (setf (gethash symbol *candle-histories-tf*) (make-hash-table :test 'equal)))
                 
                 (let ((existing (gethash tf (gethash symbol *candle-histories-tf*))))
                   (if existing
                       (setf bars (sort (remove-duplicates (append bars existing)
                                                           :key #'candle-timestamp
                                                           :test #'=)
                                        #'> :key #'candle-timestamp))
                       (setf bars (sort bars #'> :key #'candle-timestamp)))
                   ;; V15.7: Limit to 50k bars to prevent slow merge operations
                   (when (> (length bars) 50000)
                     (setf bars (subseq bars 0 50000))))
                   
                 (setf (gethash tf (gethash symbol *candle-histories-tf*)) bars)
                 (format t "[L] 📚 Loaded ~a ~a data: ~d bars (Merged, capped at 50k)~%" symbol tf (length bars))

                 ;; If M1, also store in legacy/default locations
                 (when (or (string= tf "M1") (string= tf "Default"))
                     (setf (gethash symbol *candle-histories*) bars)
                     (setf *candle-history* bars)  ; Legacy compat - use first symbol
                  
                     ;; V11.0: Data Gap Check
                     (let ((has-gap (swimmy.core:check-data-gap symbol bars)))
                       (when has-gap
                         (format t "[L] ⚠️ Data Gap detected in ~a, but proceeding with Startup Check (Taleb's Resilience)...~%" symbol))
                           
                       (when (and (fboundp 'swimmy.school:batch-backtest-knowledge)
                                  (not (and (boundp '*initial-backtest-done*) *initial-backtest-done*))
                                  (> (length bars) 100))
                         (format t "[L] 🧪 Starting Batch Backtest Verification...~%")
                         (swimmy.school:batch-backtest-knowledge)
                         (setf *initial-backtest-done* t)))))))) ; Close let bars
                         
          ((string= type "SYSTEM_COMMAND")
           (let ((action (jsown:val json "action")))
             (cond
               ((string= action "REPORT_STATUS")
                (format t "[L] 🏰 REPORT_STATUS command received.~%")
                (swimmy.school:report-active-positions))

               ((string= action "DAILY_REPORT")
                (format t "[L] 📊 DAILY_REPORT command received.~%")
                (send-daily-tribal-narrative))

               ((string= action "BACKTEST_SUMMARY")
                (format t "[L] 📊 BACKTEST_SUMMARY command received.~%")
                (notify-backtest-summary))

               ((string= action "RELOAD_CONFIG")
                (format t "[L] 🔄 Hot Reloading Configuration...~%")
                (handler-case
                    (progn
                      (load #P"/home/swimmy/swimmy/src/lisp/core/globals.lisp")
                      (load #P"/home/swimmy/swimmy/src/lisp/core/config.lisp")
                      (format t "[L] ✅ Configuration Reloaded!~%"))
                  (error (e) (format t "[L] ❌ Reload Failed: ~a~%" e))))
                  
               ((string= action "RESET_WARRIORS")
                (format t "[L] 🧹 RESET_WARRIORS command received. Clearing warrior allocation...~%")
                (handler-case
                    (progn
                      (when (fboundp 'swimmy.school:debug-reset-warriors)
                        (funcall 'swimmy.school:debug-reset-warriors))
                       (format t "[L] ✅ Warriors Reset Complete!~%"))
                  (error (e) (format t "[L] ❌ Reset Warriors Failed: ~a~%" e))))
                  
               ((string= action "DEBUG_ENTRY")
                (let ((target-symbol (if (jsown:keyp json "symbol") (jsown:val json "symbol") "USDJPY")))
                  (format t "[L] 🧪 DEBUG_ENTRY command received for ~a. Triggering test entry...~%" target-symbol)
                  (handler-case
                      (let ((symbol target-symbol)
                            (magic 999999)
                            (lot 0.01))
                        (swimmy.engine:safe-order "BUY" symbol lot 130.00 160.00 magic)
                        (swimmy.shell:notify-discord-symbol symbol 
                          (format nil "🧪 **DEBUG TEST ENTRY** (🕐 ~a)~%Strategy: MANUAL_TEST~%Action: BUY ~a~%Lot: ~,2f~%Magic: ~d" 
                                  (swimmy.core:get-jst-timestamp)
                                  symbol lot magic)
                          :color 3066993)
                        (format t "[L] ✅ DEBUG ENTRY (~a) triggered! Check Discord.~%" symbol))
                      (error (e) (format t "[L] ❌ DEBUG ENTRY Failed: ~a~%" e)))))
                      
               ((string= action "DEBUG_FORCE_EVAL")
                (let ((target (if (jsown:keyp json "target") (jsown:val json "target") "ALL")))
                  (format t "[L] 🧪 FORCE EVAL: Clearing execution history for ~a~%" target)
                  (if (string= target "ALL")
                      (clrhash swimmy.school::*processed-candle-time*)
                      (remhash target swimmy.school::*processed-candle-time*))
                  (format t "[L] ✅ Cleared. Next tick will trigger re-evaluation.~%")))
                  
               ((string= action "RESET_RISK")
                (format t "[L] 🚨 RESET_RISK command received. Resetting drawdown and equity...~%")
                (handler-case
                    (progn
                      (setf swimmy.globals::*max-drawdown* 0.0)
                      (setf swimmy.globals::*daily-pnl* 0.0)
                      (setf swimmy.globals::*consecutive-losses* 0)
                      (when (boundp 'swimmy.globals::*current-equity*)
                        (setf swimmy.globals::*peak-equity* swimmy.globals::*current-equity*))
                      (format t "[L] ✅ Risk State Reset! DD=0, PnL=0. Peak adjusted.~%"))
                  (error (e) (format t "[L] ❌ Reset Risk Failed: ~a~%" e))))
               (t (format t "[L] ⚠️ Unknown System Command: ~a~%" action)))))
               
          ((string= type "BACKTEST_RESULT")
           (let* ((result (jsown:val json "result"))
                  (name (jsown:val result "strategy_name"))
                  (sharpe (float (or (handler-case (jsown:val result "sharpe") (error () 0.0)) 0.0)))
                  (trades (or (handler-case (jsown:val result "trades") (error () 0)) 0))
                  (pnl (float (or (handler-case (jsown:val result "pnl") (error () 0.0)) 0.0)))
                  (win-rate (float (or (handler-case (jsown:val result "win_rate") (error () 0.0)) 0.0)))
                  (profit-factor (float (or (handler-case (jsown:val result "profit_factor") (error () 0.0)) 0.0))))
              
             (swimmy.school:cache-backtest-result name 
               (list :sharpe sharpe :trades trades :pnl pnl :win-rate win-rate :profit-factor profit-factor))
               
             (when (fboundp 'swimmy.school:process-wfv-result)
               (swimmy.school:process-wfv-result name 
                  (list :sharpe sharpe :trades trades :pnl pnl :win-rate win-rate :profit-factor profit-factor)))))

          ((string= type "CLONE_CHECK_RESULT")
           (let* ((name (jsown:val json "candidate"))
                  (is-clone (jsown:val json "is_clone"))
                  (similar (jsown:val json "similar_to"))
                  (sim-score (jsown:val json "similarity")))
             (when (fboundp 'swimmy.school:process-clone-check-result)
               (swimmy.school:process-clone-check-result name is-clone similar sim-score))))

          (t nil))) ;; Ignore unknown
    (error (e)
      (format t "[L] ⚠️ Error processing message: ~a~%" e)
      (format t "[L] Raw Msg: ~a~%" msg)
      nil)))

(defun process-msg (msg)
  "Main message dispatcher - Delegates to internal robust processor"
  (internal-process-msg msg))
