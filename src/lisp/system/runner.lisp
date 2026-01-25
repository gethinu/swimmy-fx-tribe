(in-package :swimmy.main)

;;; ==========================================
;;; SWIMMY SYSTEM: RUNNER (runner.lisp)
;;; ==========================================
;;; Contains: Main Loop, Startup Sequence, ZMQ Communication
;;; Extracted from brain-ritual.lisp (Strangler Fig Phase 4)

(defun start-brain ()
  "Main entry point for Swimmy Brain"
  (format t "~%[L] 🦈 Swimmy Ver 41.0 - V2.1 INDEPENDENT CLANS (Refactored)~%")
  (format t "[L] 🏹 Hunters | 🔮 Shamans | ⚔️ Breakers | 🗡️ Raiders~%")
  (format t "[L] 📜 Constitution | 👴 Elders | 🗣️ Tribal Dialect~%")
  
  ;; Initialize clan treasury (if function exists)
  (if (fboundp 'initialize-clan-treasury)
      (initialize-clan-treasury))
  
  ;; V5.1: Setup multi-channel Discord webhooks (if function exists)
  (when (fboundp 'setup-symbol-webhooks)
      (setup-symbol-webhooks))
  
  ;; Morning Ritual
  (if (fboundp 'morning-ritual)
      (morning-ritual)
      (format t "[L] ⚠️ Warning: Morning ritual skipped (function not found)~%"))
  
  ;; Genome loading removed (Dead Code Elimination V41.4)
      
  (format t "[L] 📚 Strategies: ~d knowledge base + ~d evolved~%" 
          (length *strategy-knowledge-base*) (length *evolved-strategies*))
  (format t "[L] ⚙️ Daily Limit: ~d | Max Losses: ~d~%" *daily-loss-limit* *max-streak-losses*)
  
  ;; ZMQ and Main Loop
  (let ((ctx (pzmq:ctx-new)))
    (unwind-protect
         ;; V41.1: Fix ZMQ topology - Brain BINDS, Guardian CONNECTS
         ;; Guardian: PUSH->5555 (connect), SUB<-5556 (connect)  
         ;; Brain: PULL<-5555 (bind), PUB->5556 (bind), PUSH->5580 (connect)
         (let ((pull (pzmq:socket ctx :pull)) 
               (pub (pzmq:socket ctx :pub))
               (bind-success nil))
           
           ;; V7.1: Robust Bind with Retries (Prevents Debugger Hang)
           (loop for i from 1 to 3 while (not bind-success) do
             (handler-case
                 (progn
                   (pzmq:bind pull "tcp://*:5555")
                   (pzmq:bind pub "tcp://*:5556")
                   ;; V44.9: Removed dead port 5580 (bt-req). Backtest now uses pub channel (5556).
                   (setf bind-success t))
               (error (e)
                 (format t "[FATAL] Bind attempt ~d failed: ~a~%" i e)
                 (if (< i 3)
                     (progn 
                       (format t "[RETRY] Waiting 5s for ports to clear...~%")
                       (sleep 5))
                     (progn
                       (format t "[CRITICAL] All bind attempts failed. Exiting to prevent debugger hang.~%")
                       (sb-ext:exit :code 1))))))

           (setf swimmy.globals:*cmd-publisher* pub)
           ;; swimmy.globals:*backtest-requester* remains nil, triggering fallback to *cmd-publisher* in school-backtest.lisp

           ;; V41.7: Set Receive Timeout to 100ms for non-blocking loop
           (pzmq:setsockopt pull :rcvtimeo 100)
           
           (sleep 1)
           
           ;; Close all existing positions on startup (clean slate)
           (format t "[L] 🧹 Closing all existing positions...~%")
           (dolist (sym *supported-symbols*)
             (pzmq:send pub (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" sym) ("close_all" t)))))
           
           (sleep 1)
           
           ;; V9.5: Pre-load from Data Keeper (Expert Panel 2026-01-14)
           ;; This primes the cache with local M1 data before MT5 requests
           (format t "[L] 📦 Pre-loading history from Data Keeper (local cache)...~%")
           (when (fboundp 'swimmy.core:init-data-keeper-client)
             (swimmy.core:init-data-keeper-client))
           (dolist (sym *supported-symbols*)
             (let ((local-data (swimmy.core:get-history-from-keeper sym 100000 "M1")))
               (when (and local-data (> (length local-data) 0))
                 (setf (gethash sym *candle-histories*) local-data)
                 ;; Also populate MTF structure for tick-handler merging
                 (unless (gethash sym *candle-histories-tf*)
                   (setf (gethash sym *candle-histories-tf*) (make-hash-table :test 'equal)))
                 (setf (gethash "M1" (gethash sym *candle-histories-tf*)) local-data)
                 (format t "[L] ✅ Pre-loaded ~a M1: ~d bars from Data Keeper~%" sym (length local-data)))))
           
           ;; V6.10: Request history for EACH symbol individually (EURUSD/GBPUSD fix)
           ;; Now only fills the gap between Data Keeper's latest and NOW
           (format t "[L] 📊 Requesting history for all symbols...~%")
           (let ((tfs '("M1" "M5" "M15" "M30" "H1" "H4" "H12" "D1" "W1")))
             (dolist (sym *supported-symbols*)
               (format t "[L] 📊 Requesting history for ~a...~%" sym)
               (dolist (tf tfs)
                 (format t "[L]    → Requesting ~a ~a history...~%" sym tf)
                 (pzmq:send pub (jsown:to-json (jsown:new-js ("action" "REQ_HISTORY") ("symbol" sym) ("volume" 2000) ("tf" tf))))
                 (sleep 0.2))  ; Stagger requests
               (sleep 0.5)))   ; Stagger symbols
           
           ;; Initial setup
           (if (fboundp 'assemble-team)
               (assemble-team))
           
           (if (fboundp 'request-prediction)
               (request-prediction))
           
           (format t "[BRAIN] 🚀 System active and running...~%")
           
           ;; V50.7: Flush Deferred Backtests (ZMQ-Ready Phase)
           (format t "[SYSTEM] 🚀 ZMQ Ready. Triggering Acceleration Flush...~%")
           (let ((flush-sym (find-symbol "FLUSH-DEFERRED-FOUNDERS" "SWIMMY.SCHOOL")))
             (when (and flush-sym (fboundp flush-sym))
               (funcall flush-sym)))
           
           ;; V44.5: Brain Restart Notification (Expert Panel P3)
           (handler-case
               (notify-discord-alert 
                "🔄 **Brain Restarted** - System is now active and running." 
                :color 3447003)  ; Blue
             (error (e) (format t "[L] ⚠️ Restart notification failed: ~a~%" e)))
           
           ;; MAIN LOOP - V41.7: Non-blocking with Timeout
           ;; V9.7: Gene Kim - Latency Monitoring
           (loop 
             (let ((loop-start-time (get-internal-real-time)))
               (handler-case 
                   (let ((msg (pzmq:recv-string pull)))
                     (process-msg msg))
                 ;; Ignore timeout errors (EAGAIN)
                 (error (e) 
                   (let ((err-str (format nil "~a" e)))
                     (unless (search "Resource temporarily unavailable" err-str)
                        (format t "[L] 🚨 Main Loop Error: ~a~%" e)
                        (sleep 0.1)))))
               
               ;; Periodic Maintenance
               (handler-case
                   (when (fboundp 'run-periodic-maintenance)
                     (run-periodic-maintenance))
                 (error (e) (format t "[L] Maintenance Error: ~a~%" e)))
               
               ;; Heartbeat
               (when (fboundp 'swimmy.engine::check-discord-heartbeat)
                 (swimmy.engine::check-discord-heartbeat))
               
               ;; Latency Check
               (let* ((loop-end-time (get-internal-real-time))
                      (duration (- loop-end-time loop-start-time))
                      (duration-ms (* (/ duration internal-time-units-per-second) 1000.0)))
                 (when (> duration-ms 500) ; Alert if loop takes > 500ms
                   (format t "[LATENCY] 🐢 Slow Tick detected: ~,2f ms~%" duration-ms))))))
      
      (pzmq:ctx-term ctx))))
