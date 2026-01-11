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
               (bt-req (pzmq:socket ctx :push))
               (bind-success nil))
           
           ;; V7.1: Robust Bind with Retries (Prevents Debugger Hang)
           (loop for i from 1 to 3 while (not bind-success) do
             (handler-case
                 (progn
                   (pzmq:bind pull "tcp://*:5555")
                   (pzmq:bind pub "tcp://*:5556")
                   (pzmq:connect bt-req "tcp://localhost:5580")
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

           (setf *cmd-publisher* pub)
           (setf *backtest-requester* bt-req)

           ;; V41.7: Set Receive Timeout to 100ms for non-blocking loop
           (pzmq:setsockopt pull :rcvtimeo 100)
           
           (sleep 1)
           
           ;; Close all existing positions on startup (clean slate)
           (format t "[L] 🧹 Closing all existing positions...~%")
           (dolist (sym *supported-symbols*)
             (pzmq:send pub (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" sym) ("close_all" t)))))
           
           (sleep 1)
           
           ;; V6.10: Request history for EACH symbol individually (EURUSD/GBPUSD fix)
           (format t "[L] 📊 Requesting history for all symbols...~%")
           (dolist (sym *supported-symbols*)
             (format t "[L]    → Requesting ~a history...~%" sym)
             (pzmq:send pub (jsown:to-json (jsown:new-js ("action" "REQ_HISTORY") ("symbol" sym) ("volume" 0))))
             (sleep 0.5))  ; Stagger requests to avoid overwhelming MT5
           
           ;; Initial setup
           (if (fboundp 'assemble-team)
               (assemble-team))
           
           (if (fboundp 'request-prediction)
               (request-prediction))
           
           (format t "[BRAIN] 🚀 System active and running...~%")
           
           ;; Connectivity Verification - REMOVED: Already sent from main.lisp via notify-apex
           ;; (notify-discord is deprecated for system notifications)
           
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
