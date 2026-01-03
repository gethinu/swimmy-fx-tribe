(in-package :cl-user)

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
         ;; Brain: PULL<-5555 (bind), PUB->5556 (bind)
         (let ((pull (pzmq:socket ctx :pull)) (pub (pzmq:socket ctx :pub)))
           (pzmq:bind pull "tcp://*:5555")  ; Receive market data from Guardian
           (pzmq:bind pub "tcp://*:5556")    ; Send commands to Guardian
           (setf *cmd-publisher* pub)
           
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
           
           ;; MAIN LOOP
           (loop 
             (handler-case 
                 (process-msg (pzmq:recv-string pull))
               (error (e) 
                 (format t "[L] 🚨 Main Loop Error: ~a~%" e)
                 (sleep 1)))))
      
      (pzmq:ctx-term ctx))))
