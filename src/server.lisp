(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :pzmq :silent t)
  (ql:quickload :jsown :silent t))

(defpackage :swimmy.server
  (:use :cl)
  (:import-from :swimmy.constitution :judge-action))

(in-package :swimmy.server)

;;; ============================================================
;;; THE BRAIN - CORE NERVOUS SYSTEM
;;; ============================================================

(defun start-brain ()
  (format t "~%🧠 Swimmy Brain V7.0 (The Dual Monarchy) is waking up...~%")
  (format t "   Nervous System Initializing...~%")
  (force-output)
  
  (let ((ctx (pzmq:ctx-new)))
    (unwind-protect
         (let ((afferent-nerve (pzmq:socket ctx :pull))   ; PULL: Sensory Input
               (efferent-nerve (pzmq:socket ctx :pub)))   ; PUB: Motor Output (Commands)
           (unwind-protect
                (progn
                  ;; 1. Afferent Nerve (Sensory Input) - Port 5555
                  ;; Receives market data and trade proposals from Rust Body
                  (pzmq:bind afferent-nerve "tcp://*:5555")
                  (format t "   [SENSORY] Listening on tcp://*:5555 (PULL)~%")
                  
                  ;; 2. Efferent Nerve (Motor Output) - Port 5556
                  ;; Broadcasts Constitutional Verdicts and Commands to Rust Body
                  (pzmq:bind efferent-nerve "tcp://*:5556")
                  (format t "   [MOTOR]   Broadcasting on tcp://*:5556 (PUB)~%")
                  
                  (loop
                    (format t "Thinking... (Waiting for input)~%")
                    (force-output)
                    
                    ;; Blocking receive
                    (let ((raw-msg (pzmq:recv-string afferent-nerve)))
                      (handler-case
                          (let* ((data (jsown:parse raw-msg))
                                 (msg-type (if (jsown:keyp data "type") (jsown:val data "type") "unknown")))
                            
                            (cond
                              ;; CASE 1: PROPOSAL (Trade Request)
                              ((string= msg-type "proposal")
                               (let ((agent (if (jsown:keyp data "agent") (jsown:val data "agent") "unknown")))
                                 (format t "📝 Constitution Check: Proposal from ~A... " agent)
                                 
                                 ;; JUDGE ACTION via Constitution
                                 (multiple-value-bind (verdict reason)
                                     (judge-action data)
                                   
                                   (format t "[~A]~%" verdict)
                                   (when (string= verdict "REJECTED")
                                     (format t "   Reason: ~A~%" reason))
                                   
                                   ;; SEND VERDICT
                                   (let ((response (jsown:to-json
                                                    (jsown:new-js
                                                      ("type" "verdict")
                                                      ("verdict" verdict)
                                                      ("reason" reason)
                                                      ("reference_id" (if (jsown:keyp data "id") (jsown:val data "id") "0"))))))
                                     (pzmq:send-string efferent-nerve response)
                                     (format t "   -> Sent Verdict via Motor Nerve~%")
                                     (force-output)))))
                              
                              ;; CASE 2: MARKET DATA (Update State)
                              ((string= msg-type "market_data")
                               ;; Update internal state (Equity etc.)
                               ;; For now just acknowledge
                               (format t "👁️ Market Data Received~%"))
                              
                              (t
                               (format t "❓ Unknown signal type: ~A~%" msg-type))))
                        
                        (error (e)
                          (format t "!! BRAIN SEIZURE (Error): ~A~%" e)
                          (force-output))))))
             
             ;; Cleanup
             (pzmq:close afferent-nerve)
             (pzmq:close efferent-nerve)))
      (pzmq:ctx-destroy ctx))))
