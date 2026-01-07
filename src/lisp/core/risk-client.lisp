(in-package :swimmy.core)

;; core/risk-client.lisp - Client for Risk Gateway Service
;; V8.0: Phase 3 Risk Authority
;; Communicates with tools/risk_gateway.py via ZMQ REQ

(defparameter *risk-gateway-endpoint* "tcp://localhost:5563")
(defparameter *risk-gateway-socket* nil)
(defparameter *risk-gateway-context* nil)

(defun ensure-risk-gateway-connection ()
  "Ensure ZMQ connection to Risk Gateway is active"
  (unless *risk-gateway-context*
    (setf *risk-gateway-context* (pzmq:ctx-new)))
  (unless *risk-gateway-socket*
    (setf *risk-gateway-socket* (pzmq:socket *risk-gateway-context* :req))
    (pzmq:connect *risk-gateway-socket* *risk-gateway-endpoint*)
    ;; Set timeout to prevent hanging if gateway is down (1000ms)
    (pzmq:setsockopt *risk-gateway-socket* :rcvtimeo 1000)
    (pzmq:setsockopt *risk-gateway-socket* :sndtimeo 1000)
    (format t "[RISK-CLIENT] Connected to Risk Gateway at ~a~%" *risk-gateway-endpoint*)))

(defun request-trade-approval (action symbol lot daily-pnl equity consecutive-losses)
  "Ask Risk Gateway for permission to trade.
   Returns: (values approved-p reason)"
  (ensure-risk-gateway-connection)
  
  (handler-case
      (let* ((payload (jsown:new-js 
                        ("action" (string action))
                        ("symbol" symbol)
                        ("lot" lot)
                        ("daily_pnl" daily-pnl)
                        ("equity" equity)
                        ("consecutive_losses" consecutive-losses)))
             (cmd (format nil "CHECK_RISK:~a" (jsown:to-json payload))))
        
        ;; Send request
        (pzmq:send *risk-gateway-socket* cmd)
        
        ;; Receive response
        (let* ((msg (pzmq:recv-string *risk-gateway-socket*))
               (json (jsown:parse msg))
               (status (jsown:val json "status"))
               (reason (jsown:val json "reason")))
          
          (cond
            ((string= status "APPROVED")
             (values t reason))
            ((string= status "DENIED")
             (values nil reason))
            (t
             (values nil (format nil "UNKNOWN_STATUS: ~a" status))))))
    
    (error (e)
      ;; Fail safe: If Gateway is down, DENY trade.
      (format t "[RISK-CLIENT] Gateway Error: ~a~%" e)
      (values nil "GATEWAY_UNREACHABLE"))))

(format t "[RISK-CLIENT] Risk Gateway client loaded (Phase 3)~%")
