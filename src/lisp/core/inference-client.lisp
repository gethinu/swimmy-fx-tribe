(in-package :swimmy.core)

;; core/inference-client.lisp - Client for Inference Worker Service
;; V8.0: Phase 4 LLM Service
;; Communicates with tools/inference_worker.py via ZMQ REQ

(defparameter *inference-endpoint* "tcp://localhost:5564")
(defparameter *inference-socket* nil)
(defparameter *inference-context* nil)

(defun ensure-inference-connection ()
  "Ensure ZMQ connection to Inference Worker is active"
  (unless *inference-context*
    (setf *inference-context* (pzmq:ctx-new)))
  (unless *inference-socket*
    (setf *inference-socket* (pzmq:socket *inference-context* :req))
    (pzmq:connect *inference-socket* *inference-endpoint*)
    ;; Set timeout to 40 seconds (Gemini can be slow)
    (pzmq:setsockopt *inference-socket* :rcvtimeo 40000)
    (pzmq:setsockopt *inference-socket* :sndtimeo 5000)
    (format t "[AI-CLIENT] Connected to Inference Worker at ~a~%" *inference-endpoint*)))

(defun ask-ai (prompt &key (temperature 0.5) (max-tokens 512))
  "Send prompt to Inference Worker. Returns text response or NIL on error."
  (ensure-inference-connection)
  
  (handler-case
      (let* ((key (if (boundp 'cl-user::*gemini-api-key*) 
                      (symbol-value 'cl-user::*gemini-api-key*) 
                      nil))
             (payload (jsown:new-js 
                        ("prompt" prompt)
                        ("temperature" temperature)
                        ("max_tokens" max-tokens)
                        ("api_key" key)))
             (msg-str (jsown:to-json payload)))
        
        ;; Send request
        (pzmq:send *inference-socket* msg-str)
        
        ;; Receive response
        (let* ((resp (pzmq:recv-string *inference-socket*))
               (json (jsown:parse resp))
               (status (jsown:val json "status")))
          
          (if (string= status "OK")
              (jsown:val json "text")
              (progn
                (format t "[AI-CLIENT] Error from worker: ~a~%" (jsown:val json "error"))
                nil))))
    
    (error (e)
      (format t "[AI-CLIENT] ZMQ Error: ~a~%" e)
      nil)))

(format t "[AI-CLIENT] Inference client loaded (Phase 4)~%")
