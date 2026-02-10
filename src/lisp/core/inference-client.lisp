(in-package :swimmy.core)

;; core/inference-client.lisp - Client for Inference Worker Service
;; V8.0: Phase 4 LLM Service
;; Communicates with tools/inference_worker.py via ZMQ REQ (S-expression)

(defparameter *inference-endpoint* (zmq-connect-endpoint *port-inference*)
  "ZMQ endpoint for Inference Worker Service.")
(defparameter *inference-socket* nil)
(defparameter *inference-context* nil)
(defparameter *inference-timeout-ms* 40000) ; Gemini can be slow

(defun ensure-inference-connection ()
  "Ensure ZMQ connection to Inference Worker is active"
  (unless *inference-context*
    (setf *inference-context* (pzmq:ctx-new)))
  (unless *inference-socket*
    (setf *inference-socket* (pzmq:socket *inference-context* :req))
    (pzmq:connect *inference-socket* *inference-endpoint*)
    (pzmq:setsockopt *inference-socket* :rcvtimeo *inference-timeout-ms*)
    (pzmq:setsockopt *inference-socket* :sndtimeo 5000)
    (format t "[AI-CLIENT] Connected to Inference Worker at ~a~%" *inference-endpoint*)))

(defun ask-ai (prompt &key (temperature 0.5) (max-tokens 512))
  "Send prompt to Inference Worker. Returns text response or NIL on error."
  (ensure-inference-connection)
  
  (handler-case
      (let* ((payload `((type . "INFERENCE")
                        (schema_version . 1)
                        (action . "ASK")
                        (prompt . ,prompt)
                        (temperature . ,temperature)
                        (max_tokens . ,max-tokens)))
             (msg-str (encode-sexp payload)))
        
        ;; Send request
        (pzmq:send *inference-socket* msg-str)
        
        ;; Receive response
        (let* ((resp (pzmq:recv-string *inference-socket*))
               (alist (safe-read-sexp resp :package :swimmy.core))
               (status (and alist (sexp-alist-get alist 'status))))
          (if (and status (string= status "ok"))
              (sexp-alist-get alist 'text)
              (progn
                (format t "[AI-CLIENT] Error from worker: ~a~%"
                        (and alist (sexp-alist-get alist 'error)))
                nil))))
    
    (error (e)
      (format t "[AI-CLIENT] ZMQ Error: ~a~%" e)
      nil)))

(format t "[AI-CLIENT] Inference client loaded (Phase 4)~%")
