;;; src/lisp/core/pattern-similarity-client.lisp
;;; Pattern Similarity Service Client (REQ/REP, S-expression)

(in-package :swimmy.core)

(defparameter *pattern-similarity-endpoint* (zmq-connect-endpoint *port-pattern-similarity*)
  "ZMQ endpoint for Pattern Similarity Service.")

(defparameter *pattern-similarity-socket* nil)
(defparameter *pattern-similarity-context* nil)
(defparameter *pattern-similarity-timeout-ms* 1000)

(defun ensure-pattern-similarity-connection ()
  "Ensure ZMQ connection to Pattern Similarity Service is active."
  (unless *pattern-similarity-context*
    (setf *pattern-similarity-context* (pzmq:ctx-new)))
  (unless *pattern-similarity-socket*
    (setf *pattern-similarity-socket* (pzmq:socket *pattern-similarity-context* :req))
    (pzmq:connect *pattern-similarity-socket* *pattern-similarity-endpoint*)
    (pzmq:setsockopt *pattern-similarity-socket* :rcvtimeo *pattern-similarity-timeout-ms*)
    (pzmq:setsockopt *pattern-similarity-socket* :sndtimeo *pattern-similarity-timeout-ms*)
    (format t "[PATTERN-CLIENT] Connected to Pattern Similarity at ~a~%" *pattern-similarity-endpoint*)))

(defun %candle->alist (c)
  "Convert candle struct to an alist expected by Pattern Similarity Service."
  `((timestamp . ,(candle-timestamp c))
    (open . ,(float (candle-open c)))
    (high . ,(float (candle-high c)))
    (low . ,(float (candle-low c)))
    (close . ,(float (candle-close c)))
    (volume . ,(truncate (or (candle-volume c) 0)))))

(defun build-pattern-similarity-query-request (symbol timeframe candles &key (k 30) as-of)
  "Build QUERY request S-expression string for Pattern Similarity Service."
  (let* ((candles-alist (mapcar #'%candle->alist candles))
         (payload `((type . "PATTERN_SIMILARITY")
                    (schema_version . 1)
                    (action . "QUERY")
                    (symbol . ,symbol)
                    (timeframe . ,timeframe)
                    (k . ,k)
                    (candles . ,candles-alist))))
    (when as-of
      (setf payload (append payload `((as_of . ,as-of)))))
    (encode-sexp payload)))

(defun pattern-similarity-query (symbol timeframe candles &key (k 30) as-of)
  "Send QUERY to Pattern Similarity Service.

Returns parsed S-expression (alist) on success, or NIL on error."
  (ensure-pattern-similarity-connection)
  (handler-case
      (let* ((msg (build-pattern-similarity-query-request symbol timeframe candles :k k :as-of as-of)))
        (pzmq:send *pattern-similarity-socket* msg)
        (let ((resp (pzmq:recv-string *pattern-similarity-socket*)))
          (safe-read-sexp resp :package :swimmy.core)))
    (error (e)
      (format t "[PATTERN-CLIENT] Error: ~a~%" e)
      ;; Force reconnect next time (REQ socket can desync after timeout).
      (when *pattern-similarity-socket*
        (ignore-errors (pzmq:close *pattern-similarity-socket*)))
      (setf *pattern-similarity-socket* nil)
      nil)))

(format t "[PATTERN-CLIENT] Pattern similarity client loaded~%")

