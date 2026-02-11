;;; src/lisp/core/pattern-similarity-client.lisp
;;; Pattern Similarity Service Client (Phase 1)
;;; ============================================

(in-package :swimmy.core)

(defparameter *pattern-similarity-address* (zmq-connect-endpoint *port-pattern-similarity*)
  "ZMQ address of Pattern Similarity service")

(defparameter *pattern-similarity-socket* nil
  "ZMQ REQ socket for Pattern Similarity queries")

(defparameter *pattern-similarity-context* nil
  "ZMQ context for Pattern Similarity connection")

(defparameter *pattern-similarity-timeout* 1000
  "Timeout in milliseconds for Pattern Similarity queries")

(defparameter *pattern-window-bars*
  '(("M5" . 120)
    ("M15" . 120)
    ("H1" . 120)
    ("H4" . 120)
    ("D1" . 120)
    ("W1" . 104)
    ("MN1" . 120))
  "Window bars required by Pattern Similarity QUERY.")

(defun pattern-window-bars (timeframe)
  "Get required window size for TIMEFRAME or NIL."
  (cdr (assoc (string-upcase (format nil "~a" timeframe)) *pattern-window-bars* :test #'string=)))

(defun ensure-pattern-similarity-connection ()
  "Ensure ZMQ connection to Pattern Similarity service is active."
  (unless *pattern-similarity-context*
    (setf *pattern-similarity-context* (pzmq:ctx-new)))
  (unless *pattern-similarity-socket*
    (setf *pattern-similarity-socket* (pzmq:socket *pattern-similarity-context* :req))
    (pzmq:connect *pattern-similarity-socket* *pattern-similarity-address*)
    (pzmq:setsockopt *pattern-similarity-socket* :rcvtimeo *pattern-similarity-timeout*)
    (pzmq:setsockopt *pattern-similarity-socket* :sndtimeo *pattern-similarity-timeout*)
    (format t "[PATTERN-CLIENT] Connected to Pattern Similarity at ~a~%" *pattern-similarity-address*)))

(defun close-pattern-similarity-client ()
  "Close Pattern Similarity sockets/context."
  (when *pattern-similarity-socket*
    (pzmq:close *pattern-similarity-socket*)
    (setf *pattern-similarity-socket* nil))
  (when *pattern-similarity-context*
    (pzmq:ctx-term *pattern-similarity-context*)
    (setf *pattern-similarity-context* nil)))

(defun %pattern-candle->alist (c)
  "Convert candle struct to Pattern Similarity candle payload."
  `((timestamp . ,(candle-timestamp c))
    (open . ,(float (candle-open c)))
    (high . ,(float (candle-high c)))
    (low . ,(float (candle-low c)))
    (close . ,(float (candle-close c)))
    (volume . ,(round (candle-volume c)))))

(defun %pattern-query-candles (history timeframe)
  "Convert newest-first HISTORY into oldest->newest payload candles for TIMEFRAME."
  (let ((need (pattern-window-bars timeframe)))
    (when (and need history (>= (length history) need))
      (mapcar #'%pattern-candle->alist
              (reverse (subseq history 0 need))))))

(defun query-pattern-similarity (symbol timeframe history &key (k 30))
  "Query Pattern Similarity service.
Returns: (values result-alist nil) on success, (values nil reason-string) on skip/error."
  (let ((candles (%pattern-query-candles history timeframe)))
    (unless candles
      (return-from query-pattern-similarity (values nil "INSUFFICIENT_HISTORY")))

    (handler-case
        (progn
          (ensure-pattern-similarity-connection)
          (let* ((payload `((type . "PATTERN_SIMILARITY")
                            (schema_version . 1)
                            (action . "QUERY")
                            (symbol . ,symbol)
                            (timeframe . ,(string-upcase timeframe))
                            (k . ,k)
                            (candles . ,candles)))
                 (cmd (encode-sexp payload)))
            (pzmq:send *pattern-similarity-socket* cmd)
            (let* ((msg (pzmq:recv-string *pattern-similarity-socket*))
                   (sexp (safe-read-sexp msg :package :swimmy.core))
                   (status (and sexp (sexp-alist-get sexp 'status))))
              (cond
                ((and status (string= status "ok"))
                 (values (sexp-alist-get sexp 'result) nil))
                (t
                 (values nil (or (and sexp (sexp-alist-get sexp 'error)) "QUERY_ERROR")))))))
      (error (e)
        ;; Force reconnect on next request after transport errors.
        (setf *pattern-similarity-socket* nil)
        (values nil (format nil "GATEWAY_UNREACHABLE: ~a" e))))))

(format t "[PATTERN-CLIENT] Pattern Similarity client loaded~%")
