;;; src/lisp/core/pattern-similarity-client.lisp
;;; Pattern Similarity Service Client (REQ/REP, S-expression)

(in-package :swimmy.core)

(defparameter *pattern-similarity-endpoint*
  (zmq-connect-endpoint *port-pattern-similarity*)
  "ZMQ endpoint for Pattern Similarity Service.")

(defparameter *pattern-similarity-socket* nil
  "ZMQ REQ socket for Pattern Similarity queries.")

(defparameter *pattern-similarity-context* nil
  "ZMQ context for Pattern Similarity connection.")

(defparameter *pattern-similarity-timeout-ms* 1000
  "Timeout in milliseconds for Pattern Similarity queries.")

(defparameter *pattern-window-bars*
  '(("M5" . 120)
    ("M15" . 120)
    ("H1" . 120)
    ("H4" . 120)
    ("D1" . 120)
    ("W1" . 104)
    ("MN1" . 120)
    ("MN" . 120))
  "Window bars required by Pattern Similarity QUERY.")

(defun pattern-window-bars (timeframe)
  "Get required window size for TIMEFRAME or NIL."
  (cdr (assoc (string-upcase (format nil "~a" timeframe))
              *pattern-window-bars*
              :test #'string=)))

(defun ensure-pattern-similarity-connection ()
  "Ensure ZMQ connection to Pattern Similarity Service is active."
  (unless *pattern-similarity-context*
    (setf *pattern-similarity-context* (pzmq:ctx-new)))
  (unless *pattern-similarity-socket*
    (setf *pattern-similarity-socket* (pzmq:socket *pattern-similarity-context* :req))
    (pzmq:connect *pattern-similarity-socket* *pattern-similarity-endpoint*)
    (pzmq:setsockopt *pattern-similarity-socket* :rcvtimeo *pattern-similarity-timeout-ms*)
    (pzmq:setsockopt *pattern-similarity-socket* :sndtimeo *pattern-similarity-timeout-ms*)
    (format t "[PATTERN-CLIENT] Connected to Pattern Similarity at ~a~%"
            *pattern-similarity-endpoint*)))

(defun close-pattern-similarity-client ()
  "Close Pattern Similarity sockets/context."
  (when *pattern-similarity-socket*
    (ignore-errors (pzmq:close *pattern-similarity-socket*))
    (setf *pattern-similarity-socket* nil))
  (when *pattern-similarity-context*
    (ignore-errors (pzmq:ctx-term *pattern-similarity-context*))
    (setf *pattern-similarity-context* nil)))

(defun %candle->alist (c)
  "Convert candle struct to an alist expected by Pattern Similarity Service."
  `((timestamp . ,(candle-timestamp c))
    (open . ,(float (candle-open c)))
    (high . ,(float (candle-high c)))
    (low . ,(float (candle-low c)))
    (close . ,(float (candle-close c)))
    (volume . ,(truncate (or (candle-volume c) 0)))))

(defun %sort-candles-oldest-first (candles)
  "Return a new list sorted by timestamp ascending."
  (sort (copy-list candles) #'< :key #'candle-timestamp))

(defun %normalize-intended-direction (direction)
  "Normalize DIRECTION into \"BUY\"/\"SELL\" or NIL."
  (cond
    ((or (eq direction :buy) (string= (string-upcase (format nil "~a" direction)) "BUY")) "BUY")
    ((or (eq direction :sell) (string= (string-upcase (format nil "~a" direction)) "SELL")) "SELL")
    (t nil)))

(defun build-pattern-similarity-query-request (symbol timeframe candles &key (k 30) as-of direction)
  "Build QUERY request S-expression string for Pattern Similarity Service."
  (let* ((tf (string-upcase (format nil "~a" timeframe)))
         (intended-direction (%normalize-intended-direction direction))
         (candles-alist (mapcar #'%candle->alist (%sort-candles-oldest-first candles)))
         (payload `((type . "PATTERN_SIMILARITY")
                    (schema_version . 1)
                    (action . "QUERY")
                    (symbol . ,symbol)
                    (timeframe . ,tf)
                    (k . ,k)
                    (candles . ,candles-alist))))
    (when as-of
      (setf payload (append payload `((as_of . ,as-of)))))
    (when intended-direction
      (setf payload (append payload `((intended_direction . ,intended-direction)))))
    (encode-sexp payload)))

(defun pattern-similarity-query (symbol timeframe candles &key (k 30) as-of direction)
  "Send QUERY to Pattern Similarity Service.

Returns parsed S-expression (alist) on success, or NIL on error."
  (ensure-pattern-similarity-connection)
  (handler-case
      (let* ((msg (build-pattern-similarity-query-request
                   symbol timeframe candles :k k :as-of as-of :direction direction)))
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

(defun %history->query-candles (history timeframe)
  "Convert newest-first HISTORY into oldest->newest candle list for TIMEFRAME."
  (let ((need (pattern-window-bars timeframe)))
    (when (and need history (>= (length history) need))
      (reverse (subseq history 0 need)))))

(defun query-pattern-similarity (symbol timeframe history &key (k 30) direction)
  "Query Pattern Similarity service using newest-first HISTORY.

Returns: (values result-alist nil) on success, (values nil reason-string) on skip/error."
  (let ((candles (%history->query-candles history timeframe)))
    (unless candles
      (return-from query-pattern-similarity (values nil "INSUFFICIENT_HISTORY")))
    (let ((resp (pattern-similarity-query symbol timeframe candles :k k :direction direction)))
      (cond
        ((null resp)
         (values nil "GATEWAY_UNREACHABLE"))
        ((string= (sexp-alist-get resp 'status) "ok")
         (values (sexp-alist-get resp 'result) nil))
        (t
         (values nil (or (sexp-alist-get resp 'error) "QUERY_ERROR")))))))

(format t "[PATTERN-CLIENT] Pattern similarity client loaded~%")
