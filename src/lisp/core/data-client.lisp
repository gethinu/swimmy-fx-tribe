;;; src/lisp/core/data-client.lisp
;;; Data Keeper Client (Phase 7: System Separation)
;;; ================================================
;;; Expert Panel Approved (2026-01-07)
;;;
;;; Purpose:
;;;   Query the external Data Keeper service for historical candle data.
;;;   Falls back to local *candle-history* if Data Keeper is unavailable.
;;;
;;; Usage:
;;;   (get-history-from-keeper "USDJPY" 1000)  ; Get 1000 M1 candles
;;;   (add-candle-to-keeper "USDJPY" candle)   ; Push new candle
;;;   (data-keeper-status)                      ; Check service status

(in-package :swimmy.core)

(defparameter *data-keeper-address* (zmq-connect-endpoint *port-data-keeper*)
  "ZMQ address of the Data Keeper service")

(defparameter *data-keeper-socket* nil
  "ZMQ REQ socket for Data Keeper queries")

(defparameter *data-keeper-context* nil
  "ZMQ context for Data Keeper connection")

(defparameter *data-keeper-timeout* 60000
  "Timeout in milliseconds for Data Keeper queries")

(defparameter *data-keeper-available* nil
  "Flag indicating if Data Keeper is reachable")

(defun %alist-val (alist keys &optional default)
  "Return first matching value from ALIST by key list."
  (or (loop for k in keys
            for cell = (assoc k alist)
            when cell do (return (cdr cell)))
      default))

(defun %sexp-candle->struct (entry)
  "Convert DATA_KEEPER candle alist to candle struct."
  (when (listp entry)
    (let* ((ts (%alist-val entry '(timestamp t) nil))
           (open (%alist-val entry '(open o) nil))
           (high (%alist-val entry '(high h) nil))
           (low (%alist-val entry '(low l) nil))
           (close (%alist-val entry '(close c) nil))
           (vol (%alist-val entry '(volume v) 0)))
      (when ts
        (make-candle :timestamp ts
                     :open (if open (float open) 0.0)
                     :high (if high (float high) 0.0)
                     :low (if low (float low) 0.0)
                     :close (if close (float close) 0.0)
                     :volume (if vol (float vol) 0.0))))))

(defun build-data-keeper-request (action &key symbol timeframe count candle)
  "Build S-expression request string for Data Keeper."
  (let ((alist `((type . "DATA_KEEPER")
                 (schema_version . 1)
                 (action . ,action))))
    (when symbol
      (setf alist (append alist `((symbol . ,symbol)))))
    (when timeframe
      (setf alist (append alist `((timeframe . ,timeframe)))))
    (when count
      (setf alist (append alist `((count . ,count)))))
    (when candle
      (setf alist (append alist `((candle . ,candle)))))
    (encode-sexp alist)))

(defun init-data-keeper-client ()
  "Initialize connection to Data Keeper service."
  (handler-case
      (progn
        (when *data-keeper-socket*
          (pzmq:close *data-keeper-socket*)
          (setf *data-keeper-socket* nil))
        (unless *data-keeper-context*
          (setf *data-keeper-context* (pzmq:ctx-new)))
        (setf *data-keeper-socket* (pzmq:socket *data-keeper-context* :req))
        (pzmq:setsockopt *data-keeper-socket* :rcvtimeo *data-keeper-timeout*)
        (pzmq:setsockopt *data-keeper-socket* :sndtimeo *data-keeper-timeout*)
        (pzmq:connect *data-keeper-socket* *data-keeper-address*)
        (format t "[DATA-CLIENT] Connected to Data Keeper at ~a~%" *data-keeper-address*)
        ;; Test connection
        (let* ((status (data-keeper-status))
               (status-val (and status (sexp-alist-get status 'status))))
          (if (and status (string= status-val "running"))
              (progn
                (setf *data-keeper-available* t)
                (format t "[DATA-CLIENT] Data Keeper is ONLINE. Symbols: ~a~%"
                        (or (sexp-alist-get status 'symbols) "")))
              (progn
                (setf *data-keeper-available* nil)
                (format t "[DATA-CLIENT] Data Keeper is OFFLINE. Using local history.~%")))))
    (error (e)
      (format t "[DATA-CLIENT] Failed to connect to Data Keeper: ~a~%" e)
      (setf *data-keeper-available* nil))))

(defun data-keeper-query (payload)
  "Send a request to Data Keeper and return the S-expression response."
  (when *data-keeper-socket*
    (handler-case
        (progn
          (pzmq:send *data-keeper-socket* payload)
          (let ((response (pzmq:recv-string *data-keeper-socket*)))
            (safe-read-sexp response :package :swimmy.core)))
      (error (e)
        (format t "[DATA-CLIENT] Query failed: ~a~%" e)
        nil))))

(defun data-keeper-status ()
  "Get Data Keeper service status."
  (data-keeper-query (build-data-keeper-request "STATUS")))

(defun get-history-from-keeper (symbol count &optional (timeframe "M1"))
  "Get historical candles from Data Keeper.
   Returns list of candle structs, newest first (matching *candle-history* format).
   Falls back to local history if Data Keeper is unavailable.
   If TIMEFRAME is not M1, stores/retrieves from *candle-histories-tf*."
  (if *data-keeper-available*
      ;; Determine query format based on timeframe
      (let* ((query (build-data-keeper-request "GET_HISTORY"
                                              :symbol symbol
                                              :timeframe timeframe
                                              :count count))
             (response (data-keeper-query query))
             (status (and response (sexp-alist-get response 'status))))
        (if (and response (string= status "ok"))
            (let ((candles (sexp-alist-get response 'candles)))
              (remove nil (mapcar #'%sexp-candle->struct (or candles '()))))
            ;; Fallback to local
            (progn
              (if (string= timeframe "M1")
                  (gethash symbol *candle-histories*)
                  (let ((tf-hash (gethash symbol *candle-histories-tf*)))
                    (if tf-hash (gethash timeframe tf-hash) nil))))))
      ;; Data Keeper not available, use local
      (if (string= timeframe "M1")
          (gethash symbol *candle-histories*)
          (let ((tf-hash (gethash symbol *candle-histories-tf*)))
            (if tf-hash (gethash timeframe tf-hash) nil)))))

(defun add-candle-to-keeper (symbol candle)
  "Push a new candle to Data Keeper for persistence."
  (when *data-keeper-available*
    (let* ((candle-alist `((timestamp . ,(candle-timestamp candle))
                           (open . ,(candle-open candle))
                           (high . ,(candle-high candle))
                           (low . ,(candle-low candle))
                           (close . ,(candle-close candle))
                           (volume . ,(candle-volume candle))))
           (command (build-data-keeper-request "ADD_CANDLE"
                                               :symbol symbol
                                               :candle candle-alist)))
      (data-keeper-query command))))

(defvar *last-backfill-time* (make-hash-table :test 'equal)
  "Throttle backfill requests per symbol to prevent infinite loop")

(defun check-data-gap (symbol history)
  "Check for data gaps at the end of history and request backfill if needed.
   Returns T if a gap was detected and backfill requested, NIL otherwise.
   V15.6: Added 60s throttle per symbol to prevent request spam."
  (if (and history (> (length history) 0))
      (let* ((last-candle (first history)) ; newest first
             (last-ts (candle-timestamp last-candle))
             (now (get-universal-time))
             ;; V15.5 Fix: MT5 sends Unix Time (1970), Lisp uses Universal (1900).
             ;; Universal = Unix + 2208988800.
             ;; So convert Now(Universal) to Unix for comparison.
             (now-unix (- now 2208988800))
             (gap (- now-unix last-ts))
             (threshold 180) ; 3 minutes tolerance for M1 (Relaxed for latency)
             ;; V15.6: Throttle - only request once per 60 seconds per symbol
             (last-request (gethash symbol *last-backfill-time* 0))
             (throttle-elapsed (> (- now last-request) 60)))
        
        (if (and (> gap threshold) throttle-elapsed)
            (let ((missing-min (floor gap 60)))
               (format t "[L] ⚠️ DATA GAP DETECTED for ~a: Gap is ~d seconds (~d bars). Last: ~a~%" 
                       symbol gap missing-min last-ts)
               
               (when (> missing-min 0)
                 (setf (gethash symbol *last-backfill-time*) now) ; Update throttle
                 (let ((request-bars (if (> missing-min 14400) 14400 (+ missing-min 10))))
                   (format t "[L] 🔄 Requesting backfill from MT5 (~d bars)...~%" request-bars)
                   (when (and (boundp 'swimmy.globals:*cmd-publisher*) swimmy.globals:*cmd-publisher*)
                     (let ((cmd (swimmy.core:encode-sexp
                                 `((type . "REQ_HISTORY")
                                   (symbol . ,symbol)
                                   (tf . "M1")
                                   (count . ,request-bars)
                                   (start . ,now-unix))))) ; Force start from NOW to get latest data
                       (pzmq:send swimmy.globals:*cmd-publisher* cmd))))
                 t)) ; Return T (Gap detected)
            nil)) ; No gap or throttled
      nil))

(defun close-data-keeper-client ()
  "Clean up Data Keeper connection."
  (when *data-keeper-socket*
    (pzmq:close *data-keeper-socket*)
    (setf *data-keeper-socket* nil))
  (when *data-keeper-context*
    (pzmq:ctx-term *data-keeper-context*)
    (setf *data-keeper-context* nil))
  (setf *data-keeper-available* nil)
  (format t "[DATA-CLIENT] Disconnected from Data Keeper~%"))

(format t "[DATA-CLIENT] Data Keeper client loaded (Phase 7)~%")
