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

(defparameter *data-keeper-address* "tcp://localhost:5561"
  "ZMQ address of the Data Keeper service")

(defparameter *data-keeper-socket* nil
  "ZMQ REQ socket for Data Keeper queries")

(defparameter *data-keeper-context* nil
  "ZMQ context for Data Keeper connection")

(defparameter *data-keeper-timeout* 3000
  "Timeout in milliseconds for Data Keeper queries")

(defparameter *data-keeper-available* nil
  "Flag indicating if Data Keeper is reachable")

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
        (let ((status (data-keeper-status)))
          (if status
              (progn
                (setf *data-keeper-available* t)
                (format t "[DATA-CLIENT] Data Keeper is ONLINE. Symbols: ~a~%" 
                        (jsown:val status "symbols")))
              (progn
                (setf *data-keeper-available* nil)
                (format t "[DATA-CLIENT] Data Keeper is OFFLINE. Using local history.~%")))))
    (error (e)
      (format t "[DATA-CLIENT] Failed to connect to Data Keeper: ~a~%" e)
      (setf *data-keeper-available* nil))))

(defun data-keeper-query (command)
  "Send a command to Data Keeper and return the JSON response."
  (when *data-keeper-socket*
    (handler-case
        (progn
          (pzmq:send *data-keeper-socket* command)
          (let ((response (pzmq:recv-string *data-keeper-socket*)))
            (jsown:parse response)))
      (error (e)
        (format t "[DATA-CLIENT] Query failed: ~a~%" e)
        nil))))

(defun data-keeper-status ()
  "Get Data Keeper service status."
  (data-keeper-query "STATUS"))

(defun get-history-from-keeper (symbol count &optional (timeframe "M1"))
  "Get historical candles from Data Keeper.
   Returns list of candle structs, newest first (matching *candle-history* format).
   Falls back to local history if Data Keeper is unavailable.
   If TIMEFRAME is not M1, stores/retrieves from *candle-histories-tf*."
  (if *data-keeper-available*
      ;; Determine query format based on timeframe
      (let* ((query (if (string= timeframe "M1")
                        (format nil "GET_HISTORY:~a:~d" symbol count) ;; Legacy
                        (format nil "GET_HISTORY:~a:~a:~d" symbol timeframe count))) ;; New
             (response (data-keeper-query query)))
        (if (and response (not (jsown:keyp response "error")))
            (let ((candles (jsown:val response "candles")))
              ;; Convert JSON candles to structs
              (mapcar (lambda (c)
                        (make-candle 
                          :timestamp (jsown:val c "timestamp")
                          :open (jsown:val c "open")
                          :high (jsown:val c "high")
                          :low (jsown:val c "low")
                          :close (jsown:val c "close")
                          :volume (or (jsown:val-safe c "volume") 0)))
                      candles))
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
    (let* ((candle-json (jsown:to-json
                          (jsown:new-js
                            ("timestamp" (candle-timestamp candle))
                            ("open" (candle-open candle))
                            ("high" (candle-high candle))
                            ("low" (candle-low candle))
                            ("close" (candle-close candle))
                            ("volume" (candle-volume candle)))))
           (command (format nil "ADD_CANDLE:~a:~a" symbol candle-json)))
      (data-keeper-query command))))

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
