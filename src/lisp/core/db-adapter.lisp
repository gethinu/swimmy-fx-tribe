(in-package :cl-user)

(defpackage :swimmy.db
  (:use :cl :postmodern)
  (:export #:init-db
           #:save-tick
           #:save-bar
           #:save-signal
           #:save-audit-event
           #:fetch-trade-history))

(in-package :swimmy.db)

;; Configuration
(defparameter *db-mode* :jsonl "Mode: :postgres or :jsonl")
(defparameter *jsonl-dir* "db/data/" "Directory for JSONL files")
(defparameter *db-conn-params* '("swimmy_db" "swimmy" "swimmy_password" "localhost" :port 5432))
(defvar *db-connected* nil)

;; Ensure directory exists
(defun ensure-db-dir ()
  (ensure-directories-exist *jsonl-dir*))

(defun init-db ()
  "Initialize DB connection or file structure."
  (format t "[DB] Initializing DB Adapter (Mode: ~a)...~%" *db-mode*)
  (cond
    ((eq *db-mode* :postgres)
     (handler-case
         (progn
           (connect-toplevel (first *db-conn-params*) 
                             (second *db-conn-params*) 
                             (third *db-conn-params*) 
                             (fourth *db-conn-params*) 
                             :port (fifth *db-conn-params*))
           (setf *db-connected* t)
           (format t "[DB] ✅ Connected to PostgreSQL.~%"))
       (error (e)
         (format t "[DB] ❌ Failed to connect to PostgreSQL: ~a~%" e)
         (format t "[DB] ⚠️ Falling back to JSONL mode.~%")
         (setf *db-mode* :jsonl)
         (ensure-db-dir))))
    ((eq *db-mode* :jsonl)
     (ensure-db-dir))))

(defun append-jsonl (filename data-wrapper)
  "Append a JSON object to a file."
  (let ((path (merge-pathnames filename *jsonl-dir*))
        (json-str (jsown:to-json data-wrapper)))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (write-line json-str stream))))

(defun save-tick (tick)
  "Save a tick."
  ;; Example tick: '((:bid . 140.0) (:symbol . "USDJPY"))
  (case *db-mode*
    (:postgres
     (when *db-connected*
       (handler-case
           ;; Assuming 'ticks' table exists from schema
           (query "INSERT INTO ticks (instrument, ts, bid, ask, tick_volume) VALUES ($1, $2, $3, $4, $5)"
                  (cdr (assoc :symbol tick))
                  (local-time:now) ;; ts
                  (cdr (assoc :bid tick))
                  (cdr (assoc :ask tick))
                  1)
         (error (e)
           (format t "[DB] Insert Error: ~a- Fallback to JSONL~%" e)
           (append-jsonl "ticks.jsonl" (jsown:new-js ("table" "ticks") ("data" tick)))))))
    (:jsonl
     (append-jsonl "ticks.jsonl" (jsown:new-js ("table" "ticks") ("data" tick))))))

(defun save-bar (bar)
  "Save a bar."
  (case *db-mode*
    (:postgres
     (when *db-connected*
       (handler-case
           (query "INSERT INTO bars (instrument, timeframe, ts, open, high, low, close, volume) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
                  "USDJPY" ;; TODO: Pass instrument in BAR struct or arg
                  "M1"
                  (local-time:universal-to-timestamp (swimmy.globals:candle-timestamp bar))
                  (swimmy.globals:candle-open bar)
                  (swimmy.globals:candle-high bar)
                  (swimmy.globals:candle-low bar)
                  (swimmy.globals:candle-close bar)
                  (swimmy.globals:candle-volume bar))
         (error (e)
           (format t "[DB] Insert Error: ~a~%" e)
           (append-jsonl "bars.jsonl" (jsown:new-js ("table" "bars") ("data" bar)))))))
    (:jsonl
     (append-jsonl "bars.jsonl" (jsown:new-js ("table" "bars") ("data" bar))))))

(defun save-signal (signal-data)
  "Save a signal."
  (case *db-mode*
    (:postgres
     (when *db-connected*
        ;; Schema: id, strategy_id, instrument, timeframe, side, price, confidence, suggested_lot, ts, reason
       (handler-case
           (query "INSERT INTO signals (strategy_id, instrument, timeframe, side, price, confidence, suggested_lot, ts, reason) VALUES ($1, 'USDJPY', 'M1', $2, $3, $4, 0.01, $5, $6)"
                  (jsown:val signal-data "strategy")
                  (jsown:val signal-data "action")
                  0.0 ;; Price 
                  0.0 ;; Confidence
                  (local-time:now)
                  "Signal")
         (error (e)
           (format t "[DB] Insert Error: ~a~%" e)
           (append-jsonl "signals.jsonl" (jsown:new-js ("table" "signals") ("data" signal-data)))))))
    (:jsonl
     (append-jsonl "signals.jsonl" (jsown:new-js ("table" "signals") ("data" signal-data))))))

(defun save-audit-event (event-type payload)
  "Save an audit event (BENCH, KILL, etc)."
  (case *db-mode*
    (:postgres
     (when *db-connected*
       (handler-case
           (query "INSERT INTO audit_events (event_type, payload, ts) VALUES ($1, $2, $3)"
                  event-type
                  (jsown:to-json payload)
                  (local-time:now))
         (error (e)
           (format t "[DB] Audit Error: ~a~%" e)
           (append-jsonl "audit.jsonl" (jsown:new-js ("table" "audit_events") ("type" event-type) ("payload" payload)))))))
    (:jsonl
     (append-jsonl "audit.jsonl" (jsown:new-js ("table" "audit_events") ("type" event-type) ("payload" payload))))))

;;; Data Fetching (Phase 11) ---------------------------------------------------

(defun fetch-trade-history (strategy-name)
  "Fetch PnL history for a strategy from DB or JSONL."
  (format t "[DB] Fetching trade history for ~a...~%" strategy-name)
  (case *db-mode*
    (:postgres
     (if *db-connected*
         (handler-case
             (let ((rows (query "SELECT pnl FROM trades WHERE strategy_name = $1 ORDER BY close_time ASC" 
                                strategy-name)))
               ;; Flatten list of lists ((10.5) (-5.0) ...) -> (10.5 -5.0 ...)
               (mapcar #'first rows))
           (error (e)
             (format t "[DB] Fetch Error: ~a~%" e)
             nil))
         nil))
    (:jsonl
     ;; Fallback: Parse trades.jsonl if it exists (Not implemented fully yet, return mock for test)
     ;; In a real JSONL impl, we'd read line by line and filter.
     (let ((path (merge-pathnames "trades.jsonl" *jsonl-dir*)))
       (if (probe-file path)
           (let ((pnls nil))
             (with-open-file (stream path)
               (loop for line = (read-line stream nil)
                     while line do
                     (handler-case 
                         (let* ((json (jsown:parse line))
                                (data (jsown:val json "data")))
                           (when (string= (jsown:val data "strategy") strategy-name)
                             (push (jsown:val data "pnl") pnls)))
                       (error () nil))))
             (reverse pnls))
           nil)))))
