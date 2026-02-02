;;; school-db.lisp - SQLite Persistence Layer for Swimmy School
;;; V49.8: Scaling the Strategy KB and Trade Logs (Refactored)

(in-package :swimmy.school)

(defun init-db ()
  "Initialize SQLite database and create tables if not exist."
  ;; Table: Strategies
  (execute-non-query
   "CREATE TABLE IF NOT EXISTS strategies (
      name TEXT PRIMARY KEY,
      indicators TEXT,
      entry TEXT,
      exit TEXT,
      sl REAL,
      tp REAL,
      volume REAL,
      sharpe REAL,
      profit_factor REAL,
      win_rate REAL,
      trades INTEGER,
      max_dd REAL,
      category TEXT,
      timeframe INTEGER,
      generation INTEGER,
      rank TEXT,
      symbol TEXT,
      direction TEXT,
      last_bt_time INTEGER,
      hash TEXT,
      oos_sharpe REAL,
      cpcv_median REAL,
      cpcv_pass_rate REAL,
      data_sexp TEXT
    )")

  ;; V49.8: Migration for existing DBs
  (handler-case
      (execute-non-query "ALTER TABLE strategies ADD COLUMN hash TEXT")
    (error () nil))
  (handler-case
      (execute-non-query "ALTER TABLE strategies ADD COLUMN oos_sharpe REAL")
    (error () nil))
  (handler-case
      (execute-non-query "ALTER TABLE strategies ADD COLUMN cpcv_median REAL")
    (error () nil))
  (handler-case
      (execute-non-query "ALTER TABLE strategies ADD COLUMN cpcv_pass_rate REAL")
    (error () nil))


  ;; Table: Trades
  (execute-non-query
   "CREATE TABLE IF NOT EXISTS trade_logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp INTEGER,
      strategy_name TEXT,
      symbol TEXT,
      direction TEXT,
      category TEXT,
      regime TEXT,
      pnl REAL,
      hold_time INTEGER
    )")

  ;; Table: Swap History (QS Architecture Data Lake)
  (execute-non-query
   "CREATE TABLE IF NOT EXISTS swap_history (
      symbol TEXT,
      timestamp INTEGER,
      swap_long REAL,
      swap_short REAL,
      spread REAL,
      PRIMARY KEY (symbol, timestamp)
    )")

  ;; Table: Swap History (QS Architecture Data Lake)
  (execute-non-query
   "CREATE TABLE IF NOT EXISTS swap_history (
      symbol TEXT,
      timestamp INTEGER,
      swap_long REAL,
      swap_short REAL,
      spread REAL,
      PRIMARY KEY (symbol, timestamp)
    )")

  ;; Table: OOS Request Queue
  (execute-non-query
   "CREATE TABLE IF NOT EXISTS oos_queue (
      request_id TEXT PRIMARY KEY,
      name TEXT,
      requested_at INTEGER,
      status TEXT,
      last_error TEXT
    )")

  ;; Indices for fast draft/selection
  (execute-non-query "CREATE INDEX IF NOT EXISTS idx_trade_name ON trade_logs(strategy_name)")
  
  (format t "[DB] 🗄️ SQLite tables ensured.~%")
  
  ;; Auto-Migration Check (Phase 39 Recovery)
  (handler-case
      ;; Check for VALID strategies (starting with #S), ignoring legacy lists
      (let ((count (execute-single "SELECT count(*) FROM strategies WHERE data_sexp LIKE '#S(%'")))
        (when (< count 100) ;; If fewer than 100 valid strategies, assume migration needed
          (format t "[DB] 🆕 Low valid strategy count (~d). Triggering data migration from file...~%" count)
          (migrate-existing-data)))
    (error (e) (format t "[DB] ⚠️ Auto-migration check failed: ~a~%" e))))

;; OOS queue helpers ----------------------------------------------------------

(defun enqueue-oos-request (name request-id)
  "Insert or replace an OOS request."
  (let ((now (get-universal-time)))
    (execute-non-query
     "INSERT OR REPLACE INTO oos_queue (request_id, name, requested_at, status, last_error)
      VALUES (?, ?, ?, 'sent', NULL)"
     request-id name now)))

(defun lookup-oos-request (name)
  "Return (values request-id requested-at status) for the latest request by name, or NILs."
  (let ((row (first (execute-to-list
                     "SELECT request_id, requested_at, status FROM oos_queue WHERE name=? ORDER BY requested_at DESC LIMIT 1"
                     name))))
    (when row
      (destructuring-bind (req-id req-at status) row
        (values req-id req-at status)))))

(defun complete-oos-request (name request-id)
  "Mark OOS request done by request-id (preferred) or name fallback."
  (if request-id
      (execute-non-query "DELETE FROM oos_queue WHERE request_id=?" request-id)
      (execute-non-query "DELETE FROM oos_queue WHERE name=?" name)))

(defun record-oos-error (name request-id error-msg)
  (let ((target (if request-id
                    (list "request_id=?" request-id)
                    (list "name=?" name))))
    (apply #'execute-non-query
           (format nil "UPDATE oos_queue SET status='error', last_error=?, requested_at=? WHERE ~a"
                   (first target))
           error-msg (get-universal-time) (rest target))))

(defun %parse-rank-safe (rank-str)
  "Safely parse rank string from DB. Returns rank and valid-p."
  (when (and rank-str (stringp rank-str))
    (let ((trimmed (string-upcase (string-trim '(#\Space #\Newline #\Tab) rank-str))))
      (cond
        ((string= trimmed "NIL") (values nil t))
        (t
         (let ((rank (swimmy.core:safe-read-sexp rank-str :package :swimmy.school)))
           (if (and (symbolp rank) (keywordp rank))
               (values rank t)
               (values nil nil))))))))

(defun upsert-strategy (strat)
  "Save or update strategy in SQL."
  (unless (strategy-hash strat)
    (setf (strategy-hash strat) (calculate-strategy-hash strat)))
  (let* ((name (strategy-name strat))
         (existing-row (ignore-errors (first (execute-to-list
                                              "SELECT sharpe, profit_factor, win_rate, trades, max_dd FROM strategies WHERE name=?"
                                              name))))
         (db-sharpe (if existing-row (or (first existing-row) 0.0) 0.0))
         (db-pf (if existing-row (or (second existing-row) 0.0) 0.0))
         (db-wr (if existing-row (or (third existing-row) 0.0) 0.0))
         (db-trades (if existing-row (or (fourth existing-row) 0) 0))
         (db-max-dd (if existing-row (or (fifth existing-row) 0.0) 0.0))
         (cur-sharpe (or (strategy-sharpe strat) 0.0))
         (cur-pf (or (strategy-profit-factor strat) 0.0))
         (cur-wr (or (strategy-win-rate strat) 0.0))
         (cur-trades (or (strategy-trades strat) 0))
         (cur-max-dd (or (strategy-max-dd strat) 0.0)))
    ;; Preserve non-zero DB metrics if in-memory values are zero.
    (when (and existing-row (zerop cur-sharpe) (not (zerop db-sharpe)))
      (setf cur-sharpe db-sharpe))
    (when (and existing-row (zerop cur-pf) (not (zerop db-pf)))
      (setf cur-pf db-pf))
    (when (and existing-row (zerop cur-wr) (not (zerop db-wr)))
      (setf cur-wr db-wr))
    (when (and existing-row (zerop cur-trades) (> db-trades 0))
      (setf cur-trades db-trades))
    (when (and existing-row (zerop cur-max-dd) (not (zerop db-max-dd)))
      (setf cur-max-dd db-max-dd))
    (handler-case
      (execute-non-query
       "INSERT OR REPLACE INTO strategies (
          name, indicators, entry, exit, sl, tp, volume, 
          sharpe, profit_factor, win_rate, trades, max_dd, 
          category, timeframe, generation, rank, symbol, direction, hash, 
          oos_sharpe, cpcv_median, cpcv_pass_rate, data_sexp
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
       name
       (format nil "~a" (strategy-indicators strat))
       (format nil "~a" (strategy-entry strat))
       (format nil "~a" (strategy-exit strat))
       (strategy-sl strat)
       (strategy-tp strat)
       (strategy-volume strat)
       cur-sharpe
       cur-pf
       cur-wr
       cur-trades
       cur-max-dd
       (format nil "~a" (strategy-category strat))
       (strategy-timeframe strat)
       (strategy-generation strat)
       (format nil "~s" (strategy-rank strat)) ; Store as ":RANK"
       (or (strategy-symbol strat) "USDJPY")
       (format nil "~a" (strategy-direction strat))
       (strategy-hash strat)
       (or (strategy-oos-sharpe strat) 0.0)
       (or (strategy-cpcv-median-sharpe strat) 0.0)
       (or (strategy-cpcv-pass-rate strat) 0.0)
       (format nil "~s" strat)) ; Store full serialized object as backup
    (error (e) (format t "[DB] ❌ Upsert error for ~a: ~a~%" (strategy-name strat) e))))
  )

(defun record-trade-to-db (record)
  "Log trade to SQL."
  (execute-non-query
   "INSERT INTO trade_logs (timestamp, strategy_name, symbol, direction, category, regime, pnl, hold_time)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
   (trade-record-timestamp record)
   (trade-record-strategy-name record)
   (trade-record-symbol record)
   (format nil "~a" (trade-record-direction record))
   (format nil "~a" (trade-record-category record))
   (format nil "~a" (trade-record-regime record))
   (trade-record-pnl record)
   (trade-record-hold-time record)))

(defparameter *last-db-sync-time* 0)
(defparameter *db-sync-interval* 60
  "Minimum seconds between DB syncs for strategy metrics.")

(defun refresh-strategy-metrics-from-db (&key (force nil))
  "Refresh in-memory strategy metrics from the DB (for multi-process coherence)."
  (let ((now (get-universal-time)))
    (when (or force (> (- now *last-db-sync-time*) *db-sync-interval*))
      (setf *last-db-sync-time* now)
      (let ((rows (execute-to-list
                   "SELECT name, sharpe, profit_factor, win_rate, trades, max_dd, rank, oos_sharpe, cpcv_median, cpcv_pass_rate FROM strategies"))
            (updated 0))
        (dolist (row rows)
          (destructuring-bind (name sharpe pf wr trades maxdd rank oos cpcv-median cpcv-pass) row
            (let ((strat (find-strategy name)))
              (when strat
                (when sharpe (setf (strategy-sharpe strat) (float sharpe 0.0)))
                (when pf (setf (strategy-profit-factor strat) (float pf 0.0)))
                (when wr (setf (strategy-win-rate strat) (float wr 0.0)))
                (when trades (setf (strategy-trades strat) trades))
                (when maxdd (setf (strategy-max-dd strat) (float maxdd 0.0)))
                (when oos (setf (strategy-oos-sharpe strat) (float oos 0.0)))
                (when cpcv-median (setf (strategy-cpcv-median-sharpe strat) (float cpcv-median 0.0)))
                (when cpcv-pass (setf (strategy-cpcv-pass-rate strat) (float cpcv-pass 0.0)))
                (when (and rank (stringp rank))
                  (multiple-value-bind (rank-sym ok) (%parse-rank-safe rank)
                    (when ok
                      (setf (strategy-rank strat) rank-sym))))
                (incf updated)))))
        (when (> updated 0)
          (format t "[DB] 🔄 Synced metrics for ~d strategies~%" updated))))))

(defun fetch-candidate-strategies (&key (min-sharpe 0.1) (ranks '(":B" ":A" ":S")))
  "Fetch strategies from DB matching criteria for the global draft."
  (let ((query (format nil "SELECT data_sexp FROM strategies WHERE sharpe >= ? AND rank IN (~{~a~^,~})"
                       (mapcar (lambda (r) (format nil "'~a'" r)) ranks))))
    (let ((rows (execute-to-list query min-sharpe)))
      (remove-if #'null 
                 (mapcar (lambda (row) 
                           (let ((sexp-str (first row)))
                             (let ((obj (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school)))
                               (if (strategy-p obj)
                                   obj
                                   (progn
                                     (when (and sexp-str (> (length sexp-str) 0))
                                       (format t "[DB] 💥 Corrupted Strategy SEXP (safe-read): ~a...~%"
                                               (subseq sexp-str 0 (min 30 (length sexp-str)))))
                                     nil)))))
                         rows)))))

(defun fetch-all-strategies-from-db ()
  "Fetch EVERY strategy from the DB, including unranked and legends."
  (let ((rows (execute-to-list "SELECT data_sexp FROM strategies")))
    (remove-if #'null 
               (mapcar (lambda (row) 
                         (let ((sexp-str (first row)))
                           (let ((obj (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school)))
                             (if (strategy-p obj)
                                 obj
                                 (progn
                                   (when (and sexp-str (> (length sexp-str) 0))
                                     (format t "[DB] 💥 Corrupted Strategy SEXP (safe-read): ~a...~%"
                                             (subseq sexp-str 0 (min 30 (length sexp-str)))))
                                   nil)))))
                       rows))))

(defun collect-all-strategies-unpruned ()
  "Return all strategies from DB + Library without pruning."
  (let* ((db-strats (fetch-all-strategies-from-db))
         (file-strats (ignore-errors (swimmy.persistence:load-all-strategies)))
         (all (copy-list db-strats)))
    (dolist (fs (or file-strats '()))
      (unless (find (strategy-name fs) all :key #'strategy-name :test #'string=)
        (push fs all)))
    all))

(defun map-strategies-from-db (fn &key (batch-size 1000) limit)
  "Call FN for each strategy in DB, processing in batches. Returns count."
  (block done
    (let ((offset 0)
          (processed 0))
      (loop
        (when (and limit (>= processed limit))
          (return-from done processed))
        (let ((rows (execute-to-list "SELECT data_sexp FROM strategies LIMIT ? OFFSET ?" batch-size offset)))
          (when (null rows)
            (return-from done processed))
          (dolist (row rows)
            (when (and limit (>= processed limit))
              (return-from done processed))
            (let ((sexp-str (first row)))
              (handler-case
                  (let ((*package* (find-package :swimmy.school)))
                    (let ((obj (read-from-string sexp-str)))
                      (when (strategy-p obj)
                        (funcall fn obj)
                        (incf processed))))
                (error (e)
                  (format t "[DB] 💥 Corrupted Strategy SEXP (pkg: ~a): ~a... Error: ~a~%"
                          *package*
                          (subseq sexp-str 0 (min 30 (length sexp-str))) e))))))
          (incf offset batch-size)))))

(defun get-db-stats ()
  "Return summary of DB contents."
  (let ((strat-count (execute-single "SELECT count(*) FROM strategies"))
        (trade-count (execute-single "SELECT count(*) FROM trade_logs")))
    (list :strategies strat-count :trades trade-count)))

(defun migrate-existing-data ()
  "Migrate existing flat-files (ranks, graveyard, legacy KB) to SQLite."
  (let ((count 0)
        (legacy-kb-path "data/knowledge_base.sexp"))
    
    ;; 1. Migrate Legacy Knowledge Base File (The Mother Lode)
    (when (probe-file legacy-kb-path)
      (format t "[DB] 🚜 Migrating ~a to SQL... (This may take a moment)~%" legacy-kb-path)
      (with-transaction
        (with-open-file (in legacy-kb-path :direction :input :if-does-not-exist nil)
          (let ((*package* (find-package :swimmy.school)))
            (loop for obj = (handler-case (read in nil :eof) 
                                (error (e) 
                                  (format t "[DB] ⚠️ Corrupted entry in KB: ~a~%" e) 
                                  :error))
                  until (eq obj :eof)
                  do (cond
                       ((strategy-p obj) 
                        (upsert-strategy obj) 
                        (incf count))
                       ((listp obj) 
                        ;; Handle file being a single list of strategies
                        (dolist (item obj)
                          (when (strategy-p item)
                            (upsert-strategy item)
                            (incf count)))))))))

    ;; 2. Migrate In-Memory Ranks (if any)
    (with-transaction
      (when (boundp '*strategy-knowledge-base*)
        (dolist (strat *strategy-knowledge-base*)
          (upsert-strategy strat)
          (incf count)))
      
      (when (boundp '*evolved-strategies*)
        (dolist (strat *evolved-strategies*)
          (upsert-strategy strat)
          (incf count))))

    (format t "[DB] 🚜 Migrated ~d strategies to SQL.~%" count)
    
    (let ((graveyard-path "data/memory/graveyard.sexp"))
      (when (probe-file graveyard-path)
        (with-open-file (in graveyard-path :direction :input :if-does-not-exist nil)
          (let ((g-count 0))
            (with-transaction
              (loop for p = (handler-case
                                (let ((*package* (find-package :swimmy.school)))
                                  (read in nil :eof))
                              (error (e)
                                (format t "[DB] 💥 Corrupted Graveyard SEXP: ~a~%" e)
                                :eof))
                    until (eq p :eof)
                    do (when (and p (listp p) (getf p :name))
                         (let ((g-name (format nil "GY-~a" (getf p :name))))
                           (let* ((fake-strat (make-strategy :indicators (getf p :indicators)
                                                            :entry (getf p :entry)
                                                            :exit (getf p :exit)))
                                  (hash (calculate-strategy-hash fake-strat)))
                             (execute-non-query 
                              "INSERT OR REPLACE INTO strategies (name, rank, hash, data_sexp) VALUES (?, ?, ?, ?)"
                              g-name ":GRAVEYARD" hash (format nil "~s" p))
                             (incf g-count))))))
            (format t "[DB] 🪦 Migrated ~d graveyard patterns to SQL.~%" g-count))))))
    count))

;; Graceful shutdown hook
(defun close-db ()
  (swimmy.core:close-db-connection))


(defun record-swap-data (symbol swap-long swap-short spread)
  "Log daily swap/spread data to SQL."
  (let ((now (get-universal-time)))
    ;; Quantize to day (midnight) to avoid duplicates if called multiple times
    (let ((day-timestamp (* (floor now 86400) 86400)))
      (handler-case
          (execute-non-query
           "INSERT OR REPLACE INTO swap_history (symbol, timestamp, swap_long, swap_short, spread)
            VALUES (?, ?, ?, ?, ?)"
           symbol day-timestamp (float swap-long) (float swap-short) (float spread))
        (error (e) 
          (format t "[DB] ⚠️ Failed to record swap data for ~a: ~a~%" symbol e))))))

(defun get-top-carry-pairs (&key (limit 5))
  "SQL-Screening: Get top symbols by Swap Long value for today (or latest).
   Equivalent to: 'Find High Earnings Yield Stocks'"
  (let ((query "SELECT symbol, swap_long FROM swap_history 
                WHERE timestamp = (SELECT MAX(timestamp) FROM swap_history)
                AND swap_long > 0
                ORDER BY swap_long DESC
                LIMIT ?"))
    (execute-to-list query limit)))

(defun fetch-swap-history (symbol &key start-ts end-ts)
  "Fetch historical swap data for a symbol within a time range.
   Returns a list of plists: (:t timestamp :sl swap-long :ss swap-short)"
  (let ((query "SELECT timestamp, swap_long, swap_short FROM swap_history WHERE symbol = ?"))
    (when start-ts
      (setf query (concatenate 'string query (format nil " AND timestamp >= ~d" start-ts))))
    (when end-ts
      (setf query (concatenate 'string query (format nil " AND timestamp <= ~d" end-ts))))
    (setf query (concatenate 'string query " ORDER BY timestamp ASC"))
    
    (let ((rows (execute-to-list query symbol)))
      (mapcar (lambda (row)
                (list :t (first row) :sl (second row) :ss (third row)))
              rows))))

;; (init-db) ; Moved to initialize-system in main.lisp for bootstrap safety.
