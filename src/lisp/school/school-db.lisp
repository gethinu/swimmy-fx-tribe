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

  ;; Indices for fast draft/selection
  (execute-non-query "CREATE INDEX IF NOT EXISTS idx_strat_rank ON strategies(rank)")
  (execute-non-query "CREATE INDEX IF NOT EXISTS idx_strat_sharpe ON strategies(sharpe)")
  (execute-non-query "CREATE INDEX IF NOT EXISTS idx_strat_hash ON strategies(hash)")
  (execute-non-query "CREATE INDEX IF NOT EXISTS idx_trade_name ON trade_logs(strategy_name)")
  
  (format t "[DB] 🗄️ SQLite tables ensured.~%"))

(defun upsert-strategy (strat)
  "Save or update strategy in SQL."
  (unless (strategy-hash strat)
    (setf (strategy-hash strat) (calculate-strategy-hash strat)))
  (handler-case
      (execute-non-query
       "INSERT OR REPLACE INTO strategies (
          name, indicators, entry, exit, sl, tp, volume, 
          sharpe, profit_factor, win_rate, trades, max_dd, 
          category, timeframe, generation, rank, symbol, direction, hash, 
          oos_sharpe, cpcv_median, cpcv_pass_rate, data_sexp
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
       (strategy-name strat)
       (format nil "~a" (strategy-indicators strat))
       (format nil "~a" (strategy-entry strat))
       (format nil "~a" (strategy-exit strat))
       (strategy-sl strat)
       (strategy-tp strat)
       (strategy-volume strat)
       (or (strategy-sharpe strat) 0.0)
       (or (strategy-profit-factor strat) 0.0)
       (or (strategy-win-rate strat) 0.0)
       (or (strategy-trades strat) 0)
       (or (strategy-max-dd strat) 0.0)
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

(defun fetch-candidate-strategies (&key (min-sharpe 0.1) (ranks '(":B" ":A" ":S")))
  "Fetch strategies from DB matching criteria for the global draft."
  (let ((query (format nil "SELECT data_sexp FROM strategies WHERE sharpe >= ? AND rank IN (~{~a~^,~})"
                       (mapcar (lambda (r) (format nil "'~a'" r)) ranks))))
    (let ((rows (execute-to-list query min-sharpe)))
      (mapcar (lambda (row) (read-from-string (first row))) rows))))

(defun get-db-stats ()
  "Return summary of DB contents."
  (let ((strat-count (execute-single "SELECT count(*) FROM strategies"))
        (trade-count (execute-single "SELECT count(*) FROM trade_logs")))
    (list :strategies strat-count :trades trade-count)))

(defun migrate-existing-data ()
  "Migrate existing flat-files (ranks and graveyard) to SQLite."
  (let ((count 0))
    ;; 1. & 2. Migrate Ranks and evolved strategies
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
              (loop for p = (read in nil :eof)
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
            (format t "[DB] 🪦 Migrated ~d graveyard patterns to SQL.~%" g-count)))))
    count))

;; Graceful shutdown hook
(defun close-db ()
  (swimmy.core:close-db-connection))

(init-db)
