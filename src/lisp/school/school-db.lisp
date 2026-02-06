;;; school-db.lisp - SQLite Persistence Layer for Swimmy School
;;; V49.8: Scaling the Strategy KB and Trade Logs (Refactored)

(in-package :swimmy.school)

(defparameter *disable-auto-migration* nil
  "When true, skip auto-migration in init-db (useful for tests).")

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
      data_sexp TEXT,
      updated_at INTEGER
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
  (handler-case
      (execute-non-query "ALTER TABLE strategies ADD COLUMN updated_at INTEGER")
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
      hold_time INTEGER,
      pair_id TEXT
    )")
  (handler-case
      (execute-non-query "ALTER TABLE trade_logs ADD COLUMN pair_id TEXT")
    (error () nil))

  ;; Table: Backtest Trades (OOS/CPCV/Backtest)
  (execute-non-query
   "CREATE TABLE IF NOT EXISTS backtest_trade_logs (
      request_id TEXT,
      strategy_name TEXT,
      timestamp INTEGER,
      pnl REAL,
      symbol TEXT,
      direction TEXT,
      entry_price REAL,
      exit_price REAL,
      sl REAL,
      tp REAL,
      volume REAL,
      hold_time INTEGER,
      rank TEXT,
      timeframe INTEGER,
      category TEXT,
      regime TEXT,
      oos_kind TEXT
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
  (unless *disable-auto-migration*
    (handler-case
        ;; Check for VALID strategies (starting with #S), ignoring legacy lists
        (let ((count (execute-single "SELECT count(*) FROM strategies WHERE data_sexp LIKE '#S(%'")))
          (when (< count 100) ;; If fewer than 100 valid strategies, assume migration needed
            (format t "[DB] 🆕 Low valid strategy count (~d). Triggering data migration from file...~%" count)
            (migrate-existing-data)))
      (error (e) (format t "[DB] ⚠️ Auto-migration check failed: ~a~%" e)))))

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
         (cur-max-dd (or (strategy-max-dd strat) 0.0))
         (updated-at (get-universal-time)))
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
          oos_sharpe, cpcv_median, cpcv_pass_rate, data_sexp, updated_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
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
         (format nil "~s" strat)
         updated-at) ; Store full serialized object as backup
      (error (e) (format t "[DB] ❌ Upsert error for ~a: ~a~%" (strategy-name strat) e))))
  )

(defun record-trade-to-db (record)
  "Log trade to SQL."
  (execute-non-query
   "INSERT INTO trade_logs (timestamp, strategy_name, symbol, direction, category, regime, pnl, hold_time, pair_id)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
   (trade-record-timestamp record)
   (trade-record-strategy-name record)
   (trade-record-symbol record)
   (format nil "~a" (trade-record-direction record))
   (format nil "~a" (trade-record-category record))
   (format nil "~a" (trade-record-regime record))
   (trade-record-pnl record)
   (trade-record-hold-time record)
   (trade-record-pair-id record)))

(defun %backtest-entry-val (entry keys)
  "Extract value from ENTRY (alist or plist) using KEYS."
  (cond
    ((and (listp entry) (keywordp (first entry)))
     (loop for k in keys
           for kw = (cond ((keywordp k) k)
                          ((symbolp k) (intern (string-upcase (symbol-name k)) :keyword))
                          (t nil))
           for val = (and kw (getf entry kw :missing))
           unless (eq val :missing) do (return val)))
    (t
     (loop for k in keys
           for cell = (assoc k entry)
           when cell do (return (cdr cell))))))

(defun record-backtest-trades (request-id strategy-name oos-kind trade-list)
  "Persist trade_list entries for backtest/OOS/CPCV."
  (dolist (entry trade-list)
    (let* ((timestamp (%backtest-entry-val entry '(timestamp t)))
           (pnl (%backtest-entry-val entry '(pnl)))
           (symbol (%backtest-entry-val entry '(symbol)))
           (direction (%backtest-entry-val entry '(direction)))
           (entry-price (%backtest-entry-val entry '(entry_price entry-price)))
           (exit-price (%backtest-entry-val entry '(exit_price exit-price)))
           (sl (%backtest-entry-val entry '(sl)))
           (tp (%backtest-entry-val entry '(tp)))
           (volume (%backtest-entry-val entry '(volume)))
           (hold-time (%backtest-entry-val entry '(hold_time hold-time)))
           (rank (%backtest-entry-val entry '(rank)))
           (timeframe (%backtest-entry-val entry '(timeframe tf)))
           (category (%backtest-entry-val entry '(category)))
           (regime (%backtest-entry-val entry '(regime))))
      (execute-non-query
       "INSERT INTO backtest_trade_logs (
          request_id, strategy_name, timestamp, pnl, symbol, direction,
          entry_price, exit_price, sl, tp, volume, hold_time,
          rank, timeframe, category, regime, oos_kind
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
       request-id
       strategy-name
       timestamp
       pnl
       symbol
       (when direction (format nil "~a" direction))
       entry-price
       exit-price
       sl
       tp
       volume
       hold-time
       (when rank (format nil "~a" rank))
       timeframe
       (when category (format nil "~a" category))
       (when regime (format nil "~a" regime))
       oos-kind))))

(defun fetch-backtest-trades (strategy-name &key oos-kind)
  "Fetch persisted backtest trades for STRATEGY-NAME, optionally filtered by OOS-KIND."
  (if oos-kind
      (execute-to-list
       "SELECT request_id, strategy_name, timestamp, pnl, symbol, direction,
               entry_price, exit_price, sl, tp, volume, hold_time,
               rank, timeframe, category, regime, oos_kind
        FROM backtest_trade_logs
        WHERE strategy_name = ? AND oos_kind = ?
        ORDER BY timestamp"
       strategy-name
       oos-kind)
      (execute-to-list
       "SELECT request_id, strategy_name, timestamp, pnl, symbol, direction,
               entry_price, exit_price, sl, tp, volume, hold_time,
               rank, timeframe, category, regime, oos_kind
        FROM backtest_trade_logs
        WHERE strategy_name = ?
        ORDER BY timestamp"
       strategy-name)))

(defparameter *last-db-sync-time* 0)
(defparameter *db-sync-interval* 60
  "Minimum seconds between DB syncs for strategy metrics.")

(defun refresh-strategy-metrics-from-db (&key (force nil) since-timestamp)
  "Refresh in-memory strategy metrics from the DB (for multi-process coherence).
   If SINCE-TIMESTAMP is provided, only rows with updated_at >= SINCE-TIMESTAMP are fetched (when column exists)."
  (let ((now (get-universal-time)))
    (when (or force (> (- now *last-db-sync-time*) *db-sync-interval*))
      (setf *last-db-sync-time* now)
      (let ((updated 0)
            (max-updated 0))
        (labels ((apply-row (row)
                   (destructuring-bind (name sharpe pf wr trades maxdd rank oos cpcv-median cpcv-pass &optional updated-at) row
                     (setf max-updated (max max-updated (or updated-at 0)))
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
                         (incf updated))))))
          (handler-case
              (let* ((query (if since-timestamp
                                "SELECT name, sharpe, profit_factor, win_rate, trades, max_dd, rank, oos_sharpe, cpcv_median, cpcv_pass_rate, updated_at FROM strategies WHERE updated_at >= ?"
                                "SELECT name, sharpe, profit_factor, win_rate, trades, max_dd, rank, oos_sharpe, cpcv_median, cpcv_pass_rate, updated_at FROM strategies"))
                     (rows (if since-timestamp
                               (execute-to-list query since-timestamp)
                               (execute-to-list query))))
                (dolist (row rows) (apply-row row))
                (when (> updated 0)
                  (format t "[DB] 🔄 Synced metrics for ~d strategies~%" updated))
                (when (> max-updated 0)
                  (setf *last-db-sync-time* max-updated)))
            (error (e)
              (format t "[DB] ⚠️ Incremental sync fallback (~a)~%" e)
              (setf updated 0 max-updated 0)
              (dolist (row (execute-to-list
                            "SELECT name, sharpe, profit_factor, win_rate, trades, max_dd, rank, oos_sharpe, cpcv_median, cpcv_pass_rate FROM strategies"))
                (apply-row row))
              (when (> updated 0)
                (format t "[DB] 🔄 Synced metrics for ~d strategies (full scan fallback)~%" updated)))))))))

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
            (format t "[DB] 🪦 Migrated ~d graveyard patterns to SQL.~%" g-count)))))
    count)))

;; Graceful shutdown hook
(defun close-db ()
  (swimmy.core:close-db-connection))
