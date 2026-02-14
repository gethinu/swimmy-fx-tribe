;;; school-db.lisp - SQLite Persistence Layer for Swimmy School
;;; V49.8: Scaling the Strategy KB and Trade Logs (Refactored)

(in-package :swimmy.school)

(defparameter *disable-auto-migration* nil
  "When true, skip auto-migration in init-db (useful for tests).")

(defvar *db-initialized* nil
  "When true and an SQLite connection is already established, skip init-db DDL work.
   This prevents expensive/verbose schema checks from running in tight loops.")

(defun init-db ()
  "Initialize SQLite database and create tables if not exist."
  ;; NOTE: init-db is safe to call repeatedly, but doing full DDL checks in hot
  ;; paths is expensive and spams logs. Once a connection exists, assume schema
  ;; is already ensured for this process.
  (when (and *db-initialized* swimmy.core::*sqlite-conn*)
    (return-from init-db t))
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
      cpcv_median_pf REAL,
      cpcv_median_wr REAL,
      cpcv_median_maxdd REAL,
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
      (execute-non-query "ALTER TABLE strategies ADD COLUMN cpcv_median_pf REAL")
    (error () nil))
  (handler-case
      (execute-non-query "ALTER TABLE strategies ADD COLUMN cpcv_median_wr REAL")
    (error () nil))
  (handler-case
      (execute-non-query "ALTER TABLE strategies ADD COLUMN cpcv_median_maxdd REAL")
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

  ;; Table: Daily PnL Aggregates (per strategy per day)
  (execute-non-query
   "CREATE TABLE IF NOT EXISTS strategy_daily_pnl (
      strategy_name TEXT NOT NULL,
      trade_date TEXT NOT NULL,
      pnl_sum REAL NOT NULL,
      trade_count INTEGER NOT NULL,
      updated_at INTEGER NOT NULL,
      PRIMARY KEY (strategy_name, trade_date)
    )")

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

  ;; Table: Pair Strategies (Composite)
  (execute-non-query
   "CREATE TABLE IF NOT EXISTS pair_strategies (
      pair_id TEXT PRIMARY KEY,
      strategy_a TEXT,
      strategy_b TEXT,
      weight_a REAL,
      weight_b REAL,
      symbol TEXT,
      timeframe INTEGER,
      sharpe REAL,
      profit_factor REAL,
      score REAL,
      corr REAL,
      rank TEXT,
      oos_sharpe REAL,
      cpcv_median REAL,
      cpcv_pass_rate REAL,
      last_updated INTEGER
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

  ;; Table: DryRun Slippage Samples (Stage2 persistence)
  (execute-non-query
   "CREATE TABLE IF NOT EXISTS dryrun_slippage_samples (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      strategy_name TEXT NOT NULL,
      sample_abs_pips REAL NOT NULL,
      observed_at INTEGER NOT NULL
    )")

  ;; Indices for fast draft/selection
  (execute-non-query
   "CREATE INDEX IF NOT EXISTS idx_strategies_rank_sharpe
      ON strategies(rank, sharpe)")
  (execute-non-query
   "CREATE INDEX IF NOT EXISTS idx_strategies_category_rank
      ON strategies(timeframe, direction, symbol, rank)")
  (execute-non-query "CREATE INDEX IF NOT EXISTS idx_trade_name ON trade_logs(strategy_name)")
  (execute-non-query
   "CREATE INDEX IF NOT EXISTS idx_dryrun_slippage_strategy_id
      ON dryrun_slippage_samples(strategy_name, id DESC)")

  (setf *db-initialized* t)
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

(defparameter *allow-rank-regression-write* nil
  "When T, upsert can persist explicit active-rank regression (e.g., A->B).")

(defun upsert-strategy (strat)
  "Save or update strategy in SQL."
  (unless (strategy-hash strat)
    (setf (strategy-hash strat) (calculate-strategy-hash strat)))
  (let* ((name (strategy-name strat))
         (existing-row (ignore-errors (first (execute-to-list
                                              "SELECT sharpe, profit_factor, win_rate, trades, max_dd,
                                                      cpcv_median, cpcv_median_pf, cpcv_median_wr,
                                                      cpcv_median_maxdd, cpcv_pass_rate, rank
                                                 FROM strategies WHERE name=?"
                                              name))))
         (db-sharpe (if existing-row (or (first existing-row) 0.0) 0.0))
         (db-pf (if existing-row (or (second existing-row) 0.0) 0.0))
         (db-wr (if existing-row (or (third existing-row) 0.0) 0.0))
         (db-trades (if existing-row (or (fourth existing-row) 0) 0))
         (db-max-dd (if existing-row (or (fifth existing-row) 0.0) 0.0))
         (db-cpcv-median (if existing-row (or (sixth existing-row) 0.0) 0.0))
         (db-cpcv-pf (if existing-row (or (seventh existing-row) 0.0) 0.0))
         (db-cpcv-wr (if existing-row (or (eighth existing-row) 0.0) 0.0))
         (db-cpcv-maxdd (if existing-row (or (ninth existing-row) 0.0) 0.0))
         (db-cpcv-pass (if existing-row (or (tenth existing-row) 0.0) 0.0))
         (db-rank-raw (if existing-row (nth 10 existing-row) nil))
         (cur-sharpe (or (strategy-sharpe strat) 0.0))
         (cur-pf (or (strategy-profit-factor strat) 0.0))
         (cur-wr (or (strategy-win-rate strat) 0.0))
         (cur-trades (or (strategy-trades strat) 0))
         (cur-max-dd (or (strategy-max-dd strat) 0.0))
         (cur-cpcv-median (or (strategy-cpcv-median-sharpe strat) 0.0))
         (cur-cpcv-pf (or (strategy-cpcv-median-pf strat) 0.0))
         (cur-cpcv-wr (or (strategy-cpcv-median-wr strat) 0.0))
         (cur-cpcv-maxdd (or (strategy-cpcv-median-maxdd strat) 0.0))
         (cur-cpcv-pass (or (strategy-cpcv-pass-rate strat) 0.0))
         (incoming-rank (strategy-rank strat))
         (updated-at (get-universal-time)))
    (labels ((normalize-rank (rank)
               (cond
                 ((null rank) "NIL")
                 ((stringp rank)
                  (let ((s (string-upcase (string-trim '(#\Space #\Tab #\Newline) rank))))
                    (if (and (> (length s) 0) (char= (char s 0) #\:))
                        (subseq s 1)
                        s)))
                 ((symbolp rank) (normalize-rank (symbol-name rank)))
                 (t (normalize-rank (format nil "~a" rank)))))
             (archive-rank-p (rank)
               (member (normalize-rank rank) '("GRAVEYARD" "RETIRED") :test #'string=))
             (active-rank-level (rank)
               (let ((norm (normalize-rank rank)))
                 (cond
                   ((string= norm "B") 1)
                   ((string= norm "A") 2)
                   ((string= norm "S") 3)
                   ((or (string= norm "LEGEND")
                        (string= norm "LEGEND-ARCHIVE")) 4)
                   (t nil))))
             (rank->db-string (rank)
               (let ((norm (normalize-rank rank)))
                 (if (string= norm "NIL")
                     "NIL"
                     (format nil ":~a" norm)))))
      ;; Guard against rank drift from stale in-memory objects:
      ;; if incoming rank is NIL but DB already has a non-NIL rank, keep DB rank.
      (when (and existing-row
                 (null incoming-rank)
                 (not (string= (normalize-rank db-rank-raw) "NIL")))
        (multiple-value-bind (db-rank db-rank-ok)
            (%parse-rank-safe (rank->db-string db-rank-raw))
          (when db-rank-ok
            (setf incoming-rank db-rank)
            (setf (strategy-rank strat) incoming-rank))))
      ;; Guard against accidental resurrection: keep archived rank unless an archived rank is explicitly provided.
      (when (and existing-row
                 (archive-rank-p db-rank-raw)
                 (not (archive-rank-p incoming-rank)))
        (setf incoming-rank (swimmy.core:safe-read-sexp (rank->db-string db-rank-raw) :package :swimmy.school))
        (setf (strategy-rank strat) incoming-rank))
      ;; Guard against stale active-rank regression from old in-memory objects.
      ;; Explicit demotions should set *allow-rank-regression-write* while calling upsert.
      (let ((db-level (and existing-row (active-rank-level db-rank-raw)))
            (incoming-level (active-rank-level incoming-rank)))
        (when (and existing-row
                   (not *allow-rank-regression-write*)
                   db-level
                   incoming-level
                   (< incoming-level db-level))
          (multiple-value-bind (db-rank db-rank-ok)
              (%parse-rank-safe (rank->db-string db-rank-raw))
            (when db-rank-ok
              (setf incoming-rank db-rank)
              (setf (strategy-rank strat) incoming-rank))))))
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
    ;; Preserve non-zero DB CPCV metrics when in-memory values are all zero.
    (let ((cur-cpcv-empty (and (zerop cur-cpcv-median)
                               (zerop cur-cpcv-pf)
                               (zerop cur-cpcv-wr)
                               (zerop cur-cpcv-maxdd)
                               (zerop cur-cpcv-pass)))
          (db-has-cpcv (or (not (zerop db-cpcv-median))
                           (not (zerop db-cpcv-pf))
                           (not (zerop db-cpcv-wr))
                           (not (zerop db-cpcv-maxdd))
                           (not (zerop db-cpcv-pass)))))
      (when (and existing-row cur-cpcv-empty db-has-cpcv)
        (setf cur-cpcv-median db-cpcv-median
              cur-cpcv-pf db-cpcv-pf
              cur-cpcv-wr db-cpcv-wr
              cur-cpcv-maxdd db-cpcv-maxdd
              cur-cpcv-pass db-cpcv-pass)))
    (handler-case
        (execute-non-query
         "INSERT OR REPLACE INTO strategies (
          name, indicators, entry, exit, sl, tp, volume,
          sharpe, profit_factor, win_rate, trades, max_dd,
          category, timeframe, generation, rank, symbol, direction, hash,
          oos_sharpe, cpcv_median, cpcv_median_pf, cpcv_median_wr, cpcv_median_maxdd, cpcv_pass_rate, data_sexp, updated_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
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
         (format nil "~s" incoming-rank) ; Store as ":RANK"
         (or (strategy-symbol strat) "USDJPY")
         (format nil "~a" (strategy-direction strat))
         (strategy-hash strat)
         (or (strategy-oos-sharpe strat) 0.0)
         cur-cpcv-median
         cur-cpcv-pf
         cur-cpcv-wr
         cur-cpcv-maxdd
         cur-cpcv-pass
         (format nil "~s" strat)
         updated-at) ; Store full serialized object as backup
      (error (e) (format t "[DB] ❌ Upsert error for ~a: ~a~%" (strategy-name strat) e))))
  )

(defun update-cpcv-metrics-by-name (name median median-pf median-wr median-maxdd pass-rate
                                    &key request-id)
  "Update CPCV metrics for a strategy row by name (when in-memory object missing)."
  (when (and name (stringp name) (not (string= name "")))
    (let ((updated-at (get-universal-time)))
      (handler-case
          (execute-non-query
           "UPDATE strategies
              SET cpcv_median = ?,
                  cpcv_median_pf = ?,
                  cpcv_median_wr = ?,
                  cpcv_median_maxdd = ?,
                  cpcv_pass_rate = ?,
                  updated_at = ?
            WHERE name = ?"
           (float (or median 0.0))
           (float (or median-pf 0.0))
           (float (or median-wr 0.0))
           (float (or median-maxdd 0.0))
           (float (or pass-rate 0.0))
           updated-at
           name)
        (error (e)
          (format t "[DB] ❌ CPCV update error for ~a (req=~a): ~a~%"
                  name (or request-id "N/A") e)
          nil)))))

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

(defun refresh-strategy-daily-pnl ()
  "Aggregate trade_logs into daily PnL (JST localtime)."
  (init-db)
  (let ((now (get-universal-time)))
    (execute-non-query
     "INSERT INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
      SELECT strategy_name,
             date(datetime(timestamp - 2208988800, 'unixepoch', 'localtime')) AS trade_date,
             SUM(pnl) AS pnl_sum,
             COUNT(*) AS trade_count,
             ? AS updated_at
        FROM trade_logs
       GROUP BY strategy_name, trade_date
      ON CONFLICT(strategy_name, trade_date)
      DO UPDATE SET pnl_sum=excluded.pnl_sum,
                    trade_count=excluded.trade_count,
                    updated_at=excluded.updated_at"
     now)))

(defun fetch-strategy-daily-pnl (strategy-name &key (limit 30))
  "Fetch last N daily pnl rows (date, pnl_sum) for a strategy."
  (init-db)
  (execute-to-list
   "SELECT trade_date, pnl_sum FROM strategy_daily_pnl
     WHERE strategy_name = ?
     ORDER BY trade_date DESC
     LIMIT ?"
   strategy-name limit))

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
     (or
      ;; Fast path: exact key match for alists in the current package.
      (loop for k in keys
            for cell = (and (listp entry) (assoc k entry))
            when cell do (return (cdr cell)))
      ;; Fallback: match by key name (package-insensitive), to accept trade_list output
      ;; read in a different package (e.g., swimmy.main via safe-read-sexp).
      (when (listp entry)
        (let ((wanted (remove nil (mapcar (lambda (k)
                                            (cond
                                              ((keywordp k) (string-upcase (symbol-name k)))
                                              ((symbolp k) (string-upcase (symbol-name k)))
                                              ((stringp k) (string-upcase k))
                                              (t nil)))
                                          keys))))
          (block found
            (dolist (cell entry)
              (when (consp cell)
                (let* ((ck (car cell))
                       (ck-name (cond
                                  ((keywordp ck) (string-upcase (symbol-name ck)))
                                  ((symbolp ck) (string-upcase (symbol-name ck)))
                                  ((stringp ck) (string-upcase ck))
                                  (t nil))))
                  (when (and ck-name (member ck-name wanted :test #'string=))
                    (return-from found (cdr cell))))))
            nil)))))))

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

(defun backtest-rows->trade-list (rows)
  "Convert backtest_trade_logs rows into trade_list alists."
  (mapcar (lambda (row)
            (destructuring-bind (_rid _name ts pnl sym _dir _ep _xp _sl _tp _vol _hold _rank _tf _cat _reg _kind) row
              (declare (ignore _rid _name _dir _ep _xp _sl _tp _vol _hold _rank _tf _cat _reg _kind))
              (list (cons 'timestamp ts) (cons 'pnl pnl) (cons 'symbol sym))))
          rows))

(defun upsert-pair-strategy (pair)
  "Save or update pair strategy row in SQL."
  (let ((updated-at (get-universal-time)))
    (execute-non-query
     "INSERT OR REPLACE INTO pair_strategies (
        pair_id, strategy_a, strategy_b, weight_a, weight_b,
        symbol, timeframe, sharpe, profit_factor, score, corr,
        rank, oos_sharpe, cpcv_median, cpcv_pass_rate, last_updated
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (getf pair :pair-id)
     (getf pair :strategy-a)
     (getf pair :strategy-b)
     (getf pair :weight-a)
     (getf pair :weight-b)
     (getf pair :symbol)
     (getf pair :timeframe)
     (getf pair :sharpe)
     (getf pair :profit-factor)
     (getf pair :score)
     (getf pair :corr)
     (format nil "~s" (getf pair :rank))
     (getf pair :oos-sharpe)
     (getf pair :cpcv-median)
     (getf pair :cpcv-pass-rate)
     updated-at)))

(defun fetch-pair-strategy (pair-id)
  "Fetch a pair strategy row by PAIR-ID."
  (let ((row (first (execute-to-list
                     "SELECT pair_id, strategy_a, strategy_b, weight_a, weight_b,
                             symbol, timeframe, sharpe, profit_factor, score, corr,
                             rank, oos_sharpe, cpcv_median, cpcv_pass_rate, last_updated
                      FROM pair_strategies WHERE pair_id = ?"
                     pair-id))))
    (when row
      (destructuring-bind (pid a b wa wb sym tf sharpe pf score corr rank oos cpcv pass updated) row
        (list :pair-id pid
              :strategy-a a
              :strategy-b b
              :weight-a wa
              :weight-b wb
              :symbol sym
              :timeframe tf
              :sharpe sharpe
              :profit-factor pf
              :score score
              :corr corr
              :rank (and rank (swimmy.core:safe-read-sexp rank :package :swimmy.school))
              :oos-sharpe oos
              :cpcv-median cpcv
              :cpcv-pass-rate pass
              :last-updated updated)))))

(defun fetch-pair-strategies ()
  "Fetch all pair strategy rows."
  (mapcar (lambda (row)
            (destructuring-bind (pid a b wa wb sym tf sharpe pf score corr rank oos cpcv pass updated) row
              (list :pair-id pid
                    :strategy-a a
                    :strategy-b b
                    :weight-a wa
                    :weight-b wb
                    :symbol sym
                    :timeframe tf
                    :sharpe sharpe
                    :profit-factor pf
                    :score score
                    :corr corr
                    :rank (and rank (swimmy.core:safe-read-sexp rank :package :swimmy.school))
                    :oos-sharpe oos
                    :cpcv-median cpcv
                    :cpcv-pass-rate pass
                    :last-updated updated)))
          (execute-to-list
           "SELECT pair_id, strategy_a, strategy_b, weight_a, weight_b,
                   symbol, timeframe, sharpe, profit_factor, score, corr,
                   rank, oos_sharpe, cpcv_median, cpcv_pass_rate, last_updated
            FROM pair_strategies")))

(defun record-dryrun-slippage-sample (strategy-name
                                      sample-abs-pips
                                      &key
                                        (max-samples 200)
                                        max-age-seconds
                                        observed-at)
  "Persist one absolute slippage sample.
Prune per-strategy history by MAX-SAMPLES and optional MAX-AGE-SECONDS."
  (when (and strategy-name (numberp sample-abs-pips))
    (let* ((ts (if (numberp observed-at)
                   (truncate observed-at)
                   (get-universal-time)))
           (retention-cutoff (and (numberp max-age-seconds)
                                  (> max-age-seconds 0)
                                  (- ts (truncate max-age-seconds)))))
      (handler-case
          (progn
            (execute-non-query
             "INSERT INTO dryrun_slippage_samples (strategy_name, sample_abs_pips, observed_at)
              VALUES (?, ?, ?)"
             strategy-name
             (float sample-abs-pips 0.0)
             ts)
            (when retention-cutoff
              (execute-non-query
               "DELETE FROM dryrun_slippage_samples
                 WHERE strategy_name = ?
                   AND observed_at < ?"
               strategy-name
               retention-cutoff))
            (when (and (integerp max-samples) (> max-samples 0))
              (execute-non-query
               "DELETE FROM dryrun_slippage_samples
                 WHERE strategy_name = ?
                   AND id NOT IN (
                       SELECT id
                         FROM dryrun_slippage_samples
                        WHERE strategy_name = ?
                        ORDER BY id DESC
                        LIMIT ?
                   )"
               strategy-name
               strategy-name
               max-samples))
            t)
        (error (e)
          (format t "[DB] ⚠️ Failed to persist dryrun slippage for ~a: ~a~%"
                  strategy-name e)
          nil)))))

(defun fetch-dryrun-slippage-samples (strategy-name &key (limit 200) max-age-seconds)
  "Fetch recent absolute slippage samples for STRATEGY-NAME."
  (if (or (null strategy-name) (not (stringp strategy-name)))
      '()
      (handler-case
          (let* ((retention-cutoff (and (numberp max-age-seconds)
                                        (> max-age-seconds 0)
                                        (- (get-universal-time) (truncate max-age-seconds))))
                 (rows (if retention-cutoff
                           (execute-to-list
                            "SELECT sample_abs_pips
                               FROM dryrun_slippage_samples
                              WHERE strategy_name = ?
                                AND observed_at >= ?
                              ORDER BY id DESC
                              LIMIT ?"
                            strategy-name
                            retention-cutoff
                            limit)
                           (execute-to-list
                            "SELECT sample_abs_pips
                               FROM dryrun_slippage_samples
                              WHERE strategy_name = ?
                              ORDER BY id DESC
                              LIMIT ?"
                            strategy-name
                            limit))))
            (remove-if-not #'numberp (mapcar #'first rows)))
        (error (e)
          (format t "[DB] ⚠️ Failed to fetch dryrun slippage for ~a: ~a~%"
                  strategy-name e)
          '()))))

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
            (max-updated 0)
            (processed 0)
            (log-every 50000)
            (sync-start (get-internal-real-time))
            (strategy-index (make-hash-table :test 'equal)))
        (dolist (strat *strategy-knowledge-base*)
          (when (and strat (strategy-name strat))
            (setf (gethash (strategy-name strat) strategy-index) strat)))
        (when (and (boundp 'swimmy.globals:*evolved-strategies*)
                   swimmy.globals:*evolved-strategies*)
          (dolist (strat swimmy.globals:*evolved-strategies*)
            (when (and strat (strategy-name strat))
              (setf (gethash (strategy-name strat) strategy-index) strat))))
        (labels ((apply-row (row)
                 (destructuring-bind (name sharpe pf wr trades maxdd rank oos cpcv-median cpcv-median-pf cpcv-median-wr cpcv-median-maxdd cpcv-pass &optional updated-at) row
                     (setf max-updated (max max-updated (or updated-at 0)))
                     (let ((strat (gethash name strategy-index)))
                       (when strat
                         (when sharpe (setf (strategy-sharpe strat) (float sharpe 0.0)))
                         (when pf (setf (strategy-profit-factor strat) (float pf 0.0)))
                         (when wr (setf (strategy-win-rate strat) (float wr 0.0)))
                         (when trades (setf (strategy-trades strat) trades))
                         (when maxdd (setf (strategy-max-dd strat) (float maxdd 0.0)))
                         (when oos (setf (strategy-oos-sharpe strat) (float oos 0.0)))
                         (when cpcv-median (setf (strategy-cpcv-median-sharpe strat) (float cpcv-median 0.0)))
                         (when cpcv-median-pf (setf (strategy-cpcv-median-pf strat) (float cpcv-median-pf 0.0)))
                         (when cpcv-median-wr (setf (strategy-cpcv-median-wr strat) (float cpcv-median-wr 0.0)))
                         (when cpcv-median-maxdd (setf (strategy-cpcv-median-maxdd strat) (float cpcv-median-maxdd 0.0)))
                         (when cpcv-pass (setf (strategy-cpcv-pass-rate strat) (float cpcv-pass 0.0)))
                         (when (and rank (stringp rank))
                           (multiple-value-bind (rank-sym ok) (%parse-rank-safe rank)
                             (when ok
                               (setf (strategy-rank strat) rank-sym))))
                         (incf updated))))))
          (handler-case
              (let* ((query (if since-timestamp
                                "SELECT name, sharpe, profit_factor, win_rate, trades, max_dd, rank, oos_sharpe, cpcv_median, cpcv_median_pf, cpcv_median_wr, cpcv_median_maxdd, cpcv_pass_rate, updated_at FROM strategies WHERE updated_at >= ?"
                                "SELECT name, sharpe, profit_factor, win_rate, trades, max_dd, rank, oos_sharpe, cpcv_median, cpcv_median_pf, cpcv_median_wr, cpcv_median_maxdd, cpcv_pass_rate, updated_at FROM strategies")))
                (format t "[DB] 🔍 Sync query start (~a)~%"
                        (if since-timestamp "incremental" "full"))
                (finish-output)
                (let* ((t0 (get-internal-real-time))
                       (rows (if since-timestamp
                                 (execute-to-list query since-timestamp)
                                 (execute-to-list query)))
                       (elapsed (/ (- (get-internal-real-time) t0)
                                   internal-time-units-per-second)))
                  (format t "[DB] 🔍 Sync query end (~,2fs)~%" elapsed)
                  (finish-output)
                  (dolist (row rows)
                    (apply-row row)
                    (incf processed)
                    (when (and (> processed 0) (zerop (mod processed log-every)))
                      (let ((p-elapsed (/ (- (get-internal-real-time) sync-start)
                                          internal-time-units-per-second)))
                        (format t "[DB] 🔍 Sync progress: ~d rows (~,2fs)~%" processed p-elapsed)
                        (finish-output)))))
                (when (> updated 0)
                  (format t "[DB] 🔄 Synced metrics for ~d strategies~%" updated))
                (when (> max-updated 0)
                  (setf *last-db-sync-time* max-updated)))
            (error (e)
              (format t "[DB] ⚠️ Incremental sync fallback (~a)~%" e)
              (setf updated 0 max-updated 0 processed 0 sync-start (get-internal-real-time))
              (format t "[DB] 🔍 Sync query start (full fallback)~%")
              (finish-output)
              (let* ((t0 (get-internal-real-time))
                     (rows (execute-to-list
                            "SELECT name, sharpe, profit_factor, win_rate, trades, max_dd, rank, oos_sharpe, cpcv_median, cpcv_median_pf, cpcv_median_wr, cpcv_median_maxdd, cpcv_pass_rate FROM strategies"))
                     (elapsed (/ (- (get-internal-real-time) t0)
                                 internal-time-units-per-second)))
                (format t "[DB] 🔍 Sync query end (~,2fs)~%" elapsed)
                (finish-output)
                (dolist (row rows)
                  (apply-row row)
                  (incf processed)
                  (when (and (> processed 0) (zerop (mod processed log-every)))
                    (let ((p-elapsed (/ (- (get-internal-real-time) sync-start)
                                        internal-time-units-per-second)))
                      (format t "[DB] 🔍 Sync progress: ~d rows (~,2fs)~%" processed p-elapsed)
                      (finish-output)))))
              (when (> updated 0)
                (format t "[DB] 🔄 Synced metrics for ~d strategies (full scan fallback)~%" updated)))))))))

(defun %migrate-graveyard-patterns (graveyard-path)
  "Migrate graveyard.sexp patterns into SQL and continue on malformed lines."
  (let ((g-count 0)
        (g-skipped 0)
        (line-no 0))
    (when (probe-file graveyard-path)
      (with-open-file (in graveyard-path :direction :input :if-does-not-exist nil)
        (with-transaction
          (loop for line = (read-line in nil nil)
                while line
                do
                   (incf line-no)
                   (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
                     (unless (string= trimmed "")
                       (let ((p (swimmy.core:safe-read-sexp trimmed :package :swimmy.school)))
                         (cond
                           ((and p (listp p) (getf p :name))
                            (handler-case
                                (let* ((g-name (format nil "GY-~a" (getf p :name)))
                                       (fake-strat (make-strategy :indicators (getf p :indicators)
                                                                  :entry (getf p :entry)
                                                                  :exit (getf p :exit)))
                                       (hash (calculate-strategy-hash fake-strat)))
                                  (execute-non-query
                                   "INSERT OR REPLACE INTO strategies (name, rank, hash, data_sexp) VALUES (?, ?, ?, ?)"
                                   g-name ":GRAVEYARD" hash (format nil "~s" p))
                                  (incf g-count))
                              (error ()
                                (incf g-skipped))))
                           (t
                            (incf g-skipped)
                            (when (<= g-skipped 5)
                              (format t "[DB] ⚠️ Skip malformed graveyard line ~d~%" line-no)))))))))
      (format t "[DB] 🪦 Migrated ~d graveyard patterns to SQL (~d skipped).~%" g-count g-skipped))
    (values g-count g-skipped))))

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
    (%migrate-graveyard-patterns "data/memory/graveyard.sexp")
    count)))

;; Graceful shutdown hook
(defun close-db ()
  (swimmy.core:close-db-connection))
