;;; school-db.lisp - SQLite Persistence Layer for Swimmy School
;;; V49.8: Scaling the Strategy KB and Trade Logs (Refactored)

(in-package :swimmy.school)

(defparameter *disable-auto-migration* nil
  "When true, skip auto-migration in init-db (useful for tests).")

(defvar *db-initialized* nil
  "When true and an SQLite connection is already established, skip init-db DDL work.
   This prevents expensive/verbose schema checks from running in tight loops.")

(defparameter *disable-timeframe-backfill* nil
  "When true, skip automatic DB timeframe backfill in init-db.")

(defparameter *timeframe-backfill-batch-size* 1000
  "Batch size for strategy timeframe backfill updates.")

(defun %db-timeframe-token->minutes (tf &optional default)
  "Normalize timeframe token (integer/string) to minutes(int)."
  (labels ((all-digits-p (s)
             (and (stringp s)
                  (> (length s) 0)
                  (loop for ch across s always (digit-char-p ch))))
           (parse-int (s fallback)
             (handler-case (parse-integer s) (error () fallback))))
    (cond
      ((numberp tf)
       (let ((m (round tf)))
         (if (> m 0) m default)))
      ((stringp tf)
       (let* ((up (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return) tf))))
         (cond
           ((or (string= up "") (string= up "NIL")) default)
           ((or (string= up "MN") (string= up "MN1")) 43200)
           ((and (>= (length up) 2)
                 (char= (char up 0) #\M)
                 (all-digits-p (subseq up 1)))
            (max 1 (parse-int (subseq up 1) 1)))
           ((and (>= (length up) 2)
                 (char= (char up 0) #\H)
                 (all-digits-p (subseq up 1)))
            (* 60 (max 1 (parse-int (subseq up 1) 1))))
           ((and (>= (length up) 2)
                 (char= (char up 0) #\D)
                 (all-digits-p (subseq up 1)))
            (* 1440 (max 1 (parse-int (subseq up 1) 1))))
           ((and (>= (length up) 2)
                 (char= (char up 0) #\W)
                 (all-digits-p (subseq up 1)))
            (* 10080 (max 1 (parse-int (subseq up 1) 1))))
           ((all-digits-p up)
            (max 1 (parse-int up 1)))
           (t default))))
      (t default))))

(defun %extract-timeframe-token-from-data-sexp (sexp-str)
  "Best-effort extraction of :TIMEFRAME token from serialized strategy SEXP."
  (when (and (stringp sexp-str) (> (length sexp-str) 0))
    (let* ((needle ":TIMEFRAME")
           (up (string-upcase sexp-str))
           (pos (search needle up))
           (len (length sexp-str)))
      (when pos
        (let ((i (+ pos (length needle))))
          (loop while (and (< i len)
                           (find (char sexp-str i) '(#\Space #\Tab #\Newline #\Return)))
                do (incf i))
          (when (< i len)
            (cond
              ;; :TIMEFRAME "H1"
              ((char= (char sexp-str i) #\")
               (let ((j (position #\" sexp-str :start (1+ i))))
                 (when j
                   (subseq sexp-str (1+ i) j))))
              ;; :TIMEFRAME #A((2) BASE-CHAR . "H1")
              ((char= (char sexp-str i) #\#)
               (let ((q1 (position #\" sexp-str :start i)))
                 (when q1
                   (let ((q2 (position #\" sexp-str :start (1+ q1))))
                     (when q2
                       (subseq sexp-str (1+ q1) q2))))))
              ;; :TIMEFRAME 300 / H1 / MN1 / NIL ...
              (t
               (let ((j i))
                 (loop while (and (< j len)
                                  (not (find (char sexp-str j)
                                             '(#\Space #\Tab #\Newline #\Return #\) #\( #\"))))
                       do (incf j))
                 (when (> j i)
                   (subseq sexp-str i j)))))))))))

(defun backfill-strategy-timeframes-to-minutes (&key force (batch-size *timeframe-backfill-batch-size*))
  "Backfill mixed/legacy DB timeframe values to minutes(int).
Returns plist stats: :scanned :updated :rewritten :defaulted :remaining."
  (init-db)
  (let* ((target-count (execute-single
                        "SELECT count(*) FROM strategies
                          WHERE timeframe IS NULL OR typeof(timeframe)='text'")))
    (when (and (not force) (or (null target-count) (<= target-count 0)))
      (return-from backfill-strategy-timeframes-to-minutes
        (list :scanned 0 :updated 0 :rewritten 0 :defaulted 0 :remaining 0)))
    (let ((last-rowid 0)
          (scanned 0)
          (updated 0)
          (rewritten 0)
          (defaulted 0))
      (loop
        for rows = (execute-to-list
                    "SELECT rowid, timeframe, data_sexp
                       FROM strategies
                      WHERE (timeframe IS NULL OR typeof(timeframe)='text')
                        AND rowid > ?
                      ORDER BY rowid
                      LIMIT ?"
                    last-rowid batch-size)
        while rows
        do (with-transaction
             (dolist (row rows)
               (destructuring-bind (rowid tf-col data-sexp) row
                 (setf last-rowid rowid)
                 (incf scanned)
                 (let* ((col-min (%db-timeframe-token->minutes tf-col nil))
                        (sexp-token (and (null col-min)
                                         (%extract-timeframe-token-from-data-sexp data-sexp)))
                        (sexp-min (%db-timeframe-token->minutes sexp-token nil))
                        (new-tf (or col-min sexp-min 1))
                        (needs-default (and (null col-min) (null sexp-min)))
                        (rewrite-p (and (stringp data-sexp)
                                        (or (search ":TIMEFRAME \"" data-sexp :test #'char-equal)
                                            (search ":TIMEFRAME #A(" data-sexp :test #'char-equal))))
                        (new-data-sexp nil))
                   (when needs-default
                     (incf defaulted))
                   (when rewrite-p
                     (let ((obj (ignore-errors
                                  (swimmy.core:safe-read-sexp data-sexp :package :swimmy.school))))
                       (when (and obj (strategy-p obj))
                         (setf (strategy-timeframe obj) new-tf
                               new-data-sexp (format nil "~s" obj)))))
                   (if new-data-sexp
                       (progn
                         (execute-non-query
                          "UPDATE strategies
                              SET timeframe = ?, data_sexp = ?, updated_at = ?
                            WHERE rowid = ?"
                          new-tf new-data-sexp (get-universal-time) rowid)
                         (incf rewritten))
                       (execute-non-query
                        "UPDATE strategies
                            SET timeframe = ?, updated_at = ?
                          WHERE rowid = ?"
                        new-tf (get-universal-time) rowid))
                   (incf updated))))))
      (let ((remaining (execute-single
                        "SELECT count(*) FROM strategies
                          WHERE timeframe IS NULL OR typeof(timeframe)='text'")))
        (format t "[DB] ♻️ Timeframe backfill complete: scanned=~d updated=~d rewritten=~d defaulted=~d remaining=~d~%"
                scanned updated rewritten defaulted remaining)
        (list :scanned scanned
              :updated updated
              :rewritten rewritten
              :defaulted defaulted
              :remaining remaining)))))

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
      pair_id TEXT,
      execution_mode TEXT NOT NULL DEFAULT 'LIVE'
    )")
  (handler-case
      (execute-non-query "ALTER TABLE trade_logs ADD COLUMN pair_id TEXT")
    (error () nil))
  (handler-case
      (execute-non-query "ALTER TABLE trade_logs ADD COLUMN execution_mode TEXT NOT NULL DEFAULT 'LIVE'")
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

  ;; Table: Deployment Gate Status (Go/No-Go for live deployment)
  (execute-non-query
   "CREATE TABLE IF NOT EXISTS deployment_gate_status (
      strategy_name TEXT PRIMARY KEY,
      research_passed INTEGER NOT NULL DEFAULT 0,
      oos_passed INTEGER NOT NULL DEFAULT 0,
      oos_window_days INTEGER,
      forward_start INTEGER,
      forward_days INTEGER,
      forward_trades INTEGER,
      forward_sharpe REAL,
      forward_pf REAL,
      decision TEXT,
      reason TEXT,
      updated_at INTEGER NOT NULL
    )")

  ;; Indices for fast draft/selection
  (execute-non-query
   "CREATE INDEX IF NOT EXISTS idx_strategies_rank_sharpe
      ON strategies(rank, sharpe)")
  (execute-non-query
   "CREATE INDEX IF NOT EXISTS idx_strategies_category_rank
      ON strategies(timeframe, direction, symbol, rank)")
  (execute-non-query "CREATE INDEX IF NOT EXISTS idx_trade_name ON trade_logs(strategy_name)")
  (execute-non-query "CREATE INDEX IF NOT EXISTS idx_trade_name_mode ON trade_logs(strategy_name, execution_mode)")
  (execute-non-query
   "CREATE INDEX IF NOT EXISTS idx_backtest_trade_strategy
      ON backtest_trade_logs(strategy_name)")
  (execute-non-query
   "CREATE INDEX IF NOT EXISTS idx_backtest_trade_strategy_kind
      ON backtest_trade_logs(strategy_name, oos_kind)")
  (handler-case
      (execute-non-query
       "CREATE UNIQUE INDEX IF NOT EXISTS idx_backtest_trade_dedupe
          ON backtest_trade_logs(request_id, strategy_name, timestamp, oos_kind)")
    (error (e)
      ;; If existing data has duplicates, keep runtime alive and rely on INSERT guard.
      (format t "[DB] ⚠️ backtest_trade_logs dedupe index skipped: ~a~%" e)))
  (execute-non-query
   "CREATE INDEX IF NOT EXISTS idx_dryrun_slippage_strategy_id
      ON dryrun_slippage_samples(strategy_name, id DESC)")
  (execute-non-query
   "CREATE INDEX IF NOT EXISTS idx_deployment_gate_decision
      ON deployment_gate_status(decision)")

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
      (error (e) (format t "[DB] ⚠️ Auto-migration check failed: ~a~%" e))))

  ;; V50.6 TF Unified: backfill legacy/null/text timeframe rows to minutes(int).
  (unless *disable-timeframe-backfill*
    (handler-case
        (backfill-strategy-timeframes-to-minutes)
      (error (e)
        (format t "[DB] ⚠️ Timeframe backfill skipped due to error: ~a~%" e)))))

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
(defparameter *allow-archived-rank-resurrection-write* nil
  "When T, upsert can persist explicit archived->active rank resurrection writes.")

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
                 (not (archive-rank-p incoming-rank))
                 (not *allow-archived-rank-resurrection-write*))
        (setf incoming-rank (swimmy.core:safe-read-sexp (rank->db-string db-rank-raw) :package :swimmy.school))
        (setf (strategy-rank strat) incoming-rank))
      ;; Guard against stale active-rank regression from old in-memory objects.
      ;; Explicit demotions should set *allow-rank-regression-write* while calling upsert.
      ;; Also block accidental active->archive writes from stale duplicates.
      (let ((db-level (and existing-row (active-rank-level db-rank-raw)))
            (incoming-level (active-rank-level incoming-rank))
            (incoming-archive-p (archive-rank-p incoming-rank)))
        (when (and existing-row
                   (not *allow-rank-regression-write*)
                   db-level
                   (or (and incoming-level
                            (< incoming-level db-level))
                       incoming-archive-p))
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
  (labels ((normalize-execution-mode (mode)
             (let ((token (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                      (format nil "~a" (or mode :live))))))
               (cond
                 ((or (string= token "SHADOW") (string= token ":SHADOW")) "SHADOW")
                 (t "LIVE")))))
  (execute-non-query
   "INSERT INTO trade_logs (timestamp, strategy_name, symbol, direction, category, regime, pnl, hold_time, pair_id, execution_mode)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
   (trade-record-timestamp record)
   (trade-record-strategy-name record)
   (trade-record-symbol record)
   (format nil "~a" (trade-record-direction record))
   (format nil "~a" (trade-record-category record))
   (format nil "~a" (trade-record-regime record))
   (trade-record-pnl record)
   (trade-record-hold-time record)
   (trade-record-pair-id record)
   (normalize-execution-mode (ignore-errors (trade-record-execution-mode record))))))

(defun refresh-strategy-daily-pnl ()
  "Aggregate LIVE trade_logs into daily PnL (JST localtime)."
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
       WHERE UPPER(COALESCE(execution_mode, 'LIVE')) = 'LIVE'
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

(defun %normalize-execution-mode-token (mode)
  "Normalize MODE token into SQL mode string (LIVE/SHADOW)."
  (let ((token (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           (format nil "~a" (or mode :live))))))
    (cond
      ((or (string= token "SHADOW") (string= token ":SHADOW")) "SHADOW")
      (t "LIVE"))))

(defun count-trade-logs-for-strategy (strategy-name &key (modes '(:live)) from-timestamp to-timestamp)
  "Count trade_logs rows for STRATEGY-NAME filtered by execution MODES.
Defaults to LIVE-only counting."
  (init-db)
  (when (and (stringp strategy-name) (> (length strategy-name) 0))
    (let* ((normalized-modes (remove-duplicates
                              (mapcar #'%normalize-execution-mode-token
                                      (or modes '(:live)))
                              :test #'string=))
           (placeholders (%sql-placeholders (length normalized-modes)))
           (query (with-output-to-string (s)
                    (format s "SELECT COUNT(*) FROM trade_logs WHERE strategy_name = ? ")
                    (when (numberp from-timestamp)
                      (format s "AND timestamp >= ? "))
                    (when (numberp to-timestamp)
                      (format s "AND timestamp < ? "))
                    (format s "AND UPPER(COALESCE(execution_mode, 'LIVE')) IN (~a)" placeholders)))
           (params (append (list strategy-name)
                           (when (numberp from-timestamp) (list from-timestamp))
                           (when (numberp to-timestamp) (list to-timestamp))
                           normalized-modes)))
      (handler-case
          (apply #'execute-single query params)
        (error (e)
          (format t "[DB] ⚠️ Failed to count trade_logs for ~a: ~a~%" strategy-name e)
          0)))))

(defun fetch-recent-live-trade-metrics (strategy-name &key (limit 40))
  "Return recent LIVE metrics plist for STRATEGY-NAME from latest LIMIT rows.
Keys: :trades :wins :losses :win-rate :gross-win :gross-loss :profit-factor :net-pnl
      :latest-loss-streak :max-loss-streak."
  (init-db)
  (when (and (stringp strategy-name) (> (length strategy-name) 0))
    (let ((safe-limit (max 1 (or (and (numberp limit) (round limit)) 40))))
      (handler-case
          (let* ((recent-pnl-rows
                   (execute-to-list
                    "SELECT pnl
                       FROM trade_logs
                      WHERE strategy_name = ?
                        AND UPPER(COALESCE(execution_mode, 'LIVE')) = 'LIVE'
                      ORDER BY timestamp DESC
                      LIMIT ?"
                    strategy-name
                    safe-limit))
                 (recent-pnls
                   (mapcar (lambda (row)
                             (let ((pnl (if (and (listp row) row)
                                            (first row)
                                            row)))
                               (if (numberp pnl)
                                   (float pnl 1.0)
                                   0.0)))
                           recent-pnl-rows))
                 (latest-loss-streak
                   (let ((streak 0))
                     (dolist (pnl recent-pnls streak)
                       (if (< pnl 0.0)
                           (incf streak)
                           (return streak)))))
                 (max-loss-streak
                   (let ((best 0)
                         (current 0))
                     (dolist (pnl recent-pnls best)
                       (if (< pnl 0.0)
                           (progn
                             (incf current)
                             (when (> current best)
                               (setf best current)))
                           (setf current 0)))))
                 (row (first
                       (execute-to-list
                        "SELECT
                             COUNT(*) AS trades,
                             COALESCE(SUM(CASE WHEN pnl > 0 THEN 1 ELSE 0 END), 0) AS wins,
                             COALESCE(SUM(CASE WHEN pnl < 0 THEN 1 ELSE 0 END), 0) AS losses,
                             COALESCE(SUM(CASE WHEN pnl > 0 THEN pnl ELSE 0 END), 0.0) AS gross_win,
                             COALESCE(SUM(CASE WHEN pnl < 0 THEN -pnl ELSE 0 END), 0.0) AS gross_loss,
                             COALESCE(SUM(pnl), 0.0) AS net_pnl
                           FROM (
                             SELECT pnl
                               FROM trade_logs
                              WHERE strategy_name = ?
                                AND UPPER(COALESCE(execution_mode, 'LIVE')) = 'LIVE'
                              ORDER BY timestamp DESC
                              LIMIT ?
                           ) recent"
                        strategy-name
                        safe-limit))))
            (when row
              (destructuring-bind (trades wins losses gross-win gross-loss net-pnl) row
                (let* ((trade-count (if (numberp trades) (max 0 (round trades)) 0))
                       (win-count (if (numberp wins) (max 0 (round wins)) 0))
                       (loss-count (if (numberp losses) (max 0 (round losses)) 0))
                       (gross-win-f (if (numberp gross-win) (float gross-win 1.0) 0.0))
                       (gross-loss-f (if (numberp gross-loss) (float gross-loss 1.0) 0.0))
                       (net-pnl-f (if (numberp net-pnl) (float net-pnl 1.0) 0.0))
                       (wr (if (> trade-count 0)
                               (/ (float win-count 1.0) (float trade-count 1.0))
                               0.0))
                       (pf (cond
                             ((> gross-loss-f 0.0) (/ gross-win-f gross-loss-f))
                             ((> gross-win-f 0.0) 99.0)
                             (t 0.0))))
                  (list :trades trade-count
                        :wins win-count
                        :losses loss-count
                        :win-rate wr
                        :gross-win gross-win-f
                        :gross-loss gross-loss-f
                        :profit-factor pf
                        :net-pnl net-pnl-f
                        :latest-loss-streak (max 0 (round latest-loss-streak))
                        :max-loss-streak (max 0 (round max-loss-streak)))))))
        (error (e)
          (format t "[DB] ⚠️ Failed to fetch recent LIVE metrics for ~a: ~a~%"
                  strategy-name e)
          nil)))))

(defun fetch-trade-logs-for-learning (&key strategy-name (limit 500) (modes '(:live :shadow)))
  "Fetch recent trade_logs rows for learning bootstrap.
Returns rows: (timestamp strategy_name symbol direction category regime pnl hold_time pair_id execution_mode)."
  (init-db)
  (let* ((normalized-modes (remove-duplicates
                            (mapcar #'%normalize-execution-mode-token (or modes '(:live :shadow)))
                            :test #'string=))
         (mode-placeholders (%sql-placeholders (length normalized-modes)))
         (query (with-output-to-string (s)
                  (write-string "SELECT timestamp, strategy_name, symbol, direction, category, regime, pnl, hold_time, pair_id, COALESCE(execution_mode, 'LIVE') FROM trade_logs WHERE " s)
                  (if (and (stringp strategy-name) (> (length strategy-name) 0))
                      (write-string "strategy_name = ? " s)
                      (write-string "1=1 " s))
                  (format s "AND UPPER(COALESCE(execution_mode, 'LIVE')) IN (~a) " mode-placeholders)
                  (write-string "ORDER BY timestamp DESC LIMIT ?" s)))
         (params (append
                  (when (and (stringp strategy-name) (> (length strategy-name) 0))
                    (list strategy-name))
                  normalized-modes
                  (list (max 1 (or limit 500))))))
    (handler-case
        (apply #'execute-to-list query params)
      (error (e)
        (format t "[DB] ⚠️ Failed to fetch learning trade logs: ~a~%" e)
        '()))))

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

(defparameter *backtest-strategy-legacy-suffixes*
  '("-OOS" "_OOS" "-QUAL" "_QUAL" "-RR" "_RR" "_P1" "-P1")
  "Legacy/generated suffixes that should map to the same base strategy for evidence counting.")

(defun %string-suffix-ci-p (suffix value)
  "Case-insensitive suffix predicate."
  (let* ((s (or suffix ""))
         (v (or value ""))
         (slen (length s))
         (vlen (length v)))
    (and (> slen 0)
         (> vlen slen)
         (string-equal s v :start1 0 :end1 slen :start2 (- vlen slen) :end2 vlen))))

(defun %canonicalize-backtest-strategy-name (strategy-name)
  "Normalize strategy name by stripping one known legacy suffix."
  (let* ((raw (and strategy-name (format nil "~a" strategy-name)))
         (name (and raw (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))
    (when (and name (> (length name) 0))
      (or (loop for suffix in *backtest-strategy-legacy-suffixes*
                when (%string-suffix-ci-p suffix name)
                do (return (subseq name 0 (- (length name) (length suffix)))))
          name))))

(defun %backtest-strategy-name-aliases (strategy-name)
  "Return base + known legacy suffix aliases for STRATEGY-NAME."
  (let ((base (%canonicalize-backtest-strategy-name strategy-name)))
    (when base
      (remove-duplicates
       (cons base
             (mapcar (lambda (suffix)
                       (concatenate 'string base suffix))
                     *backtest-strategy-legacy-suffixes*))
       :test #'string=))))

(defun %sql-placeholders (n)
  "Return comma-separated SQL placeholder list with N question marks."
  (with-output-to-string (s)
    (loop for i from 0 below n do
          (when (> i 0) (write-string "," s))
          (write-char #\? s))))

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
      ;; Guard against duplicate replay of the same trade row.
      (execute-non-query
       "INSERT OR IGNORE INTO backtest_trade_logs (
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

(defun fetch-backtest-trade-count-map (&key oos-kind)
  "Return hash-table of canonical strategy-name -> persisted trade count.
Counts aggregate legacy suffix aliases to the same canonical key."
  (let* ((rows (if oos-kind
                   (execute-to-list
                    "SELECT strategy_name, COUNT(*)
                     FROM backtest_trade_logs
                     WHERE oos_kind = ?
                     GROUP BY strategy_name"
                    oos-kind)
                   (execute-to-list
                    "SELECT strategy_name, COUNT(*)
                     FROM backtest_trade_logs
                     GROUP BY strategy_name")))
         (counts (make-hash-table :test 'equal)))
    (dolist (row rows)
      (destructuring-bind (strategy-name count) row
        (let* ((base (or (%canonicalize-backtest-strategy-name strategy-name)
                         strategy-name))
               (n (if (numberp count) (round count) 0)))
          (when base
            (incf (gethash base counts 0) n)))))
    counts))

(defun count-backtest-trades-for-strategy (strategy-name &key oos-kind)
  "Count persisted backtest trades for STRATEGY-NAME including legacy suffix aliases."
  (let* ((aliases (%backtest-strategy-name-aliases strategy-name))
         (alias-count (length aliases)))
    (if (<= alias-count 0)
        0
        (let* ((placeholders (%sql-placeholders alias-count))
               (base-query (format nil "SELECT count(*) FROM backtest_trade_logs WHERE strategy_name IN (~a)"
                                   placeholders)))
          (if oos-kind
              (or (apply #'execute-single
                         (concatenate 'string base-query " AND oos_kind = ?")
                         (append aliases (list oos-kind)))
                  0)
              (or (apply #'execute-single base-query aliases) 0))))))

(defun fetch-backtest-trades (strategy-name &key oos-kind)
  "Fetch persisted backtest trades for STRATEGY-NAME including legacy suffix aliases."
  (let* ((aliases (%backtest-strategy-name-aliases strategy-name))
         (alias-count (length aliases)))
    (if (<= alias-count 0)
        '()
        (let* ((placeholders (%sql-placeholders alias-count))
               (base-query
                 (format nil
                         "SELECT request_id, strategy_name, timestamp, pnl, symbol, direction,
                                 entry_price, exit_price, sl, tp, volume, hold_time,
                                 rank, timeframe, category, regime, oos_kind
                          FROM backtest_trade_logs
                          WHERE strategy_name IN (~a)"
                         placeholders)))
          (if oos-kind
              (apply #'execute-to-list
                     (concatenate 'string base-query " AND oos_kind = ? ORDER BY timestamp")
                     (append aliases (list oos-kind)))
              (apply #'execute-to-list
                     (concatenate 'string base-query " ORDER BY timestamp")
                     aliases))))))

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

(defun %truthy->int (value)
  "Normalize VALUE into SQLite boolean integer 0/1."
  (if value 1 0))

(defparameter *deployment-gate-unset* (list :unset)
  "Sentinel used for upsert-deployment-gate-status optional keys.")

(defun fetch-deployment-gate-status (strategy-name)
  "Fetch deployment gate status for STRATEGY-NAME as plist, or NIL."
  (when (and strategy-name (stringp strategy-name) (> (length strategy-name) 0))
    (let ((row (first (execute-to-list
                       "SELECT strategy_name, research_passed, oos_passed,
                               oos_window_days, forward_start, forward_days,
                               forward_trades, forward_sharpe, forward_pf,
                               decision, reason, updated_at
                          FROM deployment_gate_status
                         WHERE strategy_name = ?"
                       strategy-name))))
      (when row
        (destructuring-bind (name research-passed oos-passed
                             oos-window-days forward-start forward-days
                             forward-trades forward-sharpe forward-pf
                             decision reason updated-at)
            row
          (list :strategy-name name
                :research-passed (and (numberp research-passed) (> research-passed 0))
                :oos-passed (and (numberp oos-passed) (> oos-passed 0))
                :oos-window-days oos-window-days
                :forward-start forward-start
                :forward-days forward-days
                :forward-trades forward-trades
                :forward-sharpe forward-sharpe
                :forward-pf forward-pf
                :decision decision
                :reason reason
                :updated-at updated-at))))))

(defun upsert-deployment-gate-status (strategy-name
                                      &key
                                        (research-passed *deployment-gate-unset*)
                                        (oos-passed *deployment-gate-unset*)
                                        (oos-window-days *deployment-gate-unset*)
                                        (forward-start *deployment-gate-unset*)
                                        (forward-days *deployment-gate-unset*)
                                        (forward-trades *deployment-gate-unset*)
                                        (forward-sharpe *deployment-gate-unset*)
                                        (forward-pf *deployment-gate-unset*)
                                        (decision *deployment-gate-unset*)
                                        (reason *deployment-gate-unset*)
                                        updated-at)
  "Insert or update deployment Go/No-Go state for STRATEGY-NAME.
Omitted keys preserve existing values; NIL is treated as an explicit value."
  (when (and strategy-name (stringp strategy-name) (> (length strategy-name) 0))
    (let* ((current (or (fetch-deployment-gate-status strategy-name) '()))
           (resolved-research (if (eq research-passed *deployment-gate-unset*)
                                  (getf current :research-passed nil)
                                  research-passed))
           (resolved-oos (if (eq oos-passed *deployment-gate-unset*)
                             (getf current :oos-passed nil)
                             oos-passed))
           (resolved-oos-window (if (eq oos-window-days *deployment-gate-unset*)
                                    (getf current :oos-window-days nil)
                                    oos-window-days))
           (resolved-forward-start (if (eq forward-start *deployment-gate-unset*)
                                       (getf current :forward-start nil)
                                       forward-start))
           (resolved-forward-days (if (eq forward-days *deployment-gate-unset*)
                                      (getf current :forward-days nil)
                                      forward-days))
           (resolved-forward-trades (if (eq forward-trades *deployment-gate-unset*)
                                        (getf current :forward-trades nil)
                                        forward-trades))
           (resolved-forward-sharpe (if (eq forward-sharpe *deployment-gate-unset*)
                                        (getf current :forward-sharpe nil)
                                        forward-sharpe))
           (resolved-forward-pf (if (eq forward-pf *deployment-gate-unset*)
                                    (getf current :forward-pf nil)
                                    forward-pf))
           (resolved-decision (if (eq decision *deployment-gate-unset*)
                                  (getf current :decision nil)
                                  decision))
           (resolved-reason (if (eq reason *deployment-gate-unset*)
                                (getf current :reason nil)
                                reason))
           (resolved-updated-at (if (numberp updated-at) updated-at (get-universal-time))))
      (execute-non-query
       "INSERT OR REPLACE INTO deployment_gate_status (
          strategy_name, research_passed, oos_passed,
          oos_window_days, forward_start, forward_days,
          forward_trades, forward_sharpe, forward_pf,
          decision, reason, updated_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
       strategy-name
       (%truthy->int resolved-research)
       (%truthy->int resolved-oos)
       resolved-oos-window
       resolved-forward-start
       resolved-forward-days
       resolved-forward-trades
       resolved-forward-sharpe
       resolved-forward-pf
       resolved-decision
       resolved-reason
       resolved-updated-at)
      (fetch-deployment-gate-status strategy-name))))

(defun fetch-deployment-gate-summary ()
  "Return aggregate counts for deployment gate decisions."
  (let* ((ready (or (execute-single
                     "SELECT count(*) FROM deployment_gate_status WHERE decision = 'LIVE_READY'")
                    0))
         (running (or (execute-single
                       "SELECT count(*) FROM deployment_gate_status WHERE decision = 'FORWARD_RUNNING'")
                      0))
         (failed (or (execute-single
                      "SELECT count(*) FROM deployment_gate_status WHERE decision = 'FORWARD_FAIL'")
                     0))
         (blocked (or (execute-single
                       "SELECT count(*) FROM deployment_gate_status WHERE decision = 'BLOCKED_OOS'")
                      0))
         (total (or (execute-single "SELECT count(*) FROM deployment_gate_status") 0)))
    (list :total total
          :live-ready ready
          :forward-running running
          :forward-fail failed
          :blocked-oos blocked)))

(defparameter *last-db-sync-time* 0)
(defparameter *db-sync-interval* 60
  "Minimum seconds between DB syncs for strategy metrics.")
(defparameter *db-active-kb-reconcile-enabled* t
  "When T, hydrate missing active DB strategies into in-memory KB during DB refresh.")
(defparameter *kb-active-db-reconcile-enabled* t
  "When T, hydrate missing active KB strategies into DB during DB refresh.")
(defparameter *db-active-library-reconcile-enabled* t
  "When T, hydrate missing active DB strategies into Library active rank dirs.")
(defparameter *db-active-kb-reconcile-max-additions* 5000
  "Safety cap for active DB<->KB reconciliation additions per refresh cycle.")
(defparameter *db-active-library-reconcile-max-additions* 500
  "Safety cap for active DB->Library hydration additions per reconciliation.")
(defparameter *db-active-library-reconcile-interval* 300
  "Minimum seconds between active DB->Library reconciliation runs.")
(defparameter *last-db-active-library-reconcile-time* 0
  "Timestamp of last active DB->Library reconciliation run.")
(defparameter *active-rank-db-where-clause*
  "UPPER(TRIM(COALESCE(rank,''))) IN (':B','B',':A','A',':S','S',':LEGEND','LEGEND')"
  "SQL WHERE clause for active (B/A/S/LEGEND) rank rows.")
(defparameter *db-archive-library-reconcile-enabled* t
  "When T, hydrate missing archived DB strategies into Library archive dirs.")
(defparameter *db-archive-library-reconcile-interval* 60
  "Minimum seconds between archive DB->Library reconciliation runs.")
(defparameter *db-archive-library-reconcile-max-additions* 2000
  "Safety cap for archive DB->Library hydration additions per reconciliation.")
(defparameter *db-archive-library-reconcile-max-removals* 2000
  "Safety cap for stale Library archive removals per reconciliation.")
(defparameter *db-archive-library-reconcile-page-size* 1000
  "Keyset page size for archive DB->Library hydration candidate scan.")
(defparameter *last-db-archive-library-reconcile-time* 0
  "Timestamp of last archive DB->Library reconciliation run.")
(defparameter *archive-rank-db-where-clause*
  "UPPER(TRIM(rank)) IN (':GRAVEYARD','GRAVEYARD',':RETIRED','RETIRED')"
  "SQL WHERE clause for archive ranks tracked in Library canonical drift.")

(defun sync-active-runtime-state-from-db-sexp (strategy-index)
  "Sync volatile in-memory fields from DB data_sexp for active ranks.
This keeps split daemons coherent for fields not represented as SQL columns."
  (let ((synced 0))
    (handler-case
        (dolist (row (execute-to-list
                      (format nil "SELECT name, data_sexp FROM strategies WHERE ~a"
                              *active-rank-db-where-clause*)))
          (destructuring-bind (name data-sexp) row
            (let ((strat (and name (gethash name strategy-index))))
              (when (and strat (stringp data-sexp) (> (length data-sexp) 0))
                (handler-case
                    (let ((obj (swimmy.core:safe-read-sexp data-sexp :package :swimmy.school)))
                      (when (strategy-p obj)
                        (let ((changed nil))
                          (when (and (slot-exists-p strat 'revalidation-pending)
                                     (slot-exists-p obj 'revalidation-pending))
                            (let ((db-pending (and (strategy-revalidation-pending obj) t))
                                  (local-pending (and (strategy-revalidation-pending strat) t)))
                              (unless (eql local-pending db-pending)
                                (setf (strategy-revalidation-pending strat) db-pending)
                                (setf changed t))))
                          (when (and (slot-exists-p strat 'breeding-count)
                                     (slot-exists-p obj 'breeding-count))
                            (let ((db-count (max 0 (or (strategy-breeding-count obj) 0)))
                                  (local-count (max 0 (or (strategy-breeding-count strat) 0))))
                              (unless (= local-count db-count)
                                (setf (strategy-breeding-count strat) db-count)
                                (setf changed t))))
                          (when changed
                            (incf synced)))))
                  (error () nil)))))
        (when (> synced 0)
          (format t "[DB] 🔄 Synced active runtime state from data_sexp for ~d strategies~%" synced)))
      (error (e)
        (format t "[DB] ⚠️ Active runtime state sync skipped: ~a~%" e)))
    synced))

(defun %normalize-rank-token-for-kb-sync (rank)
  "Normalize rank cell/symbol into uppercase token without leading colon."
  (labels ((normalize (value)
             (let* ((trimmed (string-upcase (string-trim '(#\Space #\Newline #\Tab) value))))
               (if (and (> (length trimmed) 0) (char= (char trimmed 0) #\:))
                   (subseq trimmed 1)
                   trimmed))))
    (cond
      ((null rank) "NIL")
      ((stringp rank) (normalize rank))
      ((symbolp rank) (normalize (symbol-name rank)))
      (t (normalize (format nil "~a" rank))))))

(defun %active-rank-for-kb-sync-p (rank)
  "Return T when rank should be considered active for KB reconciliation."
  (member (%normalize-rank-token-for-kb-sync rank)
          '("B" "A" "S" "LEGEND")
          :test #'string=))

(defun %db-rank-cell->keyword-for-kb-sync (rank-cell)
  "Convert DB rank cell into keyword rank (or NIL)."
  (let ((token (%normalize-rank-token-for-kb-sync rank-cell)))
    (if (string= token "NIL")
        nil
        (intern token :keyword))))

(defun %db-rank-overwrite-allowed-p (local-rank db-rank)
  "Return T when DB rank can overwrite LOCAL-RANK during refresh sync.
Preserve legend immutability against stale non-legend DB rows."
  (let ((local-token (%normalize-rank-token-for-kb-sync local-rank))
        (db-token (%normalize-rank-token-for-kb-sync db-rank)))
    (cond
      ((string= local-token "LEGEND")
       (member db-token '("LEGEND" "LEGEND-ARCHIVE") :test #'string=))
      ((string= local-token "LEGEND-ARCHIVE")
       (member db-token '("LEGEND" "LEGEND-ARCHIVE") :test #'string=))
      (t t))))

(defun %active-rank-token-for-library-sync (rank-cell)
  "Normalize active rank token for Library sync (INCUBATOR fallback)."
  (let ((token (%normalize-rank-token-for-kb-sync rank-cell)))
    (if (member token '("B" "A" "S" "LEGEND") :test #'string=)
        token
        "INCUBATOR")))

(defun %active-library-file-path (name rank-cell &optional (root swimmy.persistence:*library-path*))
  "Build Library active file path for strategy NAME and active rank cell."
  (let* ((dir (%active-rank-token-for-library-sync rank-cell))
         (safe-name (swimmy.persistence::sanitize-filename name)))
    (merge-pathnames (format nil "~a/~a.lisp" dir safe-name) root)))

(defun %library-active-file-exists-p (name rank-cell &optional (root swimmy.persistence:*library-path*))
  "Return T when active Library file for NAME exists at target rank dir."
  (let ((path (%active-library-file-path name rank-cell root)))
    (and path (probe-file path))))

(defun %find-active-kb-strategy-for-library-sync (name rank-keyword)
  "Return active in-memory strategy object by NAME, when available."
  (when (and name rank-keyword)
    (find name *strategy-knowledge-base*
          :key (lambda (s)
                 (and (strategy-p s)
                      (%active-rank-for-kb-sync-p (strategy-rank s))
                      (eq (strategy-rank s) rank-keyword)
                      (strategy-name s)))
          :test #'string=)))

(defun %persist-db-active-row-into-library (name rank-cell data-sexp)
  "Persist one DB active row into Library active rank dirs. Returns T on success."
  (handler-case
      (let* ((rank-token (%active-rank-token-for-library-sync rank-cell))
             (rank-keyword (intern rank-token :keyword))
             (parsed (and (stringp data-sexp)
                          (> (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                   data-sexp))
                             0)
                          (swimmy.core:safe-read-sexp data-sexp :package :swimmy.school)))
             (kb-obj (or (and (strategy-p parsed) parsed)
                         (%find-active-kb-strategy-for-library-sync name rank-keyword))))
        (cond
          ((strategy-p kb-obj)
           (setf (strategy-rank kb-obj) rank-keyword)
           (swimmy.persistence:save-strategy kb-obj)
           t)
          ((and (stringp data-sexp)
                (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) data-sexp))
                   0))
           (let ((path (%active-library-file-path name rank-cell)))
             (ensure-directories-exist path)
             (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
               (write-string data-sexp out)
               (terpri out))
             t))
          (t nil)))
    (error ()
      nil)))

(defun %archive-rank-token-for-library-sync (rank-cell)
  "Normalize archive rank token for Library sync (GRAVEYARD fallback)."
  (let ((token (%normalize-rank-token-for-kb-sync rank-cell)))
    (if (string= token "RETIRED") "RETIRED" "GRAVEYARD")))

(defun %filename-strip-lisp-extension (filename)
  "Return FILENAME without trailing .lisp (case-insensitive)."
  (let* ((raw (or filename ""))
         (len (length raw)))
    (if (and (> len 5)
             (string-equal ".lisp" raw :start1 0 :end1 5 :start2 (- len 5) :end2 len))
        (subseq raw 0 (- len 5))
        raw)))

(defun %library-archive-name-index (&optional (root swimmy.persistence:*library-path*))
  "Return hash-table of canonical strategy names present in Library GRAVEYARD/RETIRED."
  (let ((idx (make-hash-table :test 'equal)))
    (dolist (dir '("GRAVEYARD" "RETIRED"))
      (let* ((dirpath (merge-pathnames (format nil "~a/" dir) root))
             (exists-p (or (not (fboundp 'uiop:directory-exists-p))
                           (uiop:directory-exists-p dirpath))))
        (when exists-p
          (let ((raw (ignore-errors
                       (uiop:run-program
                        (list "find" (namestring dirpath)
                              "-maxdepth" "1"
                              "-type" "f"
                              "-name" "*.lisp"
                              "-print")
                        :output :string
                        :ignore-error-status t))))
            (when (and raw (> (length raw) 0))
              (with-input-from-string (in raw)
                (loop for line = (read-line in nil nil)
                      while line
                      for trimmed = (string-trim '(#\Space #\Tab #\Newline #\Return) line)
                      unless (string= trimmed "")
                        do
                           (let* ((filename (file-namestring trimmed))
                                  (base (%filename-strip-lisp-extension filename)))
                             (setf (gethash base idx) t)))))))))
    idx))

(defun %shell-quote-single-for-db-reconcile (s)
  "Return shell-safe single-quoted string for archive count command."
  (format nil "'~a'"
          (with-output-to-string (out)
            (loop for ch across (or s "")
                  do (if (char= ch #\')
                         (write-string "'\"'\"'" out)
                         (write-char ch out))))))

(defun %library-archive-canonical-count-for-reconcile (&optional (root swimmy.persistence:*library-path*))
  "Return unique strategy-name count across Library GRAVEYARD+RETIRED dirs."
  (labels ((path-str (dir)
             (namestring (merge-pathnames (format nil "~a/" dir) root))))
    (let* ((grave (%shell-quote-single-for-db-reconcile (path-str "GRAVEYARD")))
           (retired (%shell-quote-single-for-db-reconcile (path-str "RETIRED")))
           (cmd (format nil
                        "{ find ~a -maxdepth 1 -type f -name '*.lisp' -printf '%f\\n' 2>/dev/null; find ~a -maxdepth 1 -type f -name '*.lisp' -printf '%f\\n' 2>/dev/null; } | sed 's/\\.lisp$//' | LC_ALL=C sort -u | wc -l"
                        grave retired))
           (raw (ignore-errors (uiop:run-program cmd :output :string :ignore-error-status t :force-shell t)))
           (trimmed (and raw (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
           (n (and trimmed (> (length trimmed) 0) (parse-integer trimmed :junk-allowed t))))
      (or n 0))))

(defun %db-archive-canonical-count-for-reconcile ()
  "Return canonical archive-name count in DB using Library filename normalization.
Uses sanitize-filename-equivalent normalization (slash and backslash -> underscore)
to avoid false drift when multiple DB names map to the same Library filename."
  (or (execute-single
       (format nil
               "SELECT COUNT(DISTINCT REPLACE(REPLACE(name, '/', '_'), CHAR(92), '_'))
                  FROM strategies
                 WHERE ~a
                   AND name IS NOT NULL
                   AND TRIM(name) <> ''"
               *archive-rank-db-where-clause*))
      0))

(defun %library-archive-has-name-p (name &optional (root swimmy.persistence:*library-path*))
  "Return T when archive file for NAME exists in GRAVEYARD or RETIRED."
  (let ((safe-name (swimmy.persistence::sanitize-filename name)))
    (or (probe-file (merge-pathnames (format nil "GRAVEYARD/~a.lisp" safe-name) root))
        (probe-file (merge-pathnames (format nil "RETIRED/~a.lisp" safe-name) root)))))

(defparameter *archive-reconcile-backup-table-available* :unknown
  "Cache for archive_reconcile_backup table availability (:unknown/T/NIL).")

(defun %archive-reconcile-backup-table-available-p ()
  "Return T when archive_reconcile_backup table exists."
  (case *archive-reconcile-backup-table-available*
    ((t) t)
    ((nil) nil)
    (otherwise
     (let ((exists-p
             (ignore-errors
               (let ((cnt (execute-single
                           "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='archive_reconcile_backup'")))
                 (and cnt (> cnt 0))))))
       (setf *archive-reconcile-backup-table-available* (if exists-p t nil))
       exists-p))))

(defun %blank-string-p (value)
  "Return T when VALUE is NIL or empty after trim."
  (or (null value)
      (not (stringp value))
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))))

(defun %fetch-archive-backup-data-sexp (name rank-cell)
  "Fetch latest non-empty archive payload from archive_reconcile_backup for NAME.
Prefer matching rank token, then fallback to any rank for the same NAME."
  (when (and name (%archive-reconcile-backup-table-available-p))
    (let* ((rank-token (%archive-rank-token-for-library-sync rank-cell))
           (rank-plain rank-token)
           (rank-colon (format nil ":~a" rank-token))
           (exact (ignore-errors
                    (execute-single
                     "SELECT data_sexp
                        FROM archive_reconcile_backup
                       WHERE name = ?
                         AND UPPER(TRIM(rank)) IN (?, ?)
                         AND data_sexp IS NOT NULL
                         AND TRIM(data_sexp) <> ''
                       ORDER BY backed_up_at DESC, id DESC
                       LIMIT 1"
                     name rank-plain rank-colon))))
      (if (and (stringp exact)
               (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) exact)) 0))
          exact
          (ignore-errors
            (execute-single
             "SELECT data_sexp
                FROM archive_reconcile_backup
               WHERE name = ?
                 AND data_sexp IS NOT NULL
                 AND TRIM(data_sexp) <> ''
               ORDER BY backed_up_at DESC, id DESC
               LIMIT 1"
             name))))))

(defun %purge-stale-archive-files-from-library
    (&key (max-removals *db-archive-library-reconcile-max-removals*)
          (root swimmy.persistence:*library-path*))
  "Delete Library archive files whose names no longer exist in DB archive ranks.
Returns plist: :removed-names :removed-files :truncated."
  (let ((removed-names 0)
        (removed-files 0)
        (truncated nil)
        (db-archive-name-index (make-hash-table :test 'equal))
        (library-name-index (%library-archive-name-index root)))
    (dolist (row (execute-to-list
                  (format nil "SELECT name FROM strategies WHERE ~a"
                          *archive-rank-db-where-clause*)))
      (let ((name (first row)))
        (when name
          (setf (gethash name db-archive-name-index) t))))
    (maphash
     (lambda (name _present)
       (declare (ignore _present))
       (when (and max-removals (>= removed-names max-removals))
         (setf truncated t)
         (return-from %purge-stale-archive-files-from-library
           (list :removed-names removed-names
                 :removed-files removed-files
                 :truncated truncated)))
       (unless (gethash name db-archive-name-index)
         (let* ((safe-name (swimmy.persistence::sanitize-filename name))
                (grave (merge-pathnames (format nil "GRAVEYARD/~a.lisp" safe-name) root))
                (retired (merge-pathnames (format nil "RETIRED/~a.lisp" safe-name) root))
                (name-removed nil))
           (dolist (path (list grave retired))
             (when (probe-file path)
               (ignore-errors
                 (delete-file path)
                 (incf removed-files)
                 (setf name-removed t))))
           (when name-removed
             (incf removed-names)))))
     library-name-index)
    (list :removed-names removed-names
          :removed-files removed-files
          :truncated truncated)))

(defun %archive-library-file-path (name rank-cell &optional (root swimmy.persistence:*library-path*))
  "Build Library archive file path for strategy NAME and archive rank cell."
  (let* ((dir (%archive-rank-token-for-library-sync rank-cell))
         (safe-name (swimmy.persistence::sanitize-filename name)))
    (merge-pathnames (format nil "~a/~a.lisp" dir safe-name) root)))

(defun %persist-db-archive-row-into-library (name rank-cell data-sexp)
  "Persist one DB archive row into Library archive dirs. Returns T on success."
  (handler-case
      (let* ((resolved-data-sexp
               (if (%blank-string-p data-sexp)
                   (%fetch-archive-backup-data-sexp name rank-cell)
                   data-sexp))
             (parsed (and (stringp resolved-data-sexp)
                          (> (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                   resolved-data-sexp))
                             0)
                          (swimmy.core:safe-read-sexp resolved-data-sexp :package :swimmy.school)))
             (rank-token (%archive-rank-token-for-library-sync rank-cell))
             (rank-keyword (if (string= rank-token "RETIRED") :RETIRED :GRAVEYARD)))
        (cond
          ((strategy-p parsed)
           (setf (strategy-rank parsed) rank-keyword)
           (swimmy.persistence:save-strategy parsed)
           t)
          ((and (stringp resolved-data-sexp)
                (> (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        resolved-data-sexp))
                   0))
           (let ((path (%archive-library-file-path name rank-cell)))
             (ensure-directories-exist path)
             (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
               (write-string resolved-data-sexp out)
               (terpri out))
             t))
          (t nil)))
    (error ()
      nil)))

(defun reconcile-active-library-with-db
    (&key (max-additions *db-active-library-reconcile-max-additions*)
          (interval *db-active-library-reconcile-interval*)
          (now (get-universal-time)))
  "Ensure active DB rows (B/A/S/LEGEND) exist in Library active rank dirs.
Returns plist with :db-active :added :write-failed :truncated :skipped."
  (when (and (numberp interval)
             (> interval 0)
             (< (- now *last-db-active-library-reconcile-time*) interval))
    (return-from reconcile-active-library-with-db
      (list :skipped :cooldown
            :db-active nil
            :added 0
            :write-failed 0
            :truncated nil)))
  (setf *last-db-active-library-reconcile-time* now)
  (let* ((db-active (or (execute-single (format nil "SELECT count(*) FROM strategies WHERE ~a"
                                                *active-rank-db-where-clause*))
                        0))
         (added 0)
         (write-failed 0)
         (truncated nil))
    (dolist (row (execute-to-list
                  (format nil
                          "SELECT rowid, name, rank
                             FROM strategies
                            WHERE ~a
                            ORDER BY rowid"
                          *active-rank-db-where-clause*)))
      (when (and max-additions (>= added max-additions))
        (setf truncated t)
        (return))
      (destructuring-bind (rowid name rank-cell) row
        (when (and name (not (%library-active-file-exists-p name rank-cell)))
          (let ((data-sexp (execute-single
                            "SELECT data_sexp FROM strategies WHERE rowid = ? LIMIT 1"
                            rowid)))
            (if (%persist-db-active-row-into-library name rank-cell data-sexp)
                (incf added)
                (incf write-failed))))))
    (when (> added 0)
      (format t "[DB] 🧩 Library active reconciled from DB: added=~d write_failed=~d~%"
              added write-failed))
    (when (and (> write-failed 0) (zerop added))
      (format t "[DB] ⚠️ Library active reconcile write_failed=~d (added=0; missing payload in DB/KB).~%"
              write-failed))
    (list :skipped nil
          :db-active db-active
          :added added
          :write-failed write-failed
          :truncated truncated)))

(defun reconcile-archive-library-with-db
    (&key (max-additions *db-archive-library-reconcile-max-additions*)
          (max-removals *db-archive-library-reconcile-max-removals*)
          (interval *db-archive-library-reconcile-interval*)
          (now (get-universal-time)))
  "Ensure archive DB rows (:GRAVEYARD/:RETIRED) exist in Library archive dirs.
Returns plist with :db-archive :library-canonical :added :removed-names :removed-files :write-failed :truncated :skipped."
  (when (and (numberp interval)
             (> interval 0)
             (< (- now *last-db-archive-library-reconcile-time*) interval))
    (return-from reconcile-archive-library-with-db
      (list :skipped :cooldown
            :db-archive nil
            :library-canonical nil
            :added 0
            :removed-names 0
            :removed-files 0
            :write-failed 0
            :truncated nil)))
  (setf *last-db-archive-library-reconcile-time* now)
  (let* ((db-archive (or (execute-single (format nil "SELECT count(*) FROM strategies WHERE ~a"
                                                  *archive-rank-db-where-clause*))
                         0))
         (db-archive-canonical (%db-archive-canonical-count-for-reconcile))
         (lib-canonical (%library-archive-canonical-count-for-reconcile))
         (library-name-index nil)
         (added 0)
         (removed-names 0)
         (removed-files 0)
         (write-failed 0)
         (truncated nil))
    ;; If Library archive has more canonical names than DB archive,
    ;; purge stale names that no longer exist as archived rows in DB.
    (when (> lib-canonical db-archive-canonical)
      (let* ((gap (- lib-canonical db-archive-canonical))
             (purge-summary (%purge-stale-archive-files-from-library
                             :max-removals (if max-removals
                                               (min max-removals gap)
                                               gap)))
             (p-removed-names (or (getf purge-summary :removed-names) 0))
             (p-removed-files (or (getf purge-summary :removed-files) 0)))
        (setf removed-names p-removed-names
              removed-files p-removed-files
              truncated (or truncated (getf purge-summary :truncated)))
        (when (> p-removed-names 0)
          (format t "[DB] 🧹 Library archive stale cleanup: removed_names=~d removed_files=~d~%"
                  p-removed-names p-removed-files))
        ;; Refresh canonical count after cleanup to decide DB->Library hydration.
        (setf lib-canonical (%library-archive-canonical-count-for-reconcile))))
    ;; If DB archive still exceeds Library canonical, hydrate missing files from DB.
    (when (> db-archive-canonical lib-canonical)
      (let* ((gap (max 0 (- db-archive-canonical lib-canonical)))
             (page-size (max 1 (or *db-archive-library-reconcile-page-size* 1000)))
             (next-rowid nil)
             (seen-canonical (make-hash-table :test 'equal)))
        ;; Build once and reuse; avoid per-row probe-file checks.
        (setf library-name-index (%library-archive-name-index))
        ;; Keyset pagination: avoid materializing all archive rows in one cycle.
        (loop
          (when (and max-additions (>= added max-additions))
            (setf truncated t)
            (return))
          (when (>= added gap)
            (return))
          (let ((rows (if next-rowid
                          (execute-to-list
                           (format nil
                                   "SELECT rowid, name, rank
                                      FROM strategies
                                     WHERE ~a
                                       AND name IS NOT NULL
                                       AND TRIM(name) <> ''
                                       AND rowid < ?
                                     ORDER BY rowid DESC
                                     LIMIT ?"
                                   *archive-rank-db-where-clause*)
                           next-rowid
                           page-size)
                          (execute-to-list
                           (format nil
                                   "SELECT rowid, name, rank
                                      FROM strategies
                                     WHERE ~a
                                       AND name IS NOT NULL
                                       AND TRIM(name) <> ''
                                     ORDER BY rowid DESC
                                     LIMIT ?"
                                   *archive-rank-db-where-clause*)
                           page-size))))
            (when (or (null rows) (null (first rows)))
              (return))
            (dolist (row rows)
              (destructuring-bind (rowid name rank-cell) row
                (setf next-rowid rowid)
                (when (and name
                           (< added gap)
                           (or (null max-additions) (< added max-additions)))
                  (let ((canonical-name (swimmy.persistence::sanitize-filename name)))
                    (unless (gethash canonical-name seen-canonical)
                      (setf (gethash canonical-name seen-canonical) t)
                      (unless (gethash canonical-name library-name-index)
                        (let ((data-sexp (execute-single
                                          "SELECT data_sexp FROM strategies WHERE rowid = ? LIMIT 1"
                                          rowid)))
                          (if (%persist-db-archive-row-into-library name rank-cell data-sexp)
                              (progn
                                (setf (gethash canonical-name library-name-index) t)
                                (incf added))
                              (incf write-failed))))))))))))
      (when (> added 0)
        (format t "[DB] 🧩 Library archive reconciled from DB: added=~d write_failed=~d~%"
                added write-failed))
      (when (and (> write-failed 0) (zerop added))
        (format t "[DB] ⚠️ Library archive reconcile write_failed=~d (added=0; missing payload in DB/backup).~%"
                write-failed))
      (setf lib-canonical (%library-archive-canonical-count-for-reconcile)))
    (list :skipped nil
          :db-archive db-archive
          :db-archive-canonical db-archive-canonical
          :library-canonical lib-canonical
          :added added
          :removed-names removed-names
          :removed-files removed-files
          :write-failed write-failed
          :truncated truncated)))

(defun reconcile-active-kb-with-db (&key (max-additions *db-active-kb-reconcile-max-additions*))
  "Ensure active strategies present in DB are loaded into in-memory KB.
Returns a plist summary with :db-active :kb-active :added :parse-failed :truncated."
  (let* ((db-active (or (execute-single (format nil "SELECT count(*) FROM strategies WHERE ~a"
                                                 *active-rank-db-where-clause*))
                        0))
         (kb-active (count-if (lambda (s)
                                (and (strategy-p s)
                                     (%active-rank-for-kb-sync-p (strategy-rank s))))
                              *strategy-knowledge-base*)))
    (if (<= db-active kb-active)
        (list :db-active db-active :kb-active kb-active :added 0 :parse-failed 0 :truncated nil)
        (let* ((rows (execute-to-list
                      (format nil "SELECT name, data_sexp, rank FROM strategies WHERE ~a"
                              *active-rank-db-where-clause*)))
               (added 0)
               (parse-failed 0)
               (truncated nil))
          (bt:with-lock-held (*kb-lock*)
            (let ((kb-index (make-hash-table :test 'equal)))
              (dolist (strat *strategy-knowledge-base*)
                (when (and (strategy-p strat)
                           (strategy-name strat))
                  (setf (gethash (strategy-name strat) kb-index) strat)))
              (loop for row in rows do
                (when (and max-additions (>= added max-additions))
                  (setf truncated t)
                  (return))
                (destructuring-bind (name data-sexp rank-cell) row
                  (unless (or (null name) (gethash name kb-index))
                    (handler-case
                        (let ((obj (and (stringp data-sexp)
                                        (> (length data-sexp) 0)
                                        (swimmy.core:safe-read-sexp data-sexp :package :swimmy.school))))
                          (let ((rank-keyword (%db-rank-cell->keyword-for-kb-sync rank-cell)))
                            (if (and (strategy-p obj)
                                     (%active-rank-for-kb-sync-p rank-keyword))
                                (progn
                                  (setf (strategy-rank obj) rank-keyword)
                                  (push obj *strategy-knowledge-base*)
                                  (setf (gethash name kb-index) obj)
                                  (incf added))
                                (unless (strategy-p obj)
                                  (incf parse-failed)))))
                      (error ()
                        (incf parse-failed))))))
              (when (and (> added 0)
                         (fboundp 'build-category-pools))
                (build-category-pools)))
          (when (> added 0)
            (format t "[DB] 🧩 KB reconciled from DB active set: added=~d parse_failed=~d~%"
                    added parse-failed))
          (list :db-active db-active
                :kb-active kb-active
                :added added
                :parse-failed parse-failed
                :truncated truncated))))))
(defun reconcile-active-db-with-kb (&key (max-additions *db-active-kb-reconcile-max-additions*))
  "Ensure active strategies present in KB are persisted in DB.
Returns a plist summary with :db-active :kb-active :added :upsert-failed :truncated."
  (let* ((db-active (or (execute-single (format nil "SELECT count(*) FROM strategies WHERE ~a"
                                                 *active-rank-db-where-clause*))
                        0))
         (kb-active (count-if (lambda (s)
                                (and (strategy-p s)
                                     (%active-rank-for-kb-sync-p (strategy-rank s))))
                              *strategy-knowledge-base*)))
    (if (<= kb-active db-active)
        (list :db-active db-active :kb-active kb-active :added 0 :upsert-failed 0 :truncated nil)
        (let ((added 0)
              (upsert-failed 0)
              (truncated nil)
              (db-index (make-hash-table :test 'equal)))
          (dolist (row (execute-to-list (format nil "SELECT name FROM strategies WHERE ~a"
                                                *active-rank-db-where-clause*)))
            (let ((name (first row)))
              (when name
                (setf (gethash name db-index) t))))
          (with-transaction
            (dolist (strat *strategy-knowledge-base*)
              (when (and max-additions (>= added max-additions))
                (setf truncated t)
                (return))
              (let ((name (and (strategy-p strat) (strategy-name strat))))
                (when (and name
                           (%active-rank-for-kb-sync-p (strategy-rank strat))
                           (not (gethash name db-index)))
                  (handler-case
                      (progn
                        (upsert-strategy strat)
                        (setf (gethash name db-index) t)
                        (incf added))
                    (error ()
                      (incf upsert-failed)))))))
          (when (> added 0)
            (format t "[DB] 🧩 DB reconciled from KB active set: added=~d upsert_failed=~d~%"
                    added upsert-failed))
          (list :db-active db-active
                :kb-active kb-active
                :added added
                :upsert-failed upsert-failed
                :truncated truncated)))))

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
            (strategy-index (make-hash-table :test 'equal))
            (legend-rank-guarded 0)
            (legend-rank-heal (make-hash-table :test 'equal)))
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
                               (if (%db-rank-overwrite-allowed-p (strategy-rank strat) rank-sym)
                                   (setf (strategy-rank strat) rank-sym)
                                   (progn
                                     (incf legend-rank-guarded)
                                     (setf (gethash (strategy-name strat) legend-rank-heal) strat))))))
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
                (format t "[DB] 🔄 Synced metrics for ~d strategies (full scan fallback)~%" updated)))))
        (when (> legend-rank-guarded 0)
          (let ((healed 0)
                (heal-failed 0))
            (maphash (lambda (_name strat)
                       (declare (ignore _name))
                       (handler-case
                           (progn
                             (upsert-strategy strat)
                             (incf healed))
                         (error ()
                           (incf heal-failed))))
                     legend-rank-heal)
            (format t "[DB] 🛡️ Legend rank sync guard blocked ~d stale demotions (healed=~d failed=~d)~%"
                    legend-rank-guarded healed heal-failed)))
        (sync-active-runtime-state-from-db-sexp strategy-index)
        (when *db-active-kb-reconcile-enabled*
          (ignore-errors
            (reconcile-active-kb-with-db
             :max-additions *db-active-kb-reconcile-max-additions*)))
        (when *kb-active-db-reconcile-enabled*
          (ignore-errors
            (reconcile-active-db-with-kb
             :max-additions *db-active-kb-reconcile-max-additions*)))
        (when *db-active-library-reconcile-enabled*
          (ignore-errors
            (reconcile-active-library-with-db
             :max-additions *db-active-library-reconcile-max-additions*
             :interval *db-active-library-reconcile-interval*
             :now now)))
        (when *db-archive-library-reconcile-enabled*
          (ignore-errors
            (reconcile-archive-library-with-db
             :max-additions *db-archive-library-reconcile-max-additions*
             :interval *db-archive-library-reconcile-interval*
             :now now)))))))

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
