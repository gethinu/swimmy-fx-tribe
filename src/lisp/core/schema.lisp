
(defpackage :swimmy.db
  (:use :cl :sqlite)
  (:export :*db-path*
           :init-db
           :with-db
           :save-strategy
           :load-all-strategies
           :save-trade-result
           :find-strategy-by-name
           :find-strategies-by-tier))

(in-package :swimmy.db)

;; Core DB Settings
(defparameter *db-path* "data/swimmy.db")

(defmacro with-db ((db-var) &body body)
  `(sqlite:with-open-database (,db-var *db-path*)
     ,@body))

(defun init-db ()
  "Initialize the SQLite database schema"
  (ensure-directories-exist *db-path*)
  
  ;; Load foreign library explicitly for this environment
  (cffi:load-foreign-library "libsqlite3.so.0")

  (with-db (db)
    ;; 1. Strategy Definition Table
    (sqlite:execute-non-query db
      "CREATE TABLE IF NOT EXISTS strategies (
         name TEXT PRIMARY KEY,
         tier TEXT CHECK(tier IN ('GRAVEYARD', 'INCUBATOR', 'TRAINING', 'BATTLEFIELD', 'VETERAN', 'LEGEND')),
         timeframe TEXT,
         symbol TEXT,
         params_json TEXT,  -- JSON blob for SMA-SHORT, SL, TP etc.
         created_at INTEGER,
         generation INTEGER,
         parent_id TEXT
       )")

    ;; 2. Performance Metrics Table (One-to-One with Strategy, Updated periodically)
    (sqlite:execute-non-query db
      "CREATE TABLE IF NOT EXISTS performance (
         strategy_name TEXT PRIMARY KEY,
         total_trades INTEGER DEFAULT 0,
         win_rate REAL DEFAULT 0.0,
         profit_factor REAL DEFAULT 0.0,
         sharpe_ratio REAL DEFAULT 0.0,
         max_drawdown REAL DEFAULT 0.0,
         last_net_profit REAL DEFAULT 0.0,
         FOREIGN KEY(strategy_name) REFERENCES strategies(name)
       )")

    ;; 3. Trade History Table (For detailed analysis)
    (sqlite:execute-non-query db
      "CREATE TABLE IF NOT EXISTS trades (
         ticket INTEGER PRIMARY KEY,
         strategy_name TEXT,
         symbol TEXT,
         type TEXT, -- BUY/SELL
         lots REAL,
         open_price REAL,
         close_price REAL,
         profit REAL,
         open_time INTEGER,
         close_time INTEGER,
         FOREIGN KEY(strategy_name) REFERENCES strategies(name)
       )")

    ;; 4. Evolution Tree (Genealogy)
    (sqlite:execute-non-query db
      "CREATE TABLE IF NOT EXISTS evolutions (
         child_name TEXT,
         parent1_name TEXT,
         parent2_name TEXT,
         mutation_type TEXT,
         timestamp INTEGER
       )")
       

    (format t "[DB] Schema Initialized at ~a~%" *db-path*)))

;; ==========================================
;; DAO Implementation
;; ==========================================

(defun save-strategy (name tier timeframe symbol params-json gen &optional parent-id)
  "Save or Update a strategy"
  (with-db (db)
    (sqlite:execute-non-query db
      "INSERT OR REPLACE INTO strategies (name, tier, timeframe, symbol, params_json, generation, parent_id, created_at) 
       VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      name tier timeframe symbol params-json gen parent-id (get-universal-time))))

(defun load-all-strategies ()
  "Load all strategies as a list of property lists (or objects if mapped)"
  (with-db (db)
    (sqlite:execute-to-list db "SELECT name, tier, timeframe, symbol, params_json, generation FROM strategies")))

(defun find-strategy-by-name (name)
  (with-db (db)
    (let ((rows (sqlite:execute-to-list db "SELECT name, tier, timeframe, symbol, params_json, generation FROM strategies WHERE name = ?" name)))
      (first rows))))

(defun find-strategies-by-tier (tier)
  (with-db (db)
    (sqlite:execute-to-list db "SELECT name, timeframe, params_json FROM strategies WHERE tier = ?" tier)))

(defun save-trade-result (ticket strategy-name symbol type lots open-price close-price profit open-time close-time)
  (with-db (db)
    (sqlite:execute-non-query db
      "INSERT INTO trades (ticket, strategy_name, symbol, type, lots, open_price, close_price, profit, open_time, close_time)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ticket strategy-name symbol type lots open-price close-price profit open-time close-time)
    
    ;; Update Performance Cache
    (sqlite:execute-non-query db
       "UPDATE performance 
        SET total_trades = total_trades + 1,
            last_net_profit = last_net_profit + ?
        WHERE strategy_name = ?"
       profit strategy-name)
       
    ;; If performance row doesn't exist, ignore (handled by periodic sync)
    ))
