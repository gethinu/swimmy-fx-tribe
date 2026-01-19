(require :sb-posix)
;; Ensure Quicklisp is loaded (usually by .sbclrc, but just in case)
(unless (find-package :ql)
  (let ((ql-setup "~/quicklisp/setup.lisp"))
    (if (probe-file ql-setup)
        (load ql-setup)
        (format t "⚠️ Quicklisp not found at ~a~%" ql-setup))))

;; Load Dependencies
(ql:quickload :cl-sqlite)
(ql:quickload :cl-json)
(ql:quickload :pzmq)
(ql:quickload :jsown)


(load "src/lisp/packages.lisp")
(load "src/lisp/packages-school.lisp")
;; Load State/Structs before Strategies
(load "src/lisp/school/school-state.lisp")
(load "src/lisp/school/school-strategy.lisp")
(load "src/lisp/strategies/strategies.lisp")
(load "src/lisp/core/schema.lisp")

;; Load SQLite Lib Explicitly
(cffi:load-foreign-library "libsqlite3.so.0")

(defpackage :swimmy.tools.migrate
  (:use :cl :swimmy.db :swimmy.school :swimmy.strategies))

(in-package :swimmy.tools.migrate)

(defun safe-get (plist key)
  (getf plist key))

(defun migrate-strategies ()
  (format t "[MIGRATE] STARTING DATA MIGRATION (LISP -> SQLITE)~%")
  
  ;; 1. Load Legacy Data
  ;; strategies.lisp already loads dynamic strategies into *strategy-knowledge-base*
  ;; BUT we need to apply params from school-optimized-params.lisp
  (load "src/lisp/school/school-optimized-params.lisp")
  
  ;; Note: apply-optimized-params might not be exported or might need calling.
  ;; Assuming it ran on load or we call it.
  (if (find-symbol "APPLY-OPTIMIZED-PARAMS" :swimmy.school)
      (funcall (find-symbol "APPLY-OPTIMIZED-PARAMS" :swimmy.school))
      (format t "[WARN] apply-optimized-params not found!~%"))

  (let ((count 0)
        (total (length swimmy.school::*strategy-knowledge-base*)))
    
    (swimmy.db:with-db (db)
      (sqlite:execute-non-query db "BEGIN TRANSACTION")
      
      (dolist (strat swimmy.school::*strategy-knowledge-base*)
        (incf count)
        (let* ((name (swimmy.school::strategy-name strat))
               (tier (string (swimmy.school::strategy-tier strat))) ;; keyword -> string
               (tf (format nil "~a" (swimmy.school::strategy-timeframe strat)))
               (symbol "USDJPY") ;; Default
               (sl (swimmy.school::strategy-sl strat))
               (tp (swimmy.school::strategy-tp strat))
               (indicators (swimmy.school::strategy-indicators strat))
               (gen (or (swimmy.school::strategy-generation strat) 0))
               ;; Build JSON for Params
               (params-obj (list :obj 
                                  (cons "sl" sl) 
                                  (cons "tp" tp) 
                                  (cons "indicators" indicators)
                                  (cons "volume" (swimmy.school::strategy-volume strat))))
               (params-json (cl-json:encode-json-to-string params-obj)))
               
          ;; Insert Strategy
          (handler-case
              (sqlite:execute-non-query db
                "INSERT OR REPLACE INTO strategies (name, tier, timeframe, symbol, params_json, generation, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
                name tier tf symbol params-json gen (get-universal-time))
            (error (e)
              (format t "[ERR] Failed to insert ~a: ~a~%" name e)))
              
          ;; Insert Performance (Extract from object if available)
          (let ((sharpe (swimmy.school::strategy-sharpe strat))
                (pf (swimmy.school::strategy-profit-factor strat))
                (wr (swimmy.school::strategy-win-rate strat))
                (trades (swimmy.school::strategy-total-trades strat)))
            
             (sqlite:execute-non-query db
               "INSERT OR REPLACE INTO performance (strategy_name, sharpe_ratio, profit_factor, win_rate, total_trades) VALUES (?, ?, ?, ?, ?)"
               name (or sharpe 0.0) (or pf 0.0) (or wr 0.0) (or trades 0)))
        
        (when (zerop (mod count 100))
          (format t "[MIGRATE] Processed ~d/~d strategies...~%" count total))))

      (sqlite:execute-non-query db "COMMIT"))
      
    (format t "[MIGRATE] COMPLETED. Migrated ~d strategies.~%" count)))

(migrate-strategies)
(sb-ext:exit)
