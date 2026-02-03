;;; tools/migrate_strategy_sexp.lisp
(require :asdf)
(load "swimmy.asd")
(asdf:load-system :swimmy)

(in-package :cl-user)

(defparameter *source-db* "data/memory/swimmy.db")
(defparameter *dest-db* "data/memory/swimmy.db.migrated")
(defparameter *quarantine-dir* "data/memory/quarantine/")

(setf swimmy.school::*disable-auto-migration* t)

(defun %ensure-quarantine ()
  (ensure-directories-exist *quarantine-dir*))

(defun %copy-table (conn table)
  (sqlite:execute-non-query conn (format nil "INSERT INTO ~a SELECT * FROM old.~a" table table)))

(defun %migrate-strategies (conn)
  (let ((rows (sqlite:execute-to-list conn "SELECT name, indicators, entry, exit, sl, tp, volume, sharpe, profit_factor, win_rate, trades, max_dd, category, timeframe, generation, rank, symbol, direction, last_bt_time, hash, oos_sharpe, cpcv_median, cpcv_pass_rate, data_sexp, updated_at FROM old.strategies"))
        (ok 0)
        (bad 0)
        (qfile (merge-pathnames (format nil "strategies_quarantine_~d.sexp" (get-universal-time)) *quarantine-dir*)))
    (%ensure-quarantine)
    (with-open-file (q qfile :direction :output :if-exists :supersede)
      (dolist (row rows)
        (destructuring-bind (name indicators entry exit sl tp volume sharpe profit-factor win-rate trades max-dd category timeframe generation rank symbol direction last-bt-time hash oos-sharpe cpcv-median cpcv-pass-rate data-sexp updated-at) row
          (let* ((ind (swimmy.school::%parse-maybe-form indicators))
                 (ent (swimmy.school::%parse-maybe-form entry))
                 (exi (swimmy.school::%parse-maybe-form exit))
                 (result (swimmy.school::normalize-strategy-sexp data-sexp :indicators ind :entry ent :exit exi)))
            (if (eq (getf result :status) :ok)
                (progn
                  (incf ok)
                  (sqlite:execute-non-query conn
                    "INSERT OR REPLACE INTO strategies (name, indicators, entry, exit, sl, tp, volume, sharpe, profit_factor, win_rate, trades, max_dd, category, timeframe, generation, rank, symbol, direction, last_bt_time, hash, oos_sharpe, cpcv_median, cpcv_pass_rate, data_sexp, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                    name indicators entry exit sl tp volume sharpe profit-factor win-rate trades max-dd category timeframe generation rank symbol direction last-bt-time hash oos-sharpe cpcv-median cpcv-pass-rate (getf result :sexp) updated-at))
                (progn
                  (incf bad)
                  (format q "~s~%" (list :name name :reason (getf result :reason) :data_sexp data-sexp)))))))
      (format t "[MIGRATE] ok=~d bad=~d quarantine=~a~%" ok bad qfile))))

(defun run-migration ()
  (when (probe-file *dest-db*) (delete-file *dest-db*))
  (setf swimmy.core:*sqlite-conn* nil)
  (setf swimmy.core:*db-path-default* *dest-db*)
  (swimmy.school:init-db)
  (let ((conn (swimmy.core:get-db-connection)))
    (sqlite:execute-non-query conn (format nil "ATTACH DATABASE '~a' AS old" *source-db*))
    (%copy-table conn "trade_logs")
    (%copy-table conn "swap_history")
    (%copy-table conn "oos_queue")
    (%migrate-strategies conn)
    (sqlite:execute-non-query conn "DETACH DATABASE old"))
  (swimmy.core:close-db-connection)
  (format t "[MIGRATE] done -> ~a~%" *dest-db*))

(run-migration)
(sb-ext:exit)
