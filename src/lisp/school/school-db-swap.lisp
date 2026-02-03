;;; school-db-swap.lisp - Swap history helpers

(in-package :swimmy.school)

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
