;;; school-db-oos.lisp - OOS queue helpers

(in-package :swimmy.school)

(declaim (special *oos-min-sharpe*))

(defun enqueue-oos-request (name request-id &key (status "sent") (requested-at (get-universal-time)))
  "Insert or replace an OOS request. Keeps the latest row per strategy name."
  ;; Replace any older rows for the same strategy to avoid accumulation.
  (with-transaction
    (execute-non-query "DELETE FROM oos_queue WHERE name=?" name)
    (execute-non-query
     "INSERT OR REPLACE INTO oos_queue (request_id, name, requested_at, status, last_error)
      VALUES (?, ?, ?, ?, NULL)"
     request-id name requested-at status)))

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

(defun cleanup-oos-queue-on-startup ()
  "Clear all OOS queue rows on startup to avoid stale requests."
  (handler-case
      (progn
        (ignore-errors (init-db))
        (execute-non-query "DELETE FROM oos_queue")
        (format t "[OOS] 🧹 Cleared oos_queue on startup.~%")
        t)
    (error (e)
      (format t "[OOS] ⚠️ Failed to clear oos_queue: ~a~%" e)
      nil)))

(defun %coerce-int (v)
  (cond
    ((numberp v) (truncate v))
    ((stringp v) (ignore-errors (parse-integer v)))
    (t nil)))

(defun fetch-oos-queue-stats ()
  "Return plist: :pending :errors :oldest-age :oldest-requested-at."
  (handler-case
      (progn
        (ignore-errors (init-db))
        (let* ((pending (or (execute-single "SELECT count(*) FROM oos_queue WHERE status != 'error'") 0))
               (errors (or (execute-single "SELECT count(*) FROM oos_queue WHERE status = 'error'") 0))
               (oldest-raw (execute-single "SELECT MIN(requested_at) FROM oos_queue WHERE status != 'error'"))
               (oldest (and oldest-raw (%coerce-int oldest-raw)))
               (age (and oldest (- (get-universal-time) oldest))))
          (list :pending pending :errors errors :oldest-requested-at oldest :oldest-age age)))
    (error (e)
      (list :error (format nil "~a" e)))))

(defun report-oos-db-metrics ()
  "Return plist: :sent :retry :success :failure sourced from DB."
  (handler-case
      (progn
        (ignore-errors (init-db))
        (let* ((min-sharpe (if (boundp '*oos-min-sharpe*) *oos-min-sharpe* 0.3))
               (sent (or (execute-single "SELECT count(*) FROM oos_queue WHERE status='sent'") 0))
               (retry (or (execute-single "SELECT count(*) FROM oos_queue WHERE status='retry'") 0))
               (success (or (execute-single
                             "SELECT count(*) FROM strategies WHERE oos_sharpe IS NOT NULL AND oos_sharpe >= ?"
                             min-sharpe)
                            0))
               (failure (or (execute-single
                             "SELECT count(*) FROM strategies WHERE oos_sharpe IS NOT NULL AND oos_sharpe < ?"
                             min-sharpe)
                            0)))
          (list :sent sent :retry retry :success success :failure failure)))
    (error (e)
      (list :error (format nil "~a" e)))))
