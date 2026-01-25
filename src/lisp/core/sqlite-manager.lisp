;;; src/lisp/core/sqlite-manager.lisp
;;; V49.8: SQL Connection Management (Decoupled)
(in-package :swimmy.core)

(defvar *sqlite-conn* nil "Global SQLite connection handle.")
(defvar *db-path-default* "data/memory/swimmy.db")

(defun get-db-connection (&optional (path *db-path-default*))
  "Ensure and return a connection to the SQLite database."
  (unless *sqlite-conn*
    (ensure-directories-exist path)
    (setf *sqlite-conn* (sqlite:connect path))
    (format t "[SQL] 🗄️ Connected to database: ~a~%" path))
  *sqlite-conn*)

(defun close-db-connection ()
  "Close the global SQLite connection if open."
  (when *sqlite-conn*
    (sqlite:disconnect *sqlite-conn*)
    (setf *sqlite-conn* nil)
    (format t "[SQL] 🚪 Connection closed.~%")))

(defun execute-non-query (sql &rest params)
  "Execute a non-query SQL command."
  (let ((conn (get-db-connection)))
    (apply #'sqlite:execute-non-query conn sql params)))

(defun execute-to-list (sql &rest params)
  "Execute query and return results as a list of rows."
  (let ((conn (get-db-connection)))
    (apply #'sqlite:execute-to-list conn sql params)))

(defun execute-single (sql &rest params)
  "Execute query and return the first column of the first row."
  (let ((conn (get-db-connection)))
    (apply #'sqlite:execute-single conn sql params)))

(defmacro with-transaction (&body body)
  "Wrap body in an SQLite transaction."
  `(let ((conn (get-db-connection)))
     (sqlite:with-transaction conn
       ,@body)))
