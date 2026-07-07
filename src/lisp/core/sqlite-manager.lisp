;;; src/lisp/core/sqlite-manager.lisp
;;; V49.8: SQL Connection Management (Decoupled)
(in-package :swimmy.core)

(defvar *sqlite-conn* nil "Global SQLite connection handle.")
(defvar *db-path-default* "data/memory/swimmy.db")
(defvar *sqlite-primed* nil
  "T once sqlite3_initialize has been called (native-Windows workaround).")

(defun prime-sqlite ()
  "Explicitly run sqlite3_initialize before the first sqlite3_open.

   On MinGW/msvcrt-based SBCL (native Windows) the UCRT-built sqlite3.dll's
   *implicit* auto-init — triggered internally by the first sqlite3_open — jumps
   through a NULL pointer (SB-SYS:MEMORY-FAULT-ERROR, pc=0x0) and kills the image
   during INIT-DB. An *explicit* sqlite3_initialize primes the library so
   subsequent opens succeed. sqlite3_initialize is idempotent (SQLite guarantees
   it is safe to call repeatedly), so this is a harmless no-op on every platform."
  (unless *sqlite-primed*
    (setf *sqlite-primed* t)
    (let ((rc (ignore-errors (cffi:foreign-funcall "sqlite3_initialize" :int))))
      (when (and rc (not (eql rc 0)))
        (format t "[SQL] ⚠️ sqlite3_initialize returned ~a~%" rc)))))

(defun get-db-connection (&optional (path *db-path-default*))
  "Ensure and return a connection to the SQLite database."
  (unless *sqlite-conn*
    (prime-sqlite)
    (ensure-directories-exist path)
    (setf *sqlite-conn* (sqlite:connect path))
    ;; Improve concurrency between brain/school processes
    (ignore-errors (sqlite:execute-non-query *sqlite-conn* "PRAGMA journal_mode=WAL"))
    (ignore-errors (sqlite:execute-non-query *sqlite-conn* "PRAGMA synchronous=NORMAL"))
    (ignore-errors (sqlite:execute-non-query *sqlite-conn* "PRAGMA busy_timeout=5000"))
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
