;;; src/lisp/core/sqlite-manager.lisp
;;; V49.8: SQL Connection Management (Decoupled)
(in-package :swimmy.core)

(defvar *sqlite-conn* nil "Global SQLite connection handle.")
(defvar *db-path-default* "data/memory/swimmy.db")
(defvar *sqlite-primed* nil
  "T once sqlite3_initialize has been called (native-Windows workaround).")

;;; ---------------------------------------------------------------------------
;;; Test-DB directory override (permanent CI fix, 2026-08-11)
;;; ---------------------------------------------------------------------------
;;; SQLite in WAL mode fails with "disk I/O error" (SQLITE_IOERR) when its DB lives
;;; on the Windows drive seen through WSL as /mnt/c (DrvFs): the WAL "-shm" file is
;;; memory-mapped and DrvFs does not support the required mmap/locking. Putting test
;;; DBs on a native Linux filesystem (e.g. /tmp = ext4 under WSL2) avoids this.
;;;
;;; SWIMMY_TEST_DB_DIR lets the CI point test databases at such a native directory.
;;; UNSET => historical behaviour (data/memory/), so default runs are byte-identical.

(defun test-db-dir ()
  "Directory for test databases: SWIMMY_TEST_DB_DIR when set, else \"data/memory\"
   (the historical default => byte-parity when the env var is unset)."
  (let ((d (uiop:getenv "SWIMMY_TEST_DB_DIR")))
    (if (and d (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) d))))
        (string-trim '(#\Space #\Tab #\Newline #\Return) d)
        "data/memory")))

(defun test-db-path (filename)
  "Resolve FILENAME under (test-db-dir). With SWIMMY_TEST_DB_DIR unset this returns
   data/memory/FILENAME exactly as before."
  (namestring (merge-pathnames filename (uiop:ensure-directory-pathname (test-db-dir)))))

(defun test-db-redirect-active-p ()
  "True only when SWIMMY_TEST_DB_DIR is explicitly set (i.e. the CI opted in)."
  (let ((d (uiop:getenv "SWIMMY_TEST_DB_DIR")))
    (and d (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) d))))))

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
