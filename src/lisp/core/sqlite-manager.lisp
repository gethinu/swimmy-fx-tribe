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

;;; ---------------------------------------------------------------------------
;;; P5 (Thread B / persistence hardening)
;;; ---------------------------------------------------------------------------

(defmacro with-immediate-transaction (&body body)
  "Like WITH-TRANSACTION but issues BEGIN IMMEDIATE, taking the RESERVED write
   lock up front. cl-sqlite's WITH-TRANSACTION uses a DEFERRED begin, so a
   read-then-write (e.g. graveyard upsert) can start a shared transaction and
   then hit SQLITE_BUSY when it tries to upgrade to a write while the other
   daemon (brain vs school) holds the lock — losing the write. BEGIN IMMEDIATE
   serializes writers cleanly. Rolls back on any non-local exit."
  (let ((conn (gensym "CONN")) (ok (gensym "OK")))
    `(let ((,conn (get-db-connection)) (,ok nil))
       (sqlite:execute-non-query ,conn "BEGIN IMMEDIATE")
       (unwind-protect
            (multiple-value-prog1 (progn ,@body)
              (sqlite:execute-non-query ,conn "COMMIT")
              (setf ,ok t))
         (unless ,ok
           (ignore-errors (sqlite:execute-non-query ,conn "ROLLBACK")))))))

(defparameter *max-data-sexp-length* 262144
  "Hard upper bound (chars) on a strategy's serialized data_sexp blob. A healthy
   strategy struct serializes to a few KB; anything past 256KB is treated as
   corruption/pathology and its blob is NOT persisted (metrics still are), so a
   poison blob can't propagate and bloat the DB. Tunable.")

(defun sha256-hex (string)
  "Lowercase 64-char hex SHA-256 of STRING (UTF-8). Integrity checksum for the
   data_sexp column: computed at write, re-verified by the persistence audit so
   silent bit-rot/truncation is caught instead of being swallowed by safe-read.
   ironclad is already a hard dependency (swimmy.asd)."
  (when (stringp string)
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence
      :sha256 (sb-ext:string-to-octets string :external-format :utf-8)))))
