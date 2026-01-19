(require :sb-posix)
(load "src/lisp/setup.lisp") 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cffi))

(cffi:define-foreign-library libsqlite3
  (:unix (:or "libsqlite3.so.0" "libsqlite3.so"))
  (t (:default "libsqlite3")))

(cffi:use-foreign-library libsqlite3)

(handler-case
    (progn
      (ql:quickload :cl-sqlite)
      (format t "✅ SQLite Loaded Successfully!~%")
      (sqlite:with-open-database (db "boot_test.db")
         (sqlite:execute-non-query db "create table if not exists test (id integer primary key, name text)")
         (sqlite:execute-non-query db "insert into test (name) values ('Swimmy')")
         (format t "✅ DB Created & Wrote Data!~%")))
  (error (e)
    (format t "❌ Failed: ~a~%" e)
    (sb-ext:exit :code 1)))
