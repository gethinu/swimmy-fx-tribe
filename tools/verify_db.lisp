(require :sb-posix)
;; Ensure Quicklisp
(unless (find-package :ql)
  (let ((ql-setup "~/quicklisp/setup.lisp"))
    (if (probe-file ql-setup)
        (load ql-setup))))

(load "src/lisp/packages.lisp")
(load "src/lisp/packages-school.lisp")
(load "src/lisp/core/schema.lisp")

(defun verify-db ()
  (swimmy.db:init-db)
  (let ((strategies (swimmy.db:load-all-strategies)))
    (format t "✅ Loaded ~d strategies from DB.~%" (length strategies))
    (dolist (row (subseq strategies 0 5))
      (format t " - ~a~%" row))
    (if (> (length strategies) 0)
        (sb-ext:exit :code 0)
        (sb-ext:exit :code 1))))

(verify-db)
