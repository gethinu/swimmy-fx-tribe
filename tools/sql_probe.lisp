;; tools/sql_probe.lisp
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

(let* ((name "RECRUIT-RND-1768783472-13")
       (row (swimmy.core:execute-to-list "SELECT name, sharpe, profit_factor, win_rate, max_dd, rank FROM strategies WHERE name = ?" name)))
  (format t "--- SQL PROBE for ~a ---~%" name)
  (if row
      (format t "DATA: ~s~%" row)
      (format t "NOT FOUND IN SQL.~%")))

(sb-ext:exit :code 0)
