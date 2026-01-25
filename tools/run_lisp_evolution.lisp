;; tools/run_lisp_evolution.lisp
(in-package :cl-user)
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(format t "[EVO] Loading Swimmy System...~%")
(handler-bind ((style-warning #'muffle-warning)
               (warning #'muffle-warning))
  (asdf:load-system :swimmy))

(format t "[EVO] Initializing System...~%")
(swimmy.main::initialize-system)

(format t "[EVO] Starting Native Evolution Service...~%")
(swimmy.school:start-evolution-service)
