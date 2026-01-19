
;; tools/verify_sharded_load.lisp

;; 1. Load Packages
(require :sb-posix)
(unless (find-package :ql)
  (let ((ql-setup "~/quicklisp/setup.lisp"))
    (if (probe-file ql-setup)
        (load ql-setup)
        (format t "⚠️ Quicklisp not found at ~a~%" ql-setup))))

(ql:quickload :cl-json)
(ql:quickload :cffi)
(ql:quickload :pzmq)
(ql:quickload :jsown)

(load "src/lisp/packages.lisp")
(load "src/lisp/packages-school.lisp")
(load "src/lisp/core/globals.lisp")
(load "src/lisp/dsl.lisp")
(load "src/lisp/core/persistence.lisp")
(load "src/lisp/school/school-state.lisp")
(load "src/lisp/school/school-strategy.lisp")

;; 2. Load Strategies (Should trigger library load)
(format t "------------- LOADING STRATEGIES ------------~%")
(load "src/lisp/strategies/strategies.lisp")

;; 3. Verify
(format t "------------- VERIFICATION ------------~%")
(if (> (length swimmy.school::*strategy-knowledge-base*) 0)
    (format t "✅ SUCCESS: Loaded ~d strategies from Sharded Library!~%" (length swimmy.school::*strategy-knowledge-base*))
    (format t "❌ FAILURE: Knowledge base is empty!~%"))

(sb-ext:exit)
